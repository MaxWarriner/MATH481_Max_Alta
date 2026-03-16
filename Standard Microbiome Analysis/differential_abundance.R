
# Microbiome Differential Abundance ---------------------------------------


library(tidyverse)
library(phyloseq)
library(vegan)
library(maaslin3)
library(ggrepel)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- data.frame(sam[,c(1:212, 214, 213)]) |>
  mutate(diarrhea = ifelse(diarrhea == 1, 'yes', 'no'), 
         bloating = ifelse(bloating == 1, 'yes', 'no'), 
         abdominalpain = ifelse(abdominalpain == 1, 'yes', 'no'), 
         lower_appetite = ifelse(lower_appetite == 1, 'yes', 'no'))

sample_data(ps) <- sam

colnames(phyloseq::tax_table(ps)) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

run_maaslin2_and_plot <- function(
    ps_obj, 
    variable, 
    title_name = gsub(pattern = "_", replacement = " ", variable), 
    taxlevel = "Genus",
    qval_sig_max = 0.1,
    min_prevalence = 0.05,
    min_samples = 5,               # Minimum number of samples with abundance > 0
    min_abundance_mean = 0.0005,    # Minimum mean relative abundance across all samples
    min_abundance_max  = 0.001,     # Minimum *max* relative abundance in any sample
    adjust_vars = c("Age", "sex"),
    remove_unassigned = TRUE
) {
  # Prepare feature table (taxa as rows, samples as columns)
  otumat <- SVs
  if (!taxa_are_rows(ps_obj)) otumat <- data.frame(t(otumat))
  
  # Aggregate at desired taxonomic level
  taxmat <- as.data.frame(phyloseq::tax_table(ps_obj))
  if (!(taxlevel %in% colnames(taxmat))) stop("taxlevel not in taxonomy table!")
  
  # Assign "Unassigned" to missing genus
  otumat$Taxon <- taxmat[rownames(otumat), taxlevel]
  otumat$Taxon[is.na(otumat$Taxon) | otumat$Taxon == "" ] <- "Unassigned"
  
  # Aggregate counts at genus level
  feature_table <- otumat %>%
    group_by(Taxon) %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
    column_to_rownames("Taxon")
  
  # Convert to relative abundance (to apply abundance filters)
  feature_table_rel <- sweep(feature_table, 2, colSums(feature_table), "/")
  
  # Filtering step: prevalence, mean abundance, and max abundance
  present_counts <- rowSums(feature_table_rel > 0)
  mean_abund <- rowMeans(feature_table_rel)
  max_abund <- apply(feature_table_rel, 1, max)
  
  keep_taxa <- (present_counts > (min_prevalence * ncol(feature_table_rel))) &
    (present_counts >= min_samples) &
    (mean_abund >= min_abundance_mean | max_abund >= min_abundance_max)
  
  feature_table <- feature_table[keep_taxa, ]
  feature_table_rel <- feature_table_rel[keep_taxa, ]
  
  # Prepare metadata
  meta <- as(sample_data(ps_obj), "data.frame")
  meta <- meta[ , , drop = FALSE]
  
  # Match samples between metadata and feature table
  colnames(feature_table) <- gsub("X", "", colnames(feature_table))
  colnames(feature_table) <- gsub("\\.", "-", colnames(feature_table))
  common_samples <- intersect(colnames(feature_table), rownames(meta))
  feature_table <- feature_table[ , common_samples, drop = FALSE]
  meta <- meta[common_samples, , drop = FALSE]
  
  # Compose fixed effects
  fixed_effects <- unique(c(variable, adjust_vars))
  fixed_effects <- fixed_effects[fixed_effects %in% colnames(meta)]
  
  # Run MaAsLin2 
  fit_data <- maaslin3(
    input_data = as.data.frame(t(feature_table)),
    input_metadata = meta,
    output = tempdir(),   
    fixed_effects = fixed_effects,
    normalization = "TSS",
    min_prevalence = min_prevalence
  )
  
  # Results
  results <- fit_data$fit_data_abundance$results
  results_var <- results[results$metadata == variable, ]
  results_var <- results_var %>%
    dplyr::mutate(sig = ifelse(qval_joint < qval_sig_max, "Significant", "NS"))
  
  # Only keep one result per genus
  results_var_unique <- results_var %>%
    group_by(feature) %>%
    slice_min(order_by = qval_joint, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Prevalence and abundance calculation (add to output)
  prevalence_df <- data.frame(
    feature = rownames(feature_table_rel),
    prevalence = rowSums(feature_table_rel > 0),
    prevalence_frac = rowSums(feature_table_rel > 0) / ncol(feature_table_rel),
    mean_abund = rowMeans(feature_table_rel),
    max_abund = apply(feature_table_rel, 1, max)
  )
  results_var_unique <- left_join(results_var_unique, prevalence_df, by = "feature")
  
  # Keep Only Genus Names That are in the Original Genus Column
  genus_list <- unique(taxmat[[taxlevel]])
  genus_list <- genus_list[!is.na(genus_list) & genus_list != ""]
  if (remove_unassigned) {
    genus_list <- genus_list[genus_list != "Unassigned"]
  }
  results_var_unique <- results_var_unique %>% filter(feature %in% genus_list)
  
  # Plot: Label rare significant genera
  # Add effect size label
  results_var_unique <- results_var_unique %>%
    mutate(
      effect_size_label = paste0(feature, " (β = ", round(coef, 3), ")"),
      label_flag = ifelse(
        sig == "Significant" & prevalence < min_samples,
        paste0(feature, " (rare, β=", round(coef,3), ")"),
        ifelse(sig == "Significant", paste0(feature, " (β=", round(coef,3), ")"), "")
      )
    )
  
  p <- ggplot(results_var_unique, aes(x = coef, y = -log10(qval_joint), color = sig)) +
    geom_point() +
    geom_text_repel(
      aes(label = label_flag),
      size = 3,
      max.overlaps = 12,
      color = "red",
      force = 2,
      box.padding = 0.5
    ) +
    scale_color_manual(values = c("Significant" = "red", "NS" = "black")) +
    labs(
      x = "Effect Size (β coefficient)",
      y = "-log10(FDR)",
      title = title_name
    ) +
    theme_minimal() +
    theme(legend.position = "none") + 
    theme(
      plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26, margin = margin(t = 10)), 
      legend.spacing.y = unit(1, "in"))
  
  ggsave(filename = paste(variable, "_maslin_volcano.png", sep = ""), plot = p, width = 8, height = 6)
  
  print(p)
  n_rare_sig <- sum(results_var_unique$sig == "Significant" & results_var_unique$prevalence < min_samples)
  if (n_rare_sig > 0) {
    message(sprintf("Warning: %d significant genera are present in fewer than %d samples (labeled as (rare)). Interpret with caution!", n_rare_sig, min_samples))
  }
  return(list(plot = p, table = results_var_unique, all_results = results))
}

plot_all_significant_boxplots <- function(
    ps_obj, 
    maaslin2_table, 
    variable, 
    variable_name = variable,
    taxlevel = "Genus"
) {
  
  sig_genera <- maaslin2_table %>%
    filter(sig == "Significant", metadata == variable) %>%
    pull(feature) %>%
    unique()
  
  # Helper function for a single genus
  plot_single <- function(genus, taxlevel = "Genus") {
    # Extract p- and q-values
    stats_row <- maaslin2_table %>%
      filter(feature == genus, metadata == variable) %>%
      arrange(qval) %>%
      slice(1)
    
    qval <- ifelse(nrow(stats_row) == 0, NA, stats_row$qval)
    pval <- ifelse(nrow(stats_row) == 0, NA, stats_row$pval)
    
    # Format nicely (not scientific notation)
    qval_label <- ifelse(is.na(qval), "NA", formatC(qval, digits = 3, format = "f"))
    pval_label <- ifelse(is.na(pval), "NA", formatC(pval, digits = 4, format = "f"))
    
    effect_size <- stats_row$coef[1]
    effect_label <- formatC(effect_size, digits = 3, format = "f")
    
    auto_title <- paste0(
      genus,
      " (β = ", effect_label,
      ", p = ", pval_label,
      ", q = ", qval_label, ")"
    )
    
    # Create data for plotting
    otumat <- SVs
    if (!taxa_are_rows(ps_obj)) otumat <- as.data.frame(t(otumat))
    taxmat <- as.data.frame(tax_table(ps_obj))
    otumat$Taxon <- taxmat[rownames(otumat), taxlevel]
    otumat$Taxon <- ifelse(is.na(otumat$Taxon), "Other", otumat$Taxon)
    
    feature_table <- otumat %>%
      group_by(Taxon) %>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>%
      column_to_rownames("Taxon")
    
    feature_table_rel <- sweep(feature_table, 2, colSums(feature_table), "/")
    if (!genus %in% rownames(feature_table_rel)) return(NULL)
    
    genus_abund <- as.numeric(feature_table_rel[genus, ])
    sample_names <- colnames(feature_table_rel)
    meta <- metadata
    meta <- meta[sample_names, , drop = FALSE]
    
    df <- data.frame(
      Sample = sample_names,
      Abundance = genus_abund,
      Group = as.factor(meta[[variable]])
    )
    
    ggplot(df, aes(x = Group, y = Abundance, fill = Group)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.25, color = "black") +
      labs(
        title = auto_title,
        x = "",
        y = "Relative Abundance"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 28),
        axis.text  = element_text(size = 26),
        legend.title = element_text(size = 28),
        legend.text  = element_text(size = 26, margin = margin(t = 10)), 
        legend.spacing.y = unit(1, "in")) + 
      geom_hline(yintercept = 0)
  }
  
  # Loop and collect
  plot_list <- lapply(sig_genera, plot_single)
  names(plot_list) <- sig_genera
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  
  print(paste("Created", length(plot_list), "boxplots for significant genera."))
  
  plot_patch <- wrap_plots(plot_list) &
    plot_annotation(
      title = gsub(pattern = "_", replacement = " ", variable_name), 
      theme = theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    )
  
  print(plot_patch)
  
  return(plot_patch)
  
  # if (length(plot_list) > 0) {
  #   ggsave(filename = paste0(variable, "_maaslin_boxplots.png"), plot = plot_patch, width = 14, height = 8)
  # }
}

metadata <- as.data.frame(sample_data(ps))
SVs <- as.data.frame(otu_table(ps))


diarrhea_maaslin2 <- run_maaslin2_and_plot(ps, "diarrhea", qval_sig_max = 0.1, title_name = "(A) Genus Abundance")$p

diarrhea_maaslin2 <- diarrhea_maaslin2 + 
  geom_hline(yintercept = 0) 

# diarrhea_maaslin2 <- run_maaslin2_and_plot(ps, "diarrhea", qval_sig_max = 0.1, taxlevel = "Phylum")


abdominalpain_maaslin2 <- run_maaslin2_and_plot(ps, "abdominalpain", qval_sig_max = 0.1, title_name = "(A) Genus Abundance")$p + 
  geom_hline(yintercept = 0)

# abdominalpain_maaslin2 <- run_maaslin2_and_plot(ps, "abdominalpain", qval_sig_max = 0.1, taxlevel = "Phylum")

bloating_maaslin2 <- run_maaslin2_and_plot(ps, "bloating", qval_sig_max = 0.1, title_name = "(A) Genus Abundance")$p + 
  geom_hline(yintercept = 0)
#bloating_maaslin2 <- run_maaslin2_and_plot(ps, "bloating", qval_sig_max = 0.1, taxlevel = "Phylum")


low_appetite_maaslin2 <- run_maaslin2_and_plot(ps, "lower_appetite", qval_sig_max = 0.1, title_name = "(A) Genus Abundance")$p + 
  geom_hline(yintercept = 0)
#low_appetite_maaslin2 <- run_maaslin2_and_plot(ps, "lower_appetite", qval_sig_max = 0.1, taxlevel = "Phylum")


# Metabolome Differential Abundance ---------------------------------------

library(tidyverse)
library(phyloseq)
library(vegan)
library(maaslin3)
library(ggrepel)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- data.frame(sam[,c(1:212, 214, 213)]) |>
  mutate(diarrhea = ifelse(diarrhea == 1, 'yes', 'no'), 
         bloating = ifelse(bloating == 1, 'yes', 'no'), 
         abdominalpain = ifelse(abdominalpain == 1, 'yes', 'no'), 
         lower_appetite = ifelse(lower_appetite == 1, 'yes', 'no'))


norm_dat <- tibble(metab = colnames(metabolite_data), p = rep(NA, ncol(metabolite_data)))

for (i in 1:ncol(metabolite_data)){
  model <- lm(paste(colnames((metabolite_data))[i], " ~ diarrhea + Age + sex"), data = sam)
  res <- model$residuals
  norm_dat$p[i] <- shapiro.test(res)$p.value
}

mean(norm_dat$p<0.05)

log_dat <- tibble(metab = colnames(metabolite_data), p = rep(NA, ncol(metabolite_data)))

for (i in 1:ncol(metabolite_data)){
  model <- lm(paste("log(",colnames((metabolite_data))[i],"+ 1) ~ diarrhea + Age + sex", sep = ""), data = sam)
  res <- model$residuals
  log_dat$p[i] <- shapiro.test(res)$p.value
}

mean(log_dat$p<0.001)

metabolite_data <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

keep_samples <- intersect(rownames(metabolite_data), rownames(sam))
sam <- sam[keep_samples,]
metabolite_data <- metabolite_data[keep_samples,]

sam <- cbind(sam, metabolite_data)

#function for diff abundance (FROM SCRATCH)
# who said I couldn't code??



metabolite_differential_abundance <- function(data = sam, metabolite_data = metabolite_data, variable, q_thresh = 0.1, title_name = gsub(pattern = "_", replacement = " ", x = variable)){
  
  testing_data <- tibble(metabolite = colnames(metabolite_data), 
                         beta = rep(NA, length(colnames(metabolite_data))), 
                         p = rep(NA, length(colnames(metabolite_data))), 
                         q = rep(NA, length(colnames(metabolite_data))))
  
  
  for (i in 1:length(colnames(metabolite_data))){
    
    model_formula <- paste("log(", colnames(metabolite_data)[i], " + 1) ~ ", variable, " + Age + sex", sep = "")
    
    lm_mod <- lm(model_formula, data = data.frame(data))
    
    coefs <- data.frame(summary(lm_mod)$coefficients)
    
    testing_data$beta[i] <- coefs$Estimate[2]
    testing_data$p[i] <- coefs$Pr...t..[2]
    
  }
  
  
  testing_data$q <- p.adjust(testing_data$p, method = "BH")
  
  testing_data <- testing_data |>
    mutate(sig = ifelse(q < q_thresh, "Significant", "N.S."),
           effect_size_label = ifelse(q < q_thresh, paste0("(β = ", round(beta, 3), ", p = ", round(p, 3), ", q = ", round(q, 3), ")"), ""))

  
  p <- ggplot(testing_data, aes(x = beta, y = -log10(q), color = sig)) +
    geom_point() +
    geom_text_repel(
      aes(label = effect_size_label),
      size = 3,
      max.overlaps = 12,
      color = "red",
      force = 2,
      box.padding = 0.5
    ) +
    scale_color_manual(values = c("Significant" = "red", "N.S." = "black")) +
    labs(
      x = "Effect Size (β coefficient)",
      y = "-log10(FDR)",
      title = title_name
    ) +
    theme_minimal() +
    theme(legend.position = "none", 
      plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26, margin = margin(t = 10)), 
      legend.spacing.y = unit(1, "in")) + 
    geom_hline(yintercept = 0)
  
  print(p)
  
  return(list(p = p, testing_data = testing_data))
  
}

diarrhea_metab <- metabolite_differential_abundance(data = sam, metabolite_data = metabolite_data, variable = "diarrhea", title_name = "(B) Metabolite Abundance")$p 

abdominalpain_metab <- metabolite_differential_abundance(data = sam, metabolite_data = metabolite_data, variable = "abdominalpain", title_name = "(B) Metabolite Abundance")$p

lower_appetite_metab <- metabolite_differential_abundance(data = sam, metabolite_data = metabolite_data, variable = "lower_appetite", title_name = "(B) Metabolite Abundance")$p

bloating_metab <- metabolite_differential_abundance(data = sam, metabolite_data = metabolite_data, variable = "bloating", title_name = "(B) Metabolite Abundance")$p



#Combined Plots
library(patchwork)
setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")

diarrhea <- diarrhea_maaslin2 + diarrhea_metab &
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))
ggsave(filename = "diarrhea_volcanoes.png", plot = diarrhea, dpi = 600, width = 14, height = 6)


abdominalpain <- abdominalpain_maaslin2 + abdominalpain_metab &
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))
ggsave(filename = "abdominalpain_volcanoes.png", plot = abdominalpain, dpi = 600, width = 14, height = 6)


bloating <- bloating_maaslin2 + bloating_metab &
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))
ggsave(filename = "bloating_volcanoes.png", plot = bloating, dpi = 600, width = 14, height = 6)


low_appetite <- low_appetite_maaslin2 + lower_appetite_metab &
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))
ggsave(filename = "low_appetite_volcanoes.png", plot = diarrhea, dpi = 600, width = 14, height = 6)




