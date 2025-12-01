library(tidyverse)
library(phyloseq)
library(vegan)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- sam[,c(1:212, 214, 213)]

health <- sam[,78:84]

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
  taxmat <- as.data.frame(tax_table(ps_obj))
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
  fit_data <- Maaslin2(
    input_data = as.data.frame(t(feature_table)),
    input_metadata = meta,
    output = tempdir(),   
    fixed_effects = fixed_effects,
    normalization = "TSS",
    min_prevalence = min_prevalence
  )
  
  # Results
  results <- fit_data$results
  results_var <- results[results$metadata == variable, ]
  results_var <- results_var %>%
    dplyr::mutate(sig = ifelse(qval < qval_sig_max, "Significant", "NS"))
  
  # Only keep one result per genus
  results_var_unique <- results_var %>%
    group_by(feature) %>%
    slice_min(order_by = qval, n = 1, with_ties = FALSE) %>%
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
  
  p <- ggplot(results_var_unique, aes(x = coef, y = -log10(qval), color = sig)) +
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
    theme(legend.position = "none")
  
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
    variable = "FoodSecure_vs_FoodInsecure", 
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
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(size = 14)
      ) + 
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
  
  if (length(plot_list) > 0) {
    ggsave(filename = paste0(variable, "_maaslin_boxplots.png"), plot = plot_patch, width = 14, height = 8)
  }
}

metadata <- as.data.frame(sample_data(ps))
SVs <- as.data.frame(otu_table(ps))

kitchen_floor_maaslin2 <- run_maaslin2_and_plot(ps, "Kitchen_Material", qval_sig_max = 0.1)

kitchen_floor_boxplots <- plot_all_significant_boxplots(
  ps_obj = ps,
  maaslin2_table = kitchen_floor_maaslin2$table,
  variable = "Kitchen_Material",
  variable_name = "Kitchen_Material"
)


