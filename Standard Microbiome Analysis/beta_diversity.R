library(tidyverse)
library(phyloseq)
library(DESeq2)
library(vegan)
library(stats)
library(MicrobiotaProcess)
library(patchwork)
library(microeco)
library(MicEco)
library(micropower)
library(simr)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

#Microbiome Analysis

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- data.frame(sam) |>
  mutate(calories_group = ifelse(calories <= median(sam$calories), "low", "high"), 
         meat_portions_group = ifelse(meat_portions <= median(sam$meat_portions), "low", "high"))

ps@sam_data$calories_group = sam$calories_group

food <- sam[,c(180:209, 213, 215, 216)]

food$fruit_or_vegetable <- ifelse(sam$fruit_or_vegetable <= median(sam$fruit_or_vegetable), "low", "high")
food$animal_product <- ifelse(sam$animal_product <= median(sam$animal_product), "low", "high")

food$age <- sam$Age
food$sex <- sam$sex


bdiv <- tibble(nutrient = colnames(food)[c(-33, -36, -37)],
               bray_p = rep(NA, 34), 
               bray_power = rep(NA, 34))

bray <- phyloseq::distance(ps, method = "bray")

posthoc_permanova_power <- function(physeq, factor_var, confounders = c("calories_group", "sex", "Age"), 
                                    distance_method = "bray", n_perm = 999, n_sim = 1000, alpha = 0.05) {
  # Combine factor_var and confounders
  all_vars <- c(factor_var, confounders)
  
  # Check all variables exist
  missing_vars <- setdiff(all_vars, colnames(sample_data(physeq)))
  if (length(missing_vars) > 0) {
    stop(paste("Variables not found in sample_data(physeq):", paste(missing_vars, collapse = ", ")))
  }
  
  # Extract distance matrix
  dist_mat <- phyloseq::distance(physeq, method = distance_method)
  
  # Extract variables
  sample_df <- data.frame(sample_data(physeq))[ , all_vars, drop = FALSE]
  
  # Convert all to factors where appropriate
  sample_df <- sample_df %>% mutate(across(all_of(all_vars), ~if(is.character(.)) as.factor(.) else .))
  
  # Construct formula for PERMANOVA
  formula_str <- paste("dist_mat ~", paste(all_vars, collapse = " + "))
  permanova_formula <- as.formula(formula_str)
  
  # Run PERMANOVA
  permanova_res <- adonis2(permanova_formula, data = sample_df, permutations = n_perm, by = "margin")
  
  # Extract effect size and p-value for primary factor
  factor_r2 <- permanova_res$R2[1]
  factor_F <- permanova_res$F[1]
  factor_p <- permanova_res$`Pr(>F)`[1]
  
  # Post hoc power analysis (simulate permuting only the primary factor while keeping confounders fixed)
  n <- nrow(sample_df)
  sig_count <- 0
  for (i in 1:n_sim) {
    permuted <- sample_df[[factor_var]]  # permute primary factor
    permuted <- sample(permuted)
    sim_df <- sample_df
    sim_df[[factor_var]] <- permuted
    sim_res <- adonis2(dist_mat ~ ., data = sim_df, permutations = n_perm, by = "margin")
    if (sim_res$`Pr(>F)`[1] < alpha) {
      sig_count <- sig_count + 1
    }
  }
  
  power_estimate <- sig_count / n_sim
  message(paste("Estimated post hoc power for", factor_var, ":", round(power_estimate, 3)))
  
  return(power_estimate)
}

for (i in 1:34){
  
  variable = bdiv$nutrient[i]
  
  bray_formula <- as.formula(paste("bray ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_bray <- vegan::adonis2(bray_formula, data = food, by = "margin")
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[4]
  
  # bdiv$bray_power[i] <- posthoc_permanova_power(ps, variable, alpha = 0.006)
  
}

bdiv <- bdiv |>
  mutate(adj_bray_p = p.adjust(bray_p, method = "BH"))

sigtable <- bdiv |>
  filter(adj_bray_p <= 0.1)

sig <- sigtable |>
  pull(nutrient)


ps@sam_data$nutrient_score <- ifelse(ps@sam_data$nutrient_score <= median(ps@sam_data$nutrient_score), "low", "high")
ps@sam_data$fruit_or_vegetable <- ifelse(ps@sam_data$fruit_or_vegetable <= median(ps@sam_data$fruit_or_vegetable), "low", "high")
ps@sam_data$animal_product <- ifelse(ps@sam_data$animal_product <= median(ps@sam_data$animal_product), "low", "high")


#Create PCOA plots for significant stuff

bray <- phyloseq::distance(ps, method = "bray")

bray_pcoa <- get_pcoa(obj = ps, distmethod = "bray", method = "hellinger")

sam <- ps@sam_data

create_pcoa_plot <- function(variable, bray_dist, bray_pcoa, sam, title) {
  set.seed(1313)
  pval_bray <- sigtable$adj_bray_p[i]
  
  p_text_bray <- ifelse(pval_bray < 0.001, "p < 0.001",
                        paste("p =", format(round(pval_bray, 3), nsmall = 3)))
  
  pcoa_bray_plot <- ggordpoint(obj = bray_pcoa, biplot = FALSE, speciesannot = TRUE,
                               factorNames = c(variable), ellipse = TRUE, linesize = 1.5,
                               ellipse_linewd = 1, ellipse_lty = 2) +
    ggtitle(title) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 28)) +
    annotate("text", x = Inf, y = Inf, label = p_text_bray,
             hjust = 1.1, vjust = 1.5, size = 16, fontface = "plain") +
    theme(
      plot.title = element_text(size = 32, face = "plain"),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 0),
      legend.text  = element_text(size = 26)
    ) + 
    scale_y_continuous(breaks = c(-0.2, 0, 0.2))
  
  # ggsave(pcoa_bray_plot,
  #        filename = paste(variable, "_pcoa.png", sep = ""),
  #        device = "png",
  #        height = 6, width = 14, units = "in", 
  #        dpi = 800)
  
  return(pcoa_bray_plot)
}

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity/Microbiome")


for (i in 1:length(sig)){
  create_pcoa_plot(sig[i], bray, bray_pcoa, sam)
}


# Specific plots for paper
i = 5
fermented_plot <- create_pcoa_plot(sig[i], bray, bray_pcoa, sam, title = "(C) Fermented Foods")

i = 2
sodium_plot <- create_pcoa_plot(sig[i], bray, bray_pcoa, sam, title = "(D) Sodium")

i = 3
vitaminA_plot <- create_pcoa_plot(sig[i], bray, bray_pcoa, sam, title = "(E) Vitamin A")

i = 4
vitaminB2_plot <- create_pcoa_plot(sig[i], bray, bray_pcoa, sam, title = "(F) Vitamin B2")

i = 1
fat_plot <- create_pcoa_plot(sig[i], bray, bray_pcoa, sam, title = "(G) Fat")

#Combined plot (uses plots from alpha diversity script)

beta_diversity <- ((fermented_plot + sodium_plot) / (vitaminA_plot + vitaminB2_plot) / (fat_plot + plot_spacer())) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 0))

diversity <- (alpha_diversity / beta_diversity) + 
  plot_layout(heights = c(1, 3.5))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(diversity, filename = 'combined_diversity_plot.png', dpi = 600, width = 15.5, height = 18)


#Metabolite Analysis


# Load packages
library(vegan)
library(ggplot2)
library(patchwork)
library(tibble)
library(dplyr)
library(tidyverse)
set.seed(1313)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

ps@sam_data$nutrient_score <- ifelse(ps@sam_data$nutrient_score <= median(ps@sam_data$nutrient_score), "low", "high")
ps@sam_data$fruit_or_vegetable <- ifelse(ps@sam_data$fruit_or_vegetable <= median(ps@sam_data$fruit_or_vegetable), "low", "high")
ps@sam_data$animal_product <- ifelse(ps@sam_data$animal_product <= median(ps@sam_data$animal_product), "low", "high")

sam <- data.frame(ps@sam_data)

sam <- data.frame(sam) |>
  mutate(calories_group = ifelse(calories <= median(sam$calories), "low", "high"), 
         meat_portions_group = ifelse(meat_portions <= median(sam$meat_portions), "low", "high"))

food <- sam[,c(180:209,211, 212 ,213, 215, 216)]
food$sex <- sam$sex
food$age <- sam$Age

metabolite_data <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

keep_samples <- intersect(rownames(metabolite_data), rownames(food))
food <- food[keep_samples,]
metabolite_data <- metabolite_data[keep_samples,]

bray <- vegan::vegdist(metabolite_data, method = "bray")

posthoc_permanova_power_dist <- function(
    dist_mat,
    metadata,
    factor_var,
    confounders = c("calories_group", "sex", "age"),
    n_perm = 999,
    n_sim = 1000,
    alpha = 0.05
) {
  
  # Combine variables
  all_vars <- c(factor_var, confounders)
  
  # Check variables exist
  missing_vars <- setdiff(all_vars, colnames(metadata))
  if (length(missing_vars) > 0) {
    stop(
      paste(
        "Variables not found in metadata:",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
  
  # Ensure metadata rows match distance matrix
  if (!all(rownames(metadata) %in% labels(dist_mat))) {
    stop("Row names of metadata must match labels of dist_mat")
  }
  
  metadata <- metadata[labels(dist_mat), all_vars, drop = FALSE]
  
  # Convert character variables to factors
  metadata <- metadata |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(all_vars),
        ~ if (is.character(.)) as.factor(.) else .
      )
    )
  
  # Construct formula
  formula_str <- paste("dist_mat ~", paste(all_vars, collapse = " + "))
  permanova_formula <- as.formula(formula_str)
  
  # Run observed PERMANOVA
  permanova_res <- vegan::adonis2(
    permanova_formula,
    data = metadata,
    permutations = n_perm,
    by = "margin"
  )
  
  # Extract stats for primary factor
  factor_r2 <- permanova_res$R2[1]
  factor_F  <- permanova_res$F[1]
  factor_p  <- permanova_res$`Pr(>F)`[1]
  
  message(
    paste0(
      "Observed PERMANOVA for ", factor_var,
      ": R2 = ", round(factor_r2, 3),
      ", F = ", round(factor_F, 2),
      ", p = ", signif(factor_p, 3)
    )
  )
  
  # Post hoc power simulation
  sig_count <- 0
  
  for (i in seq_len(n_sim)) {
    
    sim_df <- metadata
    sim_df[[factor_var]] <- sample(sim_df[[factor_var]])
    
    sim_res <- vegan::adonis2(
      dist_mat ~ .,
      data = sim_df,
      permutations = n_perm
    )
    
    if (sim_res$`Pr(>F)`[1] < alpha) {
      sig_count <- sig_count + 1
    }
  }
  
  power_estimate <- sig_count / n_sim
  
  message(
    paste(
      "Estimated post hoc power for",
      factor_var,
      ":",
      round(power_estimate, 3)
    )
  )
  
  return(
    list(
      power = power_estimate,
      observed_R2 = factor_r2,
      observed_F = factor_F,
      observed_p = factor_p
    )
  )
}

# PERMANOVA for each metadata variable
bdiv <- tibble(
  variable = colnames(food)[c(-35, -36, -37)],
  bray_p = NA_real_, 
  bray_power = NA_real_
)




for (i in seq_along(colnames(food)[c(-35, -36, -37)])) {
  variable = bdiv$variable[i]
  
  bray_formula <- as.formula(paste("bray ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_bray <- vegan::adonis2(bray_formula, data = food, by = "margin")
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[4]
  # bdiv$bray_power[i] <- posthoc_permanova_power_dist(bray, food, variable)$power
  
}

# Adjust p-values
bdiv <- bdiv %>%
  mutate(
    adjusted_bray = p.adjust(bray_p, method = "BH")
  )

sig_table <- bdiv %>%
  filter(adjusted_bray <= 0.1)

sig_vars <- bdiv %>%
  filter(adjusted_bray <= 0.1) %>%
  pull(variable)


metabolite_data <- read_csv("metabolites_transposed.csv") |> column_to_rownames("...1")
keep_samples <- intersect(rownames(metabolite_data), rownames(sam))
metabolite_data <- metabolite_data[keep_samples, ]
sam_metab <- sam[keep_samples, ]

# Convert metabolite data to OTU table format (features x samples)
otu_mat <- t(as.matrix(metabolite_data))
OTU_metab <- otu_table(otu_mat, taxa_are_rows = TRUE)

# Keep original sample data
SAM <- sample_data(sam_metab)

# Create a new phyloseq object with metabolite data
ps_metab <- phyloseq(OTU_metab, SAM)

# Compute PCoA (Bray-Curtis + Hellinger) using MicrobiotaProcess
metab_pcoa <- get_pcoa(ps_metab, distmethod = "bray", method = "hellinger")

# Now ggordpoint works exactly like before
create_pcoa_plot <- function(pcoa_obj, variable, pval, title) {
  ggordpoint(
    obj = pcoa_obj,
    biplot = FALSE,
    speciesannot = TRUE,
    factorNames = variable,
    ellipse = TRUE,
    linesize = 1.5,
    ellipse_linewd = 1,
    ellipse_lty = 2
  ) +
    ggtitle(title) +
    annotate("text", x = Inf, y = Inf,
             label = ifelse(pval < 0.001, "p < 0.001", paste("p =", round(pval,3))),
             hjust = 1.1, vjust = 1.5, size = 16, fontface = "plain") +
    theme(
      plot.title = element_text(size = 32, face = "plain"),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_blank(),
      legend.text  = element_text(size = 26)
    ) +
    scale_y_continuous(breaks = c(-0.1, 0, 0.1))
}


sodium_metab <- create_pcoa_plot(metab_pcoa, "sodium_group", sig_table$adjusted_bray[1], "(A) Sodium")    

magnesium_metab <- create_pcoa_plot(metab_pcoa, "magnesium_group", sig_table$adjusted_bray[1], "(B) Magnesium")         

fermented_metab <- create_pcoa_plot(metab_pcoa, "fermented_portions_group", sig_table$adjusted_bray[1], "(C) Fermented Foods") 

beta_diversity <- ((sodium_metab + magnesium_metab) / (fermented_metab + plot_spacer())) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 0))


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(beta_diversity, filename = 'combined_metabolome_diversity_plot.png', dpi = 600, width = 12, height = 10)




