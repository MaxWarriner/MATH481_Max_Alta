library(tidyverse)
library(phyloseq)
library(vegan)
library(stats)
library(MicrobiotaProcess)
library(patchwork)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

#Microbiome Analysis

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- data.frame(sam) |>
  mutate(calories_group = ifelse(calories <= median(sam$calories), "low", "high"), 
         meat_portions_group = ifelse(meat_portions <= median(sam$meat_portions), "low", "high"))

food <- sam[,c(180:209, 213, 215)]

food$nutrient_score <- ifelse(sam$nutrient_score <= median(sam$nutrient_score), "low", "high")
food$fruit_or_vegetable <- ifelse(sam$fruit_or_vegetable <= median(sam$fruit_or_vegetable), "low", "high")
food$animal_product <- ifelse(sam$animal_product <= median(sam$animal_product), "low", "high")

food$age <- sam$Age
food$sex <- sam$sex


bdiv <- tibble(nutrient = colnames(food)[c(-32, -36, -37)], 
               jaccard_p = rep(NA, 34), 
               bray_p = rep(NA, 34))

bray <- phyloseq::distance(ps, method = "bray")
jaccard <- phyloseq::distance(ps, method = "jaccard")

for (i in 1:34){
  
  variable = bdiv$nutrient[i]
  
  bray_formula <- as.formula(paste("bray ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_bray <- vegan::adonis2(bray_formula, data = food, by = "margin")
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[4]
  
  jaccard_formula <- as.formula(paste("jaccard ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_jaccard <- vegan::adonis2(jaccard_formula, data = food, by = "margin")
  bdiv$jaccard_p[i] <- permanova_jaccard$`Pr(>F)`[4]
  
}

bdiv <- bdiv |>
  mutate(bray_p = p.adjust(bray_p, method = "BH"), 
         jaccard_p = p.adjust(jaccard_p, method = "BH"))

sigtable <- bdiv |>
  filter(bray_p <= 0.05 | jaccard_p <= 0.05)

sig <- sigtable |>
  pull(nutrient)


ps@sam_data$nutrient_score <- ifelse(ps@sam_data$nutrient_score <= median(ps@sam_data$nutrient_score), "low", "high")
ps@sam_data$fruit_or_vegetable <- ifelse(ps@sam_data$fruit_or_vegetable <= median(ps@sam_data$fruit_or_vegetable), "low", "high")
ps@sam_data$animal_product <- ifelse(ps@sam_data$animal_product <= median(ps@sam_data$animal_product), "low", "high")


#Create PCOA plots for significant stuff

bray <- phyloseq::distance(ps, method = "bray")

bray_pcoa <- get_pcoa(obj = ps, distmethod = "bray", method = "hellinger")

sam <- ps@sam_data

create_pcoa_plot <- function(variable, bray_dist, bray_pcoa, sam) {
  
  pval_bray <- sigtable$bray_p[i]
  
  p_text_bray <- ifelse(pval_bray < 0.001, "p < 0.001",
                        paste("p =", format(round(pval_bray, 3), nsmall = 3)))
  
  pcoa_bray_plot <- ggordpoint(obj = bray_pcoa, biplot = FALSE, speciesannot = TRUE,
                               factorNames = c(variable), ellipse = TRUE, linesize = 1.5,
                               ellipse_linewd = 1, ellipse_lty = 2) +
    ggtitle(paste(gsub("_", " ", variable), "(Bray-Curtis)")) +
    guides(color=guide_legend(title=gsub("_", " ", variable), override.aes = list(size = 4))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 28)) +
    annotate("text", x = Inf, y = Inf, label = p_text_bray,
             hjust = 1.1, vjust = 1.5, size = 16, fontface = "bold") +
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  ggsave(pcoa_bray_plot,
         filename = paste(variable, "_pcoa.png", sep = ""),
         device = "png",
         height = 6, width = 14, units = "in", 
         dpi = 800)
  
  return(pcoa_bray_plot)
}

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity/Microbiome")


for (i in 1:length(sig)){
  create_pcoa_plot(sig[i], bray, bray_pcoa, sam)
}


#Metabolite Analysis


# Load packages
library(vegan)
library(ggplot2)
library(patchwork)
library(tibble)
library(dplyr)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

food <- data.frame(sam[,c(180:209, 213, 215)])
food$sex <- sam$sex
food$age <- sam$Age

metabolite_data <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

bray <- vegan::vegdist(metabolite_data, method = "bray")
jaccard <- vegan::vegdist(metabolite_data, method = "jaccard")

# PERMANOVA for each metadata variable
bdiv <- tibble(
  variable = colnames(food)[c(-32, -33, -34)],
  jaccard_p = NA_real_,
  bray_p = NA_real_
)

for (i in seq_along(colnames(food)[c(-32, -33, -34)])) {
  variable = bdiv$variable[i]
  
  bray_formula <- as.formula(paste("bray ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_bray <- vegan::adonis2(bray_formula, data = food, by = "margin")
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[4]
  
  jaccard_formula <- as.formula(paste("jaccard ~ calories_group + sex + age + ", variable, sep = ""))
  
  permanova_jaccard <- vegan::adonis2(jaccard_formula, data = food, by = "margin")
  bdiv$jaccard_p[i] <- permanova_jaccard$`Pr(>F)`[4]
}

# Adjust p-values
bdiv <- bdiv %>%
  mutate(
    adjusted_bray = p.adjust(bray_p, method = "BH"),
    adjusted_jaccard = p.adjust(jaccard_p, method = "BH")
  )

sig_vars <- bdiv %>%
  filter(adjusted_bray <= 0.1 | adjusted_jaccard <= 0.1) %>%
  pull(variable)

# Ordinations
bray_pcoa <- cmdscale(bray, eig = TRUE, k = 2)
jaccard_pcoa <- cmdscale(jaccard, eig = TRUE, k = 2)

bray_var <- round(100 * bray_pcoa$eig / sum(bray_pcoa$eig), 1)
jaccard_var <- round(100 * jaccard_pcoa$eig / sum(jaccard_pcoa$eig), 1)

extract_scores <- function(pcoa_obj) {
  as.data.frame(pcoa_obj$points) %>%
    rename(Axis1 = V1, Axis2 = V2)
}

bray_scores <- extract_scores(bray_pcoa)
jaccard_scores <- extract_scores(jaccard_pcoa)

bray_scores <- cbind(bray_scores, food)
jaccard_scores <- cbind(jaccard_scores, food)

create_pcoa_plot <- function(variable, bray_scores, jaccard_scores) {
  # Force factor for grouping
  bray_scores[[variable]] <- as.factor(bray_scores[[variable]])
  jaccard_scores[[variable]] <- as.factor(jaccard_scores[[variable]])
  
  p_bray <- bdiv$bray_p[which(bdiv$variable == variable)]
  p_jaccard <- bdiv$jaccard_p[which(bdiv$variable == variable)]
  
  p_text_bray <- ifelse(p_bray < 0.001, "p < 0.001", paste0("p = ", signif(p_bray, 3)))
  p_text_jaccard <- ifelse(p_jaccard < 0.001, "p < 0.001", paste0("p = ", signif(p_jaccard, 3)))
  
  # Bray–Curtis PCoA
  p_bray_plot <- ggplot(bray_scores, aes(x = Axis1, y = Axis2, color = .data[[variable]])) +
    geom_point(size = 3, alpha = 0.8) +
    stat_ellipse(
      aes(group = .data[[variable]]),
      geom = "path",
      linetype = "dotted",
      linewidth = 1,
      alpha = 0.9,
      type = "t",
      level = 0.95
    ) +
    labs(
      title = paste(gsub("_"," ", variable), "(Bray–Curtis)"),
      subtitle = p_text_bray,
      x = paste0("PCoA1 (", bray_var[1], "%)"),
      y = paste0("PCoA2 (", bray_var[2], "%)")
    ) +
    theme_bw(base_size = 28) +
    theme(legend.title = element_blank()) + 
        theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  # Jaccard PCoA
  p_jaccard_plot <- ggplot(jaccard_scores, aes(x = Axis1, y = Axis2, color = .data[[variable]])) +
    geom_point(size = 3, alpha = 0.8) +
    stat_ellipse(
      aes(group = .data[[variable]]),
      geom = "path",
      linetype = "dotted",
      linewidth = 1,
      alpha = 0.9,
      type = "t",
      level = 0.95
    ) +
    labs(
      title = paste(gsub("_", " ", variable), "(Jaccard)"),
      subtitle = p_text_jaccard,
      x = paste0("PCoA1 (", jaccard_var[1], "%)"),
      y = paste0("PCoA2 (", jaccard_var[2], "%)")
    ) +
    theme_bw(base_size = 28) +
    theme(legend.title = element_blank()) + 
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  combined <- p_jaccard_plot + p_bray_plot + 
    plot_layout(guides = "collect") +
    plot_annotation(title = "Metabolome Beta Diversity") &
    theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.title = element_blank(),
      legend.text  = element_text(size = 24)
    )
  
  ggsave(
    filename = paste0(variable, "_metabolite_pcoa.png"),
    plot = combined,
    width = 16, height = 8, dpi = 800
  )
  
  combined
}

# Run plots for significant variables
setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity/Metabolome")

for (var in sig_vars) {
  create_pcoa_plot(var, bray_scores, jaccard_scores)
}                                                 



