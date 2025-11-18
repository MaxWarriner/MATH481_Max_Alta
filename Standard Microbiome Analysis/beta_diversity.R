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

food <- sam[,211:213]

food$nutrient_score <- ifelse(food$nutrient_score <= median(food$nutrient_score), "low", "high")
food$fruit_or_vegetable <- ifelse(food$fruit_or_vegetable <= median(food$fruit_or_vegetable), "low", "high")
food$animal_product <- ifelse(food$animal_product <= median(food$animal_product), "low", "high")

bdiv <- tibble(nutrient = colnames(food), 
               jaccard_p = rep(NA, 3), 
               bray_p = rep(NA, 3))

bray <- phyloseq::distance(ps, method = "bray")
jaccard <- phyloseq::distance(ps, method = "jaccard")

for (i in 1:3){
  
  permanova_bray <- vegan::adonis2(bray ~ unlist(food[,i]))
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[1]
  
  permanova_jaccard <- vegan::adonis2(jaccard ~ unlist(food[,i]))
  bdiv$jaccard_p[i] <- permanova_jaccard$`Pr(>F)`[1]
  
}

sig <- bdiv |>
  filter(bray_p <= 0.05 & jaccard_p <= 0.05) |>
  pull(nutrient)

bdiv$adjusted_bray <- p.adjust(bdiv$bray_p, method = "BH")
bdiv$adjusted_jaccard <- p.adjust(bdiv$jaccard_p, method = "BH")

ps@sam_data$nutrient_score <- ifelse(ps@sam_data$nutrient_score <= median(ps@sam_data$nutrient_score), "low", "high")
ps@sam_data$fruit_or_vegetable <- ifelse(ps@sam_data$fruit_or_vegetable <= median(ps@sam_data$fruit_or_vegetable), "low", "high")
ps@sam_data$animal_product <- ifelse(ps@sam_data$animal_product <= median(ps@sam_data$animal_product), "low", "high")


#Create PCOA plots for significant stuff

bray <- phyloseq::distance(ps, method = "bray")
jaccard <- phyloseq::distance(ps, method = "jaccard")

jaccard_pcoa <- get_pcoa(obj = ps, distmethod = "jaccard", method = "hellinger")
bray_pcoa <- get_pcoa(obj = ps, distmethod = "bray", method = "hellinger")

sam <- ps@sam_data

create_pcoa_plot <- function(variable, jaccard_dist, bray_dist, jaccard_pcoa, bray_pcoa, sam) {

  permanova_jaccard <- vegan::adonis2(jaccard_dist ~ sam[[variable]])
  permanova_bray <- vegan::adonis2(bray_dist ~ sam[[variable]])
  
  pval_jaccard <- permanova_jaccard$`Pr(>F)`[1]
  pval_bray <- permanova_bray$`Pr(>F)`[1]
  
  p_text_jaccard <- ifelse(pval_jaccard < 0.001, "p < 0.001",
                           paste("p =", format(round(pval_jaccard, 3), nsmall = 3)))
  p_text_bray <- ifelse(pval_bray < 0.001, "p < 0.001",
                        paste("p =", format(round(pval_bray, 3), nsmall = 3)))
  
  pcoa_jaccard_plot <- ggordpoint(obj = jaccard_pcoa, biplot = FALSE, speciesannot = TRUE,
                                  factorNames = c(variable), ellipse = TRUE, linesize = 1.5,
                                  ellipse_linewd = 1, ellipse_lty = 2) +
    ggtitle(paste(gsub("_", " ", variable), "(Jaccard)")) +
    guides(color=guide_legend(title=gsub("_", " ", variable), override.aes = list(size = 4))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
    annotate("text", x = Inf, y = Inf, label = p_text_jaccard,
             hjust = 1.1, vjust = 1.5, size = 12, fontface = "bold")
  
  pcoa_bray_plot <- ggordpoint(obj = bray_pcoa, biplot = FALSE, speciesannot = TRUE,
                               factorNames = c(variable), ellipse = TRUE, linesize = 1.5,
                               ellipse_linewd = 1, ellipse_lty = 2) +
    ggtitle(paste(gsub("_", " ", variable), "(Bray-Curtis)")) +
    guides(color=guide_legend(title=gsub("_", " ", variable), override.aes = list(size = 4))) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
    annotate("text", x = Inf, y = Inf, label = p_text_bray,
             hjust = 1.1, vjust = 1.5, size = 12, fontface = "bold")
  
  combined_plot <- pcoa_jaccard_plot + pcoa_bray_plot
  
  ggsave(combined_plot,
         filename = paste(variable, "_pcoa_combined.png", sep = ""),
         device = "png",
         height = 6, width = 14, units = "in")
  
  return(combined_plot)
}

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity/Microbiome")


for (i in 1:2){
  create_pcoa_plot(sig[i], jaccard, bray, jaccard_pcoa, bray_pcoa, sam)
}

create_pcoa_plot("fiber_group", jaccard, bray, jaccard_pcoa, bray_pcoa, sam)

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

food <- sam[,211:213]


metabolite_data <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

bray <- vegan::vegdist(metabolite_data, method = "bray")
jaccard <- vegan::vegdist(metabolite_data, method = "jaccard")

# PERMANOVA for each metadata variable
bdiv <- tibble(
  variable = colnames(food),
  jaccard_p = NA_real_,
  bray_p = NA_real_
)

for (i in seq_along(colnames(food))) {
  var <- colnames(food)[i]
  permanova_bray <- vegan::adonis2(bray ~ food[[var]])
  permanova_jaccard <- vegan::adonis2(jaccard ~ food[[var]])
  
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[1]
  bdiv$jaccard_p[i] <- permanova_jaccard$`Pr(>F)`[1]
}

# Adjust p-values
bdiv <- bdiv %>%
  mutate(
    adjusted_bray = p.adjust(bray_p, method = "BH"),
    adjusted_jaccard = p.adjust(jaccard_p, method = "BH")
  )

sig_vars <- bdiv %>%
  filter(bray_p <= 0.1 & jaccard_p <= 0.1) %>%
  pull(variable)

# -------------------------------------------------------------------------
# Ordinations
# -------------------------------------------------------------------------
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

# -------------------------------------------------------------------------
# Function for plotting ordination with 95% ellipses
# -------------------------------------------------------------------------
create_pcoa_plot <- function(variable, bray_scores, jaccard_scores) {
  # Force factor for grouping
  bray_scores[[variable]] <- as.factor(bray_scores[[variable]])
  jaccard_scores[[variable]] <- as.factor(jaccard_scores[[variable]])
  
  # p-values
  permanova_bray <- vegan::adonis2(bray ~ food[[variable]])
  permanova_jaccard <- vegan::adonis2(jaccard ~ food[[variable]])
  
  p_bray <- permanova_bray$`Pr(>F)`[1]
  p_jaccard <- permanova_jaccard$`Pr(>F)`[1]
  
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
      title = paste(variable, "(Bray–Curtis)"),
      subtitle = p_text_bray,
      x = paste0("PCoA1 (", bray_var[1], "%)"),
      y = paste0("PCoA2 (", bray_var[2], "%)")
    ) +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank())
  
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
      title = paste(variable, "(Jaccard)"),
      subtitle = p_text_jaccard,
      x = paste0("PCoA1 (", jaccard_var[1], "%)"),
      y = paste0("PCoA2 (", jaccard_var[2], "%)")
    ) +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank())
  
  combined <- p_jaccard_plot + p_bray_plot
  
  ggsave(
    filename = paste0(variable, "_metabolite_pcoa.png"),
    plot = combined,
    width = 12, height = 6, dpi = 300
  )
  
  combined
}

# -------------------------------------------------------------------------
# Run plots for significant variables
# -------------------------------------------------------------------------
setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity/Metabolome")

for (var in sig_vars) {
  create_pcoa_plot(var, bray_scores, jaccard_scores)
}                                                 

create_pcoa_plot("fiber_group", bray_scores, jaccard_scores)
