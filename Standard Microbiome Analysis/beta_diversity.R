library(tidyverse)
library(phyloseq)
library(vegan)
library(stats)
library(MicrobiotaProcess)
library(patchwork)

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

food <- sam[,183:212]

bdiv <- tibble(nutrient = colnames(food), 
               jaccard_p = rep(NA, 30), 
               bray_p = rep(NA, 30))

bray <- phyloseq::distance(ps, method = "bray")
jaccard <- phyloseq::distance(ps, method = "jaccard")

for (i in 1:30){
  
  permanova_bray <- vegan::adonis2(bray ~ unlist(food[,i]))
  bdiv$bray_p[i] <- permanova_bray$`Pr(>F)`[1]
  
  permanova_jaccard <- vegan::adonis2(jaccard ~ unlist(food[,i]))
  bdiv$jaccard_p[i] <- permanova_jaccard$`Pr(>F)`[1]
  
}

sig <- bdiv |>
  filter(bray_p <= 0.05 & jaccard_p <= 0.05)

bdiv$adjusted_bray <- p.adjust(bdiv$bray_p, method = "BH")
bdiv$adjusted_jaccard <- p.adjust(bdiv$jaccard_p, method = "BH")


#Create PCOA plots for significant stuff

bray <- phyloseq::distance(ps, method = "bray")
jaccard <- phyloseq::distance(ps, method = "jaccard")

jaccard_pcoa <- get_pcoa(obj = ps, distmethod = "jaccard", method = "hellinger")
bray_pcoa <- get_pcoa(obj = ps, distmethod = "bray", method = "hellinger")

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

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Beta Diversity")

for (i in 1:14){
  create_pcoa_plot(sig$nutrient[i], jaccard, bray, jaccard_pcoa, bray_pcoa, sam)
}
