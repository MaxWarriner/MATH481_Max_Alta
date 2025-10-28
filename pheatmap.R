library(pheatmap)
library(extrafont)
library(tidyverse)
library(RColorBrewer)
library(compositions)
library(phyloseq)
library(webchem)


# Data Processing ---------------------------------------------------------

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")

ps <- read_rds('microbiome.RDS')

sam <- ps@sam_data

tax <- data.frame(ps@tax_table@.Data)

metab <- read_csv('metab_and_info.csv')

abundance <- data.frame(ps@otu_table@.Data)

colnames(abundance) <- gsub("X", "", colnames(abundance))

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

abundance <- cbind(abundance, tax)

  for (j in 1:48601){
  abundance$across_samples[j] = mean(as.numeric(abundance[j,1:57]))
  }

sum <- abundance |>
  arrange(-across_samples) |>
  filter(Genus != "none") |>
  group_by(Phylum, Genus) |>
  summarize(mean_abundance = mean(across_samples)) |>
  arrange(-mean_abundance)

top40 <- sum[1:40,]

top_genera <- top40$Genus

filtered_abundance <- abundance |>
  filter(Genus %in% top_genera)

genus_abundance <- filtered_abundance %>%
  group_by(Genus) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

genus_abundance_transposed <- genus_abundance %>%
  column_to_rownames("Genus") %>%
  t() %>%
  as.data.frame()

genus_abundance_transposed <- genus_abundance_transposed[-58,]

food <- sam[,153:182]


# Food vs. Microbes -------------------------------------------------------


cor_matrix <- cor(genus_abundance_transposed, food, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()

cor_matrix[abs(cor_matrix) < 0.26] <- 0

cor_matrix <- cor_matrix[rowSums(cor_matrix) != 0,
                    colSums(cor_matrix) != 0]


genus_phylum <- top40[,c(2,1)]

genus_phylum <- column_to_rownames(genus_phylum, var = "Genus")


microbes_clustered_food <- pheatmap(cor_matrix, annotation_row = genus_phylum, na_col = "white", cluster_cols = F)
microbes_food_clustered <- pheatmap(cor_matrix, annotation_row = genus_phylum, na_col = "white", cluster_rows = F)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Heatmaps")
ggsave(microbes_clustered_food, filename = 'microbes_clusted_food_heatmap.png', dpi = 800, width = 12, height = 6)
ggsave(microbes_food_clustered, filename = 'microbes_food_clustered_heatmap.png', dpi = 800, width = 12, height = 6)

# Food vs. Metabolites ----------------------------------------------------

common_rows <- intersect(rownames(food), rownames(metab))

# Subset both data frames
metab <- metab[common_rows, , drop = FALSE]
metab <- as.data.frame(lapply(metab, as.numeric))

food <- food[common_rows, , drop = FALSE]


cor_matrix <- cor(metab, food, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()

cor_matrix[abs(cor_matrix) < 0.26] <- 0

cor_matrix <- cor_matrix[rowSums(cor_matrix) != 0,
                         colSums(cor_matrix) != 0]

rs <- rowSums(abs(cor_matrix))

top_rows <- order(rs, decreasing = TRUE)[1:min(30, nrow(cor_matrix))]

cor_matrix <- cor_matrix[top_rows, ]

metabolites_clustered_food <- pheatmap(cor_matrix, cluster_cols = F)
metabolites_food_clustered <- pheatmap(cor_matrix, cluster_rows = F)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Heatmaps")
ggsave(metabolites_clustered_food, filename = 'metabolites_clustered_food_heatmap.png', dpi = 800, width = 12, height = 8)
ggsave(metabolites_food_clustered, filename = 'metabolites_food_clustered_heatmap.png', dpi = 800, width = 12, height = 8)




# Microbes vs. Metabolites ------------------------------------------------

common_rows <- intersect(rownames(genus_abundance_transposed), rownames(metab))

# Subset both data frames
metab <- metab[common_rows, , drop = FALSE]
genus_abundance_transposed <- genus_abundance_transposed[common_rows, , drop = FALSE]


cor_matrix <- cor(metab, genus_abundance_transposed, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()

cor_matrix[abs(cor_matrix) < 0.26] <- 0

cor_matrix <- cor_matrix[rowSums(cor_matrix) != 0,
                         colSums(cor_matrix) != 0]

rs <- rowSums(abs(cor_matrix))

top_rows <- order(rs, decreasing = TRUE)[1:min(30, nrow(cor_matrix))]

cor_matrix <- cor_matrix[top_rows, ]

metabolites_clustered_microbes <- pheatmap(cor_matrix, annotation_col = genus_phylum, cluster_cols = F)
metabolites_microbes_clustered <- pheatmap(cor_matrix, annotation_col = genus_phylum, cluster_rows = F)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Heatmaps")
ggsave(metabolites_clustered_microbes, filename = 'metabolites_clustered_microbes_heatmap.png', dpi = 800, width = 12, height = 8)
ggsave(metabolites_microbes_clustered, filename = 'metabolites_microbes_clustered_heatmap.png', dpi = 800, width = 12, height = 8)



