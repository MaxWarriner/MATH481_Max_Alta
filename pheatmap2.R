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

tax <- data.frame(ps@tax_table@.Data)

sam <- ps@sam_data

metab <- read_csv('metab_and_info.csv')

metab_info <- metab[,c(2,63:72)]

metab <- data.frame(t(metab[,c(2,5:61)]))
colnames(metab) <- metab[1,]
metab <- metab[-1,]

metab[] <- lapply(metab, function(x) {
  if (is.character(x)) {
    as.numeric(x)
  } else {
    x
  }
})

metab <- log(metab)

abundance <- data.frame(ps@otu_table@.Data)

colnames(abundance) <- gsub("X", "", colnames(abundance))

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

abundance <- cbind(abundance, tax)

sum <- abundance |>
  filter(Genus != "none") |>
  group_by(Phylum, Genus) 

genus_phylum <- sum[,c(63, 59)] |>
  distinct()

genus_phylum <- column_to_rownames(genus_phylum, var = "Genus")

genus_abundance <- abundance %>%
  group_by(Genus) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

genus_abundance_transposed <- genus_abundance %>%
  column_to_rownames("Genus") %>%
  t() %>%
  as.data.frame() |>
  select(-none)


food <- sam[,c(150:179, 210:212, 214)]

common_samples <- intersect(rownames(genus_abundance_transposed), rownames(metab))

genus_abundance_transposed <- genus_abundance_transposed[common_samples,]
metab <- metab[common_samples, ]
food <- food[common_samples,]

metab_class <- metab_info[,c(1,3:5)] |>
  column_to_rownames(var = "Name")

library(Silhouette)

compute_silhouette <- function(hc, dist_mat, k) {
  cluster_assignments <- cutree(hc, k = k)
  sil <- cluster::silhouette(cluster_assignments, dist_mat)
  mean(sil[, "sil_width"])
}

calc_clusters_rows <- function(cor_matrix){
  
  dist_mat_euclid <- dist(cor_matrix, method = "euclidean")
  
  hc_euclid <- hclust(dist_mat_euclid, method = "complete")
  
  dist_mat_manhattan <- dist(cor_matrix, method = "manhattan")
  
  hc_manhattan <- hclust(dist_mat_manhattan, method = "complete")
  
  dist_mat_canberra <- dist(cor_matrix, method = "canberra")
  
  hc_canberra <- hclust(dist_mat_canberra, method = "complete")
  
  
  k_range <- 2:10
  
  sil_scores_euclid <- sapply(k_range, function(k) compute_silhouette(hc_euclid, dist_mat_euclid, k))
  sil_scores_manhattan <- sapply(k_range, function(k) compute_silhouette(hc_manhattan, dist_mat_manhattan, k))
  sil_scores_canberra <- sapply(k_range, function(k) compute_silhouette(hc_canberra, dist_mat_canberra, k))
  
  ggplot(data = tibble(k = rep(2:10, times = 3), 
                       score = c(sil_scores_euclid, sil_scores_manhattan, sil_scores_canberra), 
                       method = rep(c("Euclidean", "Manhattan", "Canberra"), each = 9))) + 
    geom_line(aes(x = k, y = score, color = method)) + 
    theme_bw() + 
    xlab('# of Clusters') + 
    ylab('Silhouette Score') + 
    scale_x_continuous(breaks = c(2:10))
  
  
}

calc_clusters_cols <- function(cor_matrix){
  
  dist_mat_euclid <- dist(t(cor_matrix), method = "euclidean")
  
  hc_euclid <- hclust(dist_mat_euclid, method = "complete")
  
  dist_mat_manhattan <- dist(t(cor_matrix), method = "manhattan")
  
  hc_manhattan <- hclust(dist_mat_manhattan, method = "complete")
  
  dist_mat_canberra <- dist(t(cor_matrix), method = "canberra")
  
  hc_canberra <- hclust(dist_mat_canberra, method = "complete")
  
  
  k_range <- 2:10
  
  sil_scores_euclid <- sapply(k_range, function(k) compute_silhouette(hc_euclid, dist_mat_euclid, k))
  sil_scores_manhattan <- sapply(k_range, function(k) compute_silhouette(hc_manhattan, dist_mat_manhattan, k))
  sil_scores_canberra <- sapply(k_range, function(k) compute_silhouette(hc_canberra, dist_mat_canberra, k))
  
  ggplot(data = tibble(k = rep(2:10, times = 3), 
                       score = c(sil_scores_euclid, sil_scores_manhattan, sil_scores_canberra), 
                       method = rep(c("Euclidean", "Manhattan", "Canberra"), each = 9))) + 
    geom_line(aes(x = k, y = score, color = method)) + 
    theme_bw() + 
    xlab('# of Clusters') + 
    ylab('Silhouette Score') + 
    scale_x_continuous(breaks = c(2:10))
  
  
}

# Food vs. Microbes -------------------------------------------------------


cor_matrix <- cor(genus_abundance_transposed, food, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()

colnames(cor_matrix) <- gsub(pattern = "_norm", replacement = "",colnames(cor_matrix))
colnames(cor_matrix) <- gsub(pattern = "_", replacement = " ",colnames(cor_matrix))

rs <- rowSums(abs(cor_matrix))

top_rows <- order(rs, decreasing = TRUE)[1:min(30, nrow(cor_matrix))]

cor_matrix <- cor_matrix[top_rows, ]

sig_cutoff <- 0.26
sig <- abs(cor_matrix) >= sig_cutoff  # TRUE = significant, FALSE = insignificant


cor_matrix <- cor_matrix[rowSums(cor_matrix) != 0,
                         colSums(cor_matrix) != 0]

mat_colors <- cor_matrix
mat_colors[!sig] <- 0

mat_colors <- mat_colors[rowSums(mat_colors) != 0,
                         colSums(mat_colors) != 0]

cor_matrix <- cor_matrix[which(rownames(cor_matrix) %in% rownames(mat_colors)),]


# microbes clustered ------------------------------------------------------

#calculate silhouette scores for different clusters and distance metrics

calc_clusters_rows(cor_matrix)

#optimal clusters is 3 with Manhattan

dist <- dist(cor_matrix, method = "manhattan")

hc <- hclust(dist, method = "complete")


microbes_clustered_food <- pheatmap(mat_colors, 
                                    annotation_row = genus_phylum,
                                    na_col = "white", 
                                    cluster_cols = F,
                                    cluster_rows = hc, 
                                    cutree_rows = 3)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Heatmaps")
ggsave(microbes_clustered_food, filename = 'microbes_clustered_food_heatmap2.png', dpi = 800, width = 14, height = 6)


# Food vs. Metabolites ----------------------------------------------------

common_rows <- intersect(rownames(food), rownames(metab))

# Subset both data frames
metab <- metab[common_rows, , drop = FALSE]

complete_info <- metab_info |>
  filter(!is.na(Superclass)) |>
  pull(Name)

metab <- metab[,which(colnames(metab) %in% complete_info)]

food <- food[common_rows, , drop = FALSE]


cor_matrix <- cor(metab, food, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()

colnames(cor_matrix) <- gsub(pattern = "_norm", replacement = "",colnames(cor_matrix))
colnames(cor_matrix) <- gsub(pattern = "_", replacement = " ",colnames(cor_matrix))

rs <- rowSums(abs(cor_matrix))

top_rows <- order(rs, decreasing = TRUE)[1:min(30, nrow(cor_matrix))]

cor_matrix <- cor_matrix[top_rows, ]

sig_cutoff <- 0.26
sig <- abs(cor_matrix) >= sig_cutoff  # TRUE = significant, FALSE = insignificant

mat_colors <- cor_matrix
mat_colors[!sig] <- 0

metab_class <- metab_info[,c(1,3:5)] |>
  column_to_rownames(var = "Name")

metab_class <- metab_class[intersect(rownames(metab_class), rownames(cor_matrix)),] |>
  dplyr::select(c(Superclass, Class))



# metabolites clustered ---------------------------------------------------

#calculate silhouette scores for different clusters and distance metrics

calc_clusters_rows(cor_matrix)

#optimal clusters is 2 with manhattan

dist <- dist(cor_matrix, method = "manhattan")

hc <- hclust(dist, method = "complete")


metabolites_clustered_food <- pheatmap(mat_colors, 
                                       annotation_row = metab_class,
                                       na_col = "white", 
                                       cluster_cols = F,
                                       cluster_rows = hc, 
                                       cutree_rows = 2)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Heatmaps")
ggsave(metabolites_clustered_food, filename = 'metabolites_clustered_food_heatmap2.png', dpi = 800, width = 16, height = 8)


