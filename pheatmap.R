library(pheatmap)
library(extrafont)
library(tidyverse)
library(RColorBrewer)
library(compositions)
library(phyloseq)

sam <- read_csv('sample_data.csv')[,-1]

ps <- read_rds('microbiome.RDS')

tax <- data.frame(ps@tax_table@.Data)

abundance <- data.frame(ps@otu_table@.Data)

sam <- column_to_rownames(sam, "SampleID")

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

top20 <- sum[1:20,]

top_genera <- top20$Genus

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

food <- sam[,c(151:179, 181)]

cor_matrix <- cor(genus_abundance_transposed, food, use = "pairwise.complete.obs", method = "spearman") |>
  as.data.frame() |>
  as.matrix()


genus_phylum <- top20[,c(2,1)]

genus_phylum <- column_to_rownames(genus_phylum, var = "Genus")


pheatmap(cor_matrix, annotation_row = genus_phylum, )

