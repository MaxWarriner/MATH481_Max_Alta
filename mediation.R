library(brms)
library(compositions)
library(ggdist)
library(glue)
library(patchwork)
library(tidyverse)
library(vroom)
library(multimedia)
set.seed(20231222)

ps <- read_rds('microbiome.RDS')

metabolites <- read_csv('metabolites.csv') |>
  column_to_rownames("sampleID")

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

top50 <- sum[1:50,]

top_genera <- top50$Genus

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

colnames(genus_abundance_transposed) <- gsub(" ", "_", colnames(genus_abundance_transposed))
colnames(genus_abundance_transposed) <- gsub('\\[', "", colnames(genus_abundance_transposed))
colnames(genus_abundance_transposed) <- gsub("]", "", colnames(genus_abundance_transposed))
colnames(genus_abundance_transposed) <- gsub("-", "", colnames(genus_abundance_transposed))


metadata <- data.frame(ps@sam_data)

combined <- metabolites |>
  bind_cols(genus_abundance_transposed, metadata) |>
  as_tibble()

mediation <- mediation_data(x = combined, treatments = colnames(metadata)[c(151:179,181)], mediators = colnames(genus_abundance_transposed), outcomes = colnames(metabolites)[-1])

model <- multimedia(mediation, glmnet_model(lambda = 0.1))

results <- estimate(model, exper = mediation)

direct_effects <- direct_effect(results, exper = mediation)
