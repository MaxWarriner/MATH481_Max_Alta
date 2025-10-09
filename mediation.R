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

sam <- data.frame(ps@sam_data)


# Logistic Regression on Fiber vs. Health Outcomes --------------------
fiber_health_regression <- tibble(factor = c("illness", "diarrhea", "cough", "bloating", "abdominalpain", "lower_appetite", "nausea"), 
                                  p = rep(NA, 7))

fiber_health_regression$p[1] <- summary(glm(illness ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[2] <- summary(glm(diarrhea ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[3] <- summary(glm(cough ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[4] <- summary(glm(bloating ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[5] <- summary(glm(abdominalpain ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[6] <- summary(glm(lower_appetite ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]
fiber_health_regression$p[7] <- summary(glm(nausea ~ fiber_norm, data = data.frame(sam)))$coefficients[2,4]

significant_health <- fiber_health_regression |>
  filter(p <= 0.05) |>
  pull(factor)

# Regression on Fiber vs. Metabolites -------------------------------------

fiber_metabolites_regression <- tibble(metabolite = colnames(metabolites), 
                                       p = rep(NA, 463))

common_samples <- intersect(rownames(sam), rownames(metabolites))

# Prune both

metabolites <- metabolites |>
  filter(rownames(metabolites) %in% common_samples)

sam <- sam |>
  filter(rownames(sam) %in% common_samples)

for (i in 1:463){
  fiber_metabolites_regression$p[i] <- cor.test(sam$fiber_norm, unlist(metabolites[,i]))$p.value
}


(significant_metabolites <- fiber_metabolites_regression |>
  filter(p <= 0.05) |>
  pull(metabolite))



# Regression on Fiber vs. Genera ------------------------------------------

tax <- data.frame(ps@tax_table@.Data)

abundance <- data.frame(ps@otu_table@.Data)

colnames(abundance) <- gsub("X", "", colnames(abundance))

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

abundance <-cbind(abundance, tax)

abundance_cols <- names(abundance)[sapply(abundance, is.numeric)]

genus_abundance <- abundance %>%
  pivot_longer(
    cols = all_of(abundance_cols),  # only numeric columns
    names_to = "Sample",
    values_to = "Abundance"
  ) %>%
  group_by(Sample, Genus) %>%       # sum by genus
  summarise(Abundance = sum(Abundance), .groups = "drop") %>%
  pivot_wider(
    names_from = Genus,             # genera become columns
    values_from = Abundance,
    values_fill = 0
  ) |>
  arrange(as.numeric(Sample)) |>
  column_to_rownames(var = "Sample")

sam <- data.frame(ps@sam_data)

fiber_genus_regression <- tibble(genus = colnames(genus_abundance), 
                                 p = rep(NA, 624))

for (i in 1:624){
  fiber_genus_regression$p[i] <- cor.test(sam$fiber_norm, unlist(genus_abundance[,i]))$p.value
}

fiber_genus_regression <- fiber_genus_regression |>
  arrange(as.numeric(p))

(significant_genera <- fiber_genus_regression |>
  filter(p <= 0.05) |>
  pull(genus))



# Mediation Analysis ------------------------------------------------------

common_samples <- intersect(rownames(sam), rownames(metabolites)) |> intersect(rownames(genus_abundance))

sam <- sam |>
  filter(rownames(sam) %in% common_samples)

genus_abundance <- genus_abundance |>
  filter(rownames(genus_abundance) %in% common_samples)

metabolites <- metabolites |>
  filter(rownames(metabolites) %in% common_samples)

colnames(genus_abundance) <- gsub(" ", "_", colnames(genus_abundance))
colnames(genus_abundance) <- gsub('\\[', "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub("]", "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub("-", "", colnames(genus_abundance))


significant_genera <- gsub(" ", "_", significant_genera)
significant_genera <- gsub('\\[', "", significant_genera)
significant_genera <- gsub("]", "", significant_genera)
significant_genera <- gsub("-", "", significant_genera)

combined <- metabolites |>
  bind_cols(genus_abundance, sam) |>
  as_tibble()

mediation <- mediation_data(x = combined, treatments = c("fiber_norm") , mediators = c(significant_genera, significant_metabolites), outcomes = significant_health)

model <- multimedia(mediation, glmnet_model(lambda = 0.1))

results <- estimate(model, exper = mediation)

direct_effects <- direct_effect(results, exper = mediation)

indirect_effects <- indirect_overall(results, exper = mediation)
