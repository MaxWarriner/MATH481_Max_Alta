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

metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames(var = "...1")

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


#Transform metabolite concentrations

metabolites <- metabolites %>%
  mutate(across(where(is.numeric), ~ log(.x + 1)))


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

# Center Log Transform Genus Abundance
genus_abundance <- as.data.frame(clr(genus_abundance + 1))

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


# Combine datasets

sam$fiber_group <- as.factor(sam$fiber_group)
sam$diarrhea <- as.factor(sam$diarrhea)

metabolites <- metabolites[,colnames(metabolites) %in% significant_metabolites]
genus_abundance <- genus_abundance[, colnames(genus_abundance) %in% significant_genera]

combined_d <- metabolites |>
  bind_cols(genus_abundance, sam) |>
  as_tibble()

#mediation for diarrhea (fiber -> microbiome -> diarrhea)

#Simple

combined_d$diarrhea <- as.numeric(combined_d$diarrhea)
subset <- combined_d[, c(153, 36:69, 227)]

summary(glm(diarrhea ~ fiber_norm , data = combined_d))

filtered_microbes <- as.data.frame(summary(glm(fiber_norm ~ ., data = subset[,-1]))$coefficients) |>
  arrange(`Pr(>|t|)`) |>
  filter(`Pr(>|t|)` <= 0.1)

filtered_microbes <- rownames(filtered_microbes)






# multimedia package

exper_d <- mediation_data(x = combined_d, treatments = "fiber_norm", mediators = significant_genera, outcomes = "diarrhea")

model_d <- multimedia(exper_d, glmnet_model(lambda = 0.1)) |>
  estimate(exper_d)


# Direct Effects
direct_d <- direct_effect(model = model_d, exper = exper_d) |>
  map_dfr(effect_summary, .id = "treatment")

vis_direct_d <- direct_d |>
  slice_max(abs(direct_effect), n = 20) |>
  pull(outcome)

combined_d |>
  select(any_of(vis_direct_d), fiber_group) |>
  pivot_longer(-fiber_group, names_to = "feature") |>
  ggplot() +
  geom_boxplot(
    aes(value, reorder(feature, value, median),
        fill = fiber_group
    )
  ) +
  labs(
    x = "log(1 + intensity)",
    y = "Metabolite",
    fill = "Group"
  ) + 
  ggtitle('Direct Effects on Metabolite by Fiber Group') + 
  theme_bw()


#Indirect Effects

indirect_effect <- indirect_overall(model_d, exper_d)

top_direct_d <- dplyr::rename(direct_d, effect = direct_effect)
top_indirect_d <- dplyr::rename(bind_rows(indirect_effect),
                              effect = indirect_effect
)
top_effects_d <- list(direct = top_direct_d, indirect = top_indirect_d) |>
  bind_rows(.id = "type")

vis_outcomes <- c(
  "m0181_hydrocinnamic_acid", "m1303_lithocholate",
  "m0036_creatinine", "m0253_sphingosine", "m1478_C182_CE",
  "m0295_arginine"
)
top_effects <- bind_rows(
  filter(top_effects, outcome %in% vis_outcomes[1:3], type == "indirect"),
  filter(top_effects, outcome %in% vis_outcomes[4:6], type == "direct"),
)

