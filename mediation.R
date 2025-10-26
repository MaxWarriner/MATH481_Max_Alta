library(brms)
library(compositions)
library(ggdist)
library(glue)
library(patchwork)
library(tidyverse)
library(vroom)
library(multimedia)
set.seed(20231222)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
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


(significant_metabolites_fiber <- fiber_metabolites_regression |>
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


(significant_genera_fiber <- fiber_genus_regression |>
  filter(p <= 0.05) |>
  pull(genus))




# Regression on Genera vs. Health Outcomes --------------------------------

genus_health_dat <- cbind(sam$diarrhea, genus_abundance[,c(-1, -2)]) |>
  rename(diarrhea = sam$diarrhea) |>
  rename(diarrhea = diarrhea57)

colnames(genus_health_dat) <- gsub(pattern = "-", replacement = "_", colnames(genus_health_dat))
colnames(genus_health_dat) <- gsub(pattern = " ", replacement = "", colnames(genus_health_dat))
colnames(genus_health_dat) <- gsub(pattern = "\\[", replacement = "", colnames(genus_health_dat))
colnames(genus_health_dat) <- gsub(pattern = "]", replacement = "", colnames(genus_health_dat))

genus_health_regression <- tibble(genus = colnames(genus_abundance)[c(-1,-2)], 
                                  p = rep(NA, 622))

for (i in 2:623){
  genus <- colnames(genus_health_dat)[i]
  formula <- as.formula(paste("diarrhea ~ ", genus, sep = ""))
  genus_health_regression$p[i-1] <- as.data.frame(summary(glm(formula, data = genus_health_dat))$coefficients)$`Pr(>|t|)`[2]
}

significant_genera_health <- genus_health_regression |>
  filter(p <= 0.05) |>
  pull("genus")





# Mediation Analysis: Fiber -> Microbes -> Diarrhea ------------------------------------------------------

significant_genera <- intersect(significant_genera_fiber, significant_genera_health)[-9] #remove none

significant_genera <- gsub(" ", "_", significant_genera)
significant_genera <- gsub("-", "_", significant_genera)

# Combine datasets

colnames(genus_abundance) <- gsub(pattern = " ", replacement = "_", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "-", replacement = "_", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "\\[", replacement = "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "]", replacement = "", colnames(genus_abundance))

genus_abundance_filtered <- genus_abundance[, colnames(genus_abundance) %in% significant_genera]

combined_d <- bind_cols(genus_abundance_filtered, sam[,c(83,157)]) |>
  as_tibble()

combined_d$diarrhea <- as.numeric(combined_d$diarrhea)

summary(glm(diarrhea ~ fiber_norm , data = combined_d))
summary(glm(diarrhea ~ Hespellia + Lachnospiraceae_NK3A20_group + Lachnospiraceae_UCG_007 + Lachnospiraceae_UCG_008
            + Methanosphaera + Oribacterium + Parvimonas + Subdoligranulum , data = combined_d))

summary(glm(diarrhea ~ Lachnospiraceae_UCG_007, data = combined_d))
summary(glm(diarrhea ~ fiber_norm + Lachnospiraceae_UCG_007, data = combined_d))
asummary(glm(diarrhea ~ fiber_norm + Oribacterium, data = combined_d))


summary(glm(diarrhea ~ Hespellia + Lachnospiraceae_NK3A20_group + Lachnospiraceae_UCG_007 + Lachnospiraceae_UCG_008
            + Methanosphaera + Oribacterium + Parvimonas + Subdoligranulum + fiber_norm, data = combined_d))




# multimedia mediation: Fiber -> microbes -> metabolites ------------------

#which of the significant genera affect metabolites?

common_samples <- intersect(rownames(genus_abundance), rownames(metabolites))

genus_abundance <- genus_abundance[rownames(genus_abundance) %in% common_samples,]
metabolites <- metabolites[rownames(metabolites) %in% common_samples,]
sam <- sam[rownames(sam) %in% common_samples,]

combined <- cbind(sam, genus_abundance) |>
  cbind(metabolites)

combined$fiber_group <- as.factor(combined$fiber_group)

exper <- mediation_data(x = combined, treatments = "fiber_group", mediators = names(genus_abundance)[c(-1,-2)], outcomes = colnames(metabolites))

model <- multimedia(exper, glmnet_model(lambda = 0.1)) |>
  estimate(exper)


# Direct Effects
direct <- direct_effect(model = model, exper = exper)

vis_direct <- direct |>
  slice_max(abs(direct_effect), n = 12) |>
  pull(outcome)

direct_plot <- combined |>
  select(any_of(vis_direct), fiber_group) |>
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
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_brewer(palette = "Pastel1") + 
  theme_pubclean()

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Mediation")
ggsave(direct_plot, filename = "fiber_direct_effect_metabolites.png", width = 10, height = 6)

#Indirect Effects

indirect_effect <- indirect_overall(model, exper)

top_direct <- dplyr::rename(direct, effect = direct_effect)
top_indirect <- dplyr::rename(bind_rows(indirect_effect),
                              effect = indirect_effect
)
top_effects <- list(direct = top_direct, indirect = top_indirect) |>
  bind_rows(.id = "type")

vis_outcomes <- c("Adenine", "n.Octadecylamine", ".alpha..D.Xylopyranose", 
                  "X1.3.Dimethylurate", "Chelidonic.acid", "X1.Stearoyl.2.arachidonoyl.sn.glycero.3.phospho..1..myo.inositol.")

top_effects <- bind_rows(
  filter(top_effects, outcome %in% vis_outcomes[1:3], type == "indirect"),
  filter(top_effects, outcome %in% vis_outcomes[4:6], type == "direct"),
)


eig <- \(x, k) 100 * round(x[k] / sum(x), 4)

mds <- cmdscale(dist(mediators(exper)), eig = TRUE, k = 2)

coords <- data.frame(mds$points) |>
  bind_cols(treatments(exper)) |>
  bind_cols(outcomes(exper)[, vis_outcomes]) |>
  pivot_longer(top_effects$outcome,
               names_to = "outcome",
               values_to = "abundance"
  ) |>
  left_join(top_effects) |>
  group_by(outcome) |>
  mutate(abundance_quantile = as.integer(as.factor(cut(abundance, 10))) / 10)

library(paletteer)
direct_indirect_plot <- ggplot(coords) +
  geom_point(aes(X1, X2, col = fiber_group, size = abundance_quantile)) +
  scale_size_area(max_size = 1.5, breaks = c(0.25, 0.5, 0.75)) +
  labs(
    x = glue("MDS1 [{eig(mds$eig, 1)}%]"),
    y = glue("MDS2 [{eig(mds$eig, 2)}%]"),
    size = "Metabolite Abundance Quantile",
    col = "Fiber Group"
  ) +
  facet_wrap(~ type + reorder(outcome, -abundance)) + 
  ggtitle('Microbiome Composition: Highlighting Top Metabolites by Direct & Indirect Effects of Fiber') + 
  ylim(-15, 11) + 
  theme_classic() + 
  scale_color_paletteer_d("fishualize::Etheostoma_barrenense") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(filename = "direct_indirect_plot.png", plot = direct_indirect_plot, width = 10, height = 6)

