
ps <- read_rds('microbiome.RDS')

library(brms)
library(compositions)
library(ggdist)
library(glue)
library(patchwork)
library(tidyverse)
library(vroom)
library(multimedia)
set.seed(20231222)

metabolites <- read_csv('metabolites.csv')

taxa <- t(ps@otu_table@.Data)

taxa <- data.frame(taxa)

taxa$sampleID <- rownames(taxa)

taxa <- taxa[,c(48602, 1:48601)]

metadata <- data.frame(ps@sam_data)



combined <- metabolites |>
  bind_cols(taxa, metadata) |>
  as_tibble()

mediation <- mediation_data(combined, colnames(metadata)[125:133], colnames(metadata)[32:81], c(colnames(taxa)[-1], colnames(metabolites)[-1]))

model <- multimedia(mediation, glmnet_model(lambda = 0.1))

estimate(model, exper = mediation)
