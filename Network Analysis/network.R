library(phyloseq)
library(ggClusterNet)
library(tidyverse)
library(WGCNA)
library(igraph)
library(ggraph)
library(tidyfst)

ps <- readRDS('microbiome.RDS')

metabolites <- read_csv('metabolites.csv') 

tax <- ps@tax_table

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

tax_table(ps) <- tax

sam <- ps@sam_data

network <- network.pip(N = 100, ps = ps, group = "sex")

cor = corMicro(ps = ps, N = 100)

cor = cor[[1]]

maptree <- model_maptree2(cor = cor)

network_fiber <- Facet.network(ps, g1 = "fiber_group", fill = "Genus")

# Network of metabolites and Microbiome

# Find common samples
common_samples <- intersect(sample_names(ps), metabolites$sampleID)

# Prune both
ps <- prune_samples(common_samples, ps)
metabolites <- metabolites |>
  filter(sampleID %in% common_samples)

envGroup <- data.frame(
  ID = colnames(metabolites)[-1],  # all metabolite column names
  group = "metabolites"
)

ps_filt <- prune_taxa(taxa_sums(ps) / sum(taxa_sums(ps)) > 1e-4, ps)

network <- corBionetwork(
  ps = ps_filt,
  N = 0,
  r.threshold = 0.6,
  p.threshold = 0.05,
  group = "fiber_group",  
  env = metabolites,
  envGroup = envGroup,
  method = "spearman",
  path = "./network_results/",
  fill = "Genus",
  size = "igraph.degree",
  scale = TRUE,
  bio = TRUE,
  zipi = FALSE,
  step = 100,
  width = 18,
  height = 10,
  label = TRUE
)


library(ggraph)
library(ggplot2)
library(dplyr)

fiber_network <- network[[1]]


