library(phyloseq)
library(ggClusterNet)
library(tidyverse)
library(WGCNA)
library(igraph)
library(ggraph)
library(tidyfst)

ps <- readRDS('microbiome.RDS')

tax <- ps@tax_table

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

tax_table(ps) <- tax

sam <- ps@sam_data

network <- network.pip(N = 100, ps = ps, group = "sex")

cor = corMicro(ps = ps, N = 100)

cor = cor[[1]]

maptree <- model_maptree2(cor = cor)

network_sex <- Facet.network(ps, g1 = "sex", fill = "Genus")

ggsave(network_sex$network.plot, filename = 'network_by_sex.png', width = 10, height = 8, dpi = 1000)

