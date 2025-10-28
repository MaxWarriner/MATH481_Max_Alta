library(tidyverse)
library(phyloseq)
library(vegan)
library(stats)
library(MicrobiotaProcess)
library(patchwork)

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

# Important comparisons: fiber, fat, plant protein, PUFA, fermented foods, fiber+fat
