library(tidyverse)
library(phyloseq)

sam <- read_csv('sample_data.csv')

ps <- readRDS('microbiome.RDS')


diversity <- estimate_richness(ps, measures = c('Chao1'))

sam$diversity <- diversity$Chao1

# Preliminary Violin Plots

protein_violin <- ggplot(sam, aes(x = "", y = protein_normal)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Protein (g/1000 calories)") +
  ggtitle("Protein") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

fat_violin <- ggplot(sam, aes(x = "", y = fat_normal)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Fat (g/1000 calories)") +
  ggtitle("Fat") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

carbs_violin <- ggplot(sam, aes(x = "", y = carbs_normal)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Carbohydrates (g/1000 calories)") +
  ggtitle("Carbohydrates") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

fiber_violin <- ggplot(sam, aes(x = "", y = fiber_normal)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Fiber (g/1000 calories)") +
  ggtitle("Fiber") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

library(patchwork)

violins <- protein_violin + fat_violin + carbs_violin + fiber_violin

ggsave(violins, filename = "violin_plots.png", width = 7, height = 5)


# Scatter plots with alpha diversity

protein <- ggplot(data = sam, aes(x = protein_normal, y = diversity)) + 
  geom_point() + 
  theme_bw() +
  xlab('Protein (g/1000 calories)') +
  ylab('Alpha Diversity (Chao1)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Protein vs. Alpha Diversity: r = ', round(cor(sam$diversity, sam$protein_normal), 3), sep = ''))


fat <- ggplot(data = sam, aes(x = fat_normal, y = diversity)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fat (g/1000 calories)') +
  ylab('Alpha Diversity (Chao1)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Fat vs. Alpha Diversity: r = ', round(cor(sam$diversity, sam$fat_normal), 3), sep = ''))


carbs <- ggplot(data = sam, aes(x = carbs_normal, y = diversity)) + 
  geom_point() + 
  theme_bw() +
  xlab('Carbohydrates (g/1000 calories)') +
  ylab('Alpha Diversity (Chao1)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Carbohydrates vs. Alpha Diversity: r = ', round(cor(sam$diversity, sam$carbs_normal), 3), sep = ''))

fiber <- ggplot(data = sam, aes(x = fiber_normal, y = diversity)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fiber (g/1000 calories)') +
  ylab('Alpha Diversity (Chao1)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Fiber vs. Alpha Diversity: r = ', round(cor(sam$diversity, sam$fiber_normal), 3), sep = ''))


scatters <- protein + fat + carbs + fiber

ggsave(scatters, filename = "scatters.png", width = 8, height = 6)


mean(sam$calories)/7

summary(lm(diversity ~ protein + fiber + carbs + fat + Age + sex + calories, data = sam))
