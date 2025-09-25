library(tidyverse)
library(phyloseq)

sam <- read_csv('sample_data.csv')[,-1]

ps <- readRDS('microbiome.RDS')


diversity <- estimate_richness(ps, measures = c('Shannon', 'Chao1'))

sam$Shannon <- diversity$Shannon
sam$Chao1 <- diversity$Chao1

# Preliminary Violin Plots

protein_violin <- ggplot(sam, aes(x = "", y = protein)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Protein (g/week)") +
  ggtitle("Protein") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

fat_violin <- ggplot(sam, aes(x = "", y = protein)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Fat (g/week)") +
  ggtitle("Fat") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

carbs_violin <- ggplot(sam, aes(x = "", y = carbs)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Carbohydrates (g/week)") +
  ggtitle("Carbohydrates") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

fiber_violin <- ggplot(sam, aes(x = "", y = fiber)) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.25) +
  get("theme_bw")() +
  xlab("") +
  ylab("Fiber (g/week)") +
  ggtitle("Fiber") +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5))

library(patchwork)

violins <- protein_violin + fat_violin + carbs_violin + fiber_violin

ggsave(violins, filename = "violin_plots.png", width = 7, height = 5)


# Scatter plots with alpha diversity

protein <- ggplot(data = sam, aes(x = protein_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Protein (g/kg bodyweight)') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Protein vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$protein_norm), 3), sep = ''))


fat <- ggplot(data = sam, aes(x = fat_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fat (g/kg bodyweight)') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Fat vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$fat_norm), 3), sep = ''))


carbs <- ggplot(data = sam, aes(x = carbs_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Carbohydrates (g/kg bodyweight)') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Carbohydrates vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$carbs_norm), 3), sep = ''))

fiber <- ggplot(data = sam, aes(x = fiber_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fiber (g/kg bodyweight)') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Fiber vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$fiber_norm), 3), sep = ''))

calories <- ggplot(data = sam, aes(x = calories_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('calories/week/kg bodyweight') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Calories vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$calories_norm), 3), sep = ''))


scatters <- protein + fat + carbs + fiber

ggsave(scatters, filename = "scatters.png", width = 8, height = 6)


mean(sam$calories)/7

summary(lm(diversity ~ protein + fiber + carbs + fat + Age + sex + calories, data = sam))


metab <- readxl::read_xlsx('metabolomics_data.xlsx')



# food groups alpha diversity

veg <- ggplot(data = sam, aes(x = vegetable_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Vegetable Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Vegetable Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$vegetable_portions_norm), 3), sep = ''))


legumes <- ggplot(data = sam, aes(x = legume_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Legume Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Legume Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$legume_portions_norm), 3), sep = ''))


grains <- ggplot(data = sam, aes(x = grain_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Grain Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Grain Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$grain_portions_norm), 3), sep = ''))


fruits <- ggplot(data = sam, aes(x = fruit_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fruit Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Fruit Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$fruit_portions_norm), 3), sep = ''))


meats <- ggplot(data = sam, aes(x = meat_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Meat Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Meat Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$meat_portions_norm), 3), sep = ''))


eggs_dairy <- ggplot(data = sam, aes(x = eggs_dairy_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Eggs & Dairy Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Egggs & Dairy Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$eggs_dairy_portions_norm), 3), sep = ''))


proc_foods <- ggplot(data = sam, aes(x = processed_food_portions_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Processed Food Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Processed Food Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$processed_food_portions_norm), 3), sep = ''))


kocho <- ggplot(data = sam, aes(x = kocho, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Kocho Portions/week') +
  ylab('Alpha Diversity (Shannon)') +
  geom_smooth(method = c("lm"), se = F) + 
  ggtitle(paste('Kocho Portions vs. Alpha Diversity: r = ', round(cor(sam$Shannon, sam$kocho), 3), sep = ''))
