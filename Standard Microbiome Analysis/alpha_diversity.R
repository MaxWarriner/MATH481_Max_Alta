library(tidyverse)
library(phyloseq)
library(vegan)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")
ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

food <- sam[,c(150:179, 211:213)]

#Microbiome diversity

diversity <- estimate_richness(ps, measures = c('Shannon', 'Chao1'))

food$Shannon <- diversity$Shannon
food$Chao1 <- diversity$Chao1

alpha_cors <- tibble(nutrient = colnames(food)[c(-34, -35)], 
                     Shannon_r = rep(NA, 33),
                     Shannon_p = rep(NA, 33),
                     Chao1_r = rep(NA, 33), 
                     Chao1_p = rep(NA, 33))


#Test out nutrients against alpha diversity
for(i in 1:33){
  
shannon_cor <- cor.test(unlist(food[,i]),food$Shannon)
alpha_cors$Shannon_r[i] <- shannon_cor$statistic
alpha_cors$Shannon_p[i] <- shannon_cor$p.value
  
chao_cor <- cor.test(unlist(food[,i]),food$Chao1)
alpha_cors$Chao1_r[i] <- chao_cor$statistic
alpha_cors$Chao1_p[i] <- chao_cor$p.value
  
}

sig_cors <- alpha_cors |>
  filter(Shannon_p <= 0.05 | Chao1_p <= 0.05) |>
  pull(nutrient)

#sodium

plot_sodium_shannon <- ggplot(data = food, aes(x = sodium_norm, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Mg sodium/week/1000 calories') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Sodium vs. Shannon Diversity: r = ', round(cor(food$Shannon, food$sodium_norm), 3), ', p = ', round(alpha_cors$Shannon_p[5],3) , sep = ''))

plot_sodium_chao <- ggplot(data = food, aes(x = sodium_norm, y = Chao1)) + 
  geom_point() +
  theme_bw() +
  xlab('Mg sodium/week/1000 calories') + 
  ylab('Chao1 Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Sodium vs. Chao1 Diversity: r = ', round(cor(food$Chao1, food$sodium_norm), 3), ', p = ', round(alpha_cors$Chao1_p[5],3), sep = ''))

library(patchwork)
sodium <- plot_sodium_shannon + plot_sodium_chao

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity")
ggsave(sodium, filename = "sodium_alpha_diversity.png", width = 18, height = 6)

#Fruit or Vegetable

plot_fruit_or_vegetable_shannon <- ggplot(data = food, aes(x = fruit_or_vegetable, y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fruits/Vegetables/Week') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fruits/Vegetables vs. Shannon Diversity: r = ', round(cor(food$Shannon, food$fruit_or_vegetable), 3), ', p = ', round(alpha_cors$Shannon_p[5],3) , sep = ''))

plot_fruit_or_vegetable_chao <- ggplot(data = food, aes(x = fruit_or_vegetable, y = Chao1)) + 
  geom_point() +
  theme_bw() +
  xlab('Fruits/Vegetables/Week') + 
  ylab('Chao1 Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fruits/Vegetables vs. Chao1 Diversity: r = ', round(cor(food$Chao1, food$fruit_or_vegetable), 3), ', p = ', round(alpha_cors$Chao1_p[5],3), sep = ''))

library(patchwork)
fruit_or_vegetables <- plot_fruit_or_vegetable_shannon + plot_fruit_or_vegetable_chao

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity")
ggsave(fruit_or_vegetables, filename = "fruit_or_vegetables_alpha_diversity.png", width = 18, height = 6)



# Metabolite Diversity

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")
metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

sam <- ps@sam_data

food <- sam[,154:183]


sam$shannon <- diversity(metabolites, index = "shannon")
sam$simpson <- diversity(metabolites, index = "simpson")


alpha_cors <- tibble(nutrient = colnames(food)[c(-31,-32)], 
                     Shannon_r = rep(NA, 30),
                     Shannon_p = rep(NA, 30),
                     Simpson_r = rep(NA, 30), 
                     Simpson_p = rep(NA, 30))


#Test out nutrients against alpha diversity
for(i in 1:30){
  
  shannon_cor <- cor.test(unlist(food[,i]),food$shannon)
  alpha_cors$Shannon_r[i] <- shannon_cor$statistic
  alpha_cors$Shannon_p[i] <- shannon_cor$p.value
  
  simpson_cor <- cor.test(unlist(food[,i]),food$simpson)
  alpha_cors$Simpson_r[i] <- simpson_cor$statistic
  alpha_cors$Simpson_p[i] <- simpson_cor$p.value
  
}


sig_cors <- alpha_cors |>
  filter(Shannon_p <= 0.1 | Simpson_p <= 0.1) |>
  pull(nutrient)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity/Metabolome")



#Plant Protein

plot_PUFA_shannon <- ggplot(data = food, aes(x = PUFA_norm, y = shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('g PUFA/week/1000 calories') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('PUFA vs. Shannon Diversity: r = ', round(cor(food$shannon, food$PUFA_norm), 3), ', p = ', round(alpha_cors$Shannon_p[13],3) , sep = ''))

plot_PUFA_simpson <- ggplot(data = food, aes(x = PUFA_norm, y = simpson)) + 
  geom_point() + 
  theme_bw() +
  xlab('g PUFA/week/1000 calories') + 
  ylab('Simpson Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('PUFA vs. Simpson Diversity: r = ', round(cor(food$simpson, food$PUFA_norm), 3), ', p = ', round(alpha_cors$Simpson_p[13],3) , sep = ''))


library(patchwork)
PUFA <- plot_PUFA_shannon + plot_PUFA_simpson

ggsave(PUFA, filename = "PUFA_alpha_diversity.png", width = 18, height = 6)


#Magnesium

plot_magnesium_shannon <- ggplot(data = food, aes(x = magnesium_norm, y = shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Mg Magnesium/week/1000 calories') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Magnesium vs. Shannon Diversity: r = ', round(cor(food$shannon, food$magnesium_norm), 3), ', p = ', round(alpha_cors$Shannon_p[25],3) , sep = ''))

plot_magnesium_simpson <- ggplot(data = food, aes(x = magnesium_norm, y = simpson)) + 
  geom_point() + 
  theme_bw() +
  xlab('Mg Magnesium/week/1000 calories') + 
  ylab('Simpson Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Magnesium vs. Simpson Diversity: r = ', round(cor(food$simpson, food$magnesium_norm), 3), ', p = ', round(alpha_cors$Simpson_p[25],3) , sep = ''))


library(patchwork)
magnesium <- plot_magnesium_shannon + plot_magnesium_simpson

ggsave(magnesium, filename = "magnesium_alpha_diversity.png", width = 18, height = 6)



#fermented portions

plot_fermented_portions_shannon <- ggplot(data = food, aes(x = fermented_portions_norm, y = shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fermented Foods/week/1000 calories') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fermented Foods vs. Shannon Diversity: r = ', round(cor(food$shannon, food$fermented_portions_norm), 3), ', p = ', round(alpha_cors$Shannon_p[25],3) , sep = ''))

plot_fermented_portions_simpson <- ggplot(data = food, aes(x = fermented_portions_norm, y = simpson)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fermented Foods/week/1000 calories') + 
  ylab('Simpson Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fermented Foods vs. Simpson Diversity: r = ', round(cor(food$simpson, food$fermented_portions_norm), 3), ', p = ', round(alpha_cors$Simpson_p[25],3) , sep = ''))


fermented_portions <- plot_fermented_portions_shannon + plot_fermented_portions_simpson

ggsave(fermented_portions, filename = "fermented_portions_alpha_diversity.png", width = 18, height = 6)


#nutrient score

plot_fermented_portions_shannon <- ggplot(data = food, aes(x = fermented_portions_norm, y = shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fermented Foods/week/1000 calories') + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fermented Foods vs. Shannon Diversity: r = ', round(cor(food$shannon, food$fermented_portions_norm), 3), ', p = ', round(alpha_cors$Shannon_p[25],3) , sep = ''))

plot_fermented_portions_simpson <- ggplot(data = food, aes(x = fermented_portions_norm, y = simpson)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fermented Foods/week/1000 calories') + 
  ylab('Simpson Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste('Fermented Foods vs. Simpson Diversity: r = ', round(cor(food$simpson, food$fermented_portions_norm), 3), ', p = ', round(alpha_cors$Simpson_p[25],3) , sep = ''))


fermented_portions <- plot_fermented_portions_shannon + plot_fermented_portions_simpson

ggsave(fermented_portions, filename = "fermented_portions_alpha_diversity.png", width = 18, height = 6)



