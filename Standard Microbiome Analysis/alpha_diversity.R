library(tidyverse)
library(phyloseq)
library(vegan)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- sam[,c(1:212, 214, 213)]

food <- sam[,c(119:179, 210:213)]

#Microbiome diversity

diversity <- estimate_richness(ps, measures = c('Shannon', 'Chao1'))

food$Shannon <- diversity$Shannon
food$Chao1 <- diversity$Chao1

alpha_cors <- tibble(nutrient = colnames(food)[c(-66, -67)], 
                     Shannon_r = rep(NA, 65),
                     Shannon_p = rep(NA, 65),
                     Chao1_r = rep(NA, 65), 
                     Chao1_p = rep(NA, 65))


#Test out nutrients against alpha diversity
for(i in 1:65){
  
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

sigs <- which(alpha_cors$nutrient %in% sig_cors)

#Create Plots for Significant Nutrients

library(patchwork)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity/Microbiome")
for(i in sigs){

variable = alpha_cors$nutrient[i]

plot_shannon <- ggplot(data = food, aes(x = !!sym(variable), y = Shannon)) + 
  geom_point() + 
  theme_bw() +
  xlab(paste(gsub(pattern = "_", replacement = " ", variable), "/week", sep = "")) + 
  ylab('Shannon Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste(gsub(pattern = "_", replacement = " ", variable)," vs. Shannon Diversity: r = ", round(cor(food$Shannon, food[[variable]]), 3), ', p = ', round(alpha_cors$Shannon_p[i],3) , sep = ''))

plot_chao <- ggplot(data = food, aes(x = !!sym(variable), y = Chao1)) + 
  geom_point() +
  theme_bw() +
  xlab(paste(gsub(pattern = "_", replacement = " ", variable), "/week", sep = "")) + 
  ylab('Chao1 Diversity') + 
  geom_smooth(method = c('lm'), se = F) + 
  ggtitle(paste(gsub(pattern = "_", replacement = " ", variable)," vs. Chao1 Diversity: r = ", round(cor(food$Chao1, food[[variable]]), 3), ', p = ', round(alpha_cors$Chao1_p[i],3) , sep = ''))

combined <- plot_shannon + plot_chao + 
  plot_annotation(title = "Microbiome Alpha Diversity") &
  theme(plot.title = element_text(hjust = 0.5))

ggsave(combined, filename = paste(variable, "_alpha_diversity.png", sep = ""), width = 18, height = 6)

}


# Metabolite Diversity

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")
metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')


food$shannon <- diversity(metabolites, index = "shannon")
food$simpson <- diversity(metabolites, index = "simpson")


alpha_cors <- tibble(nutrient = colnames(food)[c(-66, -67, -68, -69)], 
                     Shannon_r = rep(NA, 65),
                     Shannon_p = rep(NA, 65),
                     Simpson_r = rep(NA, 65), 
                     Simpson_p = rep(NA, 65))


#Test out nutrients against alpha diversity
for(i in 1:65){
  
  shannon_cor <- cor.test(unlist(food[,i]),food$shannon)
  alpha_cors$Shannon_r[i] <- shannon_cor$statistic
  alpha_cors$Shannon_p[i] <- shannon_cor$p.value
  
  simpson_cor <- cor.test(unlist(food[,i]),food$simpson)
  alpha_cors$Simpson_r[i] <- simpson_cor$statistic
  alpha_cors$Simpson_p[i] <- simpson_cor$p.value
  
}


sig_cors <- alpha_cors |>
  filter(Shannon_p <= 0.05 | Simpson_p <= 0.05) |>
  pull(nutrient)

sigs <- which(alpha_cors$nutrient %in% sig_cors)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity/Metabolome")


for(i in sigs){
  
  variable = alpha_cors$nutrient[i]
  
  plot_shannon <- ggplot(data = food, aes(x = !!sym(variable), y = shannon)) + 
    geom_point() + 
    theme_bw() +
    xlab(paste(gsub(pattern = "_", replacement = " ", variable), "/week", sep = "")) + 
    ylab('Shannon Diversity') + 
    geom_smooth(method = c('lm'), se = F) + 
    ggtitle(paste(gsub(pattern = "_", replacement = " ", variable)," vs. Shannon Diversity: r = ", round(cor(food$shannon, food[[variable]]), 3), ', p = ', round(alpha_cors$Shannon_p[i],3) , sep = ''))
  
  plot_simpson <- ggplot(data = food, aes(x = !!sym(variable), y = simpson)) + 
    geom_point() +
    theme_bw() +
    xlab(paste(gsub(pattern = "_", replacement = " ", variable), "/week", sep = "")) + 
    ylab('Simpson Diversity') + 
    geom_smooth(method = c('lm'), se = F) + 
    ggtitle(paste(gsub(pattern = "_", replacement = " ", variable)," vs. Simpson Diversity: r = ", round(cor(food$simpson, food[[variable]]), 3), ', p = ', round(alpha_cors$Simpson_p[i],3) , sep = ''))
  
  combined <- plot_shannon + plot_simpson + 
    plot_annotation(title = "Metabolome Alpha Diversity") &
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(combined, filename = paste(variable, "_alpha_diversity.png", sep = ""), width = 18, height = 6)
  
}


