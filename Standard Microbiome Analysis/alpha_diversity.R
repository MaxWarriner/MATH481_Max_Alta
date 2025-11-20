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

alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -66, -67)], 
                     Shannon_beta = rep(NA, 64),
                     Shannon_p = rep(NA, 64),
                     Chao1_beta = rep(NA, 64), 
                     Chao1_p = rep(NA, 64))


#Test out nutrients against alpha diversity
for(i in 1:64){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("Shannon ~ ", variable, " + calories", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  Chao1_formula <- as.formula(paste("Chao1 ~ ", variable, " + calories", sep = ""))
  
  Chao1_mod <- data.frame(summary(lm(Chao1_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Chao1_beta[i] <- Chao1_mod$Estimate[2]
  alpha_cors$Chao1_p[i] <- Chao1_mod$Pr...t..[2]
  
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


alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -66, -67, -68, -69)], 
                     Shannon_beta = rep(NA, 64),
                     Shannon_p = rep(NA, 64),
                     Simpson_beta = rep(NA, 64), 
                     Simpson_p = rep(NA, 64))


#Test out nutrients against alpha diversity
for(i in 1:64){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("shannon ~ ", variable, " + calories", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  simpson_formula <- as.formula(paste("simpson ~ ", variable, " + calories", sep = ""))
  
  simpson_mod <- data.frame(summary(lm(simpson_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Simpson_beta[i] <- simpson_mod$Estimate[2]
  alpha_cors$Simpson_p[i] <- simpson_mod$Pr...t..[2]
  
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


