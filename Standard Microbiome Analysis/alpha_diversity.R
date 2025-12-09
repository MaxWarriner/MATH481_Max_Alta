library(tidyverse)
library(phyloseq)
library(vegan)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- sam[,c(1:212, 214, 213)]

food <- sam[,c(119:149, 210:213)]

#Microbiome diversity

diversity <- estimate_richness(ps, measures = c('Shannon', 'Chao1'))

food$Shannon <- diversity$Shannon
food$Chao1 <- diversity$Chao1
food$age <- sam$Age
food$sex <- sam$sex

alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -36, -37, -38, -39)], 
                     Shannon_beta = rep(NA, 34),
                     Shannon_p = rep(NA, 34),
                     Chao1_beta = rep(NA, 34), 
                     Chao1_p = rep(NA, 34))


#Test out nutrients against alpha diversity
for(i in 1:34){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("Shannon ~ ", variable, " + calories + sex + age", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  Chao1_formula <- as.formula(paste("Chao1 ~ ", variable, " + calories + sex + age", sep = ""))
  
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
  
  plot_shannon <- ggplot(data = food, aes(x = !!sym(variable), y = Shannon, colour = calories)) + 
    geom_point() + 
    theme_bw() +
    xlab(paste(gsub("_", " ", variable), "/week", sep = "")) + 
    ylab("Shannon Diversity") + 
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(
      bquote(.(gsub("_", " ", variable)) ~ "vs. Shannon Diversity:" ~
               beta == .(round(alpha_cors$Shannon_beta[i], 3)) * "," ~
               "p =" ~ .(round(alpha_cors$Shannon_p[i], 3)))
    ) +
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  plot_chao <- ggplot(data = food, aes(x = !!sym(variable), y = Chao1, colour = calories)) + 
    geom_point() +
    theme_bw() +
    xlab(paste(gsub("_", " ", variable), "/week", sep = "")) + 
    ylab("Chao1 Diversity") + 
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(
      bquote(.(gsub("_", " ", variable)) ~ "vs. Chao1 Diversity:" ~
               beta == .(round(alpha_cors$Chao1_beta[i], 3)) * "," ~
               "p =" ~ .(round(alpha_cors$Chao1_p[i], 3)))
    ) +
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  

  
  
  ggsave(plot_shannon, filename = paste(variable, "_shannon_alpha_diversity.png", sep = ""), width = 12, height = 6, dpi = 800)
  ggsave(plot_chao, filename = paste(variable, "_chao1_alpha_diversity.png", sep = ""), width = 12, height = 6, dpi = 800)
  
}


# Metabolite Diversity

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")
metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')


food$shannon <- diversity(metabolites, index = "shannon")
food$simpson <- diversity(metabolites, index = "simpson")


alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -36, -37, -38, -39, -40, -41)], 
                     Shannon_beta = rep(NA, 34),
                     Shannon_p = rep(NA, 34),
                     Simpson_beta = rep(NA, 34), 
                     Simpson_p = rep(NA, 34))


#Test out nutrients against alpha diversity
for(i in 1:34){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("shannon ~ ", variable, " + calories + sex + age", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  simpson_formula <- as.formula(paste("simpson ~ ", variable, " + calories + sex + age", sep = ""))
  
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
  
  plot_shannon <- ggplot(data = food, aes(x = !!sym(variable), y = shannon, colour = calories)) + 
    geom_point() + 
    theme_bw() +
    xlab(paste(gsub("_", " ", variable), "/week", sep = "")) + 
    ylab("Shannon Diversity") + 
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(
      bquote(.(gsub("_", " ", variable)) ~ "vs. Shannon Diversity:" ~
               beta == ~ .(round(alpha_cors$Shannon_beta, 5)) * "," ~
               "p =" ~ .(round(alpha_cors$Shannon_p[i], 3)))
    ) +
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  
  plot_simpson <- ggplot(data = food, aes(x = !!sym(variable), y = simpson, colour = calories)) + 
    geom_point() +
    theme_bw() +
    xlab(paste(gsub("_", " ", variable), "/week", sep = "")) + 
    ylab("Simpson Diversity") + 
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(
      bquote(.(gsub("_", " ", variable)) ~ "vs. Simpson Diversity:" ~
               beta == ~ .(round(alpha_cors$Simpson_beta, 5)) * "," ~
               "p =" ~ .(round(alpha_cors$Simpson_p[i], 3)))
    ) +
    theme(
      plot.title = element_text(size = 32),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 28),
      legend.text  = element_text(size = 26)
    )
  
  
  ggsave(plot_shannon, filename = paste(variable, "_shannon_alpha_diversity.png", sep = ""), width = 14, height = 6, dpi = 800)
  ggsave(plot_simpson, filename = paste(variable, "_simpson_alpha_diversity.png", sep = ""), width = 14, height = 6, dpi = 800)
  
}

calculate_sem <- function(x) {
  # Remove NA values if present
  x <- na.omit(x) 
  sd_val <- sd(x) # Calculate the standard deviation
  n_val <- length(x) # Get the sample size
  sem <- sd_val / sqrt(n_val) # Calculate SEM
  return(sem)
}



