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

alpha_cors <- alpha_cors |>
  mutate(Shannon_p = p.adjust(Shannon_p, method = "BH"), 
         Chao1_p = p.adjust(Chao1_p, method = "BH"))

sig_cors <- alpha_cors |>
  filter(Shannon_p <= 0.1 | Chao1_p <= 0.1) |>
  pull(nutrient)

sigs <- which(alpha_cors$nutrient %in% sig_cors)

#Create Plots for Significant Nutrients

library(patchwork)
library(emmeans)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Alpha Diversity/Microbiome")
for(i in c(sigs, 31:34)){
  
  variable = alpha_cors$nutrient[i]

  shannon_mod <- lm(as.formula(paste("Shannon ~ ", variable, " + calories + sex + age", sep = "")), data = data.frame(food))

  
  var_seq <- seq(
    min(food[[variable]], na.rm = TRUE),
    max(food[[variable]], na.rm = TRUE),
    length.out = 100
  )
  
  at_list <- list(
    calories = mean(food$calories, na.rm = TRUE),
    age = mean(food$age, na.rm = TRUE)
  )
  
  # add the variable sequence using the string name
  at_list[[variable]] <- var_seq
  
  pred <- emmeans(
    shannon_mod,
    specs = as.formula(paste("~", variable)),
    at = at_list,
    weights = "proportional"
  )
  
  pred_df <- as.data.frame(pred)
  
  
  shannon_plot <- ggplot() +
    geom_point(data = data.frame(food), aes(x = !!sym(variable), y = Shannon), alpha = 0.4) +
    geom_line(
      data = pred_df,
      aes(x = !!sym(variable), y = emmean),
      linewidth = 1.2
    ) +
    labs(
      x = paste(variable, " (mg/week)", sep = ""),
      y = "Shannon diversity",
      title = paste(variable, "vs. Shannon diversity"),
      subtitle = "Adjusted for calories and age; averaged over sex"
    ) +
    theme_bw() + 
    theme(
      axis.title = element_text(size = 14),
      axis.text  = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)
    )
  
  
  chao1_mod <- lm(as.formula(paste("Chao1 ~ ", variable, " + calories + sex + age", sep = "")), data = data.frame(food))
  
  
  var_seq <- seq(
    min(food[[variable]], na.rm = TRUE),
    max(food[[variable]], na.rm = TRUE),
    length.out = 100
  )
  
  at_list <- list(
    calories = mean(food$calories, na.rm = TRUE),
    age = mean(food$age, na.rm = TRUE)
  )
  
  # add the variable sequence using the string name
  at_list[[variable]] <- var_seq
  
  pred <- emmeans(
    chao1_mod,
    specs = as.formula(paste("~", variable)),
    at = at_list,
    weights = "proportional"
  )
  
  pred_df <- as.data.frame(pred)
  
  
chao1_plot <- ggplot() +
    geom_point(data = data.frame(food), aes(x = !!sym(variable), y = Chao1), alpha = 0.4) +
    geom_line(
      data = pred_df,
      aes(x = !!sym(variable), y = emmean),
      linewidth = 1.2
    ) +
    labs(
      x = paste(variable, " (mg/week)", sep = ""),
      y = "Chao1 diversity",
      title = paste(variable, "vs. Chao1 diversity"),
      subtitle = "Adjusted for calories and age; averaged over sex"
    ) +
    theme_bw() + 
  theme(
    axis.title = element_text(size = 28),
    axis.text  = element_text(size = 26),
    plot.title = element_text(size = 0, face = "bold"),
    plot.subtitle = element_text(size = 0),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
  

  
  
  ggsave(shannon_plot, filename = paste(variable, "_shannon_alpha_diversity.png", sep = ""), width = 8, height = 4, dpi = 800)
  ggsave(chao1_plot, filename = paste(variable, "_chao1_alpha_diversity.png", sep = ""), width = 8, height = 4, dpi = 800)
  
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

alpha_cors <- alpha_cors |>
  mutate(Shannon_p = p.adjust(Shannon_p, method = "BH"), 
         Simpson_p = p.adjust(Simpson_p, method = "BH"))

sig_cors <- alpha_cors |>
  filter(Shannon_p <= 0.05 | Simpson_p <= 0.05) |>
  pull(nutrient)

sigs <- which(alpha_cors$nutrient %in% sig_cors)


calculate_sem <- function(x) {
  # Remove NA values if present
  x <- na.omit(x) 
  sd_val <- sd(x) # Calculate the standard deviation
  n_val <- length(x) # Get the sample size
  sem <- sd_val / sqrt(n_val) # Calculate SEM
  return(sem)
}



