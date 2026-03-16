library(tidyverse)
library(phyloseq)
library(vegan)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")

ps <- readRDS('microbiome.RDS')

sam <- ps@sam_data

sam <- sam[,c(1:212, 214, 213)]

food <- sam[,c(119:149, 210:213, 79, 81:83)]

#Microbiome diversity

diversity <- estimate_richness(ps, measures = c('Shannon', 'Chao1'))

food$Shannon <- diversity$Shannon
food$Chao1 <- diversity$Chao1
food$age <- sam$Age
food$sex <- sam$sex

alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -40:-43)], 
                     Shannon_beta = rep(NA, 38),
                     Shannon_p = rep(NA, 38),
                     Shannon_power = rep(NA, 38),
                     Shannon_power_adjusted = rep(NA, 38),
                     Chao1_beta = rep(NA, 38), 
                     Chao1_p = rep(NA, 38), 
                     Chao1_power = rep(NA, 38), 
                     Chao1_power_adjusted = rep(NA, 38))

library(pwr)

#Test out nutrients against alpha diversity
for(i in 1:38){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("Shannon ~ ", variable, " + calories + sex + age", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  alpha_cors$Shannon_power[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 57 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(shannon_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05
  )$power
  
  alpha_cors$Shannon_power_adjusted[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 57 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(shannon_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05/38
  )$power

  
  Chao1_formula <- as.formula(paste("Chao1 ~ ", variable, " + calories + sex + age", sep = ""))
  
  Chao1_mod <- data.frame(summary(lm(Chao1_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Chao1_beta[i] <- Chao1_mod$Estimate[2]
  alpha_cors$Chao1_p[i] <- Chao1_mod$Pr...t..[2]
  
  alpha_cors$Chao1_power[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 57 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(Chao1_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05
  )$power
  
  alpha_cors$Chao1_power_adjusted[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 57 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(Chao1_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05/38
  )$power
  
}

alpha_cors <- alpha_cors |>
  mutate(Shannon_adj_p = p.adjust(Shannon_p, method = "BH"), 
         Chao1_adj_p = p.adjust(Chao1_p, method = "BH"))

alpha_cors[, 2:9] <- round(alpha_cors[, 2:9], 3)

sig_cors <- alpha_cors |>
  filter(Shannon_adj_p <= 0.1 | Chao1_adj_p <= 0.1) |>
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

#Combined figure for paper

zinc_mod <- lm(as.formula(paste("Chao1 ~ ", 'zinc', " + calories + sex + age", sep = "")), data = data.frame(food))


var_seq <- seq(
  min(food[["zinc"]], na.rm = TRUE),
  max(food[["zinc"]], na.rm = TRUE),
  length.out = 100
)

at_list <- list(
  calories = mean(food$calories, na.rm = TRUE),
  age = mean(food$age, na.rm = TRUE)
)

# add the variable sequence using the string name
at_list[['zinc']] <- var_seq

library(emmeans)
pred <- emmeans(
  zinc_mod,
  specs = as.formula(paste("~", 'zinc')),
  at = at_list,
  weights = "proportional"
)

pred_df <- as.data.frame(pred)

zinc_plot <- ggplot() +
  geom_point(data = data.frame(food), aes(x = zinc, y = Chao1), alpha = 0.4) +
  geom_line(
    data = pred_df,
    aes(x = zinc, y = emmean),
    linewidth = 1.2
  ) +
  labs(
    x = paste('zinc', " (mg/week)", sep = ""),
    y = "Chao1 diversity",
    title ='(A) Zinc'
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 28),
    axis.text  = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 26)
  ) + 
  annotate("text", x = Inf, y = Inf, label = "p = 0.003, q = 0.063",
           hjust = 1.01, vjust = 8, size = 12, fontface = "plain") + 
  scale_y_continuous(breaks=c(1500, 2500, 3500))




iron_mod <- lm(as.formula(paste("Chao1 ~ ", 'iron', " + calories + sex + age", sep = "")), data = data.frame(food))


var_seq <- seq(
  min(food[["iron"]], na.rm = TRUE),
  max(food[["iron"]], na.rm = TRUE),
  length.out = 100
)

at_list <- list(
  calories = mean(food$calories, na.rm = TRUE),
  age = mean(food$age, na.rm = TRUE)
)

# add the variable sequence using the string name
at_list[['iron']] <- var_seq

pred <- emmeans(
  iron_mod,
  specs = as.formula(paste("~", 'iron')),
  at = at_list,
  weights = "proportional"
)

pred_df <- as.data.frame(pred)

iron_plot <- ggplot() +
  geom_point(data = data.frame(food), aes(x = iron, y = Chao1), alpha = 0.4) +
  geom_line(
    data = pred_df,
    aes(x = iron, y = emmean),
    linewidth = 1.2
  ) +
  labs(
    x = paste('iron', " (mg/week)", sep = ""),
    y = "Chao1 diversity",
    title = '(B) Iron'
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 28),
    axis.text  = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 26)
  ) + 
  annotate("text", x = Inf, y = Inf, label = "p = 0.002, q = 0.063",
           hjust = 1.01, vjust = 8, size = 12, fontface = "plain") + 
  scale_y_continuous(breaks=c(1500, 2500, 3500))

library(patchwork)
alpha_diversity <- (zinc_plot + iron_plot) &
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(alpha_diversity, filename = 'alpha_diversity_plot.png', dpi = 600, width = 15.5, height = 5.25)


# Metabolite Diversity

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Standard Microbiome Analysis")
metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames('...1')

common_samples <- intersect(rownames(metabolites), rownames(food))

metabolites <- metabolites[common_samples,]
food <- food[common_samples]


food$shannon <- diversity(metabolites, index = "shannon")
food$simpson <- diversity(metabolites, index = "simpson")


alpha_cors <- tibble(nutrient = colnames(food)[c(-1, -40:-45)], 
                     Shannon_beta = rep(NA, 38),
                     Shannon_p = rep(NA, 38),
                     Shannon_power = rep(NA, 38),
                     Shannon_power_adjusted = rep(NA, 38),
                     Simpson_beta = rep(NA, 38), 
                     Simpson_p = rep(NA, 38), 
                     Simpson_power = rep(NA, 38), 
                     Simpson_power_adjusted = rep(NA, 38))


#Test out nutrients against alpha diversity
for(i in 1:38){
  
  variable = alpha_cors$nutrient[i]
  
  shannon_formula <- as.formula(paste("shannon ~ ", variable, " + calories + sex + age", sep = ""))
  
  shannon_mod <- data.frame(summary(lm(shannon_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Shannon_beta[i] <- shannon_mod$Estimate[2]
  alpha_cors$Shannon_p[i] <- shannon_mod$Pr...t..[2]
  
  alpha_cors$Shannon_power[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 54 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(shannon_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05
  )$power
  
  alpha_cors$Shannon_power_adjusted[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 54 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(shannon_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05/38
  )$power
  
  
  simpson_formula <- as.formula(paste("simpson ~ ", variable, " + calories + sex + age", sep = ""))
  
  simpson_mod <- data.frame(summary(lm(simpson_formula, data = data.frame(food)))$coefficients)
  alpha_cors$Simpson_beta[i] <- simpson_mod$Estimate[2]
  alpha_cors$Simpson_p[i] <- simpson_mod$Pr...t..[2]
  
  alpha_cors$Simpson_power[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 54 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(simpson_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05
  )$power
  
  alpha_cors$Simpson_power_adjusted[i] <- pwr.f2.test(
    u = 4,        # number of predictors
    v = 54 - 4 - 1,  # denominator df = n - u - 1
    f2 = summary(lm(simpson_formula, data = data.frame(food)))$r.squared,
    sig.level = 0.05/38
  )$power
  
}

alpha_cors <- alpha_cors |>
  mutate(Shannon_adj_p = p.adjust(Shannon_p, method = "BH"), 
         Simpson_adj_p = p.adjust(Simpson_p, method = "BH"))

alpha_cors[, 2:9] <- round(alpha_cors[, 2:9], 3)

sig_cors <- alpha_cors |>
  filter(Shannon_adj_p <= 0.1 | Simpson_adj_p <= 0.1) |>
  pull(nutrient)

sigs <- which(alpha_cors$nutrient %in% sig_cors)



injera_mod <- lm(as.formula(paste("shannon ~ ", 'injera_sum', " + calories + sex + age", sep = "")), data = data.frame(food))


var_seq <- seq(
  min(food[["injera_sum"]], na.rm = TRUE),
  max(food[["injera_sum"]], na.rm = TRUE),
  length.out = 100
)

at_list <- list(
  calories = mean(food$calories, na.rm = TRUE),
  age = mean(food$age, na.rm = TRUE)
)

# add the variable sequence using the string name
at_list[['injera_sum']] <- var_seq

library(emmeans)
pred <- emmeans(
  injera_mod,
  specs = as.formula(paste("~", 'injera_sum')),
  at = at_list,
  weights = "proportional"
)

pred_df <- as.data.frame(pred)

injera_plot <- ggplot() +
  geom_point(data = data.frame(food), aes(x = injera_sum, y = shannon), alpha = 0.4) +
  geom_line(
    data = pred_df,
    aes(x = injera_sum, y = emmean),
    linewidth = 1.2
  ) +
  labs(
    x = paste('Injera', " (portions/week)", sep = ""),
    y = "Shannon diversity",
    title =''
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 28),
    axis.text  = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 26)
  ) + 
  annotate("text", x = Inf, y = Inf, label = "p = 0.001, q = 0.032",
           hjust = 1, vjust = 10.5, size = 12, fontface = "plain") + 
  scale_y_continuous(breaks=c(4.2, 4.4, 4.6)) & 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, 
                             unit = "in"))


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(injera_plot, filename = "injera_metabolome_diversity.png", dpi = 600, width = 9, height = 5.5)


calculate_sem <- function(x) {
  # Remove NA values if present
  x <- na.omit(x) 
  sd_val <- sd(x) # Calculate the standard deviation
  n_val <- length(x) # Get the sample size
  sem <- sd_val / sqrt(n_val) # Calculate SEM
  return(sem)
}



