library(brms)
library(compositions)
library(ggdist)
library(glue)
library(patchwork)
library(tidyverse)
library(vroom)
library(philr)
library(phyloseq)
library(mediation)
library(semPower)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
ps <- read_rds('microbiome.RDS')

sam <- ps@sam_data


# Part 1: Nutrient Mediation ----------------------------------------------

div <- estimate_richness(ps)

food <- sam[,c(119:149, 210:212, 214)]

health <- sam[,c(79, 81, 82, 83)]

tax <- data.frame(ps@tax_table@.Data)

abundance <- data.frame(ps@otu_table@.Data)

colnames(abundance) <- gsub("X", "", colnames(abundance))

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

abundance <-cbind(abundance, tax)

abundance_cols <- names(abundance)[sapply(abundance, is.numeric)]

genus_abundance <- abundance %>%
  pivot_longer(
    cols = all_of(abundance_cols),  # only numeric columns
    names_to = "Sample",
    values_to = "Abundance"
  ) %>%
  group_by(Sample, Genus) %>%       # sum by genus
  summarise(Abundance = sum(Abundance), .groups = "drop") %>%
  pivot_wider(
    names_from = Genus,             # genera become columns
    values_from = Abundance,
    values_fill = 0
  ) |>
  arrange(as.numeric(Sample)) |>
  column_to_rownames(var = "Sample")

# Center Log Transform Genus Abundance
genus_abundance <- as.data.frame(clr(genus_abundance + 1))

colnames(genus_abundance) <- c("A284", "A28YEA48", c(colnames(genus_abundance[-c(1,2)])))

colnames(genus_abundance) <- gsub(pattern = "-", replacement = "_", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = " ", replacement = "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "\\[", replacement = "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "]", replacement = "", colnames(genus_abundance))

genus_abundance$shannon <- div$Shannon
genus_abundance$chao1 <- div$Chao1

combined <- cbind(food, health, genus_abundance)
combined$sex <- sam$sex
combined$age <- sam$Age

nutrients <- colnames(food)
microbes <- colnames(genus_abundance)
health_outcomes <- colnames(health)


#mediation package

library(mediation)
set.seed(101)

mediation_dat <- data.frame(
  nutrient = character(),
  microbe = character(),
  health_outcome = character(),
  total_effect = numeric(),
  total_p = numeric(),
  direct_effect = numeric(),
  direct_p = numeric(),
  indirect_effect = numeric(),
  indirect_p = numeric(),
  proportion_estimate = numeric(),
  proportion_p = numeric(),
  stringsAsFactors = FALSE
)


for(health_outcome in health_outcomes){
  
  for(nutrient in nutrients[-1]){
    
    nh_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + calories + sex + age", sep = ""))
    nh_regression_p <- data.frame(summary(glm(nh_formula, data = combined))$coefficients)$Pr...t..[2]
    
    if(nh_regression_p < 0.05){
      
      
      for(microbe in microbes){
        
        nm_formula <- as.formula(paste(microbe, " ~ ", nutrient, " + calories + sex + age", sep = ""))
        nm_regression_p <- data.frame(summary(lm(nm_formula, data = combined))$coefficients)$Pr...t..[2]
        
        mh_formula <- as.formula(paste(health_outcome, " ~ ", microbe, " + calories + sex + age", sep = ""))
        mh_regression_p <- data.frame(summary(glm(mh_formula, data = combined))$coefficients)$Pr...t..[2]
  
        if(nm_regression_p < 0.05 & mh_regression_p < 0.05){
          
          med.fit_formula <- as.formula(paste(microbe, " ~ ", nutrient, " + calories + sex + age", sep = ""))
          out.fit_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + ", microbe, " + calories + sex + age", sep = ""))
          
          
          med.fit <- lm(med.fit_formula, data = combined)
          
          out.fit <- glm(out.fit_formula,data = combined, family = binomial(link = "logit"))
          
          
          med.out <- mediate(med.fit, out.fit,
                             treat = nutrient, mediator =  microbe,
                             boot = TRUE, sims = 1000)
          
            
            mediation_dat <- rbind(mediation_dat, data.frame(
              nutrient = nutrient,
              microbe = microbe, 
              health_outcome = health_outcome, 
              total_effect = med.out$tau.coef,
              total_p = med.out$tau.p,
              direct_effect = med.out$z.avg,
              direct_p = med.out$z.avg.p,
              indirect_effect = med.out$d.avg, 
              indirect_p = med.out$d.avg.p, 
              proportion_estimate = med.out$n.avg, 
              proportion_p = med.out$n.avg.p
            ))
            
        }
        
      }
    }
  }
}


# Save initial mediation
mediation_dat$adjusted_total_p <- p.adjust(mediation_dat$total_p, method = "BH")

write_csv(mediation_dat, 'mediation_microbiome.csv')
# 
write_csv(combined, 'combined_mediation_microbiome_data.csv')

library(tidyverse)
mediation_dat <- read_csv('mediation_microbiome.csv')

combined <- read_csv('combined_mediation_microbiome_data.csv')



# Microbiome Mediation ----------------------------------------------------

# diarrhea Mediation
# Treatment 1: grain_portions
# Mediators: Blautia, Rothia
# Treatment 1: vitaminC 
# Mediators: Catenisphaera, FamilyXIIIUCG_001, Isobaculum, Z20

library(lavaan)

model <- '

  ###################################
  # Treatment 1: grain_portions
  ###################################
  Blautia  ~ a1*grain_portions + age + sex + calories
  
  
  ###################################
  # Treatment 2: vitaminC
  ###################################
  Catenisphaera     ~ a2*vitaminC + age + sex + calories
  FamilyXIIIUCG_001 ~ a3*vitaminC + age + sex + calories
  Isobaculum        ~ a4*vitaminC + age + sex + calories

  ###################################
  # Outcome: diarrhea
  ###################################
  diarrhea ~ b1*Blautia +
      b2*Catenisphaera +
      b3*FamilyXIIIUCG_001 +
      b4*Isobaculum +
      c1*grain_portions +
      c2*vitaminC +
      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## grain_portions
  ind_grain_1 := a1*b1
  total_ind_grain := ind_grain_1
  total_effect_grain := c1 + total_ind_grain

  ## vitaminC
  ind_vitC_1 := a2*b2
  ind_vitC_2 := a3*b3
  ind_vitC_3 := a4*b4
  total_ind_vitC := ind_vitC_1 + ind_vitC_2 + ind_vitC_3
  total_effect_vitC := c2 + total_ind_vitC

'

fit <- sem(
  model,
  data = combined,
  ordered = "diarrhea",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"), 3)

#Power calculation for diarrhea microbiome

library(semPower)
full_model_power <- semPower.postHoc(effect = 0.204345,
                                effect.measure = 'RMSEA',
                                alpha = .05,
                                N = 57,
                                df = 10)

summary(full_model_power)

tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.204345,
                                     effect.measure = 'RMSEA',
                                     alpha = .05/tests,
                                     N = 57,
                                     df = 10)

summary(full_model_power)

# Bloating Mediation
# Model 1: 
# Treatment 1: fruit_portions 
# Mediators: Atopostipes, Jeotgalicoccus, Oceanicella, Subdoligranulum

library(lavaan)

model <- '

  ###################################
  # Treatment 1: fruit_portions
  ###################################
  Atopostipes ~ a1*fruit_portions + age + sex + calories
  Oceanicella ~ a2*fruit_portions + age + sex + calories
  Subdoligranulum ~ a3*fruit_portions + age + sex + calories


  ###################################
  # Outcome: bloating
  ###################################
  bloating ~ b1*Atopostipes + 
             b2*Oceanicella +
             b3*Subdoligranulum +
             c1*fruit_portions +
             age + sex + calories

  ###################################
  # Indirect effects
  ###################################

  ## fruit_portions → mediators → bloating
  ind_fruit_1 := a1*b1
  ind_fruit_2 := a2*b2
  ind_fruit_3 := a3*b3
  total_ind_fruit := ind_fruit_1 + ind_fruit_2 + ind_fruit_3
  total_effect_fruit := c1 + total_ind_fruit

'

fit <- sem(
  model,
  data = combined,
  ordered = "bloating",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 6)

fitMeasures(fit, "fmin")


full_model_power <- semPower.postHoc(effect = 0.079,
                                     effect.measure = 'F0',
                                     alpha = .05,
                                     N = 57,
                                     df = 6)

summary(full_model_power)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"), 3)


tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.079,
                                     effect.measure = 'F0',
                                     alpha = .05/tests,
                                     N = 57,
                                     df = 6)

summary(full_model_power)


# Abdominal Pain Mediation
# Model 1: 
# Treatment 1: fruit_or_vegetable 
# Mediators: Hespellia + Mobiluncus + Papillibacter

library(lavaan)

model <- '

  ###################################
  # Treatment 1: fruit_or_vegetable
  ###################################
  Hespellia ~ a1*fruit_or_vegetable + age + sex + calories
  Mobiluncus ~ a2*fruit_or_vegetable + age + sex + calories
  Papillibacter ~ a3*fruit_or_vegetable + age + sex + calories


  ###################################
  # Outcome: abdominalpain
  ###################################
  abdominalpain ~ b1*Hespellia + 
             b2*Mobiluncus +
             b3*Papillibacter +
             c1*fruit_or_vegetable +
             age + sex + calories

  ###################################
  # Indirect effects
  ###################################

  ## fruit_or_vegetable → mediators → diarrhea
  ind_fruit_veg_1 := a1*b1
  ind_fruit_veg_2 := a2*b2
  ind_fruit_veg_3 := a3*b3
  total_ind_fruit_veg := ind_fruit_veg_1 + ind_fruit_veg_2 + ind_fruit_veg_3
  total_effect_fruit_veg := c1 + total_ind_fruit_veg

'

fit <- sem(
  model,
  data = combined,
  ordered = "abdominalpain",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"), 3)


fitMeasures(fit, "fmin")

full_model_power <- semPower.postHoc(
  effect.measure = "F0",
  effect = 0.022,
  df = 6,
  alpha = .05, 
  N = 57
)

summary(full_model_power)

tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(
  effect.measure = "F0",
  effect = 0.022,
  df = 6,
  alpha = .05/tests, 
  N = 57
)

summary(full_model_power)


# lower appetite Mediation
# Treatment 2: vitaminC  
# Mediators: LachnospiraceaeUCG_001 + Papillibacter + Yaniella

library(lavaan)

model <- '
  
  ###################################
  # Treatment 2: vitaminC
  ###################################
  LachnospiraceaeUCG_001 ~ a1*vitaminC + age + sex + calories
  Papillibacter ~ a2*vitaminC + age + sex + calories

  ###################################
  # Outcome: lower_appetite
  ###################################
  lower_appetite ~ b1*LachnospiraceaeUCG_001 + 
             b2*Papillibacter +
             c1*vitaminC + 
             age + sex + calories

  ###################################
  # Indirect effects
  ###################################
  
  ## vitaminC → mediators → lower_appetite
  ind_VitC_1 := a1*b1
  ind_VitC_2 := a2*b2
  total_ind_VitC := ind_VitC_1 + ind_VitC_2
  total_effect_VitC := c1 + total_ind_VitC

'

fit <- sem(
  model,
  data = combined,
  ordered = "lower_appetite",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"), 3)


fitMeasures(fit, "fmin")

full_model_power <- semPower.postHoc(effect = 0.001,
                                     effect.measure = 'F0',
                                     alpha = .05,
                                     N = 57,
                                     df = 3)

summary(full_model_power)


tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.001,
                                     effect.measure = 'F0',
                                     alpha = .05/tests,
                                     N = 57,
                                     df = 3)

summary(full_model_power)


# Metabolomics Mediation --------------------------------------------------

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
metab <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames(var = "ID")

common_samples <- intersect(rownames(metab), rownames(sam))

metab <- metab[common_samples,]
metab <- scale(metab)

sam <- sam[common_samples,]

food <- sam[,c(119:149, 210:212, 214)]

health <- sam[,78:84]

combined <- cbind(food, health, metab)
combined$sex <- sam$sex
combined$age <- sam$Age

nutrients <- colnames(food)
metabolites <- colnames(metab)
health_outcomes <- colnames(health)[c(-1, -3, -7)]




library(mediation)
set.seed(101)

mediation_dat <- data.frame(
  nutrient = character(),
  microbe = character(),
  health_outcome = character(),
  total_effect = numeric(),
  total_p = numeric(),
  direct_effect = numeric(),
  direct_p = numeric(),
  indirect_effect = numeric(),
  indirect_p = numeric(),
  proportion_estimate = numeric(),
  proportion_p = numeric(),
  stringsAsFactors = FALSE
)


for(health_outcome in health_outcomes){
  
  for(nutrient in nutrients[-1]){
    
    nh_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + calories + sex + age", sep = ""))
    nh_regression_p <- data.frame(summary(glm(nh_formula, data = combined))$coefficients)$Pr...t..[2]
    
    if(nh_regression_p < 0.05){
      
      
      for(metabolite in metabolites){
        
        nm_formula <- as.formula(paste(metabolite, " ~ ", nutrient, " + calories + sex + age", sep = ""))
        nm_regression_p <- data.frame(summary(lm(nm_formula, data = combined))$coefficients)$Pr...t..[2]
        
        mh_formula <- as.formula(paste(health_outcome, " ~ ", metabolite, " + calories + sex + age", sep = ""))
        mh_regression_p <- data.frame(summary(glm(mh_formula, data = combined))$coefficients)$Pr...t..[2]
        
        if(nm_regression_p < 0.05 & mh_regression_p < 0.05){
          
          med.fit_formula <- as.formula(paste(metabolite, " ~ ", nutrient, " + calories + sex + age", sep = ""))
          out.fit_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + ", metabolite, " + calories + sex + age", sep = ""))
          
          
          med.fit <- lm(med.fit_formula, data = combined)
          
          out.fit <- glm(out.fit_formula,data = combined, family = binomial(link = "logit"))
          
          
          med.out <- mediate(med.fit, out.fit,
                             treat = nutrient, mediator =  metabolite,
                             boot = TRUE, sims = 1000)
          
          
          mediation_dat <- rbind(mediation_dat, data.frame(
            nutrient = nutrient,
            metabolite = metabolite, 
            health_outcome = health_outcome, 
            total_effect = med.out$tau.coef,
            total_p = med.out$tau.p,
            direct_effect = med.out$z.avg,
            direct_p = med.out$z.avg.p,
            indirect_effect = med.out$d.avg, 
            indirect_p = med.out$d.avg.p, 
            proportion_estimate = med.out$n.avg, 
            proportion_p = med.out$n.avg.p
          ))
          
        }
        
      }
    }
  }
}

# Save initial mediation
mediation_dat$adjusted_total_p <- p.adjust(mediation_dat$total_p, method = "BH")
mediation_dat <- mediation_dat |>
  filter(adjusted_total_p < 0.05)

write_csv(mediation_dat, 'mediation_metabolomics.csv')
# 
write_csv(combined, 'combined_mediation_metabolomics_data.csv')
library(tidyverse)
mediation_dat <- read_csv('mediation_metabolomics.csv')

combined <- read_csv('combined_mediation_metabolomics_data.csv')

combined <- cbind(combined[,c(1:42, 505:506)], scale(combined[, 42:504]))


# diarrhea Mediation
# Treatment 1: grain_portions
# Mediators: X3.4.Dihydroxybenzoic.acid, Hamamelitannin, X6.Oxooctadecanoic.acid
# Treatment 3: vitaminC
# Mediators: Diisopropyl.phosphate, X1.Stearoyl.2.arachidonoyl.sn.glycero.3.phospho..1..myo.inositol.


library(lavaan)

model <- '

  ###################################
  # Treatment 1: grain_portions
  ###################################
  X3.4.Dihydroxybenzoic.acid  ~ a1*grain_portions + age + sex + calories
  Hamamelitannin  ~ a2*grain_portions + age + sex + calories
  X6.Oxooctadecanoic.acid  ~ a3*grain_portions + age + sex + calories
  
  
  ###################################
  # Treatment 3: vitaminC
  ###################################
  Diisopropyl.phosphate  ~ a4*vitaminC + age + sex + calories

  ###################################
  # Outcome: diarrhea
  ###################################
  diarrhea ~ b1*X3.4.Dihydroxybenzoic.acid +
      b2*Hamamelitannin +
      b3*X6.Oxooctadecanoic.acid +
      b4*Diisopropyl.phosphate +
      c1*grain_portions +
      c3*vitaminC +
      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## grain_portions
  ind_grain_1 := a1*b1
  ind_grain_2 := a2*b2
  ind_grain_3 := a3*b3
  total_ind_grain := ind_grain_1 + ind_grain_2 + ind_grain_3
  total_effect_grain := c1 + total_ind_grain

  ## vitaminC
  ind_vitC_1 := a4*b4
  total_ind_vitC := ind_vitC_1
  total_effect_vitC := c3 + total_ind_vitC

'

fit <- sem(
  model,
  data = combined,
  ordered = "diarrhea",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"),3)


fitMeasures(fit, "fmin")

full_model_power <- semPower.postHoc(effect = 0.227,
                                     effect.measure = 'F0',
                                     alpha = .05,
                                     N = 54,
                                     df = 10)

summary(full_model_power)

tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.227,
                                     effect.measure = 'F0',
                                     alpha = .05/tests,
                                     N = 54,
                                     df = 10)

summary(full_model_power)


# bloating Mediation
# Treatment 1: fruit_portions
# Mediators: L.Threonine, X4.Hydroxy.4.methyl.2.pentanone, Aniline, X2.Keto.3.deoxyoctonic.acid


library(lavaan)

model <- '

  ###################################
  # Treatment 1: fruit_portions
  ###################################
  L.Threonine  ~ a1*fruit_portions + age + sex + calories
  X4.Hydroxy.4.methyl.2.pentanone  ~ a2*fruit_portions + age + sex + calories
  Aniline  ~ a3*fruit_portions + age + sex + calories
  X2.Keto.3.deoxyoctonic.acid  ~ a4*fruit_portions + age + sex + calories

  ###################################
  # Outcome: bloating
  ###################################
  bloating ~ b1*L.Threonine +
      b2*X4.Hydroxy.4.methyl.2.pentanone +
      b3*Aniline +
      b4*X2.Keto.3.deoxyoctonic.acid +
      c1*fruit_portions +
      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## fruit_portions
  ind_fruit_1 := a1*b1
  ind_fruit_2 := a2*b2
  ind_fruit_3 := a3*b3
  ind_fruit_4 := a4*b4
  total_ind_fruit := ind_fruit_1 + ind_fruit_2 + ind_fruit_3 + ind_fruit_4
  total_effect_fruit := c1 + total_ind_fruit

'

fit <- sem(
  model,
  data = combined,
  ordered = "bloating",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 4)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"),3)

fitMeasures(fit, "fmin")

full_model_power <- semPower.postHoc(effect = 0.261,
                                     effect.measure = 'F0',
                                     alpha = .05,
                                     N = 54,
                                     df = 10)

summary(full_model_power)

tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.261,
                                     effect.measure = 'F0',
                                     alpha = .05/tests,
                                     N = 54,
                                     df = 10)

summary(full_model_power)

# abdominalpain Mediation
# Treatment 1: fruit_or_vegetable
# Mediators: Aniline


library(lavaan)

model <- '

  ###################################
  # Treatment 1: fruit_or_vegetable
  ###################################
  Aniline  ~ a1*fruit_or_vegetable + age + sex + calories

  ###################################
  # Outcome: abdominalpain
  ###################################
  abdominalpain ~ b1*Aniline +
      c1*fruit_or_vegetable +
      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## fruit_or_vegetable
  ind_fruit_veg_1 := a1*b1
  total_ind_fruit_veg := ind_fruit_veg_1
  total_effect_fruit_veg := c1 + total_ind_fruit_veg

'

fit <- sem(
  model,
  data = combined,
  ordered = "abdominalpain",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 4)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"),3)

tests <- sum(!is.na(sum$adj))

#unable to do SEM power with 0 df
#pivot to linear and logistic power

library(pwr)
#aniline ~ fruits/veg + calories + sex + age
power <- pwr.f2.test(
  u = 4,        # number of predictors
  v = 54 - 4 - 1,  # denominator df = n - u - 1
  f2 = summary(lm(Aniline  ~ fruit_or_vegetable + age + sex + calories, data = data.frame(combined)))$r.squared,
  sig.level = 0.05
)$power

power <- pwr.f2.test(
  u = 4,        # number of predictors
  v = 54 - 4 - 1,  # denominator df = n - u - 1
  f2 = summary(lm(Aniline  ~ fruit_or_vegetable + age + sex + calories, data = data.frame(combined)))$r.squared,
  sig.level = 0.05/tests
)$power

#abdominal pain ~ fruits/veg + aniline + calories + sex + age


posthoc_power_logistic <- function(
    formula,
    data,
    focal_var,
    B = 10000,
    alpha = 0.05,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  # Fit original model
  fit <- glm(formula, data = data, family = binomial)
  
  # Predicted probabilities
  p_hat <- fitted(fit)
  
  reject <- logical(B)
  
  for (b in seq_len(B)) {
    # Simulate outcome
    y_star <- rbinom(length(p_hat), size = 1, prob = p_hat)
    
    # Replace outcome in data
    data_star <- data
    outcome_name <- all.vars(formula)[1]
    data_star[[outcome_name]] <- y_star
    
    # Refit model
    fit_star <- glm(formula, data = data_star, family = binomial)
    
    # Extract p-value for focal variable
    p_val <- summary(fit_star)$coefficients[focal_var, "Pr(>|z|)"]
    
    reject[b] <- p_val < alpha
  }
  
  list(
    power = round(mean(reject),3),
    B = B,
    alpha = alpha
  )
}


posthoc_power_logistic(abdominalpain ~ Aniline + fruit_or_vegetable + age + sex + calories, data = combined, focal_var = "Aniline")

posthoc_power_logistic(abdominalpain ~ Aniline + fruit_or_vegetable + age + sex + calories, data = combined, focal_var = "Aniline", alpha = 0.05/tests)


# lower_appetite Mediation
# Treatment 1: vitaminA
# Mediators: Decanoylcarnitine, Phe.Val, Ala.Val, X15.Ketoprostaglandin.F2.alpha., Thr.Leu

model <- '

  ###################################
  # Treatment 1: vitaminA
  ###################################
  Decanoylcarnitine  ~ a1*vitaminA + age + sex + calories
  Phe.Val  ~ a2*vitaminA + age + sex + calories
  Ala.Val  ~ a3*vitaminA + age + sex + calories
  X15.Ketoprostaglandin.F2.alpha.  ~ a4*vitaminA + age + sex + calories
  Thr.Leu  ~ a5*vitaminA + age + sex + calories

  ###################################
  # Outcome: lower_appetite
  ###################################
  lower_appetite ~ b1*Decanoylcarnitine +
      b2*Phe.Val +
      b3*Ala.Val +
      b4*X15.Ketoprostaglandin.F2.alpha. +
      b5*Thr.Leu +
      c1*vitaminA +
      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## vitaminA
  ind_vitA_1 := a1*b1
  ind_vitA_2 := a2*b2
  ind_vitA_3 := a3*b3
  ind_vitA_4 := a4*b4
  ind_vitA_5 := a5*b5
  total_ind_vitA := ind_vitA_1 + ind_vitA_2 + ind_vitA_3 + ind_vitA_4 + ind_vitA_4
  total_effect_vitA := c1 + total_ind_vitA

'

fit <- sem(
  model,
  data = combined,
  ordered = "lower_appetite",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)

sum <- summary(fit, standardized = TRUE, fit.measures = TRUE, nd = 5)$pe

sum$adj <- round(p.adjust(sum$pvalue, method = "BH"),4)


fitMeasures(fit, "fmin")

full_model_power <- semPower.postHoc(effect = 0.704,
                                     effect.measure = 'F0',
                                     alpha = .05,
                                     N = 54,
                                     df = 15)

summary(full_model_power)

tests <- sum(!is.na(sum$adj))

full_model_power <- semPower.postHoc(effect = 0.704,
                                     effect.measure = 'F0',
                                     alpha = .05/tests,
                                     N = 54,
                                     df = 15)

summary(full_model_power)

