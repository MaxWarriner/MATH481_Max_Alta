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

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
ps <- read_rds('microbiome.RDS')

sam <- ps@sam_data


# Part 1: Nutrient Mediation ----------------------------------------------

div <- estimate_richness(ps)

food <- sam[,c(119:149, 210:212, 214)]

health <- sam[,78:84]

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

nutrients <- colnames(food)[c(1:5, 7:15, 17, 19:22, 24, 31:35)]
microbes <- colnames(genus_abundance)
health_outcomes <- colnames(health)[c(2, 4, 5, 6)]


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
                             boot = TRUE, sims = 2000)
          
            
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
mediation_dat$adjusted_indirect_p <- p.adjust(mediation_dat$indirect_p, method = "BH")
mediation_dat$adjusted_total_p <- p.adjust(mediation_dat$total_p, method = "BH")
write_csv(mediation_dat, 'mediation.csv')
# 
write_csv(combined, 'combined_mediation_data.csv')

library(tidyverse)
mediation_dat <- read_csv('mediation.csv')

combined <- read_csv('combined_mediation_data.csv')

# diarrhea Mediation
# Model 1: 
# Treatment 1: grain_portions 
# Mediators: Blautia
# Treatment 2: vitaminC 
# Mediators: Catenisphaera, FamilyXIIIUCG_001, Isobaculum, Z20

library(lavaan)

model <- '

  ###################################
  # Treatment 1: grain_portions
  ###################################
  Blautia ~ a1*grain_portions + age + sex + calories

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
  # Indirect effects
  ###################################

  ## grain_portions → Blautia → diarrhea
  ind_grain := a1*b1
  total_effect_grain := c1 + ind_grain

  ## vitaminC → mediators → diarrhea
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

summary(fit, standardized = TRUE, fit.measures = TRUE)




summary(lm(Jeotgalicoccus ~ fruit_portions + sex + age + calories, data = combined))
summary(lm(bloating ~ Jeotgalicoccus + sex + age + calories, data = combined))

med.fit <- lm(Jeotgalicoccus ~ fruit_portions + calories + sex + age, data = combined)

out.fit <- glm(bloating ~ Jeotgalicoccus + fruit_portions + calories + sex + age ,data = combined, family = binomial(link = "logit"))


med.out <- mediate(med.fit, out.fit,
                   treat = "fruit_portions", mediator =  "Jeotgalicoccus",
                   boot = TRUE, sims = 2000)

sum <- summary(med.out)


df_mediation <- data.frame(
  Effect = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
  Estimate = c(sum$d.avg, sum$z.avg, sum$tau.coef, sum$n.avg),
  CI_lower = c(sum$d.avg.ci[1], sum$z.avg.ci[1], sum$tau.ci[1], sum$n.avg.ci[1]),
  CI_upper = c(sum$d.avg.ci[2], sum$z.avg.ci[2], sum$tau.ci[2], sum$n.avg.ci[2]),
  p_value = c(sum$d.avg.p, sum$z.avg.p, sum$tau.p, sum$n.avg.p)
)







