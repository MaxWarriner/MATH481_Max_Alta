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

nutrients <- colnames(food)
microbes <- colnames(genus_abundance)
health_outcomes <- colnames(health)[c(-1, -3, -7)]


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

write_csv(mediation_dat, 'mediation.csv')
# 
write_csv(combined, 'combined_mediation_data.csv')

library(tidyverse)
mediation_dat <- read_csv('mediation.csv')

combined <- read_csv('combined_mediation_data.csv')

# diarrhea Mediation
# Model 1: 
# Treatment 1: grain_portions 
# Mediators: Blautia + Rothia 
# Treatment 2: vitaminC 
# Mediators: Catenisphaera, FamilyXIIIUCG_001, Isobaculum, Z20
# Treatment 3: phosphorus
# Mediators: Haemophilus, Rothia
# Treatment 4: iron
# Mediators: Blautia, Catenisphaera, Rothia
# Treatment 5: zinc
# Mediators: Catenisphaera, Isobaculum, Rothia
# Treatment 6: plant_protein
# Mediators: Oribacterium

library(lavaan)

model <- '

  ###################################
  # Treatment 1: grain_portions
  ###################################
  Blautia ~ a1*grain_portions + age + sex + calories
  Rothia  ~ a2*grain_portions + age + sex + calories

  ###################################
  # Treatment 2: vitaminC
  ###################################
  Catenisphaera     ~ a3*vitaminC + age + sex + calories
  FamilyXIIIUCG_001 ~ a4*vitaminC + age + sex + calories
  Isobaculum        ~ a5*vitaminC + age + sex + calories
  Z20               ~ a6*vitaminC + age + sex + calories

  ###################################
  # Treatment 3: phosphorus
  ###################################
  Haemophilus ~ a7*phosphorus + age + sex + calories
  Rothia      ~ a8*phosphorus + age + sex + calories

  ###################################
  # Treatment 4: iron
  ###################################
  Blautia       ~ a9*iron + age + sex + calories
  Catenisphaera ~ a10*iron + age + sex + calories
  Rothia        ~ a11*iron + age + sex + calories

  ###################################
  # Treatment 5: zinc
  ###################################
  Catenisphaera ~ a12*zinc + age + sex + calories
  Isobaculum    ~ a13*zinc + age + sex + calories
  Rothia        ~ a14*zinc + age + sex + calories

  ###################################
  # Treatment 6: plant_protein
  ###################################
  Oribacterium ~ a15*plant_protein + age + sex + calories

  ###################################
  # Outcome: diarrhea
  ###################################
  diarrhea ~   b1*Blautia +
      b2*Rothia +
      b3*Catenisphaera +
      b4*FamilyXIIIUCG_001 +
      b5*Isobaculum +
      b6*Z20 +
      b7*Haemophilus +
      b8*Oribacterium +

      c1*grain_portions +
      c2*vitaminC +
      c3*phosphorus +
      c4*iron +
      c5*zinc +
      c6*plant_protein +

      age + sex + calories

  ###################################
  # Indirect + total effects
  ###################################

  ## grain_portions
  ind_grain_1 := a1*b1
  ind_grain_2 := a2*b2
  total_ind_grain := ind_grain_1 + ind_grain_2
  total_effect_grain := c1 + total_ind_grain

  ## vitaminC
  ind_vitC_1 := a3*b3
  ind_vitC_2 := a4*b4
  ind_vitC_3 := a5*b5
  ind_vitC_4 := a6*b6
  total_ind_vitC := ind_vitC_1 + ind_vitC_2 + ind_vitC_3 + ind_vitC_4
  total_effect_vitC := c2 + total_ind_vitC

  ## phosphorus
  ind_phos_1 := a7*b7
  ind_phos_2 := a8*b2
  total_ind_phosphorus := ind_phos_1 + ind_phos_2
  total_effect_phosphorus := c3 + total_ind_phosphorus

  ## iron
  ind_iron_1 := a9*b1
  ind_iron_2 := a10*b3
  ind_iron_3 := a11*b2
  total_ind_iron := ind_iron_1 + ind_iron_2 + ind_iron_3
  total_effect_iron := c4 + total_ind_iron

  ## zinc
  ind_zinc_1 := a12*b3
  ind_zinc_2 := a13*b5
  ind_zinc_3 := a14*b2
  total_ind_zinc := ind_zinc_1 + ind_zinc_2 + ind_zinc_3
  total_effect_zinc := c5 + total_ind_zinc

  ## plant_protein
  ind_pp_1 := a15*b8
  total_effect_plant_protein := c6 + ind_pp_1

'

fit <- sem(
  model,
  data = combined,
  ordered = "diarrhea",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE)


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
  # Outcome: diarrhea
  ###################################
  bloating ~ b1*Atopostipes + 
             b2*Oceanicella +
             b3*Subdoligranulum +
             c1*fruit_portions +
             age + sex + calories

  ###################################
  # Indirect effects
  ###################################

  ## fruit_portions → mediators → diarrhea
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

summary(fit, standardized = TRUE, fit.measures = TRUE)


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

summary(fit, standardized = TRUE, fit.measures = TRUE)



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
  Yaniella ~ a3*vitaminC + age + sex + calories 

  ###################################
  # Outcome: lower_appetite
  ###################################
  lower_appetite ~ b1*LachnospiraceaeUCG_001 + 
             b2*Papillibacter +
             b3*Yaniella +
             c1*vitaminC + 
             age + sex + calories

  ###################################
  # Indirect effects
  ###################################
  
  ## vitaminC → mediators → lower_appetite
  ind_VitC_1 := a1*b1
  ind_VitC_2 := a2*b2
  ind_VitC_3 := a3*b3
  total_ind_VitC := ind_VitC_1 + ind_VitC_2 + ind_VitC_3
  total_effect_VitC := c1 + total_ind_VitC

'

fit <- sem(
  model,
  data = combined,
  ordered = "lower_appetite",
  estimator = "WLSMV"
)

summary(fit, standardized = TRUE, fit.measures = TRUE)




