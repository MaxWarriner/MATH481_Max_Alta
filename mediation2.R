library(brms)
library(compositions)
library(ggdist)
library(glue)
library(patchwork)
library(tidyverse)
library(vroom)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
ps <- read_rds('microbiome.RDS')

sam <- ps@sam_data

food <- sam[,154:183]

health <- sam[,83:88]

metabolites <- read_csv('metabolites_transposed.csv') |>
  column_to_rownames(var = "...1")


mediation1_dat <- list(nutrient = rep("a", 1),
                         microbe = rep("a", 1),
                         health_outcome = rep("a", 1),
                         nutrient_to_health_outcome_p = rep(0, 1),
                         nutrient_to_microbe_p = rep(0, 1),
                         microbe_to_health_outcome_p = rep(0, 1), 
                         nutrient_with_microbe_p = rep(0, 1), 
                         microbe_with_nutrient_p = rep(0, 1), 
                         interpretation = rep("a", 1), 
                         direct_effect = rep(0, 1), 
                         indirect_effect = rep(0, 1))


metabolites <- metabolites %>%
  mutate(across(where(is.numeric), ~ log(.x + 1)))


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



common_samples <- intersect(rownames(genus_abundance), rownames(metabolites))
genus_abundance <- genus_abundance[common_samples,]
metabolites <- metabolites[common_samples,]
food <- food[common_samples,]
health <- health[common_samples,]

combined <- cbind(food, health, genus_abundance, metabolites)

nutrients <- colnames(food)
microbes <- colnames(genus_abundance)
health_outcomes <- colnames(health)


# Run the mediation (procedure copyrightable to Max Warriner 2025)

for(nutrient in nutrients){
  for(health_outcome in health_outcomes){
    
    fh_formula <- as.formula(paste(health_outcomes, " ~ ", nutrient, sep = ""))
    fh_regression <- data.frame(summary(glm(fh_formula, data = combined))$coefficients)
    fh_p <- fh_regression$Pr...t..[2]
    
    if(fh_regression <= 0.05){
      
      for(microbe in microbes){
        
        fm_formula <- as.formula(paste(microbe, " ~ ", nutrient, sep = ""))
        fm_regression <- data.frame(summary(lm(fm_formula, data = combined))$coefficients)
        fm_p <- fm_regression$Pr...t..[2]
        
        if(fm_regression <= 0.05){
          
          mh_formula <- as.formula(paste(health_outcome, " ~ ", microbe, sep = ""))
          mh_regression <- data.frame(summary(glm(mh_formula, data = combined))$coefficients)
          mh_p <- mh_regression$Pr...t..[2]
          
          if(mh_regression <= 0.05){
            
            fmh_formula <- as.formula(paste(health_outcome, " ~ ", microbe, " + ", nutrient, sep = ""))
            fmh_regression <- data.frame(summary(glm(fmh_formula, data = combined))$coefficients)
            fmh_p <- fmh_regression$Pr...t..[2:3]
            
            mediation1_dat$nutrient <- c(mediation1_dat$nutrient,nutrient)
            mediation1_dat$microbe <- c(mediation1_dat$microbe,microbe)
            mediation1_dat$health_outcome <- c(mediation1_dat$health_outcome,health_outcome)
            
            mediation1_dat$nutrient_to_health_outcome_p <- c(mediation1_dat$nutrient_to_health_outcome_p,fh_p)
            mediation1_dat$nutrient_to_microbe_p <- c(mediation1_dat$nutrient_to_microbe_p,fm_p)
            mediation1_dat$microbe_to_health_outcome_p <- c(mediation1_dat$microbe_to_health_outcome_p,mh_p)
            
            mediation1_dat$nutrient_with_microbe_p <- c(mediation1_dat$nutrient_with_microbe_p,fmh_p[2])
            mediation1_dat$microbe_with_nutrient_p <- c(mediation1_dat$microbe_with_nutrient_p,fmh_p[1])
            
            mediation1_dat$interpretation <- case_when(fmh_p[2] >= 0.1 ~ c(mediation1_dat$interpretation, "full mediation"), 
                                                       fmh_p[2] <= 0.1 & fmh_p[2] > 0.05 ~ c(mediation1_dat$interpretation, "partial mediation"), 
                                                       fmh_p[2] <= 0.05 ~ c(mediation1_dat$interpretation, "no mediation"))
            
            mediation1_dat$direct_effect <- c(mediation1_dat$direct_effect, fmh_regression[3,1])
            mediation1_dat$indirect_effect <- c(mediation1_dat$indirect_effect, fm_regression[2,1] * )
            
            
          }
            
        }
      }
    }
  }
}


#mediation package

library(mediation)

for(nutrient in nutrients){
  for(health_outcome in health_outcomes){
    for(microbe in microbes){

      med.fit_formula <- as.formula(paste(microbe, " ~ ", nutrient, sep = ""))
      out.fit_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + ", microbe, sep = ""))

      
med.fit <- lm(med.fit_formula, data = combined)

out.fit <- glm(out.fit_formula,data = combined, family = binomial(link = "logit"))

set.seed(101)
med.out <- mediate(med.fit, out.fit,
                   treat = nutrient, mediator =  microbe,
                   boot = TRUE, sims = 2000)

summary(med.out)

    }
  }
}






