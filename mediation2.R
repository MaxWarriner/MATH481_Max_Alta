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

colnames(genus_abundance) <- c("A284", "A28YEA48", c(colnames(genus_abundance[-c(1,2)])))

colnames(genus_abundance) <- gsub(pattern = "-", replacement = "_", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = " ", replacement = "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "\\[", replacement = "", colnames(genus_abundance))
colnames(genus_abundance) <- gsub(pattern = "]", replacement = "", colnames(genus_abundance))


common_samples <- intersect(rownames(genus_abundance), rownames(metabolites))
genus_abundance <- genus_abundance[common_samples,]
metabolites <- metabolites[common_samples,]
food <- food[common_samples,]
health <- health[common_samples,]

combined <- cbind(food, health, genus_abundance, metabolites)

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
  
  for(nutrient in nutrients){
    
    nh_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, sep = ""))
    nh_regression_p <- data.frame(summary(glm(nh_formula, data = combined))$coefficients)$Pr...t..[2]
    
    if(nh_regression_p < 0.05){
      
      
      for(microbe in microbes){
        
        nm_formula <- as.formula(paste(microbe, " ~ ", nutrient, sep = ""))
        nm_regression_p <- data.frame(summary(lm(nm_formula, data = combined))$coefficients)$Pr...t..[2]
        
        mh_formula <- as.formula(paste(health_outcome, " ~ ", microbe, sep = ""))
        mh_regression_p <- data.frame(summary(glm(mh_formula, data = combined))$coefficients)$Pr...t..[2]
        
        if(nm_regression_p < 0.01 & mh_regression_p < 0.01){
          
          med.fit_formula <- as.formula(paste(microbe, " ~ ", nutrient, sep = ""))
          out.fit_formula <- as.formula(paste(health_outcome, " ~ ", nutrient, " + ", microbe, sep = ""))
          
          
          med.fit <- lm(med.fit_formula, data = combined)
          
          out.fit <- glm(out.fit_formula,data = combined, family = binomial(link = "logit"))
          
          
          med.out <- mediate(med.fit, out.fit,
                             treat = nutrient, mediator =  microbe,
                             boot = TRUE, sims = 2000)
          
          sum <- summary(med.out)
            
            mediation_dat <- rbind(mediation_dat, data.frame(
              nutrient = nutrient,
              microbe = microbe, 
              health_outcome = health_outcome, 
              total_effect = sum$tau.coef,
              total_p = sum$tau.p,
              direct_effect = sum$d.avg,
              direct_p = sum$d.avg.p,
              indirect_effect = sum$z.avg, 
              indirect_p = sum$z.avg.p, 
              proportion_estimate = sum$n.avg, 
              proportion_p = sum$n.avg.p
            ))
            
        }
        
      }
    }
  }
}


# Save initial mediation
write_csv(mediation_dat, 'mediation.csv')

write_csv(combined, 'combined_mediation_data.csv')


#fruit vs. Hespellia vs. abdominalpain








