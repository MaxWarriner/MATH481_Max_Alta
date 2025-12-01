library(phyloseq)
library(tidyverse)
ps <- read_rds('microbiome.RDS')
sam <- ps@sam_data


sam <- data.frame(sam) |>
  mutate(vegetable_portions = sweet_potato + carrot + potatoes + chard + kale + cabbage + greenbeans, 
         legume_portions = lentils + chickpeas + beans, 
         grain_portions = teff_injera + barley_injera + sorghum_injera + maize_injera + wheat_injera + rice + pasta + oats + barley_porride, 
         fruit_portions = pumpkin + tomatoes + banana + mango + guava + plums + oranges + avocado + papaya + pineapple, 
         meat_portions = beef + chicken + Fish + lamb + goat, 
         dairy_portions = milk + yogurt + butter, 
         processed_food_portions = soft_drinks + cakes  + french_fries + sambusa)


sam$mode_injera <- rep(NA, 57)

cols <- colnames(sam)[32:36]   # the columns to check

sam$mode_injera <- cols[max.col(sam[cols], ties.method = "first")]

sam <- data.frame(sam) |>
  mutate(injera_sum = teff_injera + barley_injera + sorghum_injera + maize_injera + wheat_injera)

sample_data(ps) <- sam



write_rds(ps, 'microbiome.RDS')



