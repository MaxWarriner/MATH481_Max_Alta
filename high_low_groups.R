
sam <- read_csv('sample_data.csv')[,-1]
ps <- read_rds('microbiome.RDS')

sam <- sam |>
  mutate(infection = ifelse(Ascaris == 1 | Trichuris == 1 | Hookworm == 1 | Schistosoma == 1, 'yes', 'no'))

sam <- sam[,c(1:135, 181, 136:180, 182:183)]

sam$calories_group <- rep(NA, 57)
sam$protein_group <- rep(NA, 57)
sam$fat_group <- rep(NA, 57)
sam$carbs_group <- rep(NA, 57)
sam$fiber_group <- rep(NA, 57)
sam$sodium_group <- rep(NA, 57)
sam$vegetable_portions_group <- rep(NA, 57)
sam$legume_portions_group <- rep(NA, 57)
sam$grain_portions_group <- rep(NA, 57)
sam$fruit_portions_group <- rep(NA, 57)
sam$meat_portions_group <- rep(NA, 57)
sam$eggs_dairy_portions_group <- rep(NA, 57)
sam$processed_food_portions_group <- rep(NA, 57)
sam$PUFA_group <- rep(NA, 57)
sam$cholesterol_group <- rep(NA, 57)
sam$vitaminA_group <- rep(NA, 57)
sam$carotene_group <- rep(NA, 57)
sam$vitaminE_group <- rep(NA, 57)
sam$vitaminB1_group <- rep(NA, 57)
sam$vitaminB2_group <- rep(NA, 57)
sam$vitaminB6_group <- rep(NA, 57)
sam$folicacid_group <- rep(NA, 57)
sam$vitaminC_group <- rep(NA, 57)
sam$potassium_group <- rep(NA, 57)
sam$calcium_group <- rep(NA, 57)
sam$magnesium_group <- rep(NA, 57)
sam$phosphorus_group <- rep(NA, 57)
sam$iron_group <- rep(NA, 57)
sam$zinc_group <- rep(NA, 57)
sam$fermented_portions_group <- rep(NA, 57)

sam <- sam[,c(1:90,183,91:182,184:213)]


for (i in 154:183){
  median <- median(unlist(sam[,i]))
  for (j in 1:57){
    sam[j,i+30] <- ifelse(sam[j,i] >= median, 'high', 'low')
  }
}

sam <- sam |>
  column_to_rownames('SampleID')

sample_data(ps) <- data.frame(sam)

write_csv(sam, 'sample_data.csv')

write_rds(ps, 'microbiome.RDS')
