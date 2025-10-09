library(tidyverse)
ps <- read_rds('microbiome.RDS')
sam <- data.frame(ps@sam_data)

sam <- sam|>
  mutate(infection = ifelse(Ascaris == 1 | Trichuris == 1 | Hookworm == 1 | Schistosoma == 1, 'yes', 'no'))

mean(sam$Age)
2 * sd(sam$Age) / sqrt(length(sam$Age))


mean(sam$Weightkg)
2 * sd(sam$Weightkg) / sqrt(length(sam$Weightkg))

mean(sam$Heightcm)
2 * sd(sam$Heightcm) / sqrt(length(sam$Heightcm))

table(sam$sex)

table(sam$Residence)

table(sam$Religion)

mean(sam$FamilySize)
2 * sd(sam$FamilySize) / sqrt(length(sam$FamilySize))

table(sam$Television)

table(sam$Mobilephone)

table(sam$Car)

table(sam$Refrigerator)

table(sam$illness)

table(sam$diarrhea)

table(sam$cough)

table(sam$deworming_pill)

table(sam$infection)
