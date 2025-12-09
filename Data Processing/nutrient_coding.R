setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Data Processing")
library(tidyverse)
ps <- read_rds('microbiome.RDS')
sam <- ps@sam_data
nutrients <- data.frame(read.csv(file = "nutrient_data.csv"))



#rename columns
library(dplyr)
sam <- data.frame(sam) |>
  rename(teff = TeffFQ, 
         maize = Maize, 
         barley = Barley, 
         wheat = WheatIncludingBREAD, 
         sorghum = SorghumMillet, 
         rice = Rice,
         pasta = pastaMacaroni, 
         oats = yeAjaKincheOATS, 
         lentils = Lentil, 
         barley_porride = BarelyPoriadgeGenfo, 
         chickpeas = ChickShinbera, 
         beans = Beans, 
         kocho = RoootTubersKOCHO13, 
         sweet_potato = sweetpotatoes, 
         carrot = caroot, 
         potatoes = potatoe, 
         chard = VegetablesKOSTAchard, 
         cabbage = cabbage, 
         kale = KaleTIKURGOMEN, 
         tomatoes = Tomatoe, 
         pumpkin = PumkinDUBA, 
         greenbeans = GreenBeansFosoliya, 
         banana = FirutBANANA, 
         oranges = Orange, 
         mango = MANGO, 
         avocado = Avocado, 
         guava = GuavaZEYITUNA, 
         papaya = Papaya, 
         plums = Preem, 
         pineapple = pinapple, 
         beef = MeatandPoltaryBEEF, 
         lamb = Lamb, 
         chicken = Chickenmeat, 
         goat = GoatMeat, 
         eggs = Egg, 
         milk = MilikandDairyCOWMILK, 
         cheese = chees, 
         yogurt = Yoghur, 
         milk_powder = packedMilk, 
         butter = Butter, 
         oil = OilPlantpalmSaturatedYerega, 
         honey = SweetHONEY, 
         sugar = Sugar44, 
         soft_drinks = SoftdrinkMIRINDAfantaCOCA, 
         cakes = CakeBiscuits,
         french_fries = FrenchFiriesCHIPS, 
         sambusa = Sambusa)






# code for nutritional intake

sam$calories <- rep(0, 57)
sam$protein <- rep(0, 57)
sam$fat <- rep(0, 57)
sam$carbs <- rep(0, 57)
sam$fiber <- rep(0, 57)
sam$sodium <- rep(0, 57)

sam <- data.frame(sam)

# *adjust sodium
nutrients <- nutrients |>
  mutate(sodium = ifelse(is.na(sodium), 0, sodium))

nutrients[is.na(nutrients)] <- 0

for (i in 1:57){
  
  for(c in 32:77){
    sam$calories[i] <- sam$calories[i] + sam[i,c]*nutrients[c-31,3]
  }
  
  for(p in 32:77){
    sam$protein[i] <- sam$protein[i] + sam[i,p]*nutrients[p-31,5]
  }
  
  for(f in 32:77){
    sam$fat[i] <- sam$fat[i] + sam[i,f]*nutrients[f-31,6]
  }
  
  for(cb in 32:77){
    sam$carbs[i] <- sam$carbs[i] + sam[i,cb]*nutrients[cb-31,7]
  }
  
  for(fi in 32:77){
    sam$fiber[i] <- sam$fiber[i] + sam[i,fi]*nutrients[fi-31,8]
  }
  
  for(s in 32:77){
    sam$sodium[i] <- sam$sodium[i] + sam[i,s]*nutrients[s-31,20]
  }
  
}

#micronutrient intake

sam$PUFA <- rep(0, 57)
sam$cholesterol <- rep(0,57)
sam$vitaminA <- rep(0,57)
sam$carotene <- rep(0,57)
sam$vitaminE <- rep(0,57)
sam$vitaminB1 <- rep(0,57)
sam$vitaminB2 <- rep(0,57)
sam$vitaminB6 <- rep(0,57)
sam$folicacid <- rep(0,57)
sam$vitaminC <- rep(0,57)
sam$potassium <- rep(0,57)
sam$calcium <- rep(0,57)
sam$magnesium <- rep(0,57)
sam$phosphorus <-rep(0,57)
sam$iron <- rep(0,57)
sam$zinc <- rep(0,57)
sam$nutrient_score <- rep(0,57)
sam$fruit_or_vegetable <- rep(0,57)
sam$animal_product <- rep(0,57)


for (i in 1:57){
  
  # for(j in 32:77){
  #   sam$PUFA[i] <- sam$PUFA[i] + sam[i,j]*nutrients[j-31,10]
  # }
  # 
  # for(j in 32:77){
  #   sam$cholesterol[i] <- sam$cholesterol[i] + sam[i,j]*nutrients[j-31,11]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminA[i] <- sam$vitaminA[i] + sam[i,j]*nutrients[j-31,12]
  # }
  # 
  # for(j in 32:77){
  #   sam$carotene[i] <- sam$carotene[i] + sam[i,j]*nutrients[j-31,13]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminE[i] <- sam$vitaminE[i] + sam[i,j]*nutrients[j-31,14]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminB1[i] <- sam$vitaminB1[i] + sam[i,j]*nutrients[j-31,15]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminB2[i] <- sam$vitaminB2[i] + sam[i,j]*nutrients[j-31,16]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminB6[i] <- sam$vitaminB6[i] + sam[i,j]*nutrients[j-31,17]
  # }
  # 
  # for(j in 32:77){
  #   sam$folicacid[i] <- sam$folicacid[i] + sam[i,j]*nutrients[j-31,18]
  # }
  # 
  # for(j in 32:77){
  #   sam$vitaminC[i] <- sam$vitaminC[i] + sam[i,j]*nutrients[j-31,19]
  # }
  # 
  # for(j in 32:77){
  #   sam$potassium[i] <- sam$potassium[i] + sam[i,j]*nutrients[j-31,21]
  # }
  # 
  # for(j in 32:77){
  #   sam$calcium[i] <- sam$calcium[i] + sam[i,j]*nutrients[j-31,22]
  # }
  # 
  # for(j in 32:77){
  #   sam$magnesium[i] <- sam$magnesium[i] + sam[i,j]*nutrients[j-31,23]
  # }
  # 
  # for(j in 32:77){
  #   sam$phosphorus[i] <- sam$phosphorus[i] + sam[i,j]*nutrients[j-31,24]
  # }
  # 
  # for(j in 32:77){
  #   sam$iron[i] <- sam$iron[i] + sam[i,j]*nutrients[j-31,25]
  # }
  # 
  # for(j in 32:77){
  #   sam$zinc[i] <- sam$zinc[i] + sam[i,j]*nutrients[j-31,26]
  # }
  # 
  for(j in 32:77){
    sam$nutrient_score[i] <- sam$nutrient_score[i] + sam[i,j]*nutrients[j-31,27]
  }

  # for(j in 32:77){
  #   sam$fruit_or_vegetable[i] <- sam$fruit_or_vegetable[i] + sam[i,j]*nutrients[j-31,28]
  # }
  # 
  # for(j in 32:77){
  #   sam$animal_product[i] <- sam$animal_product[i] + sam[i,j]*nutrients[j-31,29]
  # }
  
}

# make columns numeric
sam[, 123:152] <- lapply(sam[, 123:152], as.numeric)


nutrilist <- colnames(sam)[119:148]

sam$protein_norm <- rep(0, 57)
sam$fat_norm <- rep(0, 57)
sam$carbs_norm <- rep(0, 57)
sam$fiber_norm <- rep(0, 57)
sam$sodium_norm <- rep(0, 57)
sam$vegetable_portions_norm <- rep(0, 57)
sam$legume_portions_norm <- rep(0, 57)
sam$grain_portions_norm <- rep(0, 57)
sam$fruit_portions_norm <- rep(0, 57)
sam$meat_portions_norm <- rep(0, 57)
sam$dairy_portions <- rep(0, 57)
sam$processed_food_portions_norm <- rep(0, 57)
sam$PUFA_norm <- rep(0, 57)
sam$cholesterol_norm <- rep(0, 57)
sam$vitaminA_norm <- rep(0, 57)
sam$carotene_norm <- rep(0, 57)
sam$vitaminE_norm <- rep(0, 57)
sam$vitaminB1_norm <- rep(0, 57)
sam$vitaminB2_norm <- rep(0, 57)
sam$vitaminB6_norm <- rep(0, 57)
sam$folicacid_norm <- rep(0, 57)
sam$vitaminC_norm <- rep(0, 57)
sam$potassium_norm <- rep(0, 57)
sam$calcium_norm <- rep(0, 57)
sam$magnesium_norm <- rep(0, 57)
sam$phosphorus_norm <- rep(0, 57)
sam$iron_norm <- rep(0, 57)
sam$zinc_norm <- rep(0, 57)


for (i in 120:149){
  for (j in 1:57){
    calorie_multiplier <- sam$calories[j] / 1000
    sam[j,i+30] <- sam[j,i]/calorie_multiplier
  }
}

sam <- sam |>
  rename(teff_injera = teff, 
         barley_injera = barley, 
         sorghum_injera = sorghum, 
         maize_injera = maize, 
         wheat_injera = wheat)

sam <- sam |>
  mutate(fermented_portions = teff_injera + barley_injera + sorghum_injera + maize_injera + wheat_injera + kocho + yogurt)

sam <- data.frame(sam)
sam <- sam |>
  mutate(fermented_portions_norm = fermented_portions/(calories/1000))


sam$plant_protein <- rep(0, 57)
sam$plant_protein_norm <- rep(0,57)
sam$plant_protein_group <- rep(0,57)

for (i in 1:57){
  for(j in 32:77){
    
  sam$plant_protein[i] <- sam$plant_protein[i] + ifelse(nutrients$Fruit_or_Vegetable[j-31] == 1, sam[i,j]*nutrients$protein[j-31],0)
  
  }
}


sam <- sam |>
  mutate(plant_protein_norm = plant_protein/(calories/1000))

sam <- sam |>
  select(-calories_norm, -calories_group)


#separate into high and low groups

for (i in 1:57){
  for (j in 180:209){
    sam[i,j] <- ifelse(sam[i,j-60] <= median(unlist(sam[,j-60])), "low", "high")
  }
}

sam <- data.frame(sam) |>
  mutate(plant_protein_group = ifelse(plant_protein <= unlist(median(sam$plant_protein)), "low", "high"))

sam <- sam[,c(1:152,211, 153:181,212,182:210,213)]

library(phyloseq)

sample_data(ps) <- sam

write_rds(ps, 'microbiome.RDS')

# New nutrient score calculations

sam <- data.frame(sam) |>
  mutate(meat_fish_eggs = meat_portions + eggs, 
         fat_oil = oil + butter, 
         sugar_sweets = sugar + cakes + soft_drinks, 
         cereal_grain_tuber = injera_sum + rice + pasta + oats + barley_porride + sweet_potato + potatoes)

for (i in 1:57){


sam$nutrient_score[i] = (min(7, sam$cereal_grain_tuber[i])*2 + 
           min(7, sam$legume_portions[i])*3 + 
           min(7, sam$dairy_portions[i])*4 + 
           min(7, sam$meat_fish_eggs[i])*4 + 
           min(7, sam$vegetable_portions[i]) + 
           min(7, sam$fruit_portions[i]) + 
           min(7, sam$fat_oil[i])*0.5 + 
           min(7, sam$sugar_sweets[i])*0.5)

}


mean(sam$nutrient_score)

view(lily_ps@sam_data)

sample_data(ps) <- sam

write_rds(ps, 'microbiome.RDS')
