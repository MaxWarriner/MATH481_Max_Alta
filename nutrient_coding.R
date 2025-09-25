sam <- data.frame(read_csv('sample_data.csv'))[,-1]
nutrients <- data.frame(read.csv(file = "nutrient_data.csv"))


#rename columns
library(dplyr)
sam <- sam |>
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
         hamburger = FastfoodBURGER, 
         pizza = pizza, 
         french_fries = FrenchFiriesCHIPS, 
         sambusa = Sambusa)


# code for nutritional intake

sam$calories <- rep(0, 57)
sam$protein <- rep(0, 57)
sam$fat <- rep(0, 57)
sam$carbs <- rep(0, 57)
sam$fiber <- rep(0, 57)
sam$sodium <- rep(0, 57)

# *adjust sodium
nutrients <- nutrients |>
  mutate(sodium = ifelse(is.na(sodium), 0, sodium))

for (i in 1:57){
  
  for(c in 34:83){
    sam$calories[i] <- sam$calories[i] + sam[i,c]*nutrients[c-33,3]
  }
  
  for(p in 34:83){
    sam$protein[i] <- sam$protein[i] + sam[i,p]*nutrients[p-33,5]
  }
  
  for(f in 34:83){
    sam$fat[i] <- sam$fat[i] + sam[i,f]*nutrients[f-33,6]
  }
  
  for(cb in 34:83){
    sam$carbs[i] <- sam$carbs[i] + sam[i,cb]*nutrients[cb-33,7]
  }
  
  for(fi in 34:83){
    sam$fiber[i] <- sam$fiber[i] + sam[i,fi]*nutrients[fi-33,8]
  }
  
  for(s in 34:83){
    sam$sodium[i] <- sam$sodium[i] + sam[i,s]*nutrients[s-33,20]
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

nutrients[is.na(nutrients)] <- 0

for (i in 1:57){
  
  for(j in 34:83){
    sam$PUFA[i] <- sam$PUFA[i] + sam[i,j]*nutrients[j-33,10]
  }
  
  for(j in 34:83){
    sam$cholesterol[i] <- sam$cholesterol[i] + sam[i,j]*nutrients[j-33,11]
  }
  
  for(j in 34:83){
    sam$vitaminA[i] <- sam$vitaminA[i] + sam[i,j]*nutrients[j-33,12]
  }
  
  for(j in 34:83){
    sam$carotene[i] <- sam$carotene[i] + sam[i,j]*nutrients[j-33,13]
  }
  
  for(j in 34:83){
    sam$vitaminE[i] <- sam$vitaminE[i] + sam[i,j]*nutrients[j-33,14]
  }
  
  for(j in 34:83){
    sam$vitaminB1[i] <- sam$vitaminB1[i] + sam[i,j]*nutrients[j-33,15]
  }
  
  for(j in 34:83){
    sam$vitaminB2[i] <- sam$vitaminB2[i] + sam[i,j]*nutrients[j-33,16]
  }
  
  for(j in 34:83){
    sam$vitaminB6[i] <- sam$vitaminB6[i] + sam[i,j]*nutrients[j-33,17]
  }
  
  for(j in 34:83){
    sam$folicacid[i] <- sam$folicacid[i] + sam[i,j]*nutrients[j-33,18]
  }
  
  for(j in 34:83){
    sam$vitaminC[i] <- sam$vitaminC[i] + sam[i,j]*nutrients[j-33,19]
  }
  
  for(j in 34:83){
    sam$potassium[i] <- sam$potassium[i] + sam[i,j]*nutrients[j-33,21]
  }
  
  for(j in 34:83){
    sam$calcium[i] <- sam$calcium[i] + sam[i,j]*nutrients[j-33,22]
  }
  
  for(j in 34:83){
    sam$magnesium[i] <- sam$magnesium[i] + sam[i,j]*nutrients[j-33,23]
  }
  
  for(j in 34:83){
    sam$phosphorus[i] <- sam$phosphorus[i] + sam[i,j]*nutrients[j-33,24]
  }
  
  for(j in 34:83){
    sam$iron[i] <- sam$iron[i] + sam[i,j]*nutrients[j-33,25]
  }
  
  for(j in 34:83){
    sam$zinc[i] <- sam$zinc[i] + sam[i,j]*nutrients[j-33,26]
  }
  
}

#normalize nutrients per kg of body weight
sam <- sam[,-129:-133]

nutrilist <- colnames(sam)[123:151]

sam$calories_norm <- rep(0, 57)
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
sam$eggs_dairy_portions_norm <- rep(0, 57)
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


for (i in 123:151){
  for (j in 1:57){
    sam[j,i+29] <- sam[j,i]/sam$Weightkg[j] 
  }
}


write.csv(sam, 'sample_data.csv')



