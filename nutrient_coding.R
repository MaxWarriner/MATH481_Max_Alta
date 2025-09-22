sam <- read.csv(file = "sample_data.csv")
nutrients <- read.csv(file = "nutrient_data.csv")


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

#normalize for total calorie intake
for (i in 1:57){
  
  cal_mplr <- sam$calories[i] / 1000
  
  sam$protein_normal[i] <- sam$protein[i] / cal_mplr
  sam$fat_normal[i] <- sam$fat[i] / cal_mplr
  sam$carbs_normal[i] <- sam$carbs[i] / cal_mplr
  sam$fiber_normal[i] <- sam$fiber[i] / cal_mplr
  sam$sodium_normal[i] <- sam$sodium[i] / cal_mplr
  
}



write.csv(sam, 'sample_data.csv')



