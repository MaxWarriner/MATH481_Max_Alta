
sam <- read.csv(file = "sample_data.csv")

library(dplyr)

sam <- sam |>
  dplyr::select(-X)

sam <- sam |>
  mutate(sex = ifelse(sex == 1, 'male', 'female'), 
         Religion = case_when(Religion == 1 ~ 'orthodox', 
                              Religion == 2 ~ 'muslim', 
                              Religion == 3 ~ 'protestant', 
                              Religion == 4 ~ 'catholic'), 
         Residence = case_when(Residence == 1 ~ 'urban', 
                               Residence == 2 ~ 'semi urban', 
                               Residence == 3 ~ 'rural'), 
         EducaM = as.factor(EducaM), 
         EducatF = as.factor(EducatF), 
         EducFChi = as.factor(EducFChi), 
         ChildLiv = ifelse(ChildLiv == 1, 'with parents', 'with relative'), 
         RadioTapeCDplayer = ifelse(RadioTapeCDplayer == 1, 'yes', 'no'), 
         Television = ifelse(Television == 1, 'yes', 'no'), 
         GasStove = ifelse(GasStove == 1, 'yes', 'no'), 
         ElectricStove = ifelse(ElectricStove == 1, 'yes', 'no'), 
         MoterCycle = ifelse(MoterCycle == 1, 'yes', 'no'), 
         CartGari = ifelse(CartGari == 1, 'yes', 'no'), 
         WatchHandWall = ifelse(WatchHandWall == 1, 'yes', 'no'), 
         Mobilephone = ifelse(Mobilephone == 1, 'yes', 'no'), 
         Bicycle = ifelse(Bicycle == 1, 'yes', 'no'), 
         Sofa = ifelse(Sofa == 1, 'yes', 'no'), 
         SpongeFoamMattres = ifelse(SpongeFoamMattres == 1, 'yes', 'no'), 
         ChairStool = ifelse(ChairStool == 1, 'yes', 'no'), 
         GeneratorSolar = ifelse(GeneratorSolar == 1, 'yes', 'no'), 
         Milling = ifelse(Milling == 1, 'yes', 'no'), 
         Car = ifelse(Car == 1, 'yes', 'no'), 
         Refrigerator = ifelse(Refrigerator == 1, 'yes', 'no'), 
         Bajaj = ifelse(Bajaj == 1, 'yes', 'no'), 
         OwnHouse = ifelse(OwnHouse == 1, 'yes', 'no'))


# coding for portions of food / week

for (i in 33:82){
  for (j in 1:57){
    
    sam[j,i] <- case_when(sam[j,i] == 1 ~ 0, 
                          sam[j,i] == 2 ~ 0.25, 
                          sam[j,i] == 3 ~ 2.5/4, 
                          sam[i,j] == 4 ~ 1, 
                          sam[j,i] == 5 ~ 2.5, 
                          sam[j,i] == 6 ~ 5, 
                          sam[j,i] == 7 ~ 7, 
                          sam[j,i] == 8 ~ 10)
    
    if(is.na(sam[j,i])){
      sam[j,i] <- 0
    }
    
  }
}

write.csv(sam, file = 'sample_data.csv')

