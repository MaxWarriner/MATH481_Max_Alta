
ps <- read_rds('microbiome.RDS')

sam <- data.frame(ps@sam_data)

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

for (i in 1:57){
  for (j in 33:82){
    
    sam[i,j] <- case_when(sam[i,j] == 1 ~ 0, 
                          sam[i,j] == 2 ~ 0.25, 
                          sam[i,j] == 3 ~ 2.5/4, 
                          sam[i,j] == 4 ~ 1, 
                          sam[i,j] == 5 ~ 2.5, 
                          sam[i,j] == 6 ~ 5, 
                          sam[i,j] == 7 ~ 7, 
                          sam[i,j] == 8 ~ 10)
    
    if(is.na(sam[i,j])){
      sam[i,j] <- 0
    }
    
  }
}

library(missForest)
diet <- sam[,35:84]

diet <- missForest(diet)
diet <- diet[["ximp"]]

sam[,35:84] <- diet


sam <- sam[-83:-125]

sam <- sam[-125:-155]

sam <- sam[-123:-124]

sam <- sam |>
  rename(illness = Health.R1, 
         diarrhea = Health.R2, 
         cough = Health.R3, 
         bloating = Health.R4, 
         abdominalpain = Health.R5, 
         lower_appetite = Health.R6, 
         nausea = Health.R7, 
         deworming_pill = Health.R8)



write.csv(sam, file = 'sample_data.csv')


