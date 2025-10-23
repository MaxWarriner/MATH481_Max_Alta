setwd("C:/Users/12697/Documents/MATH481_Max_Alta")

metab <- readxl::read_xlsx('metabolomics.xlsx')

fire <- read_csv('classyfire.csv')

metab_info <- pc_prop(as.character(metab$PubChem_CID))

merged <- merge(fire, metab)
