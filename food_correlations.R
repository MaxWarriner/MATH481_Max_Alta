

sam <- read_csv('sample_data.csv')

ps <- read_rds('microbiome.RDS')

diversity <- estimate_richness(ps, measures = c("Shannon", "Chao1"))

food_data <- sam[, c(33:82, 199:216)]

food_data$Shannon <- diversity$Shannon

food_data$Chao1 <- diversity$Chao1

food_correlations <- data.frame(food = rep(NA, 68), 
                                r = rep(NA, 68), 
                                p = rep(NA, 68))

for(i in 1:68){
  
  food_correlations$food[i] = colnames(food_data)[i]
  
  cor_test <- cor.test(unlist(food_data[,i]), food_data$Shannon)
  
  food_correlations$r[i] <- cor_test$estimate
  
  food_correlations$p[i] <- cor_test$p.value
  
}

food_correlations$adjusted <- p.adjust(food_correlations$p, method = "BH")


