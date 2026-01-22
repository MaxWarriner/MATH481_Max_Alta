setwd("C:/Users/12697/Documents/MATH481_Max_Alta")
library(BiocManager)
library(phyloseq)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(patchwork)
library(RColorBrewer)
library(rlang)
library(MicrobiotaProcess)
library(vegan)
library(dplyr)
library(ALDEx2)
library(microbiomeMarker)
library(ggsci)
library(ggpubr)
library(parallel)
library(doParallel)
library(pROC)


# Overlaid ROC  -----------------------------------------------------------



#Diarrhea
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_metabolome_results.RData")
diarrhea_metabolome_results <- metabolome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_microbime_results.RData")
diarrhea_microbiome_results <- microbiome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_nutrient_results.RData")
diarrhea_nutrient_results <- nutrients_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_results.RData")
diarrhea_full_results <- full

# Overlaid ROC Curve
plot_roc_curve_gg_multi <- function(model_list, 
                                    model_names = NULL,
                                    positive_class = NULL, 
                                    factor, 
                                    filename,
                                    title) {
  
  if (is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(model_list))
  }
  
  # Storage for all ROC curve data
  roc_dfs <- list()
  auc_values <- numeric(length(model_list))
  
  for (i in seq_along(model_list)) {
    model_results <- model_list[[i]]
    y_true <- model_results$y_true
    
    # default positive class
    if (is.null(positive_class)) {
      positive_class_i <- levels(y_true)[1]
    } else {
      positive_class_i <- positive_class
    }
    
    # Extract predicted probability
    pred_prob <- model_results$xgb_fit$pred %>%
      filter(obs %in% levels(y_true)) %>%
      arrange(rowIndex) %>%
      pull(!!as.name(positive_class_i))
    
    # Compute ROC
    roc_obj <- roc(y_true, pred_prob, 
                   levels = rev(levels(y_true)), 
                   direction = "<")
    
    auc_values[i] <- auc(roc_obj)
    
    # Extract data for ggplot
    roc_df <- data.frame(
      fpr = rev(1 - roc_obj$specificities),
      tpr = rev(roc_obj$sensitivities),
      model = model_names[i]
    )
    
    roc_dfs[[i]] <- roc_df
  }
  
  # Combine all ROC curves
  roc_all <- bind_rows(roc_dfs)
  
  # Plot
  roc_plot <- ggplot(roc_all, aes(x = fpr, y = tpr, color = model)) +
    geom_line(size = 1.2) +
    geom_abline(linetype = "dashed", color = "gray50") +
    labs(
      title = title,
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model (AUC)"
    ) +
    theme_minimal(base_size = 10) +
    theme(title = element_text(hjust = 0.5, size = 12))
  
  ggsave(filename = filename, plot = roc_plot, width = 6, height = 3.5, dpi = 500)
  
  return(list(auc = auc_values, plot = roc_plot))
}

y_true_microbiome <- diarrhea_microbiome_results$y_true
y_true_metabolome <- diarrhea_metabolome_results$y_true
y_true_nutrients <- diarrhea_nutrient_results$y_true
y_true_full <- diarrhea_full_results$y_true


microbiome_results <- list(y_true = y_true_microbiome, xgb_fit = diarrhea_microbiome_results$xgb_fit)
metabolome_results <- list(y_true = y_true_metabolome, xgb_fit = diarrhea_metabolome_results$xgb_fit)
nutrient_results <- list(y_true = y_true_nutrients, xgb_fit = diarrhea_nutrient_results$xgb_fit)
full_results <- list(y_true = y_true_full, xgb_fit = diarrhea_full_results$xgb_fit)

model_list <- list(microbiome_results, metabolome_results, nutrient_results, full_results)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome (0.806)", "Metabolome (0.633)", "Diet (0.815)", "Combined (0.900)"), levels = c("Microbiome (0.806)", "Metabolome (0.633)", "Diet (0.815)", "Combined (0.900)")),
  factor = "Diarrhea",
  filename = "diarrhea_roc_overlay.png"
)



# Model Performance Heatmap -----------------------------------------------

#Diarrhea
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_metabolome_results.RData")
diarrhea_metabolome_results <- metabolome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_microbime_results.RData")
diarrhea_microbiome_results <- microbiome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_nutrient_results.RData")
diarrhea_nutrient_results <- nutrients_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_results.RData")
diarrhea_full_results <- full

y_true_microbiome <- diarrhea_microbiome_results$y_true
y_true_metabolome <- diarrhea_metabolome_results$y_true
y_true_nutrients <- diarrhea_nutrient_results$y_true
y_true_full <- diarrhea_full_results$y_true


microbiome_results <- list(y_true = y_true_microbiome, xgb_fit = diarrhea_microbiome_results$xgb_fit)
metabolome_results <- list(y_true = y_true_metabolome, xgb_fit = diarrhea_metabolome_results$xgb_fit)
nutrient_results <- list(y_true = y_true_nutrients, xgb_fit = diarrhea_nutrient_results$xgb_fit)
full_results <- list(y_true = y_true_full, xgb_fit = diarrhea_full_results$xgb_fit)

model_list <- list(microbiome_results, metabolome_results, nutrient_results, full_results)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome (0.782)", "Metabolome (0.633)", "Dietary (0.794)", "Combined (0.859)"), levels = c("Microbiome (0.782)", "Metabolome (0.633)", "Dietary (0.794)", "Combined (0.859)")),
  factor = "Diarrhea",
  filename = "diarrhea_roc_overlay.png", 
  title = "Diarrhea: ROC"
)


#Abdominal Pain
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/abdominalpain_metabolome_results.RData")
abdominalpain_metabolome_results <- metabolome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/abdominalpain_microbime_results.RData")
abdominalpain_microbiome_results <- microbiome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/abdominalpain_nutrient_results.RData")
abdominalpain_nutrient_results <- nutrients_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/abdominalpain_results.RData")
abdominalpain_full_results <- full

y_true_microbiome <- abdominalpain_microbiome_results$y_true
y_true_metabolome <- abdominalpain_metabolome_results$y_true
y_true_nutrients <- abdominalpain_nutrient_results$y_true
y_true_full <- abdominalpain_full_results$y_true


microbiome_results <- list(y_true = y_true_microbiome, xgb_fit = abdominalpain_microbiome_results$xgb_fit)
metabolome_results <- list(y_true = y_true_metabolome, xgb_fit = abdominalpain_metabolome_results$xgb_fit)
nutrient_results <- list(y_true = y_true_nutrients, xgb_fit = abdominalpain_nutrient_results$xgb_fit)
full_results <- list(y_true = y_true_full, xgb_fit = abdominalpain_full_results$xgb_fit)

model_list <- list(microbiome_results, metabolome_results, nutrient_results, full_results)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome (0.795)", "Metabolome (0.551)", "Dietary (0.567)", "Combined (0.834)"), levels = c("Microbiome (0.795)", "Metabolome (0.551)", "Dietary (0.567)", "Combined (0.834)")),
  factor = "abdominalpain",
  filename = "abdominalpain_roc_overlay.png", 
  title = "Abdominal Pain: ROC"
)


# Lower Appetite
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/lower_appetite_metabolome_results.RData")
lower_appetite_metabolome_results <- metabolome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/lower_appetite_microbime_results.RData")
lower_appetite_microbiome_results <- microbiome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/lower_appetite_nutrient_results.RData")
lower_appetite_nutrient_results <- nutrients_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/lower_appetite_results.RData")
lower_appetite_full_results <- full

y_true_microbiome <- lower_appetite_microbiome_results$y_true
y_true_metabolome <- lower_appetite_metabolome_results$y_true
y_true_nutrients <- lower_appetite_nutrient_results$y_true
y_true_full <- lower_appetite_full_results$y_true


microbiome_results <- list(y_true = y_true_microbiome, xgb_fit = lower_appetite_microbiome_results$xgb_fit)
metabolome_results <- list(y_true = y_true_metabolome, xgb_fit = lower_appetite_metabolome_results$xgb_fit)
nutrient_results <- list(y_true = y_true_nutrients, xgb_fit = lower_appetite_nutrient_results$xgb_fit)
full_results <- list(y_true = y_true_full, xgb_fit = lower_appetite_full_results$xgb_fit)

model_list <- list(microbiome_results, metabolome_results, nutrient_results, full_results)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome (0.710)", "Metabolome (0.673)", "Dietary (0.653)", "Combined (0.873)"), levels = c("Microbiome (0.710)", "Metabolome (0.673)", "Dietary (0.653)", "Combined (0.873)")),
  factor = "lower_appetite",
  filename = "lower_appetite_roc_overlay.png", 
  title = "Loss of Appetite: ROC"
)


#Bloating
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/bloating_metabolome_results.RData")
bloating_metabolome_results <- metabolome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/bloating_microbime_results.RData")
bloating_microbiome_results <- microbiome_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/bloating_nutrient_results.RData")
bloating_nutrient_results <- nutrients_results
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/bloating_results.RData")
bloating_full_results <- full



y_true_microbiome <- bloating_microbiome_results$y_true
y_true_metabolome <- bloating_metabolome_results$y_true
y_true_nutrients <- bloating_nutrient_results$y_true
y_true_full <- bloating_full_results$y_true


microbiome_results <- list(y_true = y_true_microbiome, xgb_fit = bloating_microbiome_results$xgb_fit)
metabolome_results <- list(y_true = y_true_metabolome, xgb_fit = bloating_metabolome_results$xgb_fit)
nutrient_results <- list(y_true = y_true_nutrients, xgb_fit = bloating_nutrient_results$xgb_fit)
full_results <- list(y_true = y_true_full, xgb_fit = bloating_full_results$xgb_fit)

model_list <- list(microbiome_results, metabolome_results, nutrient_results, full_results)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome (0.665)", "Metabolome (0.600)", "Dietary (0.535)", "Combined (0.889)"), levels = c("Microbiome (0.665)", "Metabolome (0.600)", "Dietary (0.535)", "Combined (0.889)")),
  factor = "bloating",
  filename = "bloating_roc_overlay.png", 
  title = "Bloating: ROC"
)


heatmapdat <- tibble(`Abdominal Pain` = abdominalpain$auc, 
                     Bloating = bloating$auc, 
                     Diarrhea = diarrhea$auc, 
                     `Lower Appetite` = lower_appetite$auc)

rownames(heatmapdat) <- c("Microbiome", "Metabolome", "Nutrients", "Combined")

heatmapdat <- as.matrix(heatmapdat)

pheatmap::pheatmap(t(heatmapdat), cluster_rows = F, cluster_cols = F, legend_title = "AUC")

library(ComplexHeatmap)
library(ggplot2)
setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning")
png("Models_Heatmap.png", width = 1200, height = 500, res = 150)
ComplexHeatmap::Heatmap(heatmapdat, name = "AUC", cluster_rows = F, cluster_columns = F, column_names_side = "top", column_names_rot = 0, column_names_centered = T, row_names_side = "left")
dev.off()



# Top Features ------------------------------------------------------------


plot_top_importance <- function(model_results, n_top = 10, bar_color = "#2c7bb6", factor, filename, title) {
  
  varimp <- model_results$feature_importance %>% 
    arrange(desc(Importance)) %>%
    head(n_top)
  
  plot <- ggplot(varimp, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = bar_color) +
    coord_flip() +
    labs(
      title = title,
      x = "",
      y = "Importance"
    ) +
    theme_minimal(base_size = 22) +
    theme(legend.position = "none")
  
  ggsave(filename = filename, plot = plot, width = 14, height = 5)
  
}

#Diarrhea top importance

load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_results.RData")

full$feature_importance$Feature <- gsub(pattern = "G__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "F__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "_", replacement = " ", full$feature_importance$Feature)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")

plot_top_importance(full, factor = "Diarrhea", filename = "diarrhea_top10.png", title = "Diarrhea: Top 10 Feature Importance")

#Abdominal Pain top importance

load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/abdominalpain_results.RData")

full$feature_importance$Feature <- gsub(pattern = "G__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "F__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "_", replacement = " ", full$feature_importance$Feature)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")

plot_top_importance(full, factor = "abdominalpain", filename = "abdominalpain_top10.png", title = "Abdominal Pain: Top 10 Feature Importance")

#Bloating top importance

load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/bloating_results.RData")

full$feature_importance$Feature <- gsub(pattern = "G__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "F__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "_", replacement = " ", full$feature_importance$Feature)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")

plot_top_importance(full, factor = "bloating", filename = "bloating_top10.png", title = "Bloating: Top 10 Feature Importance")

#Lower Appetite top importance

load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/lower_appetite_results.RData")

full$feature_importance$Feature <- gsub(pattern = "G__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "F__", replacement = "", full$feature_importance$Feature)
full$feature_importance$Feature <- gsub(pattern = "_", replacement = " ", full$feature_importance$Feature)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")

plot_top_importance(full, factor = "lower_appetite", filename = "lower_appetite_top10.png", title = "Loss of Appetite: Top 10 Feature Importance")


# Nutrient Score Bar Plot-------------------------------------------------------


ps <- read_rds("microbiome.RDS")

sam <- ps@sam_data



consump <- ggplot(data = sam, aes(y = nutrient_score, x = "")) +
  
  # Shaded regions with legend
  geom_rect(aes(
    xmin = -Inf, xmax = Inf,
    ymin = 42, ymax = Inf,
    fill = "Acceptable"
  ), alpha = 0.2) +
  
  geom_rect(aes(
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = 42,
    fill = "Borderline"
  ), alpha = 0.2) +
  
  geom_boxplot(width = 0.4) +
  
  scale_fill_manual(
    name = "",
    values = c(
      "Acceptable" = "lightgreen",
      "Borderline" = "salmon"
    )
  ) +
  
  xlab("") +
  ylab('') + 
  ggtitle("Food Consumption Score") + 
  theme_bw() &
    theme(plot.title = element_text(hjust = 0.5, size = 16), 
          legend.text = element_text(size = 16))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(consump, filename = "Food_Consumption_Score_Summary.png", width = 6, height = 5, dpi = 800)







