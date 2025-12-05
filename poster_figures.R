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
load("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models/diarrhea_microbiome_results.RData")
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
                                    filename) {
  
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
      title = paste0(factor, ": Overlaid ROC Curves"),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(filename = filename, plot = roc_plot, width = 10, height = 6, dpi = 500)
  
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
  model_names = factor(c("Microbiome", "Metabolome", "Nutrients", "Combined"), levels = c("Microbiome", "Metabolome", "Nutrients", "Combined")),
  factor = "Diarrhea",
  filename = "diarrhea_roc_overlay.png"
)


