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
      x = "FPR",
      y = "TPR",
      color = "Model"
    ) +
    theme_classic() + 
    theme(
      plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
      axis.title = element_text(size = 28),
      axis.text  = element_text(size = 26),
      legend.title = element_text(size = 0),
      legend.text  = element_text(size = 0), 
      legend.position = "none"
    ) + 
    guides(color = FALSE) + 
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 1),
    ) + 
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 1),
    )
  
  # ggsave(filename = filename, plot = roc_plot, width = 6, height = 3.5, dpi = 500)
  
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
diarrhea_roc <- plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome", "Metabolome", "Dietary", "Combined"), levels = c("Microbiome", "Metabolome", "Dietary", "Combined")),
  factor = "Diarrhea",
  filename = "diarrhea_roc_overlay.png", 
  title = "(A)"
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
abdominal_roc <- plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome", "Metabolome", "Dietary", "Combined"), levels = c("Microbiome", "Metabolome", "Dietary", "Combined")),
  factor = "abdominalpain",
  filename = "abdominalpain_roc_overlay.png", 
  title = "(A) Abdominal Pain"
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
appetite_roc <- plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome", "Metabolome", "Dietary", "Combined"), levels = c("Microbiome", "Metabolome", "Dietary", "Combined")),
  factor = "lower_appetite",
  filename = "lower_appetite_roc_overlay.png", 
  title = "(C) Loss of Appetite"
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
bloating_roc <- plot_roc_curve_gg_multi(
  model_list = model_list,
  model_names = factor(c("Microbiome", "Metabolome", "Dietary", "Combined"), levels = c("Microbiome", "Metabolome", "Dietary", "Combined")),
  factor = "bloating",
  filename = "bloating_roc_overlay.png", 
  title = "(B) Bloating"
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



# Machine Leaning Accuracy Forest Plot ------------------------------------


range_dat <- tibble(health_outcome = rep(c("Abdominal Pain", "Bloating", "Diarrhea", "Loss of Appetite"), each = 4), 
                    model = rep(c("Microbiome", "Metabolome", "Dietary", "Combined"), 4), 
                    low_accuracy = c(0.6481, 0.5926, 0.5926, 0.7222, 
                                     0.6481, 0.6111, 0.5370, 0.7407, 
                                     0.7407, 0.6852, 0.7593, 0.796296296, 
                                     0.7222, 0.7407, 0.6667, 0.8148), 
                    high_accuracy = c(0.8148, 0.6667, 0.6296, 0.8333, 
                                      0.8333, 0.7222, 0.6111, 0.8704,
                                      0.9074, 0.7222, 0.8519, 0.8518, 
                                      0.8889, 0.7778, 0.7222, 0.8519), 
                    mean_accuracy = c(0.7593, 0.6389, 0.6111, 0.7870, 
                                      0.7361, 0.6528, 0.5602, 0.7870, 
                                      0.8380, 0.7037, 0.8009, 0.8333, 
                                      0.8194, 0.7593, 0.6898, 0.8333))

range_dat$sd <- c(0.07858202, 0.03461895, 0.03401934, 0.04660389, 
                  0.08648937, 0.04764787, 0.03762679, 0.05579165, 
                  0.07145591, 0.01523336, 0.03575361, 0.02445106, 
                  0.07666897, 0.01400913, 0.02459523, 0.01372159)

range_dat$AUC_mean <- c(0.7764558, 0.5475851, 0.5525569, 0.8075282, 
                        0.763889, 0.5673613, 0.55, 0.8187499, 
                        0.8399726, 0.659684, 0.7949863, 0.8739699, 
                        0.7885804, 0.6612654, 0.5493827, 0.8749999)

range_dat$AUC_sd <- c(0.1199973, 0.05447977, 0.01137844, 0.04156686, 
            0.1314244, 0.02607008, 0.04201999, 0.04811933, 
            0.1140992, 0.0399907, 0.01347624, 0.01309763, 
            0.1280465, 0.07013834, 0.07918128, 0.02271545)

View(cbind(range_dat[, 1:2], round(range_dat[,3:8], 2)))

range_dat$health_outcome = factor(range_dat$health_outcome, levels = c("Abdominal Pain", "Bloating", "Diarrhea", "Loss of Appetite")) 
range_dat$model = factor(range_dat$model, levels = c("Microbiome", "Metabolome", "Dietary", "Combined"))
                    
range_plot_diarrhea <- ggplot(data = range_dat |> filter(health_outcome == "Diarrhea"), aes(x = health_outcome, color = model)) + 
  geom_errorbar(aes(ymin = mean_accuracy - sd, ymax = mean_accuracy + sd), 
                position = position_dodge(width = 0.25), 
                width = 0.1,
                linewidth = 1) + 
  geom_point(aes(x = health_outcome, y = mean_accuracy), 
             position = position_dodge(width = 0.25), 
             size = 3) + 
  theme_classic() + 
  ylab("Accuracy") + 
  xlab("") + 
  ggtitle("(B)") + 
  labs(color = "Model Features") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 28),
    axis.text.x = element_text(size = 0),
    axis.text.y  = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 26, margin = margin(t = 10)), 
    legend.spacing.y = unit(1, "in")) + 
  scale_y_continuous(
    limits = c(0.5, 1),
    breaks = c(0.5, 0.75, 1),
    minor_breaks = NULL,
    expand = c(0, 0)
  )

range_plot_other <- ggplot(data = range_dat |> filter(health_outcome != "Diarrhea"), 
                           aes(x = health_outcome, color = model, group = model)) + 
  geom_errorbar(aes(ymin = mean_accuracy - sd, ymax = mean_accuracy + sd), 
                position = position_dodge2(width = 0.5, padding = 0.5), 
                width = 0.5,
                linewidth = 1) + 
  geom_point(aes(x = health_outcome, y = mean_accuracy), 
             position = position_dodge2(width = 0.5, padding = 0.5), 
             size = 3) + 
  theme_classic() + 
  ylab("Accuracy") + 
  xlab("") + 
  ggtitle("(D)") + 
  labs(color = "Model Features") +
  theme(
    plot.title = element_text(size = 32, hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 28),
    axis.text  = element_text(size = 26),
    legend.title = element_text(size = 28),
    legend.text  = element_text(size = 26, margin = margin(t = 10)), 
    legend.spacing.y = unit(1, "in")) + 
  scale_y_continuous(
    limits = c(0.5, 1),
    breaks = c(0.5, 0.75, 1),
    minor_breaks = NULL,
    expand = c(0, 0)
  )


#Combined with AUC Curves 


library(patchwork)

diarrhea_combined <- diarrhea_roc$plot + range_plot_diarrhea &
  plot_layout(widths = c(3, 1.5)) &
  plot_annotation(title = "Diarrhea") & 
  theme(plot.title = element_text(size = 32, hjust = 0.5, face = "plain"))
setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(plot = diarrhea_combined, filename = "diarrhea_combined_ml.png", width = 18, height = 6, dpi = 600)
  


other_combined <- (abdominal_roc$plot + bloating_roc$plot) / (appetite_roc$plot + range_plot_other) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures")
ggsave(plot = other_combined, filename = "combined_ml.png", width = 20, height = 12, dpi = 600)



