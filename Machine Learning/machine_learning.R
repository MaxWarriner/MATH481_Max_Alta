setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning")
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
library(patchwork)
library(tidyverse)
library(parallel)
library(doParallel)
library(pROC)

ps <- read_rds('microbiome.RDS')
metab <- read_csv('metabolites_transposed.csv')
tax <- tax_table(ps)

colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

tax_table(ps) <- tax

#log transform and scale metabolites
metab <- metab |>
  mutate(across(where(is.numeric), ~ scale(log1p(.))[,1]))

metab <- metab |>
  column_to_rownames(var = "...1")


#scale nutrients
nutr <- ps@sam_data[,154:183]
nutr <- as.data.frame(scale(nutr))

health<- as.data.frame(ps@sam_data)

health <- health[,82:88]

sam <- data.frame(ps@sam_data)


#keep metabolite and microbiome overlapping samples

common_rows <- intersect(rownames(metab), rownames(nutr))
common_rows <- intersect(common_rows, rownames(health))

health <- health[common_rows, , drop = FALSE]
nutr <- nutr[common_rows, , drop = FALSE]
metab <- metab[common_rows, , drop = FALSE]
sam <- sam[common_rows, , drop = FALSE]

ps <- prune_samples(sample_names(ps) %in% common_rows, ps)

ps@sam_data$sex = ifelse(ps@sam_data$sex == "male", 1, 0)

#merge metabolites into ps sample data 
sample_data(ps) <- cbind(health, metab, nutr, sam[,1:2]) # all together
sample_data(ps) <- cbind(health, nutr, sam[,1:2]) # just nutrients

# Machine Learning Functions

get_tax_matrix <- function(ps, taxlevel, prefix, min_prevalence) {
  ps_tax <- tax_glom(ps, taxlevel)
  otumat <- as.data.frame(otu_table(ps_tax))
  if (!taxa_are_rows(ps_tax)) otumat <- t(otumat)
  taxmat <- as.data.frame(tax_table(ps_tax))
  tax_names <- taxmat[[taxlevel]]
  tax_names[is.na(tax_names) | tax_names == ""] <- "Unassigned"
  rownames(otumat) <- paste0(prefix, make.unique(tax_names))
  present_counts <- rowSums(otumat > 0)
  keep <- present_counts >= (min_prevalence * ncol(otumat))
  otumat <- otumat[keep, , drop = FALSE]
  return(otumat)
}

fam_tab <- get_tax_matrix(ps, "Family", "F__", min_prevalence = 0.05)
gen_tab <- get_tax_matrix(ps, "Genus", "G__",  min_prevalence = 0.05)
all_feat_tab <- rbind(fam_tab, gen_tab)
all_feat_tab[all_feat_tab == 0] <- 1e-6
clr_mat <- compositions::clr(all_feat_tab)
clr_mat <- t(clr_mat)

cv_predict_clr_xgb <- function(
    ps_obj,
    outcome_var,
    meta_cols,
    min_prevalence = 0.05,
    nfolds = 10,
    seed = 100,
    n_cores = 4, 
    clr_mat
) {
  set.seed(1313)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  
  meta <- data.frame(sample_data(ps_obj))
  clr_samples <- rownames(clr_mat)
  meta <- meta[clr_samples, , drop = FALSE]
  y <- as.factor(meta[[outcome_var]])
  keep <- !is.na(y)
  clr_mat <- clr_mat[keep, , drop = FALSE]
  meta <- meta[keep, , drop = FALSE]
  y <- y[keep]
  levels(y) <- make.names(levels(y))
  
  meta_cols <- intersect(meta_cols, colnames(meta))
  meta_features <- meta[, meta_cols, drop = FALSE]
  
  X_full <- cbind(clr_mat, meta_features)
  nzv <- caret::nearZeroVar(X_full)
  if (length(nzv) > 0) X_full <- X_full[, -nzv, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(200, 400, 800),
    max_depth = c(3, 5, 7, 9),
    eta = c(0.01, 0.05, 0.1, 0.3),
    gamma = c(0, 0.1, 0.5, 1),
    colsample_bytree = c(0.6, 0.8, 1.0),
    min_child_weight = c(1, 3, 5),
    subsample = c(0.6, 0.8, 1.0)
  )
  
  fitControl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    classProbs = TRUE,
    savePredictions = "final",
    allowParallel = TRUE, 
    verboseIter = T
  )
  
  xgb_fit <- caret::train(
    x = X_full,
    y = y,
    method = "xgbTree",
    tuneGrid = xgb_grid,
    trControl = fitControl,
    verbose = FALSE
  )
  
  overall_acc <- max(xgb_fit$results$Accuracy)
  best_params <- xgb_fit$bestTune
  
  varimp <- caret::varImp(xgb_fit, scale = FALSE)$importance
  varimp_df <- tibble(Feature = rownames(varimp), Importance = varimp[,1]) %>%
    arrange(desc(Importance)) %>%
    mutate(Level = dplyr::case_when(
      grepl("^F__", Feature) ~ "Family",
      grepl("^G__", Feature) ~ "Genus",
      Feature %in% meta_cols ~ "Metadata",
      TRUE ~ "Other"
    ))
  
  preds <- xgb_fit$pred$pred[order(xgb_fit$pred$rowIndex)]
  y_true <- xgb_fit$pred$obs[order(xgb_fit$pred$rowIndex)]
  overall_cm <- caret::confusionMatrix(preds, y_true)
  
  stopCluster(cl)
  registerDoSEQ()
  
  return(list(
    overall_accuracy = overall_acc,
    best_params = best_params,
    confusion_matrix = overall_cm,
    feature_importance = varimp_df,
    predictions = preds,
    y_true = y_true,
    xgb_fit = xgb_fit
  ))
}

## ROC, Variable Importance, and Heatmap plots

plot_roc_curve_gg <- function(model_results, positive_class = NULL, factor) {
  
  y_true <- model_results$y_true
  if (is.null(positive_class)) {
    positive_class <- levels(y_true)[1]
  }
  pred_prob <- model_results$xgb_fit$pred %>%
    filter(obs %in% levels(y_true)) %>%
    arrange(rowIndex) %>%
    pull(!!as.name(positive_class))
  
  roc_obj <- roc(y_true, pred_prob, levels = rev(levels(y_true)), direction = "<")
  auc_value <- auc(roc_obj)
  
  roc <- ggroc(roc_obj, legacy.axes = TRUE, colour = "darkgreen", size = 1.3) +
    geom_abline(linetype = "dashed", color = "gray50") +
    labs(
      title = paste(factor, ": ROC Curve (AUC = ", round(auc_value, 3), ")", sep = ""),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(filename = paste(factor, "ROC.png", sep = ""), plot = roc, width = 7, height = 5)
  
}

plot_top_importance <- function(model_results, n_top = 10, bar_color = "#2c7bb6", factor) {
  
  varimp <- model_results$feature_importance %>% 
    arrange(desc(Importance)) %>%
    head(n_top)
  
  plot <- ggplot(varimp, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = bar_color) +
    coord_flip() +
    labs(
      title = paste(factor, ": Top", n_top, "Feature Importance"),
      x = "",
      y = "Importance"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  ggsave(filename = paste(factor, "top_importance.png", sep = ""), plot = plot, width = 10, height = 5)
  
}

plot_top_feature_heatmap_clr <- function(
    ps_obj,
    model_results,
    n_top = 10,
    metadata_vars,
    outcome_var,
    min_prevalence = 0.05
) {
  
  # 1. Top N features
  top_features <- head(model_results$feature_importance$Feature, n_top)
  
  # 2. Get genus & family tables
  get_tax_matrix <- function(ps, taxlevel, prefix) {
    ps_tax <- tax_glom(ps, taxlevel)
    otumat <- as.data.frame(otu_table(ps_tax))
    if (!taxa_are_rows(ps_tax)) otumat <- t(otumat)
    taxmat <- as.data.frame(tax_table(ps_tax))
    tax_names <- taxmat[[taxlevel]]
    tax_names[is.na(tax_names) | tax_names == ""] <- "Unassigned"
    rownames(otumat) <- paste0(prefix, make.unique(tax_names))
    present_counts <- rowSums(otumat > 0)
    keep <- present_counts >= (min_prevalence * ncol(otumat))
    otumat <- otumat[keep, , drop=FALSE]
    return(otumat)
  }
  
  fam_tab <- get_tax_matrix(ps_obj, "Family", "F__")
  gen_tab <- get_tax_matrix(ps_obj, "Genus", "G__")
  all_feat_tab <- rbind(fam_tab, gen_tab)
  
  # 3. CLR transformation
  all_feat_tab[all_feat_tab == 0] <- 1e-6
  clr_mat <- compositions::clr(all_feat_tab)
  clr_mat <- t(clr_mat)  # samples as rows, features as columns
  
  # 4. Get metadata (as numeric)
  meta <- as.data.frame(sample_data(ps_obj))
  clr_samples <- rownames(clr_mat)
  meta <- meta[clr_samples, , drop = FALSE]
  metadata_vars <- intersect(metadata_vars, colnames(meta))
  metadata_features <- meta[, metadata_vars, drop = FALSE]
  metadata_features[] <- lapply(metadata_features, function(x) as.numeric(as.character(x)))
  
  # 5. Combine CLR and metadata
  X_full <- cbind(clr_mat, metadata_features)
  # Ensure all columns are numeric
  for (i in seq_len(ncol(X_full))) {
    if (!is.numeric(X_full[, i])) {
      X_full[, i] <- as.numeric(as.character(X_full[, i]))
    }
  }
  
  # 6. Subset to top features, as rows (order preserved)
  top_feats_present <- top_features[top_features %in% colnames(X_full)]
  if (length(top_feats_present) == 0) stop("No top features found in input matrix.")
  heatmap_mat <- t(X_full[, top_feats_present, drop = FALSE])
  
  # Remove rows (features) that are all NA or all zero
  keep_rows <- apply(heatmap_mat, 1, function(x) any(!is.na(x)) && any(x != 0))
  heatmap_mat <- heatmap_mat[keep_rows, , drop = FALSE]
  if (nrow(heatmap_mat) == 0) stop("No valid features to plot after removing all-NA/zero rows.")
  
  # 7. Get annotation for sample outcome
  sample_anno <- as(sample_data(ps_obj), "data.frame")[, outcome_var, drop = FALSE]
  sample_anno <- sample_anno[colnames(heatmap_mat), , drop = FALSE]
  sample_anno[[outcome_var]] <- as.factor(as.character(sample_anno[[outcome_var]]))
  rownames(sample_anno) <- colnames(heatmap_mat)
  
  # 8. Order columns by group
  group_order <- order(sample_anno[[outcome_var]])
  heatmap_mat <- heatmap_mat[, group_order, drop = FALSE]
  sample_anno <- sample_anno[group_order, , drop = FALSE]
  
  # 9. Z-score (standardize) each feature (row)
  heatmap_mat_scaled <- t(scale(t(heatmap_mat)))
  heatmap_mat_scaled[is.na(heatmap_mat_scaled)] <- 0
  
  # 10. Plot heatmap (no clustering of samples)
  heatmap <- pheatmap::pheatmap(
    mat = heatmap_mat_scaled,
    annotation_col = sample_anno,
    main = paste("Top", n_top, "Features Heatmap"),
    clustering_method = "complete",
    cluster_cols = FALSE,
    cluster_rows = TRUE,
    fontsize_row = 10,
    fontsize_col = 7,
    scale = "none",
    color = colorRampPalette(c("navy", "white", "firebrick3"))(100)
  )
  
  ggsave(filename = paste(outcome_var, "_heatmap.png", sep = ""), plot = heatmap, height = 6, width = 18)
  
  return(heatmap)
  
}


# Illness w/ microbiome
illness_results <- cv_predict_clr_xgb(ps, "illness", meta_cols = c("Age", "sex"), clr_mat = clr_mat)
illness_results$confusion_matrix
head(illness_results$feature_importance, 50)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Heatmaps")
plot_top_feature_heatmap_clr(ps_obj = ps, model_results = illness_results,
                             n_top = 10, metadata_vars = c("sex", "Age"), 
                             outcome_var = "illness", min_prevalence = 0.05)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg(illness_results, factor = "illness")

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")
plot_top_importance(illness_results, n_top = 10, factor = "illness")


# Illness w/ microbiome + metabolome
illness_results_plus <- cv_predict_clr_xgb(ps, "illness", meta_cols = c("Age", "sex", colnames(metab)), clr_mat = clr_mat)
illness_results$confusion_matrix
head(illness_results$feature_importance, 50)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Heatmaps")
plot_top_feature_heatmap_clr(ps_obj = ps, model_results = illness_results,
                             n_top = 10, metadata_vars = c("sex", "Age"), 
                             outcome_var = "illness", min_prevalence = 0.05)


setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/ROC Curves")
plot_roc_curve_gg(illness_results, factor = "illness")

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Top 10 Importance")
plot_top_importance(illness_results, n_top = 10, factor = "illness")

