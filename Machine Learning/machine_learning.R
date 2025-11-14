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


#Function to get workable matrix for microbiome data
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

# save(clr_mat, file = "clr_mat.RData")

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning")

clr_mat <- load('clr_mat.RData')

clr_mat <- scale(clr_mat)

# Machine Learning Functions


#Overall XGBoost function that can include metabolomics

cv_predict_clr_xgb <- function(
    ps_obj,
    outcome_var,
    meta_cols = NULL,         
    min_prevalence = 0.05,
    nfolds = 10,
    seed = 100,
    n_cores = 4, 
    clr_mat
) {
  set.seed(seed)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  # --- Metadata and outcome ---
  meta <- as.data.frame(sample_data(ps_obj))
  clr_samples <- rownames(clr_mat)
  meta <- meta[clr_samples, , drop = FALSE]
  y <- as.factor(meta[[outcome_var]])
  
  # Remove missing outcome
  keep <- !is.na(y)
  clr_mat <- clr_mat[keep, , drop = FALSE]
  meta <- meta[keep, , drop = FALSE]
  y <- droplevels(y[keep])
  levels(y) <- make.names(levels(y))  # caret-safe factor names
  
  # --- Prepare microbiome features ---
  X_micro <- as.data.frame(clr_mat)
  X_micro[] <- lapply(X_micro, function(x) as.numeric(as.character(x)))
  X_micro <- X_micro[, colSums(is.na(X_micro)) == 0, drop = FALSE]
  
  # --- Prepare metadata features if provided ---
  X_meta <- NULL
  if (!is.null(meta_cols)) {
    meta_cols <- intersect(meta_cols, colnames(meta))
    if (length(meta_cols) > 0) {
      X_meta <- meta[, meta_cols, drop = FALSE]
      
      # Convert character/factor metadata to numeric dummy variables
      X_meta <- data.frame(model.matrix(~ . - 1, data = data.frame(X_meta)))
      
      # Ensure numeric and handle NAs
      X_meta[] <- lapply(X_meta, function(x) as.numeric(as.character(x)))
      X_meta <- X_meta[, colSums(is.na(X_meta)) == 0, drop = FALSE]
    }
  }
  
  # --- Combine microbiome and metadata features ---
  if (!is.null(X_meta)) {
    X_full <- cbind(X_micro, X_meta)
  } else {
    X_full <- X_micro
  }
  
  # --- Remove zero or near-zero variance columns ---
  nzv <- caret::nearZeroVar(X_full)
  if (length(nzv) > 0) X_full <- X_full[, -nzv, drop = FALSE]
  
  # --- Check if there are enough samples per class ---
  if (any(table(y) < 2)) {
    stop("Some outcome levels have fewer than 2 samples; CV cannot proceed.")
  }
  
  # xgb_grid <- expand.grid(
  #   nrounds = c(200, 400, 800, 1200),         # more boosting rounds for convergence
  #   max_depth = c(3, 6, 9, 12, 15),           # allow deeper trees for complex patterns
  #   eta = c(0.005, 0.01, 0.05, 0.1, 0.2),     # very fine learning rate control
  #   gamma = c(0, 0.1, 0.5, 1),                # regularization strength
  #   colsample_bytree = c(0.6, 0.8, 1.0),      # feature sampling
  #   min_child_weight = c(1, 3, 5, 7),         # leaf node complexity
  #   subsample = c(0.6, 0.8, 1.0)              # row sampling
  # )
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 300),          # fast, still shows learning behavior
    max_depth = c(3, 6),            # shallow + moderate
    eta = c(0.05, 0.1),             # reasonably fast learning
    gamma = c(0, 0.1),              # mild reg range
    colsample_bytree = c(0.8),      # fixed for speed
    min_child_weight = c(1),        # fixed for speed
    subsample = c(0.8)              # fixed for speed
  )
  
  fitControl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    classProbs = TRUE,
    savePredictions = "final",
    allowParallel = TRUE, 
    verboseIter = TRUE
  )
  
  # --- Train model ---
  xgb_fit <- tryCatch({
    caret::train(
      x = X_full,
      y = y,
      method = "xgbTree",
      tuneGrid = xgb_grid,
      trControl = fitControl,
      verbose = FALSE
    )
  }, error = function(e) e)
  
  # --- Error handling ---
  if (inherits(xgb_fit, "error")) {
    stop("caret::train() failed. Check that X_full has no NAs and all numeric columns.")
  }
  if (all(is.na(xgb_fit$results$Accuracy))) {
    stop("All Accuracy values are NA. This usually means the model failed during cross-validation.")
  }
  
  # --- Results ---
  overall_acc <- max(xgb_fit$results$Accuracy, na.rm = TRUE)
  best_params <- xgb_fit$bestTune
  
  varimp <- caret::varImp(xgb_fit, scale = FALSE)$importance
  varimp_df <- tibble::tibble(Feature = rownames(varimp), Importance = varimp[,1]) %>%
    dplyr::arrange(desc(Importance)) %>%
    dplyr::mutate(Level = dplyr::case_when(
      grepl("^F__", Feature) ~ "Family",
      grepl("^G__", Feature) ~ "Genus",
      Feature %in% colnames(X_meta) ~ "Metadata",
      TRUE ~ "Other"
    ))
  
  preds <- xgb_fit$pred$pred[order(xgb_fit$pred$rowIndex)]
  y_true <- xgb_fit$pred$obs[order(xgb_fit$pred$rowIndex)]
  overall_cm <- caret::confusionMatrix(preds, y_true)
  
  # --- Stop and unregister cluster ---
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  
  # --- Return results ---
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

#Metabolomics Only XGBoost Function

cv_predict_xgb_meta <- function(
    ps_obj,
    outcome_var,
    meta_cols,
    nfolds = 10,
    seed = 100,
    n_cores = 4
) {
  set.seed(seed)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  # --- Prepare metadata and outcome ---
  meta <- as.data.frame(sample_data(ps_obj))
  y <- as.factor(meta[[outcome_var]])
  keep <- !is.na(y)
  meta <- meta[keep, , drop = FALSE]
  y <- droplevels(y[keep])
  levels(y) <- make.names(levels(y))  # caret safe
  
  # --- Select only metadata columns ---
  meta_cols <- intersect(meta_cols, colnames(meta))
  X_meta <- meta[, meta_cols, drop = FALSE]
  
  # --- Clean factor contrasts (this fixes your error) ---
  for (nm in names(X_meta)) {
    if (is.factor(X_meta[[nm]])) {
      contrasts(X_meta[[nm]]) <- NULL
    }
  }
  
  # --- Convert categorical variables to dummy variables safely ---
  dummy_model <- caret::dummyVars("~ .", data = data.frame(X_meta), fullRank = TRUE)
  X_full <- predict(dummy_model, newdata = X_meta) %>% as.data.frame()
  
  # --- Ensure numeric and remove any NA columns ---
  X_full[] <- lapply(X_full, function(x) as.numeric(as.character(x)))
  X_full <- X_full[, colSums(is.na(X_full)) == 0, drop = FALSE]
  
  # --- Remove near-zero variance predictors ---
  nzv <- caret::nearZeroVar(X_full)
  if (length(nzv) > 0) X_full <- X_full[, -nzv, drop = FALSE]
  
  # xgb_grid <- expand.grid(
  #   nrounds = c(200, 400, 800, 1200),         # more boosting rounds for convergence
  #   max_depth = c(3, 6, 9, 12, 15),           # allow deeper trees for complex patterns
  #   eta = c(0.005, 0.01, 0.05, 0.1, 0.2),     # very fine learning rate control
  #   gamma = c(0, 0.1, 0.5, 1),                # regularization strength
  #   colsample_bytree = c(0.6, 0.8, 1.0),      # feature sampling
  #   min_child_weight = c(1, 3, 5, 7),         # leaf node complexity
  #   subsample = c(0.6, 0.8, 1.0)              # row sampling
  # )
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 300),          # fast, still shows learning behavior
    max_depth = c(3, 6),            # shallow + moderate
    eta = c(0.05, 0.1),             # reasonably fast learning
    gamma = c(0, 0.1),              # mild reg range
    colsample_bytree = c(0.8),      # fixed for speed
    min_child_weight = c(1),        # fixed for speed
    subsample = c(0.8)              # fixed for speed
  )
  
  fitControl <- caret::trainControl(
    method = "cv",
    number = nfolds,
    classProbs = TRUE,
    savePredictions = "final",
    allowParallel = TRUE,
    verboseIter = TRUE
  )
  
  # --- Train the XGBoost model ---
  xgb_fit <- caret::train(
    x = X_full,
    y = y,
    method = "xgbTree",
    tuneGrid = xgb_grid,
    trControl = fitControl,
    verbose = FALSE
  )
  
  # --- Results ---
  overall_acc <- max(xgb_fit$results$Accuracy)
  best_params <- xgb_fit$bestTune
  
  varimp <- caret::varImp(xgb_fit, scale = FALSE)$importance
  varimp_df <- tibble::tibble(
    Feature = rownames(varimp),
    Importance = varimp[, 1]
  ) %>%
    dplyr::arrange(desc(Importance))
  
  preds <- xgb_fit$pred$pred[order(xgb_fit$pred$rowIndex)]
  y_true <- xgb_fit$pred$obs[order(xgb_fit$pred$rowIndex)]
  overall_cm <- caret::confusionMatrix(preds, y_true)
  
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  
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

plot_roc_curve_gg <- function(model_results, positive_class = NULL, factor, filename) {
  
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
  
  ggsave(filename = filename, plot = roc, width = 7, height = 5)
  
}

plot_top_importance <- function(model_results, n_top = 10, bar_color = "#2c7bb6", factor, filename) {
  
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
  
  ggsave(filename = filename, plot = plot, width = 10, height = 5)
  
}

plot_top_feature_heatmap_clr <- function(
    ps_obj,
    model_results,
    n_top = 10,
    metadata_vars,
    outcome_var,
    min_prevalence = 0.05, 
    filename, 
    clr_mat
) {
  
  # 1. Top N features
  top_features <- head(model_results$feature_importance$Feature, n_top)
  
  
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
  
  ggsave(filename = filename, plot = heatmap, height = 6, width = 18)
  
  return(heatmap)
  
}

for (health_outcome in colnames(health)){

# microbiome only
microbiome_results <- cv_predict_clr_xgb(ps, health_outcome, meta_cols = c("Age", "sex"), clr_mat = clr_mat)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models")
save(microbiome_results, file = paste(health_outcome, "_microbiome_results.RData", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome Only/Heatmaps")
plot_top_feature_heatmap_clr(ps_obj = ps, model_results = microbiome_results,
                             n_top = 10, metadata_vars = c("sex", "Age"), 
                             outcome_var = health_outcome, min_prevalence = 0.05, 
                             filename = paste(health_outcome, "_microbiome_heatmap.png"), 
                             clr_mat = clr_mat)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome Only/ROC Curves")
plot_roc_curve_gg(microbiome_results, factor = health_outcome, filename = paste(health_outcome, "_microbiome_ROC.png", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome Only/Top 10 Importance")
plot_top_importance(microbiome_results, n_top = 10, factor = health_outcome, filename = paste(health_outcome, "_microbiome_top10.png", sep = ""))


#Metabolome Only
metabolome_results <- cv_predict_xgb_meta(ps, health_outcome, meta_cols = c("Age", "sex", colnames(metab)))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models")
save(metabolome_results, file = paste(health_outcome, "_metabolome_results.RData", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Metabolome Only/Heatmaps")
plot_top_feature_heatmap_clr(ps_obj = ps, model_results = metabolome_results,
                             n_top = 10, metadata_vars = c("sex", "Age", colnames(metab)), 
                             outcome_var = health_outcome, min_prevalence = 0.05, 
                             filename = paste(health_outcome, "_metabolome_heatmap.png"), 
                             clr_mat = clr_mat)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Metabolome Only/ROC Curves")
plot_roc_curve_gg(metabolome_results, factor = health_outcome, filename = paste(health_outcome, "_metabolome_ROC.png", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Metabolome Only/Top 10 Importance")
plot_top_importance(metabolome_results, n_top = 10, factor = health_outcome, filename = paste(health_outcome, "_metabolome_top10.png", sep = ""))



#Microbiome + Metabolome
microbiome_metabolome_results <- cv_predict_clr_xgb(ps, health_outcome, meta_cols = c("Age", "sex", colnames(metab)), clr_mat = clr_mat)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Machine Learning/Machine Learning Models")
save(microbiome_metabolome_results, file = paste(health_outcome, "_microbiome_metabolome_results.RData", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome + Metabolome/Heatmaps")
plot_top_feature_heatmap_clr(ps_obj = ps, model_results = microbiome_metabolome_results,
                             n_top = 10, metadata_vars = c("sex", "Age", colnames(metab)), 
                             outcome_var = health_outcome, min_prevalence = 0.05, 
                             filename = paste(health_outcome, "_microbiome_metabolome_heatmap.png"), 
                             clr_mat = clr_mat)

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome + Metabolome/ROC Curves")
plot_roc_curve_gg(microbiome_metabolome_results, factor = health_outcome, filename = paste(health_outcome, "_microbiome_metabolome_ROC.png", sep = ""))

setwd("C:/Users/12697/Documents/MATH481_Max_Alta/Figures/Machine Learning/Microbiome + Metabolome/Top 10 Importance")
plot_top_importance(microbiome_metabolome_results, n_top = 10, factor = health_outcome, filename = paste(health_outcome, "_microbiome_metabolome_top10.png", sep = ""))



}

