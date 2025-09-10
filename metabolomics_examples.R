library(omu)
library(ggplot2)

meta <- omu::c57_nos2KO_mouse_metadata

count <- omu::c57_nos2KO_mouse_countDF

DF <- assign_hierarchy(count = count, keep_unknowns = T, identifier = "KEGG")

log_DF <- cbind(DF[,c(1,2, 32:36)], log(DF[,3:31]))

pca_plot <- PCA_plot(count_data = log_DF,
         metadata = meta, 
         variable = "Treatment", 
         color = "Treatment",
         response_variable = "Metabolite", 
         ellipse = T) + 
  theme_bw() + 
  ggtitle('Standard PCA')


forest <- random_forest(count_data = count, metadata = meta, 
                        model = Treatment ~ .)

plot_variable_importance(forest)

forest_pca <- plot_rf_PCA(forest, color = "Treatment", size = 1.5, ellipse = T) + 
  theme_bw() + 
  ggtitle('Random Forest')

library(patchwork)
pca_plot + forest_pca

sum_dat <- omu_summary(count_data = DF, metadata = meta,
            numerator = "Strep", denominator = "Mock", 
            response_variable = "Metabolite", Factor = "Treatment", 
            log_transform = T)

omu_anova(count_data = count, metadata = meta, 
          response_variable = "Metabolite", model = ~ Treatment, 
          log_transform = T)

signif <- sum_dat[which(sum_dat[,"padj"] <= 0.05),]

KEGG_gather(signif)

fold_changes <- count_fold_changes(count_data = sum_dat, 
                                   column = "Class", 
                                   sig_threshold = 0.05, 
                                   keep_unknowns = F)


plot_bar(fc_data = fold_changes, fill = c("red", "green"))



plot_volcano(count_data = sum_dat, size = 2, column = "Class",
             strpattern = c("Carbohydrates", "Organic acids"), 
             fill = c("firebrick2", "white", "dodgerblue2"), 
             alpha = c(1, 1, 1), shape = c(21, 21, 21), 
             color = c("black", "black", "black"))

plot_heatmap(count_data = DF, metadata = meta, Factor = "Treatment",
             response_variable = "Metabolite", low_color = "midnightblue", 
             high_color = "goldenrod1", aggregate_by = "Class", log_transform = T)

library(dplyr)
carb_subset <- DF[c(which(DF$Class == "Carbohydrates")),]

plot_heatmap(count_data = carb_subset, metadata = meta, Factor = "Treatment",
             response_variable = "Metabolite", low_color = "midnightblue", 
             high_color = "goldenrod1", aggregate_by = "Subclass_2", log_transform = T)


plot_boxplot(count_data = carb_subset[1:10,], metadata = meta, 
             log_transform = T, Factor = "Treatment", response_variable = "Metabolite",
             fill_list = c("darkgoldenrod1", "dodgerblue2"))

