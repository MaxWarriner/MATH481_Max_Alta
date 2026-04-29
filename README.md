Data and Scripts for the project: Dietary Intake Influences GI Health Outcomes via the GutMicrobiome and Metabolome in Ethiopian Schoolchildren by Max Warriner & Alta McQuillen

Data files:
- lily_ps: Raw phyloseq object from Professor Taye
- microbiome: Cleaned phyloseq object used in most analyses. Contains the sample data and microbiome data
- microbiome_diet_2.0: Nutrisurvey file containing nutritional information about each food in the FFQ
- nutrient_data: Converted Nutrisurvey file into an excel spreadsheet
- combined_mediation_metabolomics_data: Used in the mediation script. Contains nutritional information binded to metabolomics data
- combined_medation_microbiome_data: Same as the previous dataset except with microbiome data binded
- metab_and_info: Used for the metabolomics heatmaps where metabolites are annotated with more information. Contains metabolomics data and annotations
- metabolites_transposed: Metabolomics data where rows are samples and columns are metabolites. Used the most in the scripts
- Metabolomics: Raw trimmed metabolomics file with rows as metabolites and columns as samples
- Metabolomics_original: Original file from Professor Taye so untrimmed
- clr_mat: found in the machine learning folder. Center log transformed genus abundance microbiome matrix used as input in the XGBoost models
- microbiome_features: microbiome features for machine learning script
- metab_features: metabolomics features for machine learning script
- diet_features: dietary features for machine learning script
- health_feature: binary health features for machine learning script
- combined_features: combined features for machine learning script
- results_2: results for the 2nd attempt at machine learning 

Scripts:

Data Processing:
- data_cleaning: data cleaning of the original phyloseq object, renaming columns, converting questionnaire answers to food volume, imputing data, etc.
- food_groups: Summing columns to make food group variables
- nutrient_coding: Script that sums FFQ data to infer nutrient intake. Combines data from nutrisurvey data and sample data.
- high_low_groups: Adding categorical variables that split continuous variables at the median
- metabolite_cleaning: annoying metabolite annotation cleaning

Machine Learning: 
(outdated refers to the previous attempt at XGBoost models)
- machine_learning_classifiers: python notebook that contains the script for all the machine learning results

Standard Microbiome Analysis:
- alpha_diversity: Microbiome and metabolite alpha diversity regression analysis with plot making functions
- beta_diversity: Microbiome and metabolite beta diversity analysis and plot making functions
- differential_abundance: unfinished differential abundance analysis that could be updated for final paper if wanted

mediation2: mediation analysis script that contains initial screening with linear and logistic regression as well as each lavaan model constructed

pheatmap2: Script for hierarchical clustering heatmaps

poster_figures: updated special functions for figures going on the poster and paper

Figures: folders for all of the significant figures from the analyses scripts
