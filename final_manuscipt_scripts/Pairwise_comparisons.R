# Load the libraries
library(vegan)      
library(writexl)    
library(pairwiseAdonis) 
library(readxl)

rm(list = ls())

#Step1: read in data and metadata
otu_table <- read_excel("genusbac_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1] #removes labels

#Step2: log transform the data (add 1 to avoid log(0))
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

#Step3: calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

#Step4: Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

#Step5: Print the PERMANOVA results, and export them to excel
print(adonis_result)
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "$$.xlsx")

#Step6: Run Pairwise Comparisons for Treatment, padjust using BH
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

#Step7: export the pairwise comparisons for Treatment 
write_xlsx(pairwise_results_treatment, "$$_pairwise_results_treatment.xlsx")

#Step8: Run Pairwise Comparisons for Time, padjust using BH
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

#Step9: export the pairwise comparisons for Time
write_xlsx(pairwise_results_time, "$$_pairwise_results_time.xlsx")

