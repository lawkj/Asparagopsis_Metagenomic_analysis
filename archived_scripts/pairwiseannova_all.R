# Load necessary packages
library(vegan)      # For PERMANOVA (adonis) and distance calculation
library(writexl)    # For writing Excel files
library(pairwiseAdonis) # For pairwise comparisons
library(readxl)

rm(list = ls())
#genusbac
otu_table <- read_excel("genusbac_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1]
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

# Print the PERMANOVA result
print(adonis_result)

# Write PERMANOVA result to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "genusbac_adonis_result.xlsx")

# Run Pairwise Comparisons for Treatment
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

# Write pairwise comparisons for Treatment to Excel
write_xlsx(pairwise_results_treatment, "genusbac_pairwise_results_treatment.xlsx")

# Run Pairwise Comparisons for Time
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

# Write pairwise comparisons for Time to Excel
write_xlsx(pairwise_results_time, "genusbac_pairwise_results_time.xlsx")

rm(list = ls())
#genusarc
otu_table <- read_excel("genusarc_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1]
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

# Print the PERMANOVA result
print(adonis_result)

# Write PERMANOVA result to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "genusarc_adonis_result.xlsx")

# Run Pairwise Comparisons for Treatment
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

# Write pairwise comparisons for Treatment to Excel
write_xlsx(pairwise_results_treatment, "genusarc_pairwise_results_treatment.xlsx")

# Run Pairwise Comparisons for Time
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

# Write pairwise comparisons for Time to Excel
write_xlsx(pairwise_results_time, "genusarc_pairwise_results_time.xlsx")

rm(list = ls())
#CAZy
otu_table <- read_excel("CAZy_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1]
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

# Print the PERMANOVA result
print(adonis_result)

# Write PERMANOVA result to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "CAZy_adonis_result.xlsx")

# Run Pairwise Comparisons for Treatment
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

# Write pairwise comparisons for Treatment to Excel
write_xlsx(pairwise_results_treatment, "CAZy_pairwise_results_treatment.xlsx")

# Run Pairwise Comparisons for Time
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

# Write pairwise comparisons for Time to Excel
write_xlsx(pairwise_results_time, "CAZy_pairwise_results_time.xlsx")

rm(list = ls())
#EC
otu_table <- read_excel("EC_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1]
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

# Print the PERMANOVA result
print(adonis_result)

# Write PERMANOVA result to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "EC_adonis_result.xlsx")

# Run Pairwise Comparisons for Treatment
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

# Write pairwise comparisons for Treatment to Excel
write_xlsx(pairwise_results_treatment, "EC_pairwise_results_treatment.xlsx")

# Run Pairwise Comparisons for Time
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

# Write pairwise comparisons for Time to Excel
write_xlsx(pairwise_results_time, "EC_pairwise_results_time.xlsx")


rm(list = ls())
#keggpathway
otu_table <- read_excel("keggpathway_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")
numeric_otu_table <- otu_table[, -1]
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA with log-transformed Bray-Curtis distance
adonis_result <- adonis(bray_dist ~ metadata$Treatment + metadata$Time, permutations = 999)

# Print the PERMANOVA result
print(adonis_result)

# Write PERMANOVA result to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "keggpathway_adonis_result.xlsx")

# Run Pairwise Comparisons for Treatment
pairwise_results_treatment <- pairwise.adonis(bray_dist, factors = metadata$Treatment)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_treatment$adjusted_p <- p.adjust(pairwise_results_treatment$p.value, method = "BH")

# Write pairwise comparisons for Treatment to Excel
write_xlsx(pairwise_results_treatment, "keggpathway_pairwise_results_treatment.xlsx")

# Run Pairwise Comparisons for Time
pairwise_results_time <- pairwise.adonis(bray_dist, factors = metadata$Time)
# Adjust p-values for multiple comparisons (e.g., Benjamini-Hochberg)
pairwise_results_time$adjusted_p <- p.adjust(pairwise_results_time$p.value, method = "BH")

# Write pairwise comparisons for Time to Excel
write_xlsx(pairwise_results_time, "keggpathway_pairwise_results_time.xlsx")
