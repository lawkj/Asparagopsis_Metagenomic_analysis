#pairwise_allcombos
# Load necessary libraries
library(vegan)
library(readxl)
library(writexl)
library(pairwiseAdonis)

# Genusbac
otu_table <- read_excel("genusbac_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")

# Use "Med" as the grouping factor
metadata$Med <- as.factor(metadata$Med)

# Ensure OTU table matches metadata order
numeric_otu_table <- otu_table[, -1]  # Remove first column (Sample ID)
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)  # Log transform data

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA on all `Med` groups
adonis_result <- adonis(bray_dist ~ metadata$Med, permutations = 999)

# Save PERMANOVA results to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "genusbac_adonis_full_results.xlsx")

# Run **Pairwise PERMANOVA** for all `Med` group combinations
pairwise_results <- pairwise.adonis(bray_dist, factors = metadata$Med)

# Adjust p-values for multiple comparisons (Benjamini-Hochberg)
pairwise_results$adjusted_p <- p.adjust(pairwise_results$p.value, method = "BH")

# Save pairwise results to Excel
write_xlsx(pairwise_results, "genusbac_pairwise_all_comparisons.xlsx")


# Genusarc
otu_table <- read_excel("genusarc_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")

# Use "Med" as the grouping factor
metadata$Med <- as.factor(metadata$Med)

# Ensure OTU table matches metadata order
numeric_otu_table <- otu_table[, -1]  # Remove first column (Sample ID)
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)  # Log transform data

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA on all `Med` groups
adonis_result <- adonis(bray_dist ~ metadata$Med, permutations = 999)


# Save PERMANOVA results to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "genusarc_adonis_full_results.xlsx")

# Run **Pairwise PERMANOVA** for all `Med` group combinations
pairwise_results <- pairwise.adonis(bray_dist, factors = metadata$Med)

# Adjust p-values for multiple comparisons (Benjamini-Hochberg)
pairwise_results$adjusted_p <- p.adjust(pairwise_results$p.value, method = "BH")

# Save pairwise results to Excel
write_xlsx(pairwise_results, "genusarc_pairwise_all_comparisons.xlsx")


# CAZy
otu_table <- read_excel("CAZy_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")

# Use "Med" as the grouping factor
metadata$Med <- as.factor(metadata$Med)

# Ensure OTU table matches metadata order
numeric_otu_table <- otu_table[, -1]  # Remove first column (Sample ID)
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)  # Log transform data

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA on all `Med` groups
adonis_result <- adonis(bray_dist ~ metadata$Med, permutations = 999)


# Save PERMANOVA results to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "CAZy_adonis_full_results.xlsx")

# Run **Pairwise PERMANOVA** for all `Med` group combinations
pairwise_results <- pairwise.adonis(bray_dist, factors = metadata$Med)

# Adjust p-values for multiple comparisons (Benjamini-Hochberg)
pairwise_results$adjusted_p <- p.adjust(pairwise_results$p.value, method = "BH")

# Save pairwise results to Excel
write_xlsx(pairwise_results, "CAZy_pairwise_all_comparisons.xlsx")


# EC
otu_table <- read_excel("EC_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")

# Use "Med" as the grouping factor
metadata$Med <- as.factor(metadata$Med)

# Ensure OTU table matches metadata order
numeric_otu_table <- otu_table[, -1]  # Remove first column (Sample ID)
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)  # Log transform data

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA on all `Med` groups
adonis_result <- adonis(bray_dist ~ metadata$Med, permutations = 999)


# Save PERMANOVA results to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "EC_adonis_full_results.xlsx")

# Run **Pairwise PERMANOVA** for all `Med` group combinations
pairwise_results <- pairwise.adonis(bray_dist, factors = metadata$Med)

# Adjust p-values for multiple comparisons (Benjamini-Hochberg)
pairwise_results$adjusted_p <- p.adjust(pairwise_results$p.value, method = "BH")

# Save pairwise results to Excel
write_xlsx(pairwise_results, "EC_pairwise_all_comparisons.xlsx")

# KEGGPathway
otu_table <- read_excel("keggpathway_normalized_nometa.xlsx")
metadata <- read_excel("metadata.xlsx")

# Use "Med" as the grouping factor
metadata$Med <- as.factor(metadata$Med)

# Ensure OTU table matches metadata order
numeric_otu_table <- otu_table[, -1]  # Remove first column (Sample ID)
log_transformed_data <- log(as.matrix(numeric_otu_table) + 1)  # Log transform data

# Calculate Bray-Curtis distance
bray_dist <- vegdist(log_transformed_data, method = "bray")

# Run PERMANOVA on all `Med` groups
adonis_result <- adonis(bray_dist ~ metadata$Med, permutations = 999)


# Save PERMANOVA results to Excel
adonis_result_df <- as.data.frame(adonis_result$aov.tab)
write_xlsx(adonis_result_df, "genus_adonis_full_results.xlsx")

# Run **Pairwise PERMANOVA** for all `Med` group combinations
pairwise_results <- pairwise.adonis(bray_dist, factors = metadata$Med)

# Adjust p-values for multiple comparisons (Benjamini-Hochberg)
pairwise_results$adjusted_p <- p.adjust(pairwise_results$p.value, method = "BH")

# Save pairwise results to Excel
write_xlsx(pairwise_results, "keggpathway_pairwise_all_comparisons.xlsx")


