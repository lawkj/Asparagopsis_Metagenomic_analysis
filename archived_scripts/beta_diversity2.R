# bray curtis function
# Load libraries
library(vegan)
library(ggplot2)
library(readxl)

rm(list = ls())
#Genusbac
# Read the data
data6 <- read_xlsx("genusbac_normalizedm.xlsx")

# Step 1: Dynamically select species columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Step 2: Log Transform the species columns (avoid log(0) by adding 1)
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

# Step 3: Calculate Bray-Curtis Dissimilarity using vegan's vegdist function
bray_curtis <- vegdist(species_log, method = "bray")

# Step 4: Perform PCoA using the dissimilarity matrix
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k = 2 for two principal coordinates, eig = TRUE to return eigenvalues

# Step 5: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

# Step 6: Calculate the percentage variance explained for the first two principal coordinates
# This is based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

# Step 7: Plot the PCoA results with custom colors and shapes, and ellipses
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, color and shape based on Med
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, size = 20),  # Center the title and make it bigger
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14),  # Increase axis tick labels size
    )
# Save the plot
ggsave("braycurtis_genusbac_noellipses.png", width = 10, height = 8, dpi = 600)

rm(list = ls())
#Genusarc
# Read the data
data6 <- read_xlsx("genusarc_normalizedm.xlsx")

# Step 1: Dynamically select species columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Step 2: Log Transform the species columns (avoid log(0) by adding 1)
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

# Step 3: Calculate Bray-Curtis Dissimilarity using vegan's vegdist function
bray_curtis <- vegdist(species_log, method = "bray")

# Step 4: Perform PCoA using the dissimilarity matrix
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k = 2 for two principal coordinates, eig = TRUE to return eigenvalues

# Step 5: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

# Step 6: Calculate the percentage variance explained for the first two principal coordinates
# This is based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

# Step 7: Plot the PCoA results with custom colors and shapes, and ellipses
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, color and shape based on Med
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, size = 20),  # Center the title and make it bigger
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14),  # Increase axis tick labels size
    )
# Save the plot
ggsave("braycurtis_genusarc_noellipses.png", width = 10, height = 8, dpi = 600)

rm(list = ls())
#EC
# Read the data
data6 <- read_xlsx("EC_normalizedm.xlsx")

# Step 1: Dynamically select species columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Step 2: Log Transform the species columns (avoid log(0) by adding 1)
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

# Step 3: Calculate Bray-Curtis Dissimilarity using vegan's vegdist function
bray_curtis <- vegdist(species_log, method = "bray")

# Step 4: Perform PCoA using the dissimilarity matrix
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k = 2 for two principal coordinates, eig = TRUE to return eigenvalues

# Step 5: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

# Step 6: Calculate the percentage variance explained for the first two principal coordinates
# This is based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

# Step 7: Plot the PCoA results with custom colors and shapes, and ellipses
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, color and shape based on Med
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, size = 20),  # Center the title and make it bigger
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14),  # Increase axis tick labels size
    )
# Save the plot
ggsave("braycurtis_EC_noellipses.png", width = 10, height = 8, dpi = 600)

rm(list = ls())

#CAZy
# Read the data
data6 <- read_xlsx("CAZy_normalizedm.xlsx")

# Step 1: Dynamically select species columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Step 2: Log Transform the species columns (avoid log(0) by adding 1)
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

# Step 3: Calculate Bray-Curtis Dissimilarity using vegan's vegdist function
bray_curtis <- vegdist(species_log, method = "bray")

# Step 4: Perform PCoA using the dissimilarity matrix
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k = 2 for two principal coordinates, eig = TRUE to return eigenvalues

# Step 5: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

# Step 6: Calculate the percentage variance explained for the first two principal coordinates
# This is based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

# Step 7: Plot the PCoA results with custom colors and shapes, and ellipses
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, color and shape based on Med
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, size = 20),  # Center the title and make it bigger
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14),  # Increase axis tick labels size
    )
# Save the plot
ggsave("braycurtis_CAZy_noellipses.png", width = 10, height = 8, dpi = 600)

rm(list = ls())

#keggpathway
# Read the data
data6 <- read_xlsx("keggpathway_normalizedm.xlsx")

# Step 1: Dynamically select species columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Step 2: Log Transform the species columns (avoid log(0) by adding 1)
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

# Step 3: Calculate Bray-Curtis Dissimilarity using vegan's vegdist function
bray_curtis <- vegdist(species_log, method = "bray")

# Step 4: Perform PCoA using the dissimilarity matrix
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k = 2 for two principal coordinates, eig = TRUE to return eigenvalues

# Step 5: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

# Step 6: Calculate the percentage variance explained for the first two principal coordinates
# This is based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

# Step 7: Plot the PCoA results with custom colors and shapes, and ellipses
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, color and shape based on Med
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, size = 20),  # Center the title and make it bigger
        axis.title = element_text(size = 16),  # Increase axis title size
        axis.text = element_text(size = 14),  # Increase axis tick labels size
    )
# Save the plot
ggsave("braycurtis_keggpathway_noellipses.png", width = 10, height = 8, dpi = 600)
