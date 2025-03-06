# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)  # For reading Excel files
library(writexl)  # For exporting to Excel

rm(list = ls())
#PCA ALL Bacteria
# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Ensure columns representing genera are numeric and exclude non-numeric columns (Sample, Time, Treatment, Rep, Med)
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

# Perform PCA on the log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#NOELLIPSE Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(type = "norm", aes(group = Med, color = Med), level = 0.95, linetype = "dotted") +  # Dotted ellipses
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )

ggsave("genusbac_pca_all_noellipse.png", width = 10, height = 8, dpi = 300)

#ELLIPSE95 Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(type = "norm", aes(group = Med, color = Med), level = 0.95, linetype = "dotted") +  # Dotted ellipses
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )

ggsave("genusbac_pca_all_ellipse.png", width = 10, height = 8, dpi = 300)

rm(list = ls())
#P1

# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Filter the data to include only rows where Time is P1
data16 <- data16 %>%
    filter(Time == "P1")

# Ensure columns representing genera are numeric and exclude non-numeric columns
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

pca_data_log <- pca_data_log %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))

# Perform PCA on the cleaned log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#P1genusbacnoellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P1_noellipse.png", width = 10, height = 8, dpi = 300)

#P1genusellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)+
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P1_ellipse.png", width = 10, height = 8, dpi = 300)

rm(list = ls())
#P2
# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Filter the data to include only rows where Time is P1
data16 <- data16 %>%
    filter(Time == "P2")

# Ensure columns representing genera are numeric and exclude non-numeric columns
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

pca_data_log <- pca_data_log %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))

# Perform PCA on the cleaned log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#P2genusbacnoellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P2_noellipse.png", width = 10, height = 8, dpi = 300)

#P2genusbacellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)+
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P2_ellipse.png", width = 10, height = 8, dpi = 300)

rm(list = ls())
#P3

# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Filter the data to include only rows where Time is P1
data16 <- data16 %>%
    filter(Time == "P3")

# Ensure columns representing genera are numeric and exclude non-numeric columns
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

pca_data_log <- pca_data_log %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))

# Perform PCA on the cleaned log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#P3genusbacnoellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P3_noellipse.png", width = 10, height = 8, dpi = 300)

#P3genusbacellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)+
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_P3_ellipse.png", width = 10, height = 8, dpi = 300)

rm(list = ls())
#ASP

# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Filter the data to include only rows where Time is P1
data16 <- data16 %>%
    filter(Treatment == "ASP")

# Ensure columns representing genera are numeric and exclude non-numeric columns
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

pca_data_log <- pca_data_log %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))

# Perform PCA on the cleaned log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#ASPgenusbacnoellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_ASP_noellipse.png", width = 10, height = 8, dpi = 300)

#ASPgenusbacellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)+
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_ASP_ellipse.png", width = 10, height = 8, dpi = 300)

rm(list = ls())
#CNT

# Read in your data
data16 <- read_excel("genusbac_normalizedm.xlsx")

# Filter the data to include only rows where Time is P1
data16 <- data16 %>%
    filter(Treatment == "CNT")


# Ensure columns representing genera are numeric and exclude non-numeric columns
pca_data <- data16 %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric))

# Apply log transformation to the numeric data (add 1 to avoid log(0))
pca_data_log <- pca_data %>%
    mutate(across(everything(), ~log(. + 1)))

pca_data_log <- pca_data_log %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))

# Perform PCA on the cleaned log-transformed data
pca_result <- prcomp(pca_data_log, scale. = FALSE)

# Extract variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ] * 100

# Extract PCA scores and combine with metadata
pca_scores <- as.data.frame(pca_result$x)
pca_scores <- cbind(data16 %>% select(Sample, Time, Treatment, Med), pca_scores)

# Custom color and shape mapping
pca_scores$Shape <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c(16, 16, 16, 17, 17, 17))  # 16 = circle, 17 = triangle

pca_scores$Color <- factor(pca_scores$Med, levels = c("ASP_P1", "ASP_P2", "ASP_P3", "CNT_P1", "CNT_P2", "CNT_P3"),
                           labels = c("#D2E5F1", "#4393C4", "#053061", "#F5A582", "#BE172B", "#67001E"))

#CNTgenusbacnoellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    #stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_CNT_noellipse.png", width = 10, height = 8, dpi = 300)

#CNTgenusbacellipse
# Plot with custom colors and shapes, using Med for legend
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  # Plot points, keep Med for legend
    stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)+
    labs(
        title = "",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                 "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  # Custom colors
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) +  # Custom shapes (16 = circle, 17 = triangle)
    theme_minimal() +  # Minimal theme
    theme(
        legend.position = "right",
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5)
    )
ggsave("genusbac_pca_CNT_ellipse.png", width = 10, height = 8, dpi = 300)




