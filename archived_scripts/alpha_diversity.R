library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

rm(list = ls())
#GENUSbac
# Read the data
data6 <- read_excel("genusbac_normalizedm.xlsx")

# Select columns 5 to n for diversity calculations (all genus columns)
genus_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Round genus data to integers ONLY for Chao1 calculation
genus_data_rounded <- genus_data %>%
  mutate(across(everything(), ~ round(.)))  # Round all genus columns to integers

# Convert rounded data to a matrix, as required by `estimateR()`
genus_data_rounded_matrix <- as.matrix(genus_data_rounded)

# Ensure rows are samples and columns are taxa
if (!is.numeric(genus_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

# Calculate Chao1 index using the rounded genus data
chao1_index <- estimateR(genus_data_rounded_matrix)["S.chao1", ]  # Extract the Chao1 row

# Calculate Inverse Simpson index (Inverse of the Simpson index) using original data
inverse_simpson_index <- 1 / diversity(genus_data, index = "simpson")

# Calculate Shannon index using original data
shannon_index <- diversity(genus_data, index = "shannon")

# Combine indices into a data frame
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  # Based on rounded genus data
  Inverse_Simpson = inverse_simpson_index,  # Based on original data
  Shannon = shannon_index  # Based on original data
)

# Melt the data for plotting
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

# Plot the indices using ggplot2
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # Align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # Create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - Genus- Bacteria",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase size of x-axis text
        axis.text.y = element_text(size = 16),  # Increase size of y-axis text
        axis.title.x = element_text(size = 18),  # Increase size of x-axis title
        axis.title.y = element_text(size = 18),  # Increase size of y-axis title
        plot.title = element_text(hjust = 0.5, size = 20),  # Increase size of plot title
        axis.line = element_line(color = "black", linewidth = 0.5),  # Thinner axis lines
        axis.ticks = element_line(color = "black", linewidth = 0.5),  # Thinner axis ticks
        strip.text = element_text(size = 18)  # Increase size of facet titles (index labels)
    )
ggsave("alpha_genusbac.png", width = 14, height = 8, dpi = 600)

rm(list = ls())
#GENUSarc
# Read the data
data6 <- read_excel("genusarc_normalizedm.xlsx")

# Select columns 5 to n for diversity calculations (all genus columns)
genus_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Round genus data to integers ONLY for Chao1 calculation
genus_data_rounded <- genus_data %>%
  mutate(across(everything(), ~ round(.)))  # Round all genus columns to integers

# Convert rounded data to a matrix, as required by `estimateR()`
genus_data_rounded_matrix <- as.matrix(genus_data_rounded)

# Ensure rows are samples and columns are taxa
if (!is.numeric(genus_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

# Calculate Chao1 index using the rounded genus data
chao1_index <- estimateR(genus_data_rounded_matrix)["S.chao1", ]  # Extract the Chao1 row

# Calculate Inverse Simpson index (Inverse of the Simpson index) using original data
inverse_simpson_index <- 1 / diversity(genus_data, index = "simpson")

# Calculate Shannon index using original data
shannon_index <- diversity(genus_data, index = "shannon")

# Combine indices into a data frame
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  # Based on rounded genus data
  Inverse_Simpson = inverse_simpson_index,  # Based on original data
  Shannon = shannon_index  # Based on original data
)

# Melt the data for plotting
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

# Plot the indices using ggplot2
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # Align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # Create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - Genus Archaea",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase size of x-axis text
        axis.text.y = element_text(size = 16),  # Increase size of y-axis text
        axis.title.x = element_text(size = 18),  # Increase size of x-axis title
        axis.title.y = element_text(size = 18),  # Increase size of y-axis title
        plot.title = element_text(hjust = 0.5, size = 20),  # Increase size of plot title
        axis.line = element_line(color = "black", linewidth = 0.5),  # Thinner axis lines
        axis.ticks = element_line(color = "black", linewidth = 0.5),  # Thinner axis ticks
        strip.text = element_text(size = 18)  # Increase size of facet titles (index labels)
    )
ggsave("alpha_genusarc.png", width = 14, height = 8, dpi = 600)

rm(list = ls())
#EC
# Read the data
data6 <- read_excel("EC_normalizedm.xlsx")

# Select columns 5 to n for diversity calculations (all EC columns)
EC_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Round EC data to integers ONLY for Chao1 calculation
EC_data_rounded <- EC_data %>%
  mutate(across(everything(), ~ round(.)))  # Round all EC columns to integers

# Convert rounded data to a matrix, as required by `estimateR()`
EC_data_rounded_matrix <- as.matrix(EC_data_rounded)

# Ensure rows are samples and columns are taxa
if (!is.numeric(EC_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

# Calculate Chao1 index using the rounded EC data
chao1_index <- estimateR(EC_data_rounded_matrix)["S.chao1", ]  # Extract the Chao1 row

# Calculate Inverse Simpson index (Inverse of the Simpson index) using original data
inverse_simpson_index <- 1 / diversity(EC_data, index = "simpson")

# Calculate Shannon index using original data
shannon_index <- diversity(EC_data, index = "shannon")

# Combine indices into a data frame
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  # Based on rounded EC data
  Inverse_Simpson = inverse_simpson_index,  # Based on original data
  Shannon = shannon_index  # Based on original data
)

# Melt the data for plotting
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

# Plot the indices using ggplot2
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # Align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # Create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - EC",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase size of x-axis text
        axis.text.y = element_text(size = 16),  # Increase size of y-axis text
        axis.title.x = element_text(size = 18),  # Increase size of x-axis title
        axis.title.y = element_text(size = 18),  # Increase size of y-axis title
        plot.title = element_text(hjust = 0.5, size = 20),  # Increase size of plot title
        axis.line = element_line(color = "black", linewidth = 0.5),  # Thinner axis lines
        axis.ticks = element_line(color = "black", linewidth = 0.5),  # Thinner axis ticks
        strip.text = element_text(size = 18)  # Increase size of facet titles (index labels)
    )
ggsave("alpha_EC.png", width = 14, height = 8, dpi = 600)

rm(list = ls())
#CAZy
# Read the data
data6 <- read_excel("CAZy_normalizedm.xlsx")

# Select columns 5 to n for diversity calculations (all CAZy columns)
CAZy_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Round CAZy data to integers ONLY for Chao1 calculation
CAZy_data_rounded <- CAZy_data %>%
  mutate(across(everything(), ~ round(.)))  # Round all CAZy columns to integers

# Convert rounded data to a matrix, as required by `estimateR()`
CAZy_data_rounded_matrix <- as.matrix(CAZy_data_rounded)

# Ensure rows are samples and columns are taxa
if (!is.numeric(CAZy_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

# Calculate Chao1 index using the rounded CAZy data
chao1_index <- estimateR(CAZy_data_rounded_matrix)["S.chao1", ]  # Extract the Chao1 row

# Calculate Inverse Simpson index (Inverse of the Simpson index) using original data
inverse_simpson_index <- 1 / diversity(CAZy_data, index = "simpson")

# Calculate Shannon index using original data
shannon_index <- diversity(CAZy_data, index = "shannon")

# Combine indices into a data frame
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  # Based on rounded CAZy data
  Inverse_Simpson = inverse_simpson_index,  # Based on original data
  Shannon = shannon_index  # Based on original data
)

# Melt the data for plotting
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

# Plot the indices using ggplot2
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # Align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # Create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - CAZy",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase size of x-axis text
        axis.text.y = element_text(size = 16),  # Increase size of y-axis text
        axis.title.x = element_text(size = 18),  # Increase size of x-axis title
        axis.title.y = element_text(size = 18),  # Increase size of y-axis title
        plot.title = element_text(hjust = 0.5, size = 20),  # Increase size of plot title
        axis.line = element_line(color = "black", linewidth = 0.5),  # Thinner axis lines
        axis.ticks = element_line(color = "black", linewidth = 0.5),  # Thinner axis ticks
        strip.text = element_text(size = 18)  # Increase size of facet titles (index labels)
    )
ggsave("alpha_CAZy.png", width = 14, height = 8, dpi = 600)

rm(list = ls())
#keggpathway
# Read the data
data6 <- read_excel("keggpathway_normalizedm.xlsx")

# Select columns 5 to n for diversity calculations (all keggpathway columns)
keggpathway_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

# Round keggpathway data to integers ONLY for Chao1 calculation
keggpathway_data_rounded <- keggpathway_data %>%
  mutate(across(everything(), ~ round(.)))  # Round all keggpathway columns to integers

# Convert rounded data to a matrix, as required by `estimateR()`
keggpathway_data_rounded_matrix <- as.matrix(keggpathway_data_rounded)

# Ensure rows are samples and columns are taxa
if (!is.numeric(keggpathway_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

# Calculate Chao1 index using the rounded keggpathway data
chao1_index <- estimateR(keggpathway_data_rounded_matrix)["S.chao1", ]  # Extract the Chao1 row

# Calculate Inverse Simpson index (Inverse of the Simpson index) using original data
inverse_simpson_index <- 1 / diversity(keggpathway_data, index = "simpson")

# Calculate Shannon index using original data
shannon_index <- diversity(keggpathway_data, index = "shannon")

# Combine indices into a data frame
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  # Based on rounded keggpathway data
  Inverse_Simpson = inverse_simpson_index,  # Based on original data
  Shannon = shannon_index  # Based on original data
)

# Melt the data for plotting
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

# Plot the indices using ggplot2
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # Align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # Create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - keggpathway",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase size of x-axis text
        axis.text.y = element_text(size = 16),  # Increase size of y-axis text
        axis.title.x = element_text(size = 18),  # Increase size of x-axis title
        axis.title.y = element_text(size = 18),  # Increase size of y-axis title
        plot.title = element_text(hjust = 0.5, size = 20),  # Increase size of plot title
        axis.line = element_line(color = "black", linewidth = 0.5),  # Thinner axis lines
        axis.ticks = element_line(color = "black", linewidth = 0.5),  # Thinner axis ticks
        strip.text = element_text(size = 18)  # Increase size of facet titles (index labels)
    )
ggsave("alpha_keggpathway.png", width = 14, height = 8, dpi = 600)
