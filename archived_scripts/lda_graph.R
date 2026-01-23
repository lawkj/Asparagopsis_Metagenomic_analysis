# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the data
df <- read_excel("ldaarchaea.xlsx")

# Ensure Treatment is categorical
df$Treatment <- as.factor(df$Treatment)

# Create a combined label for Genome in the format Genome __ [Time]
df$Genome_Time <- paste(df$Genome, "__ [", df$Time, "]", sep = "")

# Convert Genome_Time to a factor while preserving original order
df$Genome_Time <- factor(df$Genome_Time, levels = unique(df$Genome_Time))

# Define custom colors
custom_colors <- c("#6A4C9C", "#4FA770")  # Purple & Green

# Define custom colors
custom_colors <- c("#6A4C9C", "#4FA770")  # Purple & Green

# Create the plot
ggplot(df, aes(x = Genome_Time, y = `LDA SCORE (log10)`, fill = Treatment)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = custom_colors) +  # Apply custom colors
    labs(x = "", y = "LDA SCORE (log 10)", fill = "Group") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 16),
          legend.position = "top",
          legend.title = element_blank(),
        legend.text = element_text(size=16))
ggsave("all_avg_lda_archeaa.png", width = 14, height = 10, dpi = 600)