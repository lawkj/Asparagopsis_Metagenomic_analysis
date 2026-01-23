# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(RColorBrewer)


#p1
# Read the Excel file (modify the file path and sheet name accordingly)
data <- read_excel("EC_ofinterest_averagedreps.xlsx", sheet = "p1")

# Convert data to long format for ggplot
data_long <- data %>%
    pivot_longer(cols = starts_with("ASP") | starts_with("CNT"), 
                 names_to = "Sample", values_to = "Value")

# Normalize values per EC
data_long <- data_long %>%
    group_by(EC) %>%
    mutate(Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value)))

# Convert EC to a factor to keep original order
data_long$EC <- factor(data_long$EC, levels = unique(data$EC))


# nolegened
ggplot(data_long, aes(x = EC, y = Sample, fill = Normalized_Value)) +
    geom_tile(color = "white") +  # Add thin white borders
    scale_fill_gradientn(colors = brewer.pal(8, "RdBu")) +  # Use the 8-color RdBu palette
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),  # Make x-axis text black
        axis.text.y = element_text(face = "bold", color = "black"),  # Make y-axis text bold and black
        axis.title.x = element_text(color = "black"),  # Make x-axis title black
        axis.title.y = element_text(color = "black"),  # Make y-axis title black
        plot.title = element_text(color = "black"),  # Make title text black
        text = element_text(family = "Arial", color = "black"),  # Set all text to Arial and black
        legend.position = "none"  # Remove the legend
    ) +
    labs(title = "", x = "", y = "", fill = "Relative Value")

ggsave("p1_ec.png", width = 8, height = 4, dpi = 600)

#p2
# Read the Excel file (modify the file path and sheet name accordingly)
data <- read_excel("EC_ofinterest_averagedreps.xlsx", sheet = "p2")

# Convert data to long format for ggplot
data_long <- data %>%
    pivot_longer(cols = starts_with("ASP") | starts_with("CNT"), 
                 names_to = "Sample", values_to = "Value")

# Normalize values per EC
data_long <- data_long %>%
    group_by(EC) %>%
    mutate(Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value)))

# Convert EC to a factor to keep original order
data_long$EC <- factor(data_long$EC, levels = unique(data$EC))


# nolegened
ggplot(data_long, aes(x = EC, y = Sample, fill = Normalized_Value)) +
    geom_tile(color = "white") +  # Add thin white borders
    scale_fill_gradientn(colors = brewer.pal(8, "RdBu")) +  # Use the 8-color RdBu palette
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),  # Make x-axis text black
        axis.text.y = element_text(face = "bold", color = "black"),  # Make y-axis text bold and black
        axis.title.x = element_text(color = "black"),  # Make x-axis title black
        axis.title.y = element_text(color = "black"),  # Make y-axis title black
        plot.title = element_text(color = "black"),  # Make title text black
        text = element_text(family = "Arial", color = "black"),  # Set all text to Arial and black
        legend.position = "none"  # Remove the legend
    ) +
    labs(title = "", x = "", y = "", fill = "Relative Value")

ggsave("p2_ec.png", width = 6, height = 4, dpi = 600)

#p3
# Read the Excel file (modify the file path and sheet name accordingly)
data <- read_excel("EC_ofinterest_averagedreps.xlsx", sheet = "p3")

# Convert data to long format for ggplot
data_long <- data %>%
    pivot_longer(cols = starts_with("ASP") | starts_with("CNT"), 
                 names_to = "Sample", values_to = "Value")

# Normalize values per EC
data_long <- data_long %>%
    group_by(EC) %>%
    mutate(Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value)))

# Convert EC to a factor to keep original order
data_long$EC <- factor(data_long$EC, levels = unique(data$EC))


# nolegened
ggplot(data_long, aes(x = EC, y = Sample, fill = Normalized_Value)) +
    geom_tile(color = "white") +  # Add thin white borders
    scale_fill_gradientn(colors = brewer.pal(8, "RdBu")) +  # Use the 8-color RdBu palette
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),  # Make x-axis text black
        axis.text.y = element_text(face = "bold", color = "black"),  # Make y-axis text bold and black
        axis.title.x = element_text(color = "black"),  # Make x-axis title black
        axis.title.y = element_text(color = "black"),  # Make y-axis title black
        plot.title = element_text(color = "black"),  # Make title text black
        text = element_text(family = "Arial", color = "black"),  # Set all text to Arial and black
        legend.position = "none"  # Remove the legend
    ) +
    labs(title = "", x = "", y = "", fill = "Relative Value")
ggsave("p3_ec.png", width = 14, height = 4, dpi = 600)
