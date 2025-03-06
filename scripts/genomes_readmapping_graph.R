#Horizontal
# Load necessary libraries
library(ggplot2)
library(dplyr)

rm(list = ls())
data <- read_excel("Methanogens_sep.xlsx", sheet="all")

# Select columns from 4 to 26 (the genome columns)
data_long <- pivot_longer(data, 
                          cols = 4:29,  # Select columns from 4th to 26th
                          names_to = "Genome", 
                          values_to = "Value")

# Filter data for ASP_P3-P1 and CNT_P3-P1
data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P3vsP1", "CNT_P3vsP1"))

# Create the bar chart
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  # Separate bars for each treatment
    scale_fill_manual(values = c("#4FA770", "#6A4C9C")) +  # Manually define the colors for ASP and CNT 
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  # Customize axis labels and legend title
    theme(
        axis.text.y = element_text(size = 16, face= "italic"),  # Adjust y-axis text size
        axis.title.y = element_text(size = 9),  # Change y-axis label font size
        legend.title = element_text(size = 9, face = "bold"),  # Legend title size and style
        legend.text = element_text(size = 9),  # Legend text size
        legend.position = "right"  # Place the legend on the right
    ) +
    scale_x_continuous(limits = c(-0.0002, 0.0002))

ggsave("all_avg_p3p1_diff.png", width = 14, height = 16, dpi = 600)

rm(list = ls())
#Horizontal ASP_P3-P2 and CNT_P3-P2
data <- read_excel("Methanogens_sep.xlsx", sheet="all")

# Select columns from 4 to 26 (the genome columns)
data_long <- pivot_longer(data, 
                          cols = 4:29,  # Select columns from 4th to 26th
                          names_to = "Genome", 
                          values_to = "Value")

# Filter data for ASP_P3-P2 and CNT_P3-P2
data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P3vsP2", "CNT_P3vsP2"))

# Create the bar chart
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  # Separate bars for each treatment
    scale_fill_manual(values = c("#4FA770", "#A13540")) +  # Manually define the colors for ASP and CNT
    theme_minimal(base_size = 12) +  # Minimal theme
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  # Customize axis labels and legend title
    theme(
        axis.text.y = element_text(size = 10, face= "italic"),  # Adjust y-axis text size
        axis.title.y = element_text(size = 9),  # Change y-axis label font size
        legend.title = element_text(size = 9, face = "bold"),  # Legend title size and style
        legend.text = element_text(size = 9),  # Legend text size
        legend.position = "right"  # Place the legend on the right
    )+
    scale_x_continuous(limits = c(-0.0002, 0.0002))
ggsave("all_avg_p3p2.png", width = 14, height = 10, dpi = 600)

rm(list = ls())
#Horizontal ASP_P2-P1 and CNT_P2-P1
data <- read_excel("Methanogens_sep.xlsx", sheet="all")

# Select columns from 4 to 26 (the genome columns)
data_long <- pivot_longer(data, 
                          cols = 4:29,  # Select columns from 4th to 26th
                          names_to = "Genome", 
                          values_to = "Value")

# Filter data for ASP_P3-P2 and CNT_P3-P2
data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P2vsP1", "CNT_P2vsP1"))

# Create the bar chart
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  # Separate bars for each treatment
    scale_fill_manual(values = c("#4FA770", "#A13540")) +  # Manually define the colors for ASP and CNT
    theme_minimal(base_size = 12) +  # Minimal theme
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  # Customize axis labels and legend title
    theme(
        axis.text.y = element_text(size = 10, face= "italic"),  # Adjust y-axis text size
        axis.title.y = element_text(size = 9),  # Change y-axis label font size
        legend.title = element_text(size = 9, face = "bold"),  # Legend title size and style
        legend.text = element_text(size = 9),  # Legend text size
        legend.position = "right"  # Place the legend on the right
    )+
    scale_x_continuous(limits = c(-0.0002, 0.0002))
ggsave("all_avg_p2p1.png", width = 14, height = 10, dpi = 600)