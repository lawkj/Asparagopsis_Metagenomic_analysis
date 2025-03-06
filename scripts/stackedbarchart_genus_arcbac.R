library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)  # For reading Excel files
library(writexl)



colors <- c("#92C051","#4FA770", "#278D89","#1F709A"  ,"#FBE183", "#F6CE37", "#F7B70A", "#FD9D00", "#E76624", "#C8403D", "#A13540", "#BF4861", "#E05F7E", "#DD9CA5", "#B582A2", "#A56999", 
    "#98528D", "#723B79", "#474A82", "#D3D3D3","#A9A9A9")


#Genus bacteria
data <- read_excel("genusbac_normalizedm_ra.xlsx", sheet ="final")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"), # Specify all phyla columns
    names_to = "Genus",       # Column name for the phyla
    values_to = "Abundance"    # Column name for their values
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "right",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  # Add space at the top for better appearance
    guides(fill = guide_legend(ncol = 1))  # Ensure the legend is in one column
ggsave("stackedbc_genusbac_all.png", width = 14, height = 8, dpi = 600)


#Genus bacteria average
data <- read_excel("genusbac_normalizedm_raavg.xlsx", sheet ="final")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"), # Specify all phyla columns
    names_to = "Genus",       # Column name for the phyla
    values_to = "Abundance"    # Column name for their values
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "right",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  # Add space at the top for better appearance
    guides(fill = guide_legend(ncol = 1))  # Ensure the legend is in one column
ggsave("stackedbc_genusbac_avg.png", width = 14, height = 8, dpi = 600)

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "bottom",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove maj or gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))  # Add space at the top for better appearance
    ggsave("stackedbc_genusbac_avg_axisbelow.png", width = 10, height = 14, dpi = 600)


#Genus archaea
data <- read_excel("genusarc_normalizedm_ra.xlsx")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"), # Specify all phyla columns
    names_to = "Genus",       # Column name for the phyla
    values_to = "Abundance"    # Column name for their values
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "right",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  # Add space at the top for better appearance
    guides(fill = guide_legend(ncol = 1))  # Ensure the legend is in one column
ggsave("stackedbc_genusarc_all.png", width = 14, height = 8, dpi = 600)

#Genus archaea average
data <- read_excel("genusarc_normalizedm_raavg.xlsx", sheet ="final")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"), # Specify all phyla columns
    names_to = "Genus",       # Column name for the phyla
    values_to = "Abundance"    # Column name for their values
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "right",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  # Add space at the top for better appearance
    guides(fill = guide_legend(ncol = 1))  # Ensure the legend is in one column
ggsave("stackedbc_genusarc_avg.png", width = 14, height = 8, dpi = 600)

ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  # Use custom color palette for 'Genus'
    theme_minimal(base_size = 14) +  # Minimal theme with a larger base font size
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  # Customize axis labels and legend without title
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  # Rotate x-axis labels and make them larger
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  # Rotate y-axis labels to be vertical
        axis.title.x = element_text(size = 8, face = "bold"),  # Make x-axis title bold and larger
        axis.title.y = element_text(size = 10, face = "bold"),  # Make y-axis title bold and larger
        legend.position = "bottom",  # Position the legend to the right
        legend.title = element_text(size = 8, face = "bold"),  # Bold legend title with smaller size
        legend.text = element_text(size = 7, face = "italic"),  # Italicize legend text with smaller size
        legend.key.size = unit(0.5, "cm"),  # Reduce size of the legend keys
        legend.box.spacing = unit(0.5, "cm"),  # Adjust spacing between legend items
        axis.line.x = element_line(color = "black", linewidth = 0.5),  # Black line for x-axis
        axis.line.y = element_line(color = "black", linewidth = 0.5),  # Black line for y-axis
        panel.grid.major = element_blank(),  # Remove maj or gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.background = element_blank()  # Remove background
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))  # Add space at the top for better appearance
    ggsave("stackedbc_genusarc_avg_axisbelow.png", width = 10, height = 14, dpi = 600)
