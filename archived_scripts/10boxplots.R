library(readxl)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(gridExtra)  # For arranging multiple plots in a grid


# different y axis
# Load your data
data <- read.xlsx("boxplots_bigdiff.xlsx")  # Replace with correct path

# Define a vector of genome names (the 10 unique ones from your dataset)
genomes <- unique(data$Genome)

# Create an empty list to store individual plots
plot_list <- list()

# Loop through each genome and generate a box plot
for(genome in genomes) {
  plot <- ggplot(data %>% filter(Genome == genome), aes(x = Med, y = MP_TR, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Semi-transparent boxes, hide outliers
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6) +
    theme_minimal() +
    labs(
        title = genome,  # Only display the genome name without "Genome: "
        x = "",
        y = "MP:TR"
    ) +
    theme(
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(size = 16),  # Improve readability of x-axis
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20, face = "italic"),  # Make title italic
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5)
    )
  
  # Save each individual plot
  ggsave(filename = paste0(genome, "_boxplot.png"), plot = plot, width = 14, height = 10, dpi = 600)
  
  # Add each plot to the list for later use
  plot_list[[genome]] <- plot
}

# Combine all the individual plots into one panel (grid)
combined_plot <- grid.arrange(grobs = plot_list, ncol = 2)  # You can adjust ncol (number of columns)

# Save the combined panel plot
ggsave("combined_panel_plot.png", combined_plot, width = 14, height = 16, dpi = 600)


