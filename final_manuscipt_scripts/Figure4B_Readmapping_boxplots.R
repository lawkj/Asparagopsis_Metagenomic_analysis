# Load the libraries
library(readxl)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(gridExtra)  

#Step1: read in data
data <- read.xlsx("$$.xlsx")  

#Step2: Define a list of genome names
genomes <- unique(data$Genome)

#Step3: Create an empty list to store al the individual plots from the below loop
plot_list <- list()

#Step4: Loop through each genome and generate a box plot per genome
for(genome in genomes) {
  plot <- ggplot(data %>% filter(Genome == genome), aes(x = Med, y = MP_TR, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Semi-transparent boxes, dont display outliers
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6) +
    theme_minimal() +
    labs(
        title = genome,  # Only display the genome name without "Genome: "
        x = "",
        y = "MP:TR"
    ) +
    theme(
        legend.position = "none",  
        axis.text.x = element_text(size = 16),  
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20, face = "italic"),  
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5)
    )
  
  # Save each individual plot
  ggsave(filename = paste0(genome, "_boxplot.png"), plot = plot, width = 14, height = 10, dpi = 600)
  
  #Step4: Add each plot to the list for later use
  plot_list[[genome]] <- plot
}

#Step5: Combine all the individual plots into one panel 
combined_plot <- grid.arrange(grobs = plot_list, ncol = 2)  # 5 by 2

#Step6: Save the plot
ggsave("combined_panel_plot.png", combined_plot, width = 14, height = 16, dpi = 600)


