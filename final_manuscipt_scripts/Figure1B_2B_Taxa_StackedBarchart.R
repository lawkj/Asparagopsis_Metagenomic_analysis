# Load the libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)  
library(writexl)


#Step1: define custom colours
colors <- c("#92C051","#4FA770", "#278D89","#1F709A"  ,"#FBE183", "#F6CE37", "#F7B70A", "#FD9D00", "#E76624", "#C8403D", "#A13540", "#BF4861", "#E05F7E", "#DD9CA5", "#B582A2", "#A56999", 
    "#98528D", "#723B79", "#474A82", "#D3D3D3","#A9A9A9")


#Step2: read in data, convert to long format and set factors (this first section was used to plot every sample)
data <- read_excel("$$.xlsx", sheet ="$$")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"),
    names_to = "Genus",       
    values_to = "Abundance"    
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

#Step3: plot the stacked barchart, save the plot
ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  
    scale_fill_manual(values = colors) +  
    theme_minimal(base_size = 14) +  
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  
        axis.title.x = element_text(size = 8, face = "bold"),  
        axis.title.y = element_text(size = 10, face = "bold"),  
        legend.position = "right",  
        legend.title = element_text(size = 8, face = "bold"),  
        legend.text = element_text(size = 7, face = "italic"), 
        legend.key.size = unit(0.5, "cm"),  
        legend.box.spacing = unit(0.5, "cm"),  
        axis.line.x = element_line(color = "black", linewidth = 0.5),  
        axis.line.y = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank()  
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  # this adds space to the top of the graph (looks nicer)
    guides(fill = guide_legend(ncol = 1)) 
ggsave("stackedbc_$$.png", width = 14, height = 8, dpi = 600)


#Step4: repeat process for averaged data
data <- read_excel("$$avg.xlsx", sheet ="$$")
melted_data <- pivot_longer(
    data,
    cols = starts_with("g__"), 
    names_to = "Genus",       
    values_to = "Abundance"   
)
melted_data$Genus <- factor(melted_data$Genus, levels = unique(melted_data$Genus))
melted_data$Sample <- factor(melted_data$Sample, levels = unique(melted_data$Sample))

#Axis to the side
ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
    scale_fill_manual(values = colors) +  
    theme_minimal(base_size = 14) +  
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"), 
        axis.title.x = element_text(size = 8, face = "bold"), 
        axis.title.y = element_text(size = 10, face = "bold"), 
        legend.position = "right", # legend to the right of the graph
        legend.title = element_text(size = 8, face = "bold"),  
        legend.text = element_text(size = 7, face = "italic"),  
        legend.key.size = unit(0.5, "cm"),  
        legend.box.spacing = unit(0.5, "cm"),  
        axis.line.x = element_line(color = "black", linewidth = 0.5),  
        axis.line.y = element_line(color = "black", linewidth = 0.5),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),  
        panel.background = element_blank()  
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +  
    guides(fill = guide_legend(ncol = 1))  
ggsave("stackedbc_$$.png", width = 14, height = 8, dpi = 600)

#Axis below the graph
ggplot(melted_data, aes(x = Long, y = Abundance, fill = Genus)) +
    geom_bar(stat = "identity", position = "stack") +  
    scale_fill_manual(values = colors) +  
    theme_minimal(base_size = 14) +  
    labs(
        x = "",
        y = "Relative Abundance",
        fill = "Genus"
    ) +  
    theme(
        axis.text.x = element_text(angle = 90, size = 10, face = "bold"),  
        axis.text.y = element_text(size = 10, vjust = 0.5, face = "bold"),  
        axis.title.x = element_text(size = 8, face = "bold"), 
        axis.title.y = element_text(size = 10, face = "bold"),  
        legend.position = "bottom",  # legend under the graph
        legend.title = element_text(size = 8, face = "bold"),  
        legend.text = element_text(size = 7, face = "italic"),  
        legend.key.size = unit(0.5, "cm"),  
        legend.box.spacing = unit(0.5, "cm"),  
        axis.line.x = element_line(color = "black", linewidth = 0.5),  
        axis.line.y = element_line(color = "black", linewidth = 0.5),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank()  
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))  
    ggsave("stackedbc_$$_axisbelow.png", width = 10, height = 14, dpi = 600)

