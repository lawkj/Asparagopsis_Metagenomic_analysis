# Load the  libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(RColorBrewer)

setwd()


#Step1: Read in data 
data <- read_excel("$$.xlsx", sheet = "$$")

#Step2: Convert data to long format 
data_long <- data %>%
    pivot_longer(cols = starts_with("ASP") | starts_with("CNT"), 
                 names_to = "Sample", values_to = "Value")

#Step3: Normalise values per EC number
data_long <- data_long %>%
    group_by(EC) %>%
    mutate(Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value)))

#Step4: Convert EC to a factor (keeps original order)
data_long$EC <- factor(data_long$EC, levels = unique(data$EC))


#Step5: Plot the heat map
ggplot(data_long, aes(x = EC, y = Sample, fill = Normalized_Value)) +
    geom_tile(color = "white") +  #thin white borders
    scale_fill_gradientn(colors = brewer.pal(8, "RdBu")) +  
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black"),  
        axis.text.y = element_text(face = "bold", color = "black"),  
        axis.title.x = element_text(color = "black"),  
        axis.title.y = element_text(color = "black"),  
        plot.title = element_text(color = "black"),  
        text = element_text(family = "Arial", color = "black"),  
        legend.position = "none"  
    ) +
    labs(title = "", x = "", y = "", fill = "Relative Value")

ggsave("$$.png", width = 8, height = 4, dpi = 600)

