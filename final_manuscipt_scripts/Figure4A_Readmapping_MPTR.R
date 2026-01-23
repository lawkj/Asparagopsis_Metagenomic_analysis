#Load the libraries
library(ggplot2)
library(dplyr)
library(readxl)

#Step1: read in data 
data <- read_excel("$$.xlsx", sheet="$$")

#Step2: Select the genome columns (from 4 to 26)
data_long <- pivot_longer(data, 
                          cols = 4:29,  
                          names_to = "Genome", 
                          values_to = "Value")


#Option1: Filter data for ASP_P3-P1 and CNT_P3-P1
data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P3vsP1", "CNT_P3vsP1"))

#create plot
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  # Separate bars for each treatment
    scale_fill_manual(values = c("#4FA770", "#6A4C9C")) +  #manual colours
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  
    theme(
        axis.text.y = element_text(size = 16, face= "italic"),  
        axis.title.y = element_text(size = 9),  
        legend.title = element_text(size = 9, face = "bold"),  
        legend.text = element_text(size = 9), 
        legend.position = "right" 
    ) +
    scale_x_continuous(limits = c(-0.0002, 0.0002))

ggsave("$$_p3p1.png", width = 14, height = 16, dpi = 600)


#Option2: ASP_P3-P2 and CNT_P3-P2

data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P3vsP2", "CNT_P3vsP2"))
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  
    scale_fill_manual(values = c("#4FA770", "#A13540")) +  
    theme_minimal(base_size = 12) +  # Minimal theme
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  
    theme(
        axis.text.y = element_text(size = 10, face= "italic"),  
        axis.title.y = element_text(size = 9),  
        legend.title = element_text(size = 9, face = "bold"),  
        legend.text = element_text(size = 9),  
        legend.position = "right"  
    )+
    scale_x_continuous(limits = c(-0.0002, 0.0002))
ggsave("all_avg_p3p2.png", width = 14, height = 10, dpi = 600)


#Option3: ASP_P2-P1 and CNT_P2-P1
data_filtered <- data_long %>%
    filter(Sample %in% c("ASP_P2vsP1", "CNT_P2vsP1"))
ggplot(data_filtered, aes(x = Value, y = Genome, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +  
    scale_fill_manual(values = c("#4FA770", "#A13540")) +  
    theme_minimal(base_size = 12) + 
    labs(
        x = "Average ratio of MP:TR",
        y = "",
        fill = ""
    ) +  
    theme(
        axis.text.y = element_text(size = 10, face= "italic"),  
        axis.title.y = element_text(size = 9),  
        legend.title = element_text(size = 9, face = "bold"),  
        legend.text = element_text(size = 9),  
        legend.position = "right"  
    )+
    scale_x_continuous(limits = c(-0.0002, 0.0002))
ggsave("$$_p2p1.png", width = 14, height = 10, dpi = 600)