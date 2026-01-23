# Load the libraries

library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

rm(list = ls())


#Generate indices and creat boxplots

#Step1: read in data
data6 <- read_excel("$$.xlsx")

#Step2: select the taxa columns, exluding the metadata columns (from column 5 onwards)
genus_data <- data6[, 5:ncol(data6)]  # Select all columns from the 5th to the last

#Step3: Round genus data to integers (ONLY for Chao1 calculation), and convert to matrix so it can be input to estimateR()
genus_data_rounded <- genus_data %>%
  mutate(across(everything(), ~ round(.)))  
genus_data_rounded_matrix <- as.matrix(genus_data_rounded)

#Step4: (troubleshooting errors) Ensure rows are samples and columns are taxa
if (!is.numeric(genus_data_rounded_matrix)) {
  stop("The data contains non-numeric values. Please ensure only numeric values are present.")
}

#Step5: Calculate Chao1 index using the rounded genus data, and extract the Chao1 row
chao1_index <- estimateR(genus_data_rounded_matrix)["S.chao1", ]

#Step6: Calculate Inverse Simpson index using original non rounded data
inverse_simpson_index <- 1 / diversity(genus_data, index = "simpson")

#Step7: Calculate Shannon index using original non rounded data
shannon_index <- diversity(genus_data, index = "shannon")

#Step8: Combine indices into a data frame, and transform to long format for plotting
indices_df <- data.frame(
  Sample = data6$Sample,
  Med = data6$Med,
  Chao1 = chao1_index,  
  Inverse_Simpson = inverse_simpson_index,  
  Shannon = shannon_index  
)
indices_df_long <- indices_df %>%
  gather(key = "Index", value = "Value", Chao1, Inverse_Simpson, Shannon)

#Step9: Plot the indices as a boxplot, and save the plot
ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # slightly less transparent boxes
    geom_point(aes(color = Med), size = 1.5, alpha = 0.6, position = position_dodge(0)) +  # align points along the x-axis
    facet_wrap(~Index, scales = "free_y") +  # create separate panels for each index
    theme_minimal() +
    labs(
        title = "Boxplots of Diversity Indices - Genus- Bacteria",
        x = "",
        y = "Index Value"
    ) +
    theme(
        legend.position = "none",  
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16), 
        axis.text.y = element_text(size = 16),  
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18),  
        plot.title = element_text(hjust = 0.5, size = 20),  
        axis.line = element_line(color = "black", linewidth = 0.5),  
        axis.ticks = element_line(color = "black", linewidth = 0.5),  
        strip.text = element_text(size = 18)  # index labels
    )
ggsave("alpha_genusbac.png", width = 14, height = 8, dpi = 600)


#Pairwise Wilcoxon Testing

#Step1:prep data
alpha_long <- indices_df %>%
  select(Med, Chao1, Inverse_Simpson, Shannon) %>%
  pivot_longer(cols = -Med, names_to = "Index", values_to = "Value")

#Step2: Perform Pairwise Wilcoxon Tests with BH Correction
pairwise_results <- alpha_long %>%
  group_by(Index) %>%
  group_map(~ {
    index_name <- .y$Index[[1]]  #extracts current group label
    
    test <- pairwise.wilcox.test(
      x = .x$Value,
      g = .x$Med,
      p.adjust.method = "BH"
    )
    
    res <- as.data.frame(as.table(test$p.value))
    res <- res[!is.na(res$Freq), ]
    colnames(res) <- c("Group1", "Group2", "p_value")
    res$Index <- index_name  # Add index label to results
    res
  }) %>%
  bind_rows()

#Step3: Output the data as excel
write.xlsx(pairwise_results, file = "$$.xlsx", rowNames = FALSE)
