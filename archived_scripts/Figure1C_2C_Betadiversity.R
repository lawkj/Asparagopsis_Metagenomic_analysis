# Load the libraries
library(vegan)
library(ggplot2)
library(readxl)


#Step1: Read in the data
data6 <- read_xlsx("$$.xlsx")

#Step2: select the taxa columns, exluding the metadata columns (from column 5 onwards)
species_columns <- data6[, 5:ncol(data6)] 

#Step3: Log Transform the taxa columns (add 1 to avoid log(0))
species_log <- species_columns + 1  # Add 1 to avoid log(0)
species_log <- log(species_log)

#Step4: Calculate Bray-Curtis Dissimilarity 
bray_curtis <- vegdist(species_log, method = "bray")

#Step5: Pun PCoA using the bray-curtis data
pcoa <- cmdscale(bray_curtis, k = 2, eig = TRUE)  # k=2 for two principal coordinates, eig = TRUE to return eigenvalues

#Step6: Create a data frame for plotting with the PCoA results and other metadata
pcoa_df <- data.frame(PCoA1 = pcoa$points[, 1], PCoA2 = pcoa$points[, 2], 
                      Med = data6$Med, Treatment = data6$Treatment, Time = data6$Time, Sample = data6$Sample)

#Step7: Calculate the percentage variance explained for the first two principal coordinates based on the eigenvalues (distances)
eigenvalues <- pcoa$eig
explained_variance <- eigenvalues / sum(eigenvalues) * 100

#Step8: Plot the PCoA results, Med= Time and treatment e.g. ASP_P3
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +  
    labs(
        title = "PCoA based on Bray-Curtis Dissimilarity",
        x = paste0("PC1 (", round(explained_variance[1], 1), "% variance)"),
        y = paste0("PC2 (", round(explained_variance[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c("ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061", 
                                  "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E")) +  
    scale_shape_manual(values = c("ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16, 
                                  "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17)) + 
    theme_minimal() + 
    theme(
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 20),  
        axis.title = element_text(size = 16),  
        axis.text = element_text(size = 14),  
    )
ggsave("$$.png", width = 10, height = 8, dpi = 600)
