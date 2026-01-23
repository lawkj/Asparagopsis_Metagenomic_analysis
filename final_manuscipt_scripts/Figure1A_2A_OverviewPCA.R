# Load the libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

rm(list = ls())

#Step1: Define the PCA function: Run PCA and return scores with metadata
run_pca <- function(data, filter_var = NULL, filter_value = NULL) {
  df <- data
  if (!is.null(filter_var) && !is.null(filter_value)) {
    df <- df %>% filter(.data[[filter_var]] == filter_value)
  }
  
  pca_data <- df %>%
    select(-Sample, -Time, -Treatment, -Med) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(across(everything(), ~log(. + 1))) %>%
    select(where(~ all(!is.na(.)) && var(.) > 0))
  
  pca_result <- prcomp(pca_data, scale. = FALSE)
  explained_variance <- summary(pca_result)$importance[2, ] * 100
  
  scores <- as.data.frame(pca_result$x)
  scores <- cbind(df %>% select(Sample, Time, Treatment, Med), scores)
  
  list(scores = scores, explained = explained_variance)
}

#Step2: Define the ggplot function: Make PCA plot
make_pca_plot <- function(pca_scores, explained, add_ellipse = FALSE, save_name = "pca_plot.png") {
  base_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Med, shape = Med), size = 3, alpha = 0.7) +
    labs(
      x = paste0("PC1 (", round(explained[1], 1), "% variance)"),
      y = paste0("PC2 (", round(explained[2], 1), "% variance)")
    ) +
    scale_color_manual(values = c(
      "ASP_P1" = "#D2E5F1", "ASP_P2" = "#4393C4", "ASP_P3" = "#053061",
      "CNT_P1" = "#F5A582", "CNT_P2" = "#BE172B", "CNT_P3" = "#67001E"
    )) +
    scale_shape_manual(values = c(
      "ASP_P1" = 16, "ASP_P2" = 16, "ASP_P3" = 16,
      "CNT_P1" = 17, "CNT_P2" = 17, "CNT_P3" = 17
    )) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  if (add_ellipse) {
    base_plot <- base_plot +
      stat_ellipse(aes(group = Med, color = Med), linetype = "dashed", linewidth = 0.3, level = 0.68)
  }
  
  ggsave(save_name, plot = base_plot, width = 10, height = 8, dpi = 300)
}

#Step3: Read in data
data16 <- read_excel("$$.xlsx")

#Step4: Define comparisons (timepoints & treatments)
comparisons <- list(
  list(name = "ALL", filter_var = NULL, filter_value = NULL),
  list(name = "P1", filter_var = "Time", filter_value = "P1"),
  list(name = "P2", filter_var = "Time", filter_value = "P2"),
  list(name = "P3", filter_var = "Time", filter_value = "P3"),
  list(name = "ASP", filter_var = "Treatment", filter_value = "ASP"),
  list(name = "CNT", filter_var = "Treatment", filter_value = "CNT")
)

# Loop through comparisons
for (cmp in comparisons) {
  res <- run_pca(data16, cmp$filter_var, cmp$filter_value)
  make_pca_plot(res$scores, res$explained,
                add_ellipse = FALSE,
                save_name = paste0("genusbac_pca_", cmp$name, "_noellipse.png"))
  make_pca_plot(res$scores, res$explained,
                add_ellipse = TRUE,
                save_name = paste0("genusbac_pca_", cmp$name, "_ellipse.png"))
}
