# Load the libraries
library(readxl)
library(tidyverse)



#Option1: 5% other graph

#Step1: read in data
setwd("~/Documents/$$")
df <- read_excel("$$.xlsx", sheet = "$$")

#Step2: select the genus columns, and the "Other" genus column
genus_cols <- names(df)[which(names(df) == "Other"):ncol(df)]

#Step3: change into long format (all genera + "Other")
df_long <- df %>%
  pivot_longer(
    cols = all_of(genus_cols),
    names_to = "Bacteria",
    values_to = "Value"
  )

#Step4: Create grouping variable for plotting
df_long <- df_long %>%
  mutate(
    Enzyme_Med = paste(Enzyme, Med, sep = "_"),
    Enzyme = factor(Enzyme, levels = unique(Enzyme)),
    Bacteria = factor(Bacteria, levels = unique(Bacteria))
  )

#Step5: link Enzyme_Med to Med for x-axis labels and save image
labels_map <- df_long %>%
  distinct(Enzyme_Med, Med) %>%
  deframe()

#Step6: create stacked barchart
ggplot(df_long, aes(x = Enzyme_Med, y = Value, fill = Bacteria)) +
  geom_bar(stat = "identity") +
  scale_fill_hue(direction = -1) +
  scale_x_discrete(labels = labels_map,
                   expand = expansion(mult = c(0.02, 0.02))) +
  facet_wrap(~Enzyme, scales = "free_x", nrow = 1, strip.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +  # removed limits (so no data is dropped)
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.spacing.x = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.title = element_blank(),
    legend.text  = element_text(face = "italic", size = 10)
  ) +
  ylab("Average Proportion (Function)") +
  xlab("")

ggsave("metapont_5.png", width = 15, height = 6, dpi = 600)

#Option2: 10% other graph (reversed colours)

df <- read_excel("$$.xlsx", sheet = "$$")
genus_cols <- names(df)[which(names(df) == "Other"):ncol(df)]
df_long <- df %>%
  pivot_longer(
    cols = all_of(genus_cols),
    names_to = "Bacteria",
    values_to = "Value"
  )
df_long <- df_long %>%
  mutate(
    Enzyme_Med = paste(Enzyme, Med, sep = "_"),
    Enzyme = factor(Enzyme, levels = unique(Enzyme)),
    Bacteria = factor(Bacteria, levels = unique(Bacteria))
  )
labels_map <- df_long %>%
  distinct(Enzyme_Med, Med) %>%
  deframe()

#Reverse the colour palette
colors <- rev(c(
  "#92C051","#4FA770", "#278D89","#1F709A",
  "#FBE183", "#F7B70A", "#E76624", "#A13540",
  "#E05F7E", "#DD9CA5", "#723B79", "#474A82",
  "#D3D3D3","#A9A9A9"
))

ggplot(df_long, aes(x = Enzyme_Med, y = Value, fill = Bacteria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +   # ← uses reversed color order
  scale_x_discrete(labels = labels_map,
                   expand = expansion(mult = c(0.02, 0.02))) +
  facet_wrap(~Enzyme, scales = "free_x", nrow = 1, strip.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.spacing.x = unit(0.5, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.title = element_blank(),
    legend.text  = element_text(face = "italic", size = 10)
  ) +
  ylab("Average Proportion (Function)") +
  xlab("")
ggsave("metapont_10.png", width = 15, height = 6, dpi = 600)
