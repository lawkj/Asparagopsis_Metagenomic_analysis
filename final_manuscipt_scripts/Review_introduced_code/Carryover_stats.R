
## Carryover Testing for Microbiome Beta-Diversity
## - PERMANOVA (adonis2) with blocked permutations (cluster-aware)
## - PERMDISP diagnostics in P2 and P3
## - PCoA visuals for P2-P3 (Aitchison)
## Applied to all levels- bacteria (genus), archaea (genus), enzymes (CAZY, EC, KEGG Pathway)


## Packages ----
need <- c("dplyr", "readr", "vegan", "permute", "ggplot2", "tibble", "writexl")
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
  library(vegan); library(permute)
  library(ggplot2); library(writexl)
})

rm(list = ls())

set.seed(42)

## Input paths
run_dir <- "$$"

meta <- read.csv(file.path(run_dir, "aligned_metadata.csv"), stringsAsFactors = FALSE)
D_ait <- readRDS(file.path(run_dir, "D_aitchison.rds"))

## Align metadata to distance matrix
DmA <- as.matrix(D_ait)
meta <- meta[match(rownames(DmA), meta$Sample), ]
stopifnot(all(meta$Sample == rownames(DmA)))

## Prepare factors and PrevTrt
# Set Period levels
if (all(c("P1", "P2", "P3") %in% unique(meta$Period))) {
  meta$Period <- factor(meta$Period, levels = c("P1", "P2", "P3"), ordered = TRUE)
  first_period <- "P1"
} else if (all(c("1", "2", "3") %in% unique(as.character(meta$Period)))) {
  meta$Period <- factor(meta$Period, levels = c("1", "2", "3"), ordered = TRUE)
  first_period <- "1"
} else {
  stop("Period must be named as P1/P2/P3 or 1/2/3")
}

meta$Treatment <- factor(meta$Treatment)
meta$Cow <- factor(meta$Cow)

# Compute PrevTrt (lagged Treatment within Cow)
meta <- meta %>%
  arrange(Cow, Period) %>%
  group_by(Cow) %>%
  mutate(PrevTrt = lag(Treatment)) %>%
  ungroup()

meta$PrevTrt <- factor(meta$PrevTrt, levels = levels(meta$Treatment))

## Subset to Periods ≥2 with valid PrevTrt
idx_valid <- (meta$Period != first_period) & 
  !is.na(meta$PrevTrt) &
  complete.cases(meta[, c("Treatment", "PrevTrt", "Period", "Cow")])

meta_23 <- droplevels(meta[idx_valid, ])
keep_samples <- meta_23$Sample

DmA_23 <- DmA[keep_samples, keep_samples]
D_ait_23 <- as.dist(DmA_23)

message("Using ", nrow(meta_23), " samples from Periods ≥2")

## 1) PERMANOVA with blocked permutations ----
# Standard test (9,999 permutations)
ctrl <- how(nperm = 9999, blocks = meta_23$Cow)

adon_carry <- adonis2(
  D_ait_23 ~ Treatment + PrevTrt + Period,
  data = meta_23,
  permutations = ctrl,
  by = "margin"
)

carry_tab <- as.data.frame(adon_carry) %>% rownames_to_column("Term")
write_csv(carry_tab, file.path(run_dir, "carryover_permanova_P23.csv"))
message("PERMANOVA complete (9,999 permutations)")

# Strong test (99,999 permutations for more precise p-values)
set.seed(20260123)
ctrl_strong <- how(nperm = 99999, blocks = meta_23$Cow)

adon_strong <- adonis2(
  D_ait_23 ~ Treatment + PrevTrt + Period,
  data = meta_23,
  permutations = ctrl_strong,
  by = "margin"
)

strong_tab <- as.data.frame(adon_strong) %>% rownames_to_column("Term")
write_csv(strong_tab, file.path(run_dir, "carryover_permanova_P23_strong.csv"))
message("✓ PERMANOVA strong complete (99,999 permutations)")

## 2) PERMDISP diagnostics ----
permdisp_results <- list()

for (pp in c("2", "3")) {
  # Handle both "P2"/"P3" and "2"/"3" coding
  period_val <- if (first_period == "P1") paste0("P", pp) else pp
  idx <- meta_23$Period == period_val
  
  if (sum(idx) < 3) next
  
  Dpp <- as.dist(DmA_23[idx, idx])
  mpp <- droplevels(meta_23[idx, ])
  
  # Only run if we have enough groups
  if (length(unique(mpp$PrevTrt)) >= 2) {
    bd <- betadisper(Dpp, mpp$PrevTrt)
    pt <- permutest(bd, permutations = 999)
    permdisp_results[[paste0("P", pp, "_PrevTrt")]] <- 
      data.frame(pt$tab["Groups", ], Effect = paste0("PrevTrt (P", pp, ")"))
  }
  
  if (length(unique(mpp$Treatment)) >= 2) {
    bd <- betadisper(Dpp, mpp$Treatment)
    pt <- permutest(bd, permutations = 999)
    permdisp_results[[paste0("P", pp, "_Treatment")]] <- 
      data.frame(pt$tab["Groups", ], Effect = paste0("Treatment (P", pp, ")"))
  }
  
  # Cell (Treatment × PrevTrt)
  cell <- interaction(mpp$Treatment, mpp$PrevTrt, drop = TRUE)
  if (length(unique(cell)) >= 2) {
    bd <- betadisper(Dpp, cell)
    pt <- permutest(bd, permutations = 999)
    permdisp_results[[paste0("P", pp, "_Cell")]] <- 
      data.frame(pt$tab["Groups", ], Effect = paste0("Cell (P", pp, ")"))
  }
}

message("PERMDISP diagnostics complete")

## 3) PCoA visualization ----
pcoa_23 <- cmdscale(D_ait_23, k = 2, eig = TRUE)
var1 <- 100 * pcoa_23$eig[1] / sum(pcoa_23$eig[pcoa_23$eig > 0])
var2 <- 100 * pcoa_23$eig[2] / sum(pcoa_23$eig[pcoa_23$eig > 0])

df_pcoa <- data.frame(
  Sample = rownames(DmA_23),
  PCo1 = pcoa_23$points[, 1],
  PCo2 = pcoa_23$points[, 2]
) %>% left_join(meta_23, by = "Sample")

# Plot 1: By PrevTrt (color) and Treatment (shape)
p_prev <- ggplot(df_pcoa, aes(PCo1, PCo2, color = PrevTrt, shape = Treatment)) +
  geom_point(size = 2.6, alpha = 0.9) +
  facet_wrap(~ Period) +
  theme_minimal(base_size = 12) +
  labs(
    title = "PCoA (Aitchison): P2-P3 by Previous Treatment",
    x = sprintf("PCo1 (%.1f%%)", var1),
    y = sprintf("PCo2 (%.1f%%)", var2)
  )

ggsave(file.path(run_dir, "carryover_PCoA_by_PrevTrt.png"),
       p_prev, width = 8.5, height = 4.5, dpi = 300)

# Plot 2: By Cell (Treatment × PrevTrt)
df_pcoa$CellLabel <- paste0(df_pcoa$Treatment, "_after_", df_pcoa$PrevTrt)

p_cell <- ggplot(df_pcoa, aes(PCo1, PCo2, color = CellLabel)) +
  geom_point(size = 2.6, alpha = 0.9) +
  facet_wrap(~ Period) +
  theme_minimal(base_size = 12) +
  labs(
    title = "PCoA (Aitchison): P2-P3 by Cell (Treatment × PrevTrt)",
    x = sprintf("PCo1 (%.1f%%)", var1),
    y = sprintf("PCo2 (%.1f%%)", var2),
    color = "Cell"
  )

ggsave(file.path(run_dir, "carryover_PCoA_by_Cell.png"),
       p_cell, width = 8.5, height = 4.5, dpi = 300)

# Save PCoA scores
pcoa_scores <- df_pcoa %>%
  select(Sample, PCo1, PCo2, Treatment, PrevTrt, Period, Cow)
write_csv(pcoa_scores, file.path(run_dir, "carryover_PCoA_scores.csv"))

message("✓ PCoA plots saved")

## 4) Excel workbook ----
# Sample counts
counts_tmt <- table(meta_23$Period, meta_23$Treatment)
counts_prev <- table(meta_23$Period, meta_23$PrevTrt)

# Prepare PERMDISP tables
permdisp_clean <- lapply(permdisp_results, function(df) {
  names(df) <- gsub("Pr\\(>F\\)", "p_value", names(df))
  df
})

sheets <- list(
  "PERMANOVA_Standard" = carry_tab,
  "PERMANOVA_Strong" = strong_tab,
  "Counts_Treatment_by_Period" = as.data.frame.matrix(counts_tmt) %>% 
    rownames_to_column("Period"),
  "Counts_PrevTrt_by_Period" = as.data.frame.matrix(counts_prev) %>% 
    rownames_to_column("Period"),
  "PERMDISP_P2_PrevTrt" = permdisp_clean$P2_PrevTrt,
  "PERMDISP_P3_PrevTrt" = permdisp_clean$P3_PrevTrt,
  "PERMDISP_P2_Treatment" = permdisp_clean$P2_Treatment,
  "PERMDISP_P3_Treatment" = permdisp_clean$P3_Treatment,
  "PERMDISP_P2_Cell" = permdisp_clean$P2_Cell,
  "PERMDISP_P3_Cell" = permdisp_clean$P3_Cell,
  "PCoA_Scores" = pcoa_scores,
  "Metadata_P23" = meta_23
)

# Remove NULL entries
sheets <- Filter(Negate(is.null), sheets)

write_xlsx(sheets, path = file.path(run_dir, "carryover_results.xlsx"))

message("Excel workbook saved")

## 5) Log file ----
log_lines <- c(
  paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Analysis folder: ", run_dir),
  paste0("Total samples: ", nrow(meta)),
  paste0("P2-P3 samples: ", nrow(meta_23)),
  paste0("Periods: ", paste(levels(meta$Period), collapse = ", ")),
  paste0("Treatments: ", paste(levels(meta$Treatment), collapse = ", ")),
  "",
  "Analyses performed:",
  "- PERMANOVA standard: D ~ Treatment + PrevTrt + Period (9,999 perms)",
  "- PERMANOVA strong: D ~ Treatment + PrevTrt + Period (99,999 perms)",
  "  * Blocked permutations within Cow (cluster-aware)",
  "  * Marginal (Type III) tests for each term",
  "- PERMDISP: P2 and P3 by PrevTrt, Treatment, Cell",
  "- PCoA: Aitchison distance, P2-P3",
  "",
  "Note: adonis2 with blocked permutations provides cluster-aware",
  "testing equivalent to permanovaFL but more robust across data types."
)

writeLines(log_lines, file.path(run_dir, "carryover_log.txt"))

message("✓ Carryover analysis complete")
message("✓ Results saved to: ", run_dir)
message("")
message("Key output files:")
message("  - carryover_permanova_P23.csv (standard test)")
message("  - carryover_permanova_P23_strong.csv (high-precision test)")
message("  - carryover_results.xlsx (all results)")
message("  - carryover_PCoA_*.png (visualizations)")