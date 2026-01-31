## Microbiome beta-diversity analysis with pairwise contrasts
## Latin square design: 2 treatments (e.g., ASP/CNT), 3 periods
## - Reads: genusbac_normalized_nometa.xlsx (Sample in col 1; genera in others)
##          metadata.xlsx (columns: Sample, Treatment, Period, Cow)
## - Distances: Aitchison (CLR/Euclidean; primary), Bray–Curtis (sensitivity)
## - PERMANOVA: permutations restricted within Cow (global)
## - Pairwise PERMANOVA:
##     * Treatment within each Period (unrestricted permutations)
##     * Period pairs within each Treatment (unrestricted permutations)
## - PERMDISP, PCoA with within-cow lines
## - GLMM-MiRKAT sensitivity (cluster-aware, binary Treatment)
## - Optional LMEs on PCoA axes, dbRDA (Condition(Cow))
## - Saves all outputs to outputs/analysis_YYYYMMDD_HHMMSS/ (date)

## Inputs and paramaters
# Filtering mode: "AND" (prevalence >= 10% AND total reads >= 100) or "OR" (either criterion)
filter_mode      <- "AND"    # choose "AND" or "OR"
min_prev_prop    <- 0.10     # prevalence threshold (proportion of samples)
min_total_reads  <- 100      # total reads threshold across all samples

# Input files
counts_file <- "$$.xlsx"   # Sample in column 1
meta_file   <- "metadata.xlsx"                     # Formatted as Sample, Treatment, Period, Cow

# Random seeds
set.seed(42)               # for permutations and plotting reproducibility
glmm_seed <- 42            # for GLMMMiRKAT permutation reproducibility


## 1) Packages
pkgs <- c(
  "readxl","dplyr","tibble","ggplot2","stringr",
  "vegan","permute",
  "lme4","lmerTest",
  "MiRKAT",
  "writexl"  # Excel export
  # Optional for principled zero replacement + CLR:
  # "zCompositions","compositions"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))


## 2) Output directory

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("outputs_bac", paste0("analysis_", ts))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv2 <- function(x, path) utils::write.csv(x, path, row.names = FALSE)

## 3) Read data
# Counts (Sample in col 1; genera in remaining columns)
counts_raw <- readxl::read_excel(counts_file, sheet = 1)
stopifnot("Sample" %in% colnames(counts_raw))
counts_df <- counts_raw %>%
  mutate(Sample = as.character(Sample)) %>%
  column_to_rownames("Sample")
# Ensure numeric
counts_df[] <- lapply(counts_df, function(x) as.numeric(as.character(x)))
counts <- as.data.frame(counts_df)

# Metadata
meta <- readxl::read_excel(meta_file, sheet = 1)
needed <- c("Sample","Treatment","Period","Cow")
if (!all(needed %in% names(meta))) {
  stop("metadata.xlsx must contain columns: Sample, Treatment, Period, Cow")
}
meta <- meta %>%
  mutate(
    Sample    = as.character(Sample),
    Treatment = factor(Treatment),     # e.g., ASP/CNT
    Period    = factor(Period),        # 1–3 (or P1–P3)
    Cow       = factor(Cow)
  )

## 4) Align samples
common_ids <- intersect(rownames(counts), meta$Sample)
if (length(common_ids) < 3) stop("Too few overlapping samples between counts and metadata.")
counts <- counts[common_ids, , drop = FALSE]
meta   <- meta[match(common_ids, meta$Sample), , drop = FALSE]
stopifnot(all(rownames(counts) == meta$Sample))

## Save aligned inputs
write_csv2(meta, file.path(out_dir, "aligned_metadata.csv"))
write_csv2(cbind(Sample = rownames(counts), counts), file.path(out_dir, "aligned_counts.csv"))


## 5) Feature filtering (prevalence and/or total reads)
prev_counts <- colSums(counts > 0)       # #samples with genus > 0
total_reads <- colSums(counts)           # total reads per genus
n_samp      <- nrow(counts)
min_prev    <- ceiling(min_prev_prop * n_samp)

if (identical(toupper(filter_mode), "AND")) {
  keep <- (prev_counts >= min_prev) & (total_reads >= min_total_reads)
} else if (identical(toupper(filter_mode), "OR")) {
  keep <- (prev_counts >= min_prev) | (total_reads >= min_total_reads)
} else {
  stop("filter_mode must be 'AND' or 'OR'")
}
counts_f <- counts[, keep, drop = FALSE]

# Save detailed summary of filtering
filter_summary <- data.frame(
  Genus          = colnames(counts),
  Prevalence     = prev_counts,
  PrevalenceProp = prev_counts / n_samp,
  TotalReads     = total_reads,
  Keep           = colnames(counts) %in% colnames(counts_f)
)
utils::write.csv(filter_summary, file.path(out_dir, "feature_filtering_summary.csv"), row.names = FALSE)

message("Kept ", ncol(counts_f), " genera out of ", ncol(counts),
        " using ", toupper(filter_mode), " logic (≥", min_prev_prop*100, "% prevalence and/or ≥",
        min_total_reads, " total reads).")

## 6) Distances: Aitchison (CLR/Euc) & Bray–Curtis
## Aitchison (primary; compositional): Euclidean on CLR
# (A) Simple pseudocount approach (fast and common)
pc   <- 0.5
prop <- sweep(counts_f + pc, 1, rowSums(counts_f + pc), "/")
gm   <- exp(rowMeans(log(prop)))
clr  <- log(prop / gm)
D_ait <- dist(clr, method = "euclidean")

# (B) Principled zero replacement (uncomment to use CZM, then CLR)
# library(zCompositions); library(compositions)
# imp  <- zCompositions::cmultRepl(counts_f, label = 0, method = "CZM", output = "prop")
# clr2 <- compositions::clr(imp)
# D_ait <- dist(clr2, method = "euclidean")

## Bray–Curtis (sensitivity)- didnt use results
prop_bc <- sweep(counts_f, 1, rowSums(counts_f), "/")
prop_bc[is.na(prop_bc)] <- 0
D_bray  <- vegan::vegdist(prop_bc, method = "bray")

## Save distances
saveRDS(D_ait,  file.path(out_dir, "D_aitchison.rds"))
saveRDS(D_bray, file.path(out_dir, "D_bray.rds"))


## 7) PERMANOVA with restricted permutations within Cow (global)

ctrl <- permute::how(nperm = 999, blocks = meta$Cow)  # lock permutations within Cow

adon_ait <- vegan::adonis2(D_ait ~ Treatment + Period,
                           data = meta,
                           permutations = ctrl,
                           by = "margin")
adon_bray <- vegan::adonis2(D_bray ~ Treatment + Period,
                            data = meta,
                            permutations = ctrl,
                            by = "margin")

cat("\n=== PERMANOVA (Aitchison) ===\n"); print(adon_ait)
cat("\n=== PERMANOVA (Bray–Curtis) ===\n"); print(adon_bray)


## 7b) Pairwise PERMANOVA (post hoc)

## My study design
## - Within-Period Treatment comparisons: each cow contributes only one sample -> no repeated measure; do NOT block permutations.
## - Within-Treatment Period comparisons: each cow appears once per treatment -> do NOT block permutations.
## - Report F, R2 and raw/BH-adjusted p-values for both Aitchison and Bray–Curtis.

subset_dist <- function(D, idx) as.dist(as.matrix(D)[idx, idx, drop = FALSE])

run_within_period_treatment <- function(D, meta, distance_name, nperm = 999) {
  per_levels <- levels(meta$Period)
  out <- lapply(per_levels, function(pp) {
    idx <- which(meta$Period == pp)
    if (length(idx) < 3) return(NULL)  # too few samples
    if (length(unique(meta$Treatment[idx])) < 2) return(NULL)  # need both treatments present
    
    Dsub   <- subset_dist(D, idx)
    metas  <- droplevels(meta[idx, ])
    ctrl   <- permute::how(nperm = nperm)  # no blocking here (see rationale)
    ad     <- vegan::adonis2(Dsub ~ Treatment, data = metas, permutations = ctrl)
    
    data.frame(
      Distance = distance_name,
      Scope    = "Treatment within Period",
      Period   = as.character(pp),
      Contrast = "ASP vs CNT",
      Df       = ad$Df[1],
      F        = ad$F[1],
      R2       = ad$R2[1],
      p        = ad$`Pr(>F)`[1],
      N        = nrow(metas),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(out)
  if (NROW(out)) out$p_BH <- p.adjust(out$p, method = "BH")
  out
}

run_within_treatment_period_pairs <- function(D, meta, distance_name, nperm = 999) {
  tmt_levels <- levels(meta$Treatment)
  out <- lapply(tmt_levels, function(tt) {
    idx_t <- which(meta$Treatment == tt)
    if (length(idx_t) < 3) return(NULL)
    metas_t <- droplevels(meta[idx_t, ])
    D_t     <- subset_dist(D, idx_t)
    
    # Pairwise Period comparisons inside a Treatment
    per_levels <- levels(metas_t$Period)
    if (length(per_levels) < 2) return(NULL)
    pairs <- combn(per_levels, 2, simplify = FALSE)
    
    per_res <- lapply(pairs, function(pr) {
      idx_p <- which(metas_t$Period %in% pr)
      if (length(unique(metas_t$Period[idx_p])) < 2 || length(idx_p) < 3) return(NULL)
      
      D_tp     <- subset_dist(D_t, idx_p)
      metas_tp <- droplevels(metas_t[idx_p, ])
      ctrl     <- permute::how(nperm = nperm)  # no blocking (see rationale)
      ad       <- vegan::adonis2(D_tp ~ Period, data = metas_tp, permutations = ctrl)
      
      data.frame(
        Distance  = distance_name,
        Scope     = "Period within Treatment",
        Treatment = as.character(tt),
        Contrast  = paste(as.character(pr), collapse = " vs "),
        Df        = ad$Df[1],
        F         = ad$F[1],
        R2        = ad$R2[1],
        p         = ad$`Pr(>F)`[1],
        N         = nrow(metas_tp),
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(per_res)
  })
  out <- dplyr::bind_rows(out)
  if (NROW(out)) out$p_BH <- p.adjust(out$p, method = "BH")
  out
}

# Compute pairwise sets for Aitchison
pw_tmt_in_per_ait  <- run_within_period_treatment(D_ait,  meta, "Aitchison", nperm = 999)
pw_per_in_tmt_ait  <- run_within_treatment_period_pairs(D_ait, meta, "Aitchison", nperm = 999)

# Compute pairwise sets for Bray–Curtis
pw_tmt_in_per_bray <- run_within_period_treatment(D_bray, meta, "BrayCurtis", nperm = 999)
pw_per_in_tmt_bray <- run_within_treatment_period_pairs(D_bray, meta, "BrayCurtis", nperm = 999)

# Quick CSVs (if present)
if (!is.null(pw_tmt_in_per_ait)  && nrow(pw_tmt_in_per_ait))  utils::write.csv(pw_tmt_in_per_ait,  file.path(out_dir, "pairwise_Treatment_within_Period_Aitchison.csv"), row.names = FALSE)
if (!is.null(pw_per_in_tmt_ait)  && nrow(pw_per_in_tmt_ait))  utils::write.csv(pw_per_in_tmt_ait,  file.path(out_dir, "pairwise_Period_within_Treatment_Aitchison.csv"), row.names = FALSE)
if (!is.null(pw_tmt_in_per_bray) && nrow(pw_tmt_in_per_bray)) utils::write.csv(pw_tmt_in_per_bray, file.path(out_dir, "pairwise_Treatment_within_Period_BrayCurtis.csv"), row.names = FALSE)
if (!is.null(pw_per_in_tmt_bray) && nrow(pw_per_in_tmt_bray)) utils::write.csv(pw_per_in_tmt_bray, file.path(out_dir, "pairwise_Period_within_Treatment_BrayCurtis.csv"), row.names = FALSE)


## 8) Dispersion check (PERMDISP)

bd_treat  <- vegan::betadisper(D_ait, group = meta$Treatment)
bd_period <- vegan::betadisper(D_ait, group = meta$Period)
permdisp_treat <- permutest(bd_treat, permutations = 999)
permdisp_per   <- permutest(bd_period, permutations = 999)


## 9) Ordination (PCoA on Aitchison) with within-Cow lines

## 9) Ordination (PCoA on Aitchison) with within-Cow lines
pcoa <- cmdscale(D_ait, k = 2, eig = TRUE)

df_plot <- data.frame(
  PCo1 = pcoa$points[,1],
  PCo2 = pcoa$points[,2],
  meta
)

## Create new variable: Treatment + "_" + Period
df_plot$med <- paste(df_plot$Treatment, df_plot$Period, sep = "_")

var1 <- 100 * pcoa$eig[1] / sum(pcoa$eig[pcoa$eig > 0])
var2 <- 100 * pcoa$eig[2] / sum(pcoa$eig[pcoa$eig > 0])

gg <- ggplot(df_plot, aes(x = PCo1, y = PCo2)) +
  geom_point(aes(color = med, shape = med), size = 3, alpha = 0.7) +
  labs(
    title = "PCoA (Aitchison distance)",
    x = sprintf("PCo1 (%.1f%%)", var1),
    y = sprintf("PCo2 (%.1f%%)", var2)
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
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

ggsave(file.path(out_dir, "PCoA_Aitchison.png"), gg, width = 10, height = 8, dpi = 600)
ggsave(file.path(out_dir, "PCoA_Aitchison.pdf"), gg, width = 10, height = 8)

## Save PCoA coordinates & eigenvalues
pcoa_scores <- cbind(Sample = rownames(pcoa$points),
                     as.data.frame(pcoa$points[, 1:2]))
colnames(pcoa_scores)[2:3] <- c("PCo1","PCo2")
write_csv2(pcoa_scores, file.path(out_dir, "PCoA_scores_Aitchison.csv"))
write_csv2(data.frame(Eigenvalues = pcoa$eig), file.path(out_dir, "PCoA_eigenvalues_Aitchison.csv"))


## 10) GLMM-MiRKAT (kernel mixed model) for binary Treatment

K_ait  <- MiRKAT::D2K(as.matrix(D_ait))
K_bray <- MiRKAT::D2K(as.matrix(D_bray))
Ks <- list(Aitchison = K_ait, BrayCurtis = K_bray)

# Binary outcome: 1 = ASP, 0 = CNT (edit if labels differ)
y  <- as.numeric(meta$Treatment == "ASP")
X  <- model.matrix(~ Period, data = meta)[, -1, drop = FALSE]  # adjust for Period
id <- meta$Cow

set.seed(glmm_seed)
res_glmm <- MiRKAT::GLMMMiRKAT(
  y      = y,
  X      = X,
  Ks     = Ks,
  id     = id,
  model  = "binomial",    # GLMMMiRKAT uses 'model', not 'family'
  method = "perm",        # permutation for binary
  nperm  = 10000
)
glmm_pvals <- data.frame(
  Kernel = names(Ks),
  p_value = as.numeric(res_glmm$p_values)
)
if (!is.null(res_glmm$omnibus_p)) {
  glmm_pvals <- rbind(glmm_pvals, data.frame(Kernel = "Omnibus", p_value = res_glmm$omnibus_p))
}


## 11) LMEs on PCoA axes (random intercept for Cow)
fit1 <- lmer(PCo1 ~ Treatment + Period + (1|Cow), data = df_plot)
fit2 <- lmer(PCo2 ~ Treatment + Period + (1|Cow), data = df_plot)
coef_tab <- function(fit) {
  cf <- as.data.frame(coef(summary(fit)))
  cf$Term <- rownames(cf); rownames(cf) <- NULL
  cf <- cf[, c("Term","Estimate","Std. Error","df","t value","Pr(>|t|)")]
  cf
}
lme1_tab <- coef_tab(fit1)
lme2_tab <- coef_tab(fit2)

## 12) dbRDA sensitivity (partialling out Cow)
mod_cap <- vegan::capscale(D_ait ~ Treatment + Period + Condition(Cow), data = meta)
anova_cap <- anova(mod_cap, permutations = ctrl, by = "margin")
dbrda_tab <- as.data.frame(anova_cap); dbrda_tab$Term <- rownames(dbrda_tab); rownames(dbrda_tab) <- NULL
dbrda_tab <- dbrda_tab[, c(ncol(dbrda_tab), setdiff(seq_len(ncol(dbrda_tab)-1), 0))]


## 13) Write a single Excel workbook with all results 
wb_path <- file.path(out_dir, "beta_diversity_results.xlsx")

# Build the list of sheets
sheets_raw <- list(
  "PERMANOVA_Aitchison"                 = as.data.frame(adon_ait)  %>% tibble::rownames_to_column("Term"),
  "PERMANOVA_BrayCurtis"                = as.data.frame(adon_bray) %>% tibble::rownames_to_column("Term"),
  "PERMDISP_Treatment"                  = as.data.frame(permdisp_treat$tab) %>% tibble::rownames_to_column("Effect"),
  "PERMDISP_Period"                     = as.data.frame(permdisp_per$tab)   %>% tibble::rownames_to_column("Effect"),
  "GLMMMiRKAT_pvals"                    = glmm_pvals,
  "LME_PCo1_coefs"                      = lme1_tab,
  "LME_PCo2_coefs"                      = lme2_tab,
  "dbRDA_marginal"                      = dbrda_tab,
  "PCoA_scores"                         = pcoa_scores,
  "Aligned_Metadata"                    = meta,
  # NEW pairwise sheets (may be NULL or empty if subsets were too small)
  "PW_Tmt_within_Period_Aitchison"      = pw_tmt_in_per_ait,
  "PW_Period_within_Tmt_Aitchison"      = pw_per_in_tmt_ait,
  "PW_Tmt_within_Period_BrayCurtis"     = pw_tmt_in_per_bray,
  "PW_Period_within_Tmt_BrayCurtis"     = pw_per_in_tmt_bray
)

# Keep only data.frames/tibbles with at least 1 row and ≥1 column
is_nonempty_df <- function(x) {
  inherits(x, c("data.frame","tbl","tbl_df")) && nrow(x) > 0 && ncol(x) > 0
}
sheets <- sheets_raw[vapply(sheets_raw, is_nonempty_df, logical(1))]

# Sanitize sheet names for Excel: max 31 chars, remove forbidden characters
sanitize_sheet <- function(nm) {
  nm <- gsub("[\\[\\]\\*:\\?/\\\\]", "_", nm)  # forbidden characters
  nm <- sub("^'", "", sub("'$", "", nm))       # no leading/trailing apostrophes
  if (nchar(nm) > 31) nm <- substr(nm, 1, 31)
  nm
}
names(sheets) <- make.unique(vapply(names(sheets), sanitize_sheet, character(1)))

# Finally write
writexl::write_xlsx(sheets, path = wb_path)

## 14) Save a small analysis log
log_lines <- c(
  paste0("Timestamp: ", ts),
  paste0("N samples (aligned): ", nrow(meta)),
  paste0("N genera (before filter): ", ncol(counts)),
  paste0("N genera (after filter): ", ncol(counts_f)),
  paste0("Filter mode: ", toupper(filter_mode),
         " | min_prev_prop = ", min_prev_prop,
         " | min_total_reads = ", min_total_reads),
  paste0("Pseudocount for CLR: ", pc),
  "Distances: Aitchison (CLR/Euclidean, primary), Bray–Curtis (sensitivity).",
  "PERMANOVA (global): adonis2 with permutations restricted within Cow (permute::how(blocks=Cow)).",
  "PAIRWISE PERMANOVA: within-Period (Treatment) and within-Treatment (Period pairs) with unrestricted permutations.",
  "PERMDISP: betadisper + permutest for Treatment and Period.",
  "GLMM-MiRKAT: model='binomial', method='perm', nperm=10000; kernels from Aitchison and Bray distances.",
  "PCoA: cmdscale on Aitchison; plot saved as PNG/PDF."
)
writeLines(log_lines, con = file.path(out_dir, "analysis_log.txt"))

message("All outputs saved under: ", out_dir)
