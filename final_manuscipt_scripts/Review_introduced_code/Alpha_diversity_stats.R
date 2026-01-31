## Alpha diversity (Shannon, InvSimpson, Chao1) with LMM + pairwise
## Latin square: Treatment (ASP/CNT), Period, random Cow
## Faceted boxplots by Med = Treatment_Period

## --------------------------
## Input and parameters
filter_mode        <- "AND"    # "AND" or "OR"
min_prev_prop      <- 0.10
min_total_reads    <- 100

counts_file        <- "genusbac_normalized_nometa.xlsx"
meta_file          <- "metadata.xlsx"

alpha_use_filtered <- FALSE     # ran as FALSE

set.seed(42)

## Packages
pkgs <- c("readxl","dplyr","tibble","ggplot2","vegan",
          "lme4","lmerTest","emmeans","writexl","tidyr","purrr","stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

## Output dir
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("outputs_alpha", paste0("analysisbac_", ts))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv2 <- function(x, path) utils::write.csv(x, path, row.names = FALSE)

## Read data
counts_raw <- readxl::read_excel(counts_file, sheet = 1)
stopifnot("Sample" %in% colnames(counts_raw))
counts_df <- counts_raw %>%
  mutate(Sample = as.character(Sample)) %>%
  column_to_rownames("Sample")
counts_df[] <- lapply(counts_df, function(x) as.numeric(as.character(x)))
counts <- as.data.frame(counts_df)

meta <- readxl::read_excel(meta_file, sheet = 1)
needed <- c("Sample","Treatment","Period","Cow")
if (!all(needed %in% names(meta))) stop("metadata.xlsx must contain columns: Sample, Treatment, Period, Cow")
meta <- meta %>%
  mutate(
    Sample    = as.character(Sample),
    Treatment = factor(Treatment),
    Period    = factor(Period),
    Cow       = factor(Cow)
  )

## Align samples
common_ids <- intersect(rownames(counts), meta$Sample)
if (length(common_ids) < 3) stop("Too few overlapping samples.")
counts <- counts[common_ids, , drop = FALSE]
meta   <- meta[match(common_ids, meta$Sample), , drop = FALSE]
stopifnot(all(rownames(counts) == meta$Sample))
write_csv2(meta, file.path(out_dir, "aligned_metadata.csv"))
write_csv2(cbind(Sample = rownames(counts), counts), file.path(out_dir, "aligned_counts.csv"))

## Feature filtering (same rules as my beta script)
prev_counts <- colSums(counts > 0)
total_reads <- colSums(counts)
n_samp      <- nrow(counts)
min_prev    <- ceiling(min_prev_prop * n_samp)

if (identical(toupper(filter_mode), "AND")) {
  keep <- (prev_counts >= min_prev) & (total_reads >= min_total_reads)
} else if (identical(toupper(filter_mode), "OR")) {
  keep <- (prev_counts >= min_prev) | (total_reads >= min_total_reads)
} else stop("filter_mode must be 'AND' or 'OR'")

counts_f <- counts[, keep, drop = FALSE]

filter_summary <- data.frame(
  Taxon          = colnames(counts),
  Prevalence     = prev_counts,
  PrevalenceProp = prev_counts / n_samp,
  TotalReads     = total_reads,
  Keep           = colnames(counts) %in% colnames(counts_f)
)
utils::write.csv(filter_summary, file.path(out_dir, "feature_filtering_summary.csv"), row.names = FALSE)

message("Kept ", ncol(counts_f), " of ", ncol(counts),
        " taxa using ", toupper(filter_mode),
        " (≥", min_prev_prop*100, "% prevalence and/or ≥", min_total_reads, " total reads).")

## Alpha diversity metrics (Shannon, InvSimpson, Chao1)
counts_alpha <- if (isTRUE(alpha_use_filtered)) counts_f else counts

# Diagnostics (was having mismatches)
message("counts_alpha dimensions: ", nrow(counts_alpha), " rows × ", ncol(counts_alpha), " cols")

Shannon    <- vegan::diversity(counts_alpha, index = "shannon")
InvSimpson <- vegan::diversity(counts_alpha, index = "invsimpson")

message("Shannon length: ", length(Shannon))
message("InvSimpson length: ", length(InvSimpson))

# Chao1 via estimateR - FIXED: no transpose needed
# estimateR expects samples in rows, species in columns (same as diversity())
chao_mat <- vegan::estimateR(counts_alpha)
Chao1    <- as.numeric(chao_mat["S.chao1", ])

message("Chao1 length: ", length(Chao1))

# Verification before creating data.frame
stopifnot(length(Shannon) == nrow(counts_alpha))
stopifnot(length(InvSimpson) == nrow(counts_alpha))
stopifnot(length(Chao1) == nrow(counts_alpha))

alpha_df <- data.frame(
  Sample     = rownames(counts_alpha),
  Shannon    = Shannon,
  InvSimpson = InvSimpson,
  Chao1      = Chao1,
  stringsAsFactors = FALSE
)

alpha_merged <- meta %>%
  dplyr::select(Sample, Treatment, Period, Cow) %>%
  left_join(alpha_df, by = "Sample") %>%
  mutate(Med = paste(Treatment, Period, sep = "_"))  #combined label

write_csv2(alpha_merged, file.path(out_dir, "alpha_metrics_per_sample.csv"))

## LMMs + pairwise for each metric - FIXED: added confint()
analyze_metric <- function(dat, metric_name) {
  dat <- dat %>% dplyr::filter(is.finite(.data[[metric_name]]))
  fit  <- lmer(as.formula(paste0(metric_name, " ~ Treatment + Period + (1|Cow)")),
               data = dat, REML = TRUE)
  
  an_tab <- as.data.frame(anova(fit))
  an_tab$Term <- rownames(an_tab); rownames(an_tab) <- NULL
  an_tab <- an_tab[, c("Term", setdiff(names(an_tab), "Term"))]
  
  # Treatment within each Period (ASP vs CNT) - FIXED: added confint()
  emm1 <- emmeans::emmeans(fit, ~ Treatment | Period)
  pw1_contrasts <- contrast(emm1, method = "pairwise", adjust = "BH")
  pw1_summary <- summary(pw1_contrasts, infer = TRUE) %>% as.data.frame()
  
  pw_tmt_in_per <- NULL
  if (nrow(pw1_summary)) {
    pw_tmt_in_per <- pw1_summary %>%
      mutate(
        Scope    = "Treatment within Period",
        Contrast = "ASP - CNT"
      ) %>%
      dplyr::select(Scope, Period, Contrast, everything())
  }
  
  # Period pairs within each Treatment - FIXED: get p-values AND CIs
  emm2 <- emmeans::emmeans(fit, ~ Period | Treatment)
  pw2_contrasts <- contrast(emm2, method = "pairwise", adjust = "BH")
  pw2_summary <- summary(pw2_contrasts, infer = TRUE) %>% as.data.frame()
  
  pw_per_in_tmt <- NULL
  if (nrow(pw2_summary)) {
    pw_per_in_tmt <- pw2_summary %>%
      mutate(Scope = "Period within Treatment") %>%
      dplyr::select(Scope, Treatment, everything())
  }
  
  list(fit = fit, anova = an_tab, pw_tmt_in_period = pw_tmt_in_per, pw_period_in_tmt = pw_per_in_tmt)
}

metrics <- c("Shannon","InvSimpson","Chao1")
res_list <- purrr::map(metrics, ~{
  nm <- .x
  out <- analyze_metric(alpha_merged, nm)
  utils::write.csv(out$anova, file.path(out_dir, paste0("LMM_ANOVA_", nm, ".csv")), row.names = FALSE)
  if (!is.null(out$pw_tmt_in_period)) utils::write.csv(out$pw_tmt_in_period, file.path(out_dir, paste0("PW_Tmt_within_Period_", nm, ".csv")), row.names = FALSE)
  if (!is.null(out$pw_period_in_tmt)) utils::write.csv(out$pw_period_in_tmt, file.path(out_dir, paste0("PW_Period_within_Tmt_", nm, ".csv")), row.names = FALSE)
  out
})
names(res_list) <- metrics

##boxplots by Med (Treatment_Period)
# Long format for the three indices
indices_df_long <- alpha_merged %>%
  dplyr::select(Sample, Treatment, Period, Cow, Med, Shannon, InvSimpson, Chao1) %>%
  tidyr::pivot_longer(cols = c(Shannon, InvSimpson, Chao1),
                      names_to = "Index", values_to = "Value")

# Optional ordering of facets and x-axis (ensure consistent period order inside Med)
indices_df_long <- indices_df_long %>%
  mutate(
    Index = factor(Index, levels = c("Shannon","InvSimpson","Chao1")),
    Med   = factor(Med, levels = sort(unique(Med)))  # or set manually if needed
  )

p_alpha <- ggplot(indices_df_long, aes(x = Med, y = Value, fill = Med)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(aes(color = Med), size = 1.5, alpha = 0.6,
             position = position_dodge(0)) +
  facet_wrap(~Index, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Boxplots of Diversity Indices - Genus - Bacteria",
    x = "", y = "Index Value"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title   = element_text(hjust = 0.5, size = 20),
    axis.line    = element_line(color = "black", linewidth = 0.5),
    axis.ticks   = element_line(color = "black", linewidth = 0.5),
    strip.text   = element_text(size = 18)
  )

ggsave(file.path(out_dir, "alpha_genusbac.png"), p_alpha, width = 14, height = 8, dpi = 600)
ggsave(file.path(out_dir, "alpha_genusbac.pdf"), p_alpha, width = 14, height = 8)


## Excel workbook (per-sample metrics + stats)
sheets <- list(
  "Alpha_per_sample" = alpha_merged
)
for (m in metrics) {
  sheets[[paste0("ANOVA_", m)]] <- res_list[[m]]$anova
  if (!is.null(res_list[[m]]$pw_tmt_in_period)) {
    sheets[[paste0("PW_Tmt_in_Per_", m)]] <- res_list[[m]]$pw_tmt_in_period
  }
  if (!is.null(res_list[[m]]$pw_period_in_tmt)) {
    sheets[[paste0("PW_Per_in_Tmt_", m)]] <- res_list[[m]]$pw_period_in_tmt
  }
}
sanitize_sheet <- function(nm) {
  nm <- gsub("[\\[\\]\\*:\\?/\\\\]", "_", nm)
  nm <- sub("^'", "", sub("'$", "", nm))
  if (nchar(nm) > 31) nm <- substr(nm, 1, 31)
  nm
}
names(sheets) <- make.unique(vapply(names(sheets), sanitize_sheet, character(1)))
wb_path <- file.path(out_dir, "alpha_diversity_results.xlsx")
writexl::write_xlsx(sheets, path = wb_path)

## Log
log_lines <- c(
  paste0("Timestamp: ", ts),
  paste0("N samples (aligned): ", nrow(meta)),
  paste0("N taxa (before filter): ", ncol(counts)),
  paste0("N taxa (after filter): ", ncol(counts_f)),
  paste0("Filter mode: ", toupper(filter_mode),
         " | min_prev_prop = ", min_prev_prop,
         " | min_total_reads = ", min_total_reads),
  paste0("Alpha computed on: ", ifelse(alpha_use_filtered, "FILTERED", "UNFILTERED")),
  "Metrics: Shannon, InvSimpson, Chao1.",
  "Global tests: LMM metric ~ Treatment + Period + (1|Cow).",
  "Pairwise (BH): Treatment within Period; Period within Treatment.",
  paste0("Plot: ", file.path(out_dir, "alpha_genusbac.png")),
  paste0("Excel: ", wb_path)
)
writeLines(log_lines, con = file.path(out_dir, "analysis_log.txt"))
message("Alpha-diversity outputs saved under: ", out_dir)
