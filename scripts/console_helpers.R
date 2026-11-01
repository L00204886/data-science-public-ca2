# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      console_helpers.R
# Description: Helper functions used in CA2 scripts
# ==============================================================================

options(width = 150, tibble.width = 150)

banner <- function(txt) {
  rule <- strrep("=", 66)
  cat("\n", rule, "\n ", txt, "\n", rule, "\n\n", sep = "")
}

show_table <- function(title, x, ...) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  print(x, ...)
  cat("\n")
}

# Helper to save high-res PNGs to output/figures (the device is always closed, even on error)
save_figure_png <- function(filename, width = 9, height = 5.5, res = 300, plot_fn) {
  out_path <- file.path("output/figures", filename)
  png(out_path, width = width, height = height, units = "in", res = res)
  tryCatch({
    plot_fn()
  }, finally = {
    dev.off()
  })
  cat(sprintf("Saved chart: %s\n", filename))
}

# --------------------------------------------------------------------------------------
# Effect-size helpers used by RQ1 and RQ4
# --------------------------------------------------------------------------------------

# Rank-biserial correlation from the Wilcoxon U statistic: r_rb = 2U / (n1 * n2) - 1
rank_biserial_from_U <- function(U, n1, n2) 2 * U / (as.numeric(n1) * as.numeric(n2)) - 1

# Cohen's d using the pooled standard deviation of the two groups
cohens_d <- function(x, y) {
  sd_pool <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) / (length(x) + length(y) - 2))
  (mean(x) - mean(y)) / sd_pool
}

# p-value text for the console output (R reports tiny p-values as 0)
p_text <- function(p) if (p < 2.2e-16) "p < 2.2e-16" else sprintf("p = %.3g", p)


# -----------------------------------------------------------------------------
# Helper functions used by script 04 (Supplementary Analyses)
# -----------------------------------------------------------------------------

# Cramer's V for any r x c contingency table: V = sqrt(chi2 / (N * (min(r,c) - 1)))
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sqrt(as.numeric(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}

# Rank-biserial correlation from a Wilcoxon test of x vs y
# (as.numeric avoids integer overflow: n1 * n2 is over 2 billion here)
rank_biserial <- function(x, y) {
  w <- wilcox.test(x, y, exact = FALSE)
  2 * as.numeric(w$statistic) / (as.numeric(length(x)) * as.numeric(length(y))) - 1
}

# Calculates satisfaction percentage
sat_rate <- function(s) mean(s == "Satisfied") * 100
