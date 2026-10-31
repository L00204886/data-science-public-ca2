# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      02_normality_and_assumptions.R
# Description: Step 2 of the pipeline. Checks the assumptions behind parametric
#              tests, to justify using rank-based (non-parametric) tests instead:
#                1. Normality: Shapiro-Wilk (5,000-row sample), Kolmogorov-Smirnov
#                   (all rows), skewness and kurtosis
#                2. Equal variance: Bartlett and Fligner-Killeen tests
#                3. Q-Q plots (Figure 5)
# Reads:       output/cleaned_airline_satisfaction.rds
# Writes:      output/tables/04-05, output/figures/fig05
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(e1071)
})

source("scripts/console_helpers.R")

df_clean <- readRDS("output/cleaned_airline_satisfaction.rds")

banner("STEP 1: NORMALITY TESTING (SHAPIRO-WILK & KOLMOGOROV-SMIRNOV)")

# shapiro.test() accepts at most 5,000 values, so Shapiro-Wilk runs on a
# reproducible random sample of 5,000 rows that keeps the satisfaction split.
# The Kolmogorov-Smirnov test (on standardised z-scores) and skewness/kurtosis
# use all rows. KS p-values are approximate because the mean and SD are
# estimated from the data; with this much data every variable fails anyway.
set.seed(42)
sample_idx <- df_clean |>
  mutate(row_id = row_number()) |>
  slice_sample(prop = 5000 / nrow(df_clean), by = satisfaction) |>
  pull(row_id)

df_sub5000 <- df_clean[sample_idx, ]

test_vars <- c(
  "online_boarding",
  "inflight_comfort_score",
  "digital_service_score",
  "ground_service_score",
  "arrival_delay",
  "departure_delay",
  "flight_distance",
  "age"
)

normality_results <- map_dfr(test_vars, function(v) {
  x_full <- na.omit(df_clean[[v]])
  x_sub <- na.omit(df_sub5000[[v]])
  # slice_sample(prop) can return a few rows more than 5,000; trim to the Shapiro limit
  if (length(x_sub) > 5000) x_sub <- x_sub[1:5000]

  sw <- shapiro.test(x_sub)
  # Standardised KS test against standard normal N(0,1) using fast vector arithmetic
  x_z <- (x_full - mean(x_full)) / sd(x_full)
  ks <- suppressWarnings(ks.test(x_z, "pnorm"))

  tibble(
    variable = v,
    n_full = length(x_full),
    skewness = round(e1071::skewness(x_full), 3),
    kurtosis = round(e1071::kurtosis(x_full), 3),
    shapiro_W_n5000 = round(unname(sw$statistic), 4),
    shapiro_p_value = format.pval(sw$p.value, digits = 3, eps = 2.2e-16),
    ks_D_full = round(unname(ks$statistic), 4),
    ks_p_value = format.pval(ks$p.value, digits = 3, eps = 2.2e-16),
    normality_conclusion = if_else(sw$p.value < 0.05, "Reject Normality (Non-Normal)", "Normality not rejected")
  )
})

write_csv(normality_results, "output/tables/04_normality_tests.csv")
show_table("Normality tests (one row per variable)", normality_results, width = 150)

banner("STEP 2: HOMOGENEITY OF VARIANCE TESTS (HOMOSCEDASTICITY)")

# Test equal variances across groups for RQ1, RQ3, and RQ4 using:
# 1. Bartlett's test (parametric, sensitive to non-normality)
# 2. Fligner-Killeen test (non-parametric rank-based test of homogeneity of variances)
homoscedasticity_specs <- list(
  list(rq = "RQ1: Online Boarding ~ Satisfaction", fmla = online_boarding ~ satisfaction),
  list(rq = "RQ3: Inflight Comfort Score ~ Cabin Class", fmla = inflight_comfort_score ~ class),
  list(rq = "RQ4: Arrival Delay ~ Satisfaction", fmla = arrival_delay ~ satisfaction)
)

variance_tests <- map_dfr(homoscedasticity_specs, function(s) {
  bt <- bartlett.test(s$fmla, data = df_clean)
  fk <- fligner.test(s$fmla, data = df_clean)
  tibble(
    research_question = s$rq,
    bartlett_K2 = round(unname(bt$statistic), 2),
    bartlett_p = format.pval(bt$p.value, digits = 3, eps = 2.2e-16),
    fligner_chi2 = round(unname(fk$statistic), 2),
    fligner_p = format.pval(fk$p.value, digits = 3, eps = 2.2e-16),
    variance_conclusion = if_else(bt$p.value < 0.05 & fk$p.value < 0.05,
      "Heteroscedastic (Unequal Variances)", "Equal variances not rejected"
    )
  )
})

write_csv(variance_tests, "output/tables/05_variance_homogeneity_tests.csv")
show_table("Equal-variance tests (one row per research question)", variance_tests, width = 150)

banner("STEP 3: GENERATING Q-Q DIAGNOSTIC PLOTS ")

# Helper functions to pull exact test values from the normality table for the plot titles
get_w <- function(var_nm) {
  normality_results$shapiro_W_n5000[normality_results$variable == var_nm]
}
get_skew <- function(var_nm) {
  normality_results$skewness[normality_results$variable == var_nm]
}

png("output/figures/fig05_normality_qq_diagnostics.png", width = 10, height = 7, units = "in", res = 300)
tryCatch({
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.2, 1.5), family = "sans")

  # A: Online Boarding Q-Q (Discrete Ordinal Staircase)
  qqnorm(df_sub5000$online_boarding,
    pch = 16, cex = 0.5, col = rgb(0.17, 0.24, 0.31, 0.35),
    main = sprintf(
      "A. Q-Q Plot: Online Boarding (Ordinal 1-5)\nShapiro-Wilk W = %.3f, p < 2.2e-16",
      get_w("online_boarding")
    )
  )
  qqline(df_sub5000$online_boarding, col = "#E74C3C", lwd = 2)
  grid(col = "gray88", lty = "dotted")

  # B: Engineered In-Flight Comfort Score Q-Q
  qqnorm(df_sub5000$inflight_comfort_score,
    pch = 16, cex = 0.5, col = rgb(0.11, 0.62, 0.47, 0.35),
    main = sprintf(
      "B. Q-Q Plot: Engineered In-Flight Comfort Score\nShapiro-Wilk W = %.3f, p < 2.2e-16",
      get_w("inflight_comfort_score")
    )
  )
  qqline(df_sub5000$inflight_comfort_score, col = "#E74C3C", lwd = 2)
  grid(col = "gray88", lty = "dotted")

  # C: Arrival Delay Q-Q (Severe Right Skew & Heavy Tail)
  qqnorm(df_sub5000$arrival_delay,
    pch = 16, cex = 0.5, col = rgb(0.85, 0.37, 0.01, 0.35),
    main = sprintf(
      "C. Q-Q Plot: Arrival Delay (Minutes)\nShapiro-Wilk W = %.3f, Skew = %.2f, p < 2.2e-16",
      get_w("arrival_delay"), get_skew("arrival_delay")
    )
  )
  qqline(df_sub5000$arrival_delay, col = "#E74C3C", lwd = 2)
  grid(col = "gray88", lty = "dotted")

  # D: Flight Distance Q-Q
  qqnorm(df_sub5000$flight_distance,
    pch = 16, cex = 0.5, col = rgb(0.16, 0.50, 0.73, 0.35),
    main = sprintf(
      "D. Q-Q Plot: Flight Distance (Miles)\nShapiro-Wilk W = %.3f, Skew = %.2f, p < 2.2e-16",
      get_w("flight_distance"), get_skew("flight_distance")
    )
  )
  qqline(df_sub5000$flight_distance, col = "#E74C3C", lwd = 2)
  grid(col = "gray88", lty = "dotted")
}, finally = {
  dev.off()
})
cat("Saved chart: fig05_normality_qq_diagnostics.png\n")
cat("Script 02_normality_and_assumptions.R completed successfully.\n")
