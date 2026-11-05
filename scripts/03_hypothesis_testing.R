# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      03_hypothesis_testing.R
# Description: Step 3 of the pipeline. Runs the five formal hypothesis tests and
#              reports each result with an effect size:
#                RQ1 Online boarding by satisfaction  - Wilcoxon (+ Welch t-test)
#                RQ2 Travel type vs satisfaction      - Chi-square, Cramer's V, OR
#                RQ3 Comfort score across cabin class - Kruskal-Wallis + post-hoc
#                RQ4 Arrival delay by satisfaction    - Wilcoxon (+ Welch t-test)
#                RQ5 Departure vs arrival delay       - Pearson / Spearman, VIF
#              Then saves a summary table and Figures 6-8.
# Reads:       output/cleaned_airline_satisfaction.rds
# Writes:      output/tables/06, output/figures/fig06-fig08
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
})

source("scripts/console_helpers.R")

df_clean <- readRDS("output/cleaned_airline_satisfaction.rds")

# Same group colours as script 01
col_dissat <- "#D95F02"
col_sat <- "#1B9E77"



banner("RQ1: ONLINE BOARDING RATINGS BY PASSENGER SATISFACTION")
# Variables: online_boarding (Ordinal 1-5) vs satisfaction (Binary Factor)
rq1_df <- df_clean |> filter(!is.na(online_boarding))
x_sat <- rq1_df$online_boarding[rq1_df$satisfaction == "Satisfied"]
x_dissat <- rq1_df$online_boarding[rq1_df$satisfaction == "Neutral or Dissatisfied"]
n1 <- as.numeric(length(x_sat))
n2 <- as.numeric(length(x_dissat))

# Primary Non-Parametric Test: Wilcoxon Rank-Sum (Mann-Whitney U)
rq1_wilcox <- wilcox.test(x_sat, x_dissat, alternative = "greater")
U_stat_rq1 <- as.numeric(rq1_wilcox$statistic)
r_rb_rq1 <- rank_biserial_from_U(U_stat_rq1, n1, n2)

# Parametric Sensitivity Check: Welch Two-Sample t-test & Cohen's d
rq1_ttest <- t.test(x_sat, x_dissat, alternative = "greater")
cohens_d_rq1 <- cohens_d(x_sat, x_dissat)

cat(sprintf(
  "RQ1 Valid N = %s (Satisfied = %s, Neutral/Dissatisfied = %s)\n",
  format(nrow(rq1_df), big.mark = ","), format(n1, big.mark = ","), format(n2, big.mark = ",")
))
cat(sprintf(
  "Satisfied: Median = %.1f (IQR = %.1f), Mean = %.2f (SD = %.2f)\n",
  median(x_sat), IQR(x_sat), mean(x_sat), sd(x_sat)
))
cat(sprintf(
  "Neutral/Dissatisfied: Median = %.1f (IQR = %.1f), Mean = %.2f (SD = %.2f)\n",
  median(x_dissat), IQR(x_dissat), mean(x_dissat), sd(x_dissat)
))
cat(sprintf(
  "Mann-Whitney U = %.0f, %s | Rank-Biserial r_rb = %.4f (Large Effect)\n",
  U_stat_rq1, p_text(rq1_wilcox$p.value), r_rb_rq1
))
cat(sprintf(
  "Welch t = %.2f, df = %.0f, %s | Cohen's d = %.4f\n\n",
  rq1_ttest$statistic, rq1_ttest$parameter, p_text(rq1_ttest$p.value), cohens_d_rq1
))

banner("RQ2: TYPE OF TRAVEL vs. OVERALL SATISFACTION (CHI-SQUARE)")
# Variables: type_of_travel (Nominal Factor) vs satisfaction (Nominal Factor)
tab_rq2 <- table(df_clean$type_of_travel, df_clean$satisfaction)
prop_rq2 <- prop.table(tab_rq2, margin = 1) * 100
rq2_chisq <- chisq.test(tab_rq2, correct = FALSE)

# Effect Sizes: Cramer's V and Odds Ratio
N_rq2 <- sum(tab_rq2)
cramers_v_rq2 <- sqrt(as.numeric(rq2_chisq$statistic) / (N_rq2 * (min(dim(tab_rq2)) - 1)))
# Odds Ratio: (Business_Sat * Personal_Dissat) / (Business_Dissat * Personal_Sat)
bus_sat <- as.numeric(tab_rq2["Business", "Satisfied"])
bus_dis <- as.numeric(tab_rq2["Business", "Neutral or Dissatisfied"])
per_sat <- as.numeric(tab_rq2["Personal", "Satisfied"])
per_dis <- as.numeric(tab_rq2["Personal", "Neutral or Dissatisfied"])
odds_ratio_rq2 <- (bus_sat * per_dis) / (bus_dis * per_sat)
# Standard error of log(OR), used for the 95% confidence interval
se_log_or <- sqrt(1 / bus_sat + 1 / bus_dis + 1 / per_sat + 1 / per_dis)
or_ci_low <- exp(log(odds_ratio_rq2) - 1.96 * se_log_or)
or_ci_high <- exp(log(odds_ratio_rq2) + 1.96 * se_log_or)

show_table("RQ2: passenger counts (travel type x satisfaction)", tab_rq2)
show_table("RQ2: row percentages", round(prop_rq2, 2))
cat(sprintf(
  "Pearson Chi-Square = %.2f, df = %d, %s\n",
  rq2_chisq$statistic, rq2_chisq$parameter, p_text(rq2_chisq$p.value)
))
cat(sprintf(
  "Cramer's V = %.4f (Strong Association) | Odds Ratio = %.2f (95%% CI [%.2f, %.2f])\n\n",
  cramers_v_rq2, odds_ratio_rq2, or_ci_low, or_ci_high
))

banner("RQ3: IN-FLIGHT COMFORT SCORE ACROSS CABIN CLASSES (KRUSKAL-WALLIS)")
# Variables: inflight_comfort_score (Engineered Composite 1-5) vs class (3-level Ordered Factor)
rq3_kw <- kruskal.test(inflight_comfort_score ~ class, data = df_clean)
rq3_welch <- oneway.test(inflight_comfort_score ~ class, data = df_clean, var.equal = FALSE)
rq3_pw <- pairwise.wilcox.test(df_clean$inflight_comfort_score, df_clean$class, p.adjust.method = "bonferroni")

# Effect size for Kruskal-Wallis: epsilon-squared = H / (n - 1)
H_stat_rq3 <- as.numeric(rq3_kw$statistic)
N_rq3 <- nrow(df_clean)
eps_sq_rq3 <- H_stat_rq3 / (N_rq3 - 1)

rq3_group_stats <- df_clean |>
  summarise(
    n = n(),
    mean = round(mean(inflight_comfort_score), 3),
    sd = round(sd(inflight_comfort_score), 3),
    median = round(median(inflight_comfort_score), 3),
    iqr = round(IQR(inflight_comfort_score), 3),
    .by = class
  )
show_table("RQ3: comfort score by cabin class", rq3_group_stats)
cat(sprintf(
  "Kruskal-Wallis H = %.2f, df = %d, %s | Epsilon-Squared = %.4f\n",
  H_stat_rq3, rq3_kw$parameter, p_text(rq3_kw$p.value), eps_sq_rq3
))
cat(sprintf(
  "Welch One-Way ANOVA F = %.2f, df1 = %.0f, df2 = %.0f, %s\n",
  rq3_welch$statistic, rq3_welch$parameter[1], rq3_welch$parameter[2], p_text(rq3_welch$p.value)
))
cat("Bonferroni Post-Hoc Pairwise Wilcoxon p-values:\n")
print(rq3_pw$p.value)
cat("\n")

banner("RQ4: ARRIVAL DELAY BY PASSENGER SATISFACTION")
# Variables: arrival_delay (Continuous minutes) vs satisfaction (Binary Factor)
d_dissat <- df_clean$arrival_delay[df_clean$satisfaction == "Neutral or Dissatisfied"]
d_sat <- df_clean$arrival_delay[df_clean$satisfaction == "Satisfied"]

rq4_wilcox <- wilcox.test(d_dissat, d_sat, alternative = "greater")
U_stat_rq4 <- as.numeric(rq4_wilcox$statistic)
r_rb_rq4 <- rank_biserial_from_U(U_stat_rq4, length(d_dissat), length(d_sat))

rq4_ttest <- t.test(d_dissat, d_sat, alternative = "greater")
rq4_ttest_two <- t.test(d_dissat, d_sat, alternative = "two.sided")
cohens_d_rq4 <- cohens_d(d_dissat, d_sat)

# Second view of RQ4: Chi-Square on the 3-level delay category (On Time / Minor / Major)
tab_rq4_cat <- table(df_clean$delay_category, df_clean$satisfaction)
rq4_cat_chisq <- chisq.test(tab_rq4_cat)
cramers_v_rq4_cat <- sqrt(as.numeric(rq4_cat_chisq$statistic) /
  (nrow(df_clean) * (min(dim(tab_rq4_cat)) - 1)))

cat(sprintf(
  "Neutral/Dissatisfied Delay: Mean = %.2f m (SD = %.2f), Median = %.1f m (IQR = %.1f), PosDelay%% = %.1f%%\n",
  mean(d_dissat), sd(d_dissat), median(d_dissat), IQR(d_dissat), mean(d_dissat > 0) * 100
))
cat(sprintf(
  "Satisfied Delay:            Mean = %.2f m (SD = %.2f), Median = %.1f m (IQR = %.1f), PosDelay%% = %.1f%%\n",
  mean(d_sat), sd(d_sat), median(d_sat), IQR(d_sat), mean(d_sat > 0) * 100
))
cat(sprintf(
  "Mann-Whitney U = %.0f, %s | Rank-Biserial r_rb = %.4f (Small Effect)\n",
  U_stat_rq4, p_text(rq4_wilcox$p.value), r_rb_rq4
))
cat(sprintf(
  "Welch t = %.2f, df = %.0f, %s | Mean Diff = +%.2f mins (95%% CI [%.2f, %.2f]) | Cohen's d = %.4f\n",
  rq4_ttest_two$statistic, rq4_ttest_two$parameter, p_text(rq4_ttest_two$p.value),
  mean(d_dissat) - mean(d_sat), rq4_ttest_two$conf.int[1], rq4_ttest_two$conf.int[2], cohens_d_rq4
))
cat(sprintf(
  "Engineered Delay Regime Chi-Square = %.2f, df = 2, %s | Cramer's V = %.4f\n\n",
  rq4_cat_chisq$statistic, p_text(rq4_cat_chisq$p.value), cramers_v_rq4_cat
))

banner("RQ5: DEPARTURE vs. ARRIVAL DELAY COLLINEARITY (CORRELATION)")
# Variables: departure_delay vs arrival_delay_raw (complete pairs N = 129,487)
rq5_df <- df_clean |> filter(!is.na(arrival_delay_raw))
rq5_pearson <- cor.test(rq5_df$departure_delay, rq5_df$arrival_delay_raw, method = "pearson")
rq5_spearman <- cor.test(rq5_df$departure_delay, rq5_df$arrival_delay_raw, method = "spearman", exact = FALSE)
r2_rq5 <- as.numeric(rq5_pearson$estimate)^2
vif_rq5 <- 1 / (1 - r2_rq5)

cat(sprintf("Complete Delay Pairs N = %s\n", format(nrow(rq5_df), big.mark = ",")))
cat(sprintf(
  "Pearson r = %.4f (95%% CI [%.4f, %.4f]), t = %.1f, %s | R^2 = %.4f | Pairwise VIF = %.2f\n",
  rq5_pearson$estimate, rq5_pearson$conf.int[1], rq5_pearson$conf.int[2],
  rq5_pearson$statistic, p_text(rq5_pearson$p.value), r2_rq5, vif_rq5
))
cat(sprintf(
  "Spearman rho = %.4f, S = %.3e, %s\n\n",
  rq5_spearman$estimate, rq5_spearman$statistic, p_text(rq5_spearman$p.value)
))

# Save Master Summary Table of All 5 Hypothesis Tests
# p-values and decisions are taken from the test objects, not typed in
rq_p <- c(
  rq1_wilcox$p.value, rq2_chisq$p.value, rq3_kw$p.value,
  rq4_wilcox$p.value, rq5_pearson$p.value
)
hypothesis_summary <- tibble(
  RQ = c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5"),
  Research_Focus = c(
    "Online Boarding Rating by Satisfaction",
    "Type of Travel vs. Overall Satisfaction",
    "Engineered In-Flight Comfort across Cabin Class",
    "Arrival Delay Duration by Satisfaction",
    "Departure Delay vs. Arrival Delay Collinearity"
  ),
  Primary_Test = c(
    "Wilcoxon Rank-Sum (Mann-Whitney U)",
    "Pearson's Chi-Square Test of Independence",
    "Kruskal-Wallis + Bonferroni Post-Hoc",
    "Wilcoxon Rank-Sum & Welch Two-Sample t-test",
    "Pearson's r & Spearman's Rank Correlation"
  ),
  Test_Statistic = c(
    sprintf("W = %.3e (t = %.2f)", U_stat_rq1, rq1_ttest$statistic),
    sprintf("Chi2(1) = %.1f", rq2_chisq$statistic),
    sprintf("H(2) = %.1f (F = %.1f)", H_stat_rq3, rq3_welch$statistic),
    sprintf("W = %.3e (t = %.2f)", U_stat_rq4, rq4_ttest_two$statistic),
    sprintf("r = %.4f (t = %.1f); rho = %.4f", rq5_pearson$estimate, rq5_pearson$statistic, rq5_spearman$estimate)
  ),
  p_value = format.pval(rq_p, digits = 3, eps = 2.2e-16),
  Effect_Size = c(
    sprintf("r_rb = %.3f; Cohen's d = %.3f (Large)", r_rb_rq1, cohens_d_rq1),
    sprintf("Cramer's V = %.3f; OR = %.2f (Large)", cramers_v_rq2, odds_ratio_rq2),
    sprintf("Epsilon^2 = %.3f (Moderate; Eco vs Eco+ p = 1.00)", eps_sq_rq3),
    sprintf("r_rb = %.3f; Cohen's d = %.3f (Small)", r_rb_rq4, cohens_d_rq4),
    sprintf("R^2 = %.3f; VIF = %.2f (Near-Perfect Collinearity)", r2_rq5, vif_rq5)
  ),
  Decision = if_else(rq_p < 0.05, "Reject H0", "Fail to reject H0")
)

write_csv(hypothesis_summary, "output/tables/06_hypothesis_test_results.csv")
show_table("Summary of all five hypothesis tests", hypothesis_summary, width = 150)

banner("GENERATING HYPOTHESIS TEST RESULT CHARTS (FIGURES 6-8)")

# ------------------------------------------------------------------------------
# Figure 6: RQ1 & RQ2 Results
# ------------------------------------------------------------------------------
save_figure_png("fig06_rq1_rq2_results.png", width = 10, height = 5.2, plot_fn = function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(1, 2), mar = c(4.8, 4.8, 3.5, 1.5), family = "sans")

  # Panel A: RQ1 Online Boarding Rating Proportions by Satisfaction
  tab_ob <- prop.table(table(rq1_df$satisfaction, rq1_df$online_boarding), margin = 1) * 100
  bp_ob <- barplot(
    tab_ob,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 48),
    xlab = "Online Boarding Rating (1 = Lowest to 5 = Highest)",
    ylab = "Percentage within Satisfaction Group (%)",
    main = sprintf("A. RQ1: Online Boarding by Satisfaction\n(Mann-Whitney p < 2.2e-16, r_rb = %.3f)", r_rb_rq1)
  )
  grid(nx = NA, ny = NULL, col = "gray88", lty = "dotted")
  text(bp_ob, tab_ob + 2.2, labels = sprintf("%.0f%%", tab_ob), cex = 0.72, font = 2)
  legend("topleft",
    legend = c("Neutral/Dissatisfied (Median = 3)", "Satisfied (Median = 4)"),
    fill = c(col_dissat, col_sat), border = NA, bty = "n", cex = 0.80
  )

  # Panel B: RQ2 Type of Travel vs Satisfaction
  tab_tr2 <- prop.table(table(df_clean$satisfaction, df_clean$type_of_travel), margin = 2) * 100
  bp_tr2 <- barplot(
    tab_tr2,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 105),
    xlab = "Passenger Type of Travel",
    ylab = "Percentage within Travel Group (%)",
    main = sprintf(
      "B. RQ2: Travel Purpose vs. Satisfaction\n(Chi-Square = %s, p < 2.2e-16, V = %.3f)",
      format(round(rq2_chisq$statistic), big.mark = ","), cramers_v_rq2
    )
  )
  grid(nx = NA, ny = NULL, col = "gray88", lty = "dotted")
  text(bp_tr2, tab_tr2 + 4.2, labels = sprintf("%.1f%%", tab_tr2), cex = 0.85, font = 2)
  legend("topright",
    legend = c("Neutral/Dissatisfied", sprintf("Satisfied (OR = %.2f)", odds_ratio_rq2)),
    fill = c(col_dissat, col_sat), border = NA, bty = "n", cex = 0.80
  )
})

# ------------------------------------------------------------------------------
# Figure 7: RQ3 & RQ4 Results
# ------------------------------------------------------------------------------
save_figure_png("fig07_rq3_rq4_results.png", width = 10, height = 5.2, plot_fn = function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(1, 2), mar = c(4.8, 4.8, 3.5, 1.5), family = "sans")

  # Panel A: RQ3 Engineered In-Flight Comfort Score by Cabin Class
  boxplot(
    inflight_comfort_score ~ class,
    data = df_clean,
    col = c("#BDC3C7", "#5DADE2", "#1B9E77"),
    ylim = c(1, 5.5), outline = FALSE,
    xlab = "Cabin Class Tier",
    ylab = "Engineered In-Flight Comfort Score (1-5)",
    main = sprintf(
      "A. RQ3: In-Flight Comfort Across Classes\n(Kruskal-Wallis H = %s, p < 2.2e-16)",
      format(round(H_stat_rq3), big.mark = ",")
    )
  )
  grid(nx = NA, ny = NULL, col = "gray88", lty = "dotted")
  # Overlay group means as red diamonds
  means_rq3 <- tapply(df_clean$inflight_comfort_score, df_clean$class, mean)
  x_idx <- seq_along(means_rq3)
  points(x_idx, means_rq3, pch = 18, col = "#C0392B", cex = 1.6)
  text(x_idx, means_rq3 + 0.25, labels = sprintf("Mean: %.2f", means_rq3), cex = 0.78, col = "#C0392B", font = 2)
  # Bracket showing Economy vs Economy Plus p = 1.00
  segments(1, 5.15, 2, 5.15, lwd = 1.8, col = "gray30")
  text(1.5, 5.32, labels = "Post-Hoc p = 1.00 (NS)", cex = 0.78, font = 2, col = "gray20")

  # Panel B: RQ4 satisfaction rate within each arrival-delay regime
  tab_dl_sat <- prop.table(table(df_clean$satisfaction, df_clean$delay_category), margin = 2) * 100
  bp_dl <- barplot(
    tab_dl_sat,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 82),
    names.arg = c("On Time\n(0m)", "Minor Delay\n(1-15m)", "Major Delay\n(>15m)"),
    xlab = "Arrival Delay Regime (Minutes)",
    ylab = "Percentage within Delay Regime (%)",
    # Panel B title values come from the test results, not typed in
    main = sprintf(
      "B. RQ4: Arrival Delay vs. Satisfaction\n(Mean: %.1fm vs %.1fm, t = %.1f, r_rb = %.3f)",
      mean(d_dissat), mean(d_sat), rq4_ttest_two$statistic, r_rb_rq4
    )
  )
  grid(nx = NA, ny = NULL, col = "gray88", lty = "dotted")
  text(bp_dl, tab_dl_sat + 3.5, labels = sprintf("%.1f%%", tab_dl_sat), cex = 0.78, font = 2)
})

# ------------------------------------------------------------------------------
# Figure 8: RQ5 Collinearity Scatterplot & Spearman Correlation Heatmap
# ------------------------------------------------------------------------------
save_figure_png("fig08_rq5_correlation_and_collinearity.png", width = 10, height = 5.2, plot_fn = function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(1, 2), mar = c(4.8, 4.8, 3.5, 1.5), family = "sans")

  # Panel A: 5,000 random flights keep the scatter readable; the OLS line uses all rows
  set.seed(42)
  sub_idx <- sample(seq_len(nrow(rq5_df)), 5000)
  plot(
    rq5_df$departure_delay[sub_idx], rq5_df$arrival_delay_raw[sub_idx],
    pch = 16, cex = 0.55, col = rgb(0.16, 0.50, 0.73, 0.35),
    xlim = c(0, 600), ylim = c(0, 600),
    xlab = "Departure Delay (Minutes)",
    ylab = "Arrival Delay (Minutes)",
    main = sprintf(
      "A. RQ5: Departure vs. Arrival Delay\n(Pearson r = %.3f, Spearman rho = %.3f)",
      rq5_pearson$estimate, rq5_spearman$estimate
    )
  )
  grid(col = "gray88", lty = "dotted")
  abline(0, 1, col = "gray40", lty = 2, lwd = 1.5)
  abline(lm(arrival_delay_raw ~ departure_delay, data = rq5_df), col = "#E74C3C", lwd = 2)
  legend("topleft",
    legend = c(sprintf("OLS Fit (R^2 = %.3f, VIF = %.2f)", r2_rq5, vif_rq5), "1:1 Identity Line"),
    col = c("#E74C3C", "gray40"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.80
  )

  # Panel B: Spearman Correlation Matrix of Key Composite & Operational Variables
  cor_vars <- df_clean |>
    mutate(satisfaction_num = as.numeric(satisfaction == "Satisfied")) |>
    select(
      Satisfaction = satisfaction_num,
      Online_Board = online_boarding,
      Digital_Score = digital_service_score,
      Comfort_Score = inflight_comfort_score,
      Ground_Score = ground_service_score,
      Flight_Dist = flight_distance,
      Dep_Delay = departure_delay,
      Arr_Delay = arrival_delay
    )
  cor_mat <- cor(cor_vars, use = "pairwise.complete.obs", method = "spearman")
  k <- ncol(cor_mat)

  par(mar = c(5.5, 6.5, 3.5, 1.5))
  image(
    1:k, 1:k, cor_mat[, k:1],
    col = hcl.colors(50, "Blue-Red 3"), zlim = c(-1, 1),
    axes = FALSE, xlab = "", ylab = "",
    main = "B. Spearman Rank Correlation Matrix\nAcross Key Study Variables"
  )
  axis(1, at = 1:k, labels = colnames(cor_mat), las = 2, cex.axis = 0.75)
  axis(2, at = 1:k, labels = rev(colnames(cor_mat)), las = 1, cex.axis = 0.75)
  for (i in 1:k) {
    for (j in 1:k) {
      val <- cor_mat[i, k - j + 1]
      text(i, j,
        labels = sprintf("%.2f", val), cex = 0.68,
        col = ifelse(abs(val) > 0.55, "white", "black"), font = 2
      )
    }
  }
})

cat("\nScript 03_hypothesis_testing.R completed successfully.\n")
