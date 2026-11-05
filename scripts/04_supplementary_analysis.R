# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      04_supplementary_analysis.R
# Description: Step 4 of the pipeline. Robustness checks that test whether the
#              five main results hold up. Each section asks one question:
#                S1. Is "Not Applicable" (0 -> NA) missingness informative?
#                S3. Is travel type (RQ2) confounded by cabin class? (CMH test)
#                S4. Do customer type, age and gender matter?
#                S5. Do the data-quality flags change RQ2?
#                S6. Is RQ4 robust to the imputation method and outliers?
#                S7. How much of the zero-delay spike is a recording floor?
#                S8. Is the RQ3 class gap just a flight-length effect?
# Reads:       output/cleaned_airline_satisfaction.rds
# Writes:      output/tables/07, 09-13
# (Check numbers S1-S8 match the report; S2 and the logistic model were dropped.)
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

source("scripts/console_helpers.R")

df_clean <- readRDS("output/cleaned_airline_satisfaction.rds")

# The 14 Likert items are the ones script 01 created a "<item>_na" indicator for
likert_cols <- sub("_na$", "", grep("_na$", names(df_clean), value = TRUE))


banner("S1: IS 'NOT APPLICABLE' MISSINGNESS INFORMATIVE?")
# If passengers who skip a question have a different satisfaction rate from
# those who answer it, the missingness is NOT random (Missing Not At Random).
# Then simply dropping NAs can bias results, and the "<item>_na" indicator
# columns created in script 01 carry real information.
missing_info <- map_dfr(likert_cols, function(col) {
  flag <- df_clean[[paste0(col, "_na")]]
  if (sum(flag) == 0) {
    return(NULL) # no 0s for this item, so leave it out of the table
  }
  tab <- table(flag, df_clean$satisfaction)
  tibble(
    item = col,
    n_not_applicable = sum(flag),
    sat_pct_answered = round(sat_rate(df_clean$satisfaction[!flag]), 1),
    sat_pct_not_applicable = round(sat_rate(df_clean$satisfaction[flag]), 1),
    cramers_v = round(cramers_v(tab), 4)
  )
}) |> arrange(desc(n_not_applicable))

# Overall comparison: any skipped item vs none
any_na <- df_clean$n_not_applicable > 0
cat(sprintf(
  "Satisfied %% | no skipped items: %.1f%% | >= 1 skipped item: %.1f%%\n",
  sat_rate(df_clean$satisfaction[!any_na]), sat_rate(df_clean$satisfaction[any_na])
))
write_csv(missing_info, "output/tables/07_missingness_informativeness.csv")
show_table("Satisfaction (%) when an item is answered vs. Not Applicable", missing_info)

banner("S3: IS THE TYPE-OF-TRAVEL EFFECT (RQ2) CONFOUNDED BY CABIN CLASS?")
# 66% of Business travellers sit in Business class, so the RQ2 odds ratio
# (12.44) mixes two effects together. The Cochran-Mantel-Haenszel (CMH) test
# re-tests Travel x Satisfaction *within* each class and pools the result
# into one class-adjusted OR.
strata_tab <- table(df_clean$type_of_travel, df_clean$satisfaction, df_clean$class)
# Store counts as doubles: the CMH variance formula multiplies large counts and
# overflows R's 32-bit integers otherwise (giving NA confidence limits).
storage.mode(strata_tab) <- "double"
cmh <- mantelhaen.test(strata_tab)

# Satisfaction % for each Class x Travel combination
class_travel <- df_clean |>
  summarise(n = n(), satisfied_pct = round(sat_rate(satisfaction), 1), .by = c(class, type_of_travel)) |>
  arrange(class, type_of_travel)
show_table("% satisfied by cabin class and travel type", class_travel)
write_csv(class_travel, "output/tables/09_class_by_travel_satisfaction.csv")
cat(sprintf(
  "CMH chi2 = %.2f, df = %d, p = %s | Class-adjusted common OR = %.2f (95%% CI [%.2f, %.2f])\n",
  cmh$statistic, cmh$parameter, format.pval(cmh$p.value, eps = 2.2e-16),
  cmh$estimate, cmh$conf.int[1], cmh$conf.int[2]
))

banner("S4: UNUSED DEMOGRAPHICS - CUSTOMER TYPE, AGE GROUP, GENDER")
# Chi-square tests of independence with Cramer's V. Gender is expected to be a
# 'null' finding, which is still worth reporting.
demo_tests <- map_dfr(c("customer_type", "age_group", "gender"), function(v) {
  tab <- table(df_clean[[v]], df_clean$satisfaction)
  chi <- chisq.test(tab, correct = FALSE)
  rates <- tapply(df_clean$satisfaction, df_clean[[v]], sat_rate)
  tibble(
    variable = v,
    satisfied_pct_by_level = paste(sprintf("%s: %.1f%%", names(rates), rates), collapse = "; "),
    chi_square = round(as.numeric(chi$statistic), 2),
    df = as.integer(chi$parameter),
    p_value = format.pval(chi$p.value, digits = 3, eps = 2.2e-16),
    cramers_v = round(cramers_v(tab), 4)
  )
})
write_csv(demo_tests, "output/tables/10_demographic_tests.csv")
show_table("Demographics vs. satisfaction", demo_tests, width = 150)

banner("S5: DATA-QUALITY FLAGS - DO THEY CHANGE ANY CONCLUSION?")
# Re-run the RQ2 association without the implausible under-16 business rows
# and without straight-liners. If V and the OR barely move, the flagged rows
# are not driving the result and can safely stay in the main analysis.
or_2x2 <- function(tab) {
  (tab["Business", "Satisfied"] * tab["Personal", "Neutral or Dissatisfied"]) /
    (tab["Business", "Neutral or Dissatisfied"] * tab["Personal", "Satisfied"])
}
flag_samples <- list(
  "Full sample"                      = df_clean,
  "Excluding under-16 Business rows" = filter(df_clean, !flag_minor_business),
  "Excluding straight-liners"        = filter(df_clean, !flag_straight_liner)
)
flag_check <- imap_dfr(flag_samples, function(d, label) {
  tab <- table(d$type_of_travel, d$satisfaction)
  tibble(
    sample = label, n = nrow(d),
    rq2_cramers_v = round(cramers_v(tab), 4),
    rq2_odds_ratio = round(or_2x2(tab), 4)
  )
})
cat(sprintf(
  "Flagged under-16 Business rows: %d | Straight-liners: %d (%.0f%% satisfied)\n",
  sum(df_clean$flag_minor_business), sum(df_clean$flag_straight_liner),
  sat_rate(df_clean$satisfaction[df_clean$flag_straight_liner])
))
write_csv(flag_check, "output/tables/11_data_quality_flag_sensitivity.csv")
show_table("RQ2 with flagged rows excluded", flag_check)

banner("S6: RQ4 SENSITIVITY - IMPUTATION METHOD AND EXTREME OUTLIERS")
# Re-compute the RQ4 effect sizes under five versions of arrival delay.
# If r_rb and Cohen's d stay 'small' in every version, the RQ4 conclusion is
# robust to how I handled missing values and outliers.
p99 <- quantile(df_clean$arrival_delay, 0.99)
# Keep only the chosen delay column (renamed "delay") plus satisfaction
delay_version <- function(d, col) transmute(d, delay = .data[[col]], satisfaction)
rq4_versions <- list(
  "Regression-imputed (primary)"     = delay_version(df_clean, "arrival_delay"),
  "Median-imputed (original method)" = delay_version(df_clean, "arrival_delay_median_imp"),
  "Complete cases only"              = df_clean |> filter(!is.na(arrival_delay_raw)) |> delay_version("arrival_delay_raw"),
  "Capped at 99th percentile"        = delay_version(df_clean, "arrival_delay_capped"),
  "Excluding > 99th percentile"      = df_clean |> filter(arrival_delay <= p99) |> delay_version("arrival_delay")
)
rq4_sens <- imap_dfr(rq4_versions, function(d, label) {
  x <- d$delay[d$satisfaction == "Neutral or Dissatisfied"]
  y <- d$delay[d$satisfaction == "Satisfied"]
  tibble(
    version = label, n = nrow(d),
    mean_dissat = round(mean(x), 2), mean_sat = round(mean(y), 2),
    rank_biserial = round(rank_biserial(x, y), 4),
    cohens_d = round(cohens_d(x, y), 4)
  )
})
write_csv(rq4_sens, "output/tables/12_rq4_sensitivity.csv")
show_table("RQ4 effect sizes under five versions of arrival delay", rq4_sens, width = 150)

banner("S7: ZERO-TRUNCATION OF DELAYS")
# The dataset never stores negative delays, so early arrivals are recorded as
# 0. Part of the large 'On Time' spike is therefore a recording floor, not
# necessarily punctuality. I quantified the spike and how often a late
# departure still produced a 0 arrival delay (time made up en route).
# (Printed only; these numbers are quoted in the report but not saved to a table.)
trunc_info <- tibble(
  pct_arrival_zero = round(mean(df_clean$arrival_delay == 0) * 100, 2),
  pct_departure_zero = round(mean(df_clean$departure_delay == 0) * 100, 2),
  n_dep_late_arr_zero = sum(df_clean$departure_delay > 0 & df_clean$arrival_delay == 0),
  n_dep_zero_arr_late = sum(df_clean$departure_delay == 0 & df_clean$arrival_delay > 0)
)
show_table("Zero-delay spike (recording floor)", trunc_info)

banner("S8: RQ3 - IS THE COMFORT GAP A FLIGHT-LENGTH EFFECT?")
# Business-class flights are much longer than the other cabins, and
# long-haul aircraft usually have better cabins. To separate 'class' from
# 'distance' I (a) compared classes WITHIN distance bands and (b) fitted a linear
# model with log(distance) as a covariate and compared the class coefficients.
df_rq3 <- df_clean |>
  mutate(distance_band = cut(flight_distance, c(0, 800, 2000, Inf),
    labels = c("Short (<800 mi)", "Medium (800-2000 mi)", "Long (>2000 mi)")
  ))
rq3_by_band <- df_rq3 |>
  summarise(n = n(), mean_comfort = round(mean(inflight_comfort_score), 3), .by = c(distance_band, class)) |>
  arrange(distance_band, class) |>
  pivot_wider(names_from = class, values_from = c(n, mean_comfort))

band_kw <- map_dfr(levels(df_rq3$distance_band), function(b) {
  sub_df <- df_rq3 |> filter(distance_band == b)
  kw <- kruskal.test(inflight_comfort_score ~ class, data = sub_df)
  H <- as.numeric(kw$statistic)
  tibble(
    distance_band = factor(b, levels = levels(df_rq3$distance_band)),
    kw_H = round(H, 1),
    eps_sq = round(H / (nrow(sub_df) - 1), 4)
  )
})
rq3_by_band <- left_join(rq3_by_band, band_kw, by = "distance_band")

# Unadjusted vs distance-adjusted Business-class comfort premium
lm_df <- df_rq3 |> mutate(class = factor(class, ordered = FALSE))
lm_raw <- lm(inflight_comfort_score ~ class, data = lm_df)
lm_adj <- lm(inflight_comfort_score ~ class + log(flight_distance), data = lm_df)
cat(sprintf(
  "Business vs Economy comfort premium: unadjusted = %.3f | adjusted for log(distance) = %.3f\n",
  coef(lm_raw)["classBusiness"], coef(lm_adj)["classBusiness"]
))
cat(sprintf(
  "Economy Plus vs Economy: unadjusted = %.3f | adjusted = %.3f\n",
  coef(lm_raw)["classEconomy Plus"], coef(lm_adj)["classEconomy Plus"]
))
rq3_by_band <- rq3_by_band |>
  mutate(
    business_premium_unadjusted = round(coef(lm_raw)["classBusiness"], 3),
    business_premium_distance_adjusted = round(coef(lm_adj)["classBusiness"], 3)
  )
write_csv(rq3_by_band, "output/tables/13_rq3_distance_stratified.csv")
show_table("RQ3 comfort score by distance band and class", rq3_by_band, width = 150)

cat("\nScript 04_supplementary_analysis.R completed successfully.\n")
