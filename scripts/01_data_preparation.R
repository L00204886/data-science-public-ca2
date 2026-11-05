# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      01_data_preparation.R
# Description: Step 1 of the pipeline. Loads the raw survey CSV and prepares it
#              for analysis:
#                1. Audit missing (NA) values and "0" (Not Applicable) ratings
#                2. Clean: regression-impute arrival_delay, recode 0 -> NA (keeping
#                   indicator flags), flag implausible rows, build new variables
#                3. Check composite reliability and save the
#                   descriptive tables
#                4. Draw the exploratory charts (Figures 1-4)
# Reads:       data/airline_passenger_satisfaction.csv
# Writes:      output/cleaned_airline_satisfaction.rds (used by scripts 02-04),
#              output/tables/01-03b, output/figures/fig01-fig04
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(e1071)
  library(psych)
})

source("scripts/console_helpers.R")

# Ensure output directories exist
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

banner("STEP 1: DATA LOADING & STRUCTURAL HYGIENE AUDIT")

raw_df <- read_csv("data/airline_passenger_satisfaction.csv", show_col_types = FALSE)
cat(sprintf(
  "Raw Dataset Dimensions: %s rows x %d columns\n",
  format(nrow(raw_df), big.mark = ","), ncol(raw_df)
))

# Standardise column names to snake_case
clean_col_names <- function(nms) {
  nms |>
    str_to_lower() |>
    str_replace_all("in-flight", "inflight") |>
    str_replace_all("on-board", "onboard") |>
    str_replace_all("check-in", "checkin") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

df <- raw_df
colnames(df) <- clean_col_names(colnames(df))

# Check for duplicate passenger IDs
dup_ids <- sum(duplicated(df$id))
cat(sprintf("Duplicate Passenger IDs: %d\n", dup_ids))

# Audit raw missing (NA) and "0" (Not Applicable) codes in Likert ratings
likert_cols <- c(
  "departure_and_arrival_time_convenience",
  "ease_of_online_booking",
  "checkin_service",
  "online_boarding",
  "gate_location",
  "onboard_service",
  "seat_comfort",
  "leg_room_service",
  "cleanliness",
  "food_and_drink",
  "inflight_service",
  "inflight_wifi_service",
  "inflight_entertainment",
  "baggage_handling"
)
# Create hygiene audit tibble with raw column names and NA/zero counts
hygiene_audit <- tibble(
  variable = colnames(df),
  raw_name = colnames(raw_df),
  r_class_raw = map_chr(df, ~ class(.x)[1]),
  na_count = map_int(df, ~ sum(is.na(.x))),
  na_pct = round(na_count / nrow(df) * 100, 3),
  zero_count = map_int(colnames(df), function(col) {
    if (col %in% likert_cols) sum(df[[col]] == 0, na.rm = TRUE) else 0L
  }),
  zero_pct = round(zero_count / nrow(df) * 100, 3)
)

# Display hygiene audit and save to file
write_csv(hygiene_audit, "output/tables/01_hygiene_audit.csv")
show_table("Columns with missing (NA) or Not Applicable (0) values",
  hygiene_audit |> filter(na_count > 0 | zero_count > 0),
  n = 25
)

banner("STEP 2: CLEANING, IMPUTATION & FEATURE ENGINEERING")

# Drop non-predictive identifier column (id)
df_clean <- df |> select(-id)

# Handle the 393 missing values in arrival_delay
# The median (0 mins) would wrongly label late flights as "On Time": these 393
# flights have a median departure delay of 8 mins. Departure and arrival delay
# are almost perfectly linear (r ~ 0.965, see RQ5), so I impute by regression:
#     arrival_delay = b0 + b1 * departure_delay
# fitted on the complete rows, rounded and floored at 0 (the data has no
# negative delays). The raw column is kept as arrival_delay_raw (for RQ5) and a
# median-imputed copy as arrival_delay_median_imp (for the sensitivity check in
# script 04).
median_arr_delay <- median(df_clean$arrival_delay, na.rm = TRUE)
mean_arr_delay <- mean(df_clean$arrival_delay, na.rm = TRUE)
cat(sprintf(
  "Arrival Delay (Raw) -> Mean: %.2f mins | Median: %.1f mins | SD: %.2f mins | Skewness: %.2f\n",
  mean_arr_delay, median_arr_delay,
  sd(df_clean$arrival_delay, na.rm = TRUE),
  e1071::skewness(df_clean$arrival_delay, na.rm = TRUE)
))

# Profile the departure delay of the rows that are missing an arrival delay
na_arr_dep <- df_clean$departure_delay[is.na(df_clean$arrival_delay)]
cat(sprintf(
  "Departure delay of the %d rows with missing arrival delay -> Median: %.1f | Mean: %.2f mins\n",
  length(na_arr_dep), median(na_arr_dep), mean(na_arr_dep)
))

# Create a linear regression model to predict arrival delay based on departure delay
impute_model <- lm(arrival_delay ~ departure_delay, data = df_clean)
b_imp <- coef(impute_model)
cat(sprintf(
  "Imputation model: arrival_delay = %.3f + %.3f * departure_delay (R^2 = %.4f)\n",
  b_imp[1], b_imp[2], summary(impute_model)$r.squared
))

# Perform the imputation
df_clean <- df_clean |>
  mutate(
    arrival_delay_raw = arrival_delay, # untouched original (393 NAs)
    arrival_delay_median_imp = if_else(is.na(arrival_delay), median_arr_delay, arrival_delay),
    arrival_delay_imputed_flag = is.na(arrival_delay), # TRUE = value was imputed
    arrival_delay = if_else(
      is.na(arrival_delay),
      pmax(0, round(b_imp[1] + b_imp[2] * departure_delay)),
      arrival_delay
    )
  )
cat(sprintf(
  "Regression-imputed values (n = %d) -> Median: %.1f | Mean: %.2f mins\n",
  sum(df_clean$arrival_delay_imputed_flag),
  median(df_clean$arrival_delay[df_clean$arrival_delay_imputed_flag]),
  mean(df_clean$arrival_delay[df_clean$arrival_delay_imputed_flag])
))

# Recode 0 ("Not Applicable") in the 14 Likert survey columns to NA
# A 0 means the passenger did not use the service; left as a number it would
# drag averages down. Skipping is not random though (Wi-Fi skippers are ~99.7%
# satisfied, see script 04), so I first keep a TRUE/FALSE "<item>_na" indicator
# for each item plus a per-passenger count of skipped items.
df_clean <- df_clean |>
  mutate(
    # One indicator column per Likert item, e.g. inflight_wifi_service_na
    across(all_of(likert_cols), ~ .x == 0, .names = "{.col}_na"),
    # Recode the 0s themselves to NA using native dplyr::na_if
    across(all_of(likert_cols), ~ na_if(as.numeric(.x), 0))
  ) |>
  mutate(n_not_applicable = rowSums(pick(ends_with("_na"))))

cat(sprintf(
  "\n'Not Applicable' answers: a rating of 0 means the passenger did not use that service.\nThese 0s were recoded to NA across the %d survey rating items.\n",
  length(likert_cols)
))

# Table : how many items each passenger skipped (0 = answered every item)
skip_counts <- table(df_clean$n_not_applicable)
skip_summary <- data.frame(
  `Items marked N/A` = names(skip_counts),
  Passengers = format(as.integer(skip_counts), big.mark = ","),
  `% of passengers` = sprintf("%.1f%%", 100 * as.integer(skip_counts) / nrow(df_clean)),
  check.names = FALSE
)
show_table("Passengers by how many of their survey items were 'Not Applicable'",
  skip_summary,
  row.names = FALSE
)

# Add summary info to the 'Not Applicable' section
n_any_skip <- sum(df_clean$n_not_applicable > 0)
cat(sprintf(
  "-> %s of %s passengers (%.1f%%) marked at least one item N/A; no one marked more than %d.\n",
  format(n_any_skip, big.mark = ","), format(nrow(df_clean), big.mark = ","),
  100 * n_any_skip / nrow(df_clean), max(df_clean$n_not_applicable)
))

# Table : which survey items were marked N/A, most often first
item_na <- sort(colSums(df_clean[paste0(likert_cols, "_na")]), decreasing = TRUE)
item_na <- item_na[item_na > 0]
item_pct <- 100 * as.integer(item_na) / nrow(df_clean)
item_summary <- data.frame(
  `Survey item` = sub("_na$", "", names(item_na)),
  `Passengers marking N/A` = format(as.integer(item_na), big.mark = ","),
  `% of passengers` = ifelse(item_pct < 0.01, "<0.01%", sprintf("%.2f%%", item_pct)),
  check.names = FALSE
)
show_table("Survey items most often marked 'Not Applicable'", item_summary, row.names = FALSE)

# Data-quality flags (rows are flagged, not deleted)
# - flag_minor_business: under-16s recorded as Business travel or class
#   (implausible, possibly data-entry errors).
# - flag_straight_liner: same rating given to every answered item (low effort).
# Their effect on the results is checked in script 04.
# Vectorized row-comparison via pmin/pmax
likert_sub <- df_clean[, likert_cols]
n_answered <- rowSums(!is.na(likert_sub))
r_min <- do.call(pmin, c(likert_sub, na.rm = TRUE))
r_max <- do.call(pmax, c(likert_sub, na.rm = TRUE))

# Apply the flags to remove implausible rows
df_clean <- df_clean |>
  mutate(
    flag_minor_business = age < 16 & (type_of_travel == "Business" | class == "Business"),
    flag_straight_liner = (n_answered > 1) & (r_min == r_max)
  )
cat(sprintf(
  "Flagged: %d under-16 Business travellers/class | %d straight-liners\n",
  sum(df_clean$flag_minor_business), sum(df_clean$flag_straight_liner)
))

# Capped arrival delay for outlier sensitivity checks
# ------------------------------------------------------------------------------
# Around 300 flights have delays > 5 hours (max 1,584 mins). Rank-based tests are
# robust to these, but means and t-tests are not. I created a copy capped at the
# 99th percentile so RQ4 can be re-run without extreme outlier leverage.
# ------------------------------------------------------------------------------
cap_p99 <- quantile(df_clean$arrival_delay, 0.99)
df_clean <- df_clean |>
  mutate(arrival_delay_capped = pmin(arrival_delay, cap_p99))
cat(sprintf(
  "Arrival delay 99th percentile cap = %.0f mins (%d flights capped)\n",
  cap_p99, sum(df_clean$arrival_delay > cap_p99)
))

# Feature Engineering
# - inflight_comfort_score: composite mean of Seat Comfort, Leg Room Service, Cleanliness, Inflight Entertainment (RQ3)
# - digital_service_score: composite mean of Ease of Online Booking, Online Boarding, Inflight Wifi Service
# - ground_service_score: composite mean of Gate Location, Checkin Service, Baggage Handling, Time Convenience
# - delay_category: FAA standard 15-minute operational delay threshold (RQ4)
# - age_group: Demographic life-stage brackets
# NOTE ON NaN: rowMeans(..., na.rm = TRUE) returns NaN ("not a number") when
# EVERY item in the row is NA (it divides 0 by 0). 1,603 passengers skipped all
# three digital items, so I converted NaN -> NA so R treats them as ordinary
# missing values in later tests and summaries.
nan_to_na <- function(x) if_else(is.nan(x), NA_real_, x)


# Create composite scores for inflight, digital, and ground services
# and delay category, and age groups for later RQ's
df_clean <- df_clean |>
  mutate(
    inflight_comfort_score = nan_to_na(rowMeans(
      pick(seat_comfort, leg_room_service, cleanliness, inflight_entertainment),
      na.rm = TRUE
    )),
    digital_service_score = nan_to_na(rowMeans(
      pick(ease_of_online_booking, online_boarding, inflight_wifi_service),
      na.rm = TRUE
    )),
    ground_service_score = nan_to_na(rowMeans(
      pick(departure_and_arrival_time_convenience, gate_location, checkin_service, baggage_handling),
      na.rm = TRUE
    )),
    delay_category = case_when(
      arrival_delay == 0 ~ "On Time (0 mins)",
      arrival_delay <= 15 ~ "Minor Delay (1-15 mins)",
      arrival_delay > 15 ~ "Major Delay (>15 mins)",
      .default = NA_character_
    ) |> factor(levels = c("On Time (0 mins)", "Minor Delay (1-15 mins)", "Major Delay (>15 mins)"), ordered = TRUE),
    age_group = case_when(
      age < 25 ~ "Under 25",
      age <= 40 ~ "25-40",
      age <= 60 ~ "41-60",
      .default = "Over 60"
    ) |> factor(levels = c("Under 25", "25-40", "41-60", "Over 60"), ordered = TRUE)
  )

# Encode Nominal and Ordered Categorical Factors for later RQ's
df_clean <- df_clean |>
  mutate(
    gender = factor(gender, levels = c("Female", "Male")),
    customer_type = factor(customer_type, levels = c("First-time", "Returning")),
    type_of_travel = factor(type_of_travel, levels = c("Personal", "Business")),
    class = factor(class, levels = c("Economy", "Economy Plus", "Business"), ordered = TRUE),
    satisfaction = factor(satisfaction, levels = c("Neutral or Dissatisfied", "Satisfied"))
  )

cat("Engineered Variables Summary:\n")
print(summary(df_clean |> select(inflight_comfort_score, digital_service_score, ground_service_score, delay_category)))

# Internal-consistency reliability of the composite scores
# ------------------------------------------------------------------------------
# Averaging several survey items into one score only makes sense if the items
# measure the same underlying idea. Cronbach's alpha checks this:
#   alpha >= 0.70  -> acceptable reliability
#   alpha <  0.70  -> items do not hang together; the composite is unreliable
# I also tested a reduced 2 item "ground handling" alternative (check-in +
# baggage) because the original 4 item ground score was expected to be weak.
# ------------------------------------------------------------------------------
composite_items <- list(
  inflight_comfort_score = c("seat_comfort", "leg_room_service", "cleanliness", "inflight_entertainment"),
  digital_service_score  = c("ease_of_online_booking", "online_boarding", "inflight_wifi_service"),
  ground_service_score   = c("departure_and_arrival_time_convenience", "gate_location", "checkin_service", "baggage_handling"),
  ground_handling_2item  = c("checkin_service", "baggage_handling")
)
reliability_table <- imap_dfr(composite_items, function(items, name) {
  a <- suppressWarnings(psych::alpha(df_clean[items], warnings = FALSE))
  tibble(
    composite = name,
    n_items = length(items), # count items BEFORE 'items' is turned into a label
    items = paste(items, collapse = " + "),
    cronbach_alpha = round(a$total$raw_alpha, 3),
    verdict = if_else(a$total$raw_alpha >= 0.70, "Acceptable (>= 0.70)", "Unreliable (< 0.70)")
  )
})
write_csv(reliability_table, "output/tables/03b_composite_reliability.csv")
show_table("Composite score reliability (Cronbach's alpha)", reliability_table)

# Descriptive Statistics Table
desc_vars <- c(
  "age", "flight_distance", "departure_delay", "arrival_delay",
  "online_boarding", "ease_of_online_booking", "inflight_wifi_service",
  "seat_comfort", "leg_room_service", "cleanliness", "inflight_entertainment",
  "onboard_service", "inflight_service", "baggage_handling", "checkin_service",
  "gate_location", "food_and_drink", "departure_and_arrival_time_convenience",
  "inflight_comfort_score", "digital_service_score", "ground_service_score"
)

# Creating descriptive statistics table for the continuous variables
# for RQ1-RQ4 and general descriptives
desc_table <- map_dfr(desc_vars, function(v) {
  x <- df_clean[[v]]
  x_valid <- x[!is.na(x)]
  tibble(
    variable = v,
    n_valid = length(x_valid),
    n_missing_or_na = sum(is.na(x)),
    mean = round(mean(x_valid), 2),
    sd = round(sd(x_valid), 2),
    median = round(median(x_valid), 2),
    iqr = round(IQR(x_valid), 2),
    min = round(min(x_valid), 2),
    max = round(max(x_valid), 2),
    skewness = round(e1071::skewness(x_valid), 2),
    kurtosis = round(e1071::kurtosis(x_valid), 2)
  )
})

write_csv(desc_table, "output/tables/02_descriptive_statistics.csv")
cat("\nDescriptive Statistics Table saved to output/tables/02_descriptive_statistics.csv\n")
show_table("Descriptive statistics", desc_table, n = 25)

# Categorical Frequency Summary Table
cat_vars <- c("satisfaction", "type_of_travel", "class", "customer_type", "gender", "delay_category", "age_group")
cat_summary <- map_dfr(cat_vars, function(v) {
  df_clean |>
    count(level = as.character(.data[[v]]), name = "count") |>
    mutate(
      variable = v,
      percentage = round(count / sum(count) * 100, 2)
    ) |>
    select(variable, level, count, percentage)
})
write_csv(cat_summary, "output/tables/03_categorical_frequencies.csv")
show_table("Frequency of each category", cat_summary, n = 25)

# Save cleaned dataset for subsequent scripts
saveRDS(df_clean, "output/cleaned_airline_satisfaction.rds")

banner("STEP 3: GENERATING EXPLORATORY DATA ANALYSIS (EDA) CHARTS")

# Palette definitions (Colorblind-safe Navy / Teal / Coral / Slate)
col_dissat <- "#D95F02" # Warm terracotta/orange for Neutral/Dissatisfied
col_sat <- "#1B9E77" # Deep teal/green for Satisfied
col_navy <- "#2C3E50"
col_blue <- "#2980B9"
col_slate <- "#7F8C8D"

# Figures are saved with save_figure_png() from scripts/console_helpers.R

# ------------------------------------------------------------------------------
# Figure 1: Data Hygiene Audit - "Not Applicable" (0) & Missing Value Counts
# ------------------------------------------------------------------------------
save_figure_png("fig01_missing_and_zeros_audit.png", width = 10, height = 5.5, plot_fn = function() {
  par(mar = c(5, 14, 3.5, 5), family = "sans")
  audit_sub <- hygiene_audit |>
    mutate(total_issue = na_count + zero_count) |>
    filter(total_issue > 0) |>
    arrange(total_issue)

  bp <- barplot(
    audit_sub$total_issue,
    names.arg = audit_sub$raw_name,
    horiz = TRUE,
    las = 1,
    col = ifelse(audit_sub$na_count > 0, "#E74C3C", "#3498DB"),
    border = NA,
    xlim = c(0, max(audit_sub$total_issue) * 1.28),
    main = sprintf(
      "Data Hygiene Audit: Non-Applicable ('0') Ratings & Missing Values (N = %s)",
      format(nrow(df_clean), big.mark = ",")
    ),
    xlab = "Number of Records Requiring Recoding / Imputation",
    cex.names = 0.85,
    cex.main = 1.05
  )
  grid(nx = NULL, ny = NA, col = "gray88", lty = "dotted")
  text(
    x = audit_sub$total_issue + 120,
    y = bp,
    labels = sprintf(
      "%s (%.2f%%)", format(audit_sub$total_issue, big.mark = ","),
      audit_sub$na_pct + audit_sub$zero_pct
    ),
    pos = 4, cex = 0.8, col = "gray20", font = 2
  )
  legend(
    "bottomright",
    legend = c("Likert '0' ('Not Applicable' -> Recoded to NA)", "Explicit Missing Value (NA -> Regression Imputed from Departure Delay)"),
    fill = c("#3498DB", "#E74C3C"),
    border = NA, bty = "n", cex = 0.85
  )
})

# ------------------------------------------------------------------------------
# Figure 2: Diverging Mean Survey Ratings by Passenger Satisfaction Group
# ------------------------------------------------------------------------------
save_figure_png("fig02_service_ratings_by_satisfaction.png", width = 10, height = 6.2, plot_fn = function() {
  par(mar = c(5, 14.5, 3.5, 2), family = "sans")
  means_df <- df_clean |>
    summarise(across(all_of(likert_cols), ~ mean(.x, na.rm = TRUE)), .by = satisfaction) |>
    pivot_longer(-satisfaction, names_to = "variable", values_to = "mean_score") |>
    pivot_wider(names_from = satisfaction, values_from = mean_score) |>
    rename(sat_mean = `Satisfied`, dissat_mean = `Neutral or Dissatisfied`) |>
    mutate(
      label = hygiene_audit$raw_name[match(variable, hygiene_audit$variable)],
      gap = sat_mean - dissat_mean
    ) |>
    arrange(gap)

  y_pos <- seq_len(nrow(means_df))
  plot(
    NULL,
    xlim = c(2.2, 4.5), ylim = c(0.5, nrow(means_df) + 0.8),
    yaxt = "n", ylab = "", xlab = "Mean Rating Score (1 = Lowest to 5 = Highest, '0' Excluded)",
    main = "Service Touchpoint Gap Analysis: Satisfied vs. Neutral/Dissatisfied Passengers",
    cex.main = 1.05
  )
  grid(nx = NULL, ny = NULL, col = "gray90", lty = "dotted")
  axis(2, at = y_pos, labels = means_df$label, las = 1, cex.axis = 0.85)

  # Draw connecting segments (dumbbell plot)
  segments(means_df$dissat_mean, y_pos, means_df$sat_mean, y_pos, col = "gray55", lwd = 2.5)
  points(means_df$dissat_mean, y_pos, pch = 19, col = col_dissat, cex = 1.4)
  points(means_df$sat_mean, y_pos, pch = 19, col = col_sat, cex = 1.4)

  # Annotate gap values
  text(
    x = (means_df$dissat_mean + means_df$sat_mean) / 2,
    y = y_pos + 0.30,
    labels = sprintf("%+.2f", means_df$gap),
    cex = 0.75, col = "gray20", font = 2
  )

  legend(
    "bottomright",
    legend = sprintf(
      c("Neutral or Dissatisfied (n = %s)", "Satisfied (n = %s)"),
      format(table(df_clean$satisfaction), big.mark = ",")
    ),
    col = c(col_dissat, col_sat), pch = 19, pt.cex = 1.3, bty = "n", cex = 0.88
  )
})

# ------------------------------------------------------------------------------
# Figure 3: Multi-Panel Demographic & Travel Regime Breakdown (a 2x2 Panel)
# ------------------------------------------------------------------------------
save_figure_png("fig03_categorical_satisfaction_breakdown.png", width = 10, height = 7, plot_fn = function() {
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), family = "sans")

  # Panel A: Overall Target Balance
  tab_ov <- table(df_clean$satisfaction)
  pct_ov <- prop.table(tab_ov) * 100
  bp1 <- barplot(
    pct_ov,
    col = c(col_dissat, col_sat), border = NA, ylim = c(0, 70),
    main = "A. Overall Target Class Balance", ylab = "Percentage of Passengers (%)"
  )
  text(bp1, pct_ov + 3.5, labels = sprintf("%s\n(%.1f%%)", format(as.numeric(tab_ov), big.mark = ","), pct_ov), cex = 0.85, font = 2)

  # Panel B: Type of Travel (RQ2)
  tab_tr <- prop.table(table(df_clean$satisfaction, df_clean$type_of_travel), margin = 2) * 100
  bp2 <- barplot(
    tab_tr,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 105),
    main = "B. Satisfaction by Type of Travel (RQ2)", ylab = "Percentage within Group (%)"
  )
  text(bp2, tab_tr + 4.5, labels = sprintf("%.1f%%", tab_tr), cex = 0.82, font = 2)

  # Panel C: Cabin Class (RQ3 context)
  tab_cl <- prop.table(table(df_clean$satisfaction, df_clean$class), margin = 2) * 100
  bp3 <- barplot(
    tab_cl,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 100),
    main = "C. Satisfaction by Cabin Class Tier", ylab = "Percentage within Class (%)"
  )
  text(bp3, tab_cl + 4.5, labels = sprintf("%.1f%%", tab_cl), cex = 0.82, font = 2)

  # Panel D: Engineered Delay Category (RQ4 context)
  tab_dl <- prop.table(table(df_clean$satisfaction, df_clean$delay_category), margin = 2) * 100
  bp4 <- barplot(
    tab_dl,
    beside = TRUE, col = c(col_dissat, col_sat), border = NA, ylim = c(0, 85),
    names.arg = c("On Time\n(0m)", "Minor Delay\n(1-15m)", "Major Delay\n(>15m)"),
    main = "D. Satisfaction by Arrival Delay Regime (RQ4)", ylab = "Percentage within Regime (%)"
  )
  text(bp4, tab_dl + 4, labels = sprintf("%.1f%%", tab_dl), cex = 0.80, font = 2)
  legend("topright", legend = c("Neutral/Dissatisfied", "Satisfied"), fill = c(col_dissat, col_sat), border = NA, bty = "n", cex = 0.8)
})

# ------------------------------------------------------------------------------
# Figure 4: Distributions of Continuous & Engineered Variables
# ------------------------------------------------------------------------------
save_figure_png("fig04_continuous_distributions.png", width = 10, height = 7, plot_fn = function() {
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), family = "sans")

  # A: Passenger Age
  hist(df_clean$age,
    breaks = 35, col = "#3498DB", border = "white",
    main = "A. Passenger Age Distribution (Years)", xlab = "Age (Years)", ylab = "Frequency"
  )
  abline(v = mean(df_clean$age), col = "#E74C3C", lwd = 2, lty = 2)
  legend("topright", legend = sprintf(
    "Mean = %.1f yrs\nMedian = %.0f yrs\nSkew = %.2f",
    mean(df_clean$age), median(df_clean$age), e1071::skewness(df_clean$age)
  ), bty = "n", cex = 0.85)

  # B: Flight Distance
  hist(df_clean$flight_distance,
    breaks = 40, col = "#2ECC71", border = "white",
    main = "B. Flight Distance Distribution (Miles)", xlab = "Flight Distance (Miles)", ylab = "Frequency"
  )
  abline(v = median(df_clean$flight_distance), col = "#E74C3C", lwd = 2, lty = 2)
  legend("topright", legend = sprintf(
    "Mean = %.0f mi\nMedian = %.0f mi\nSkew = %.2f",
    mean(df_clean$flight_distance), median(df_clean$flight_distance), e1071::skewness(df_clean$flight_distance)
  ), bty = "n", cex = 0.85)

  # C: Log10(Arrival Delay + 1) to show extreme right skew
  hist(log10(df_clean$arrival_delay + 1),
    breaks = 35, col = "#E67E22", border = "white",
    main = "C. Arrival Delay (log scale)", xlab = "log10(Arrival Delay + 1)", ylab = "Frequency"
  )
  legend("topright", legend = sprintf(
    "Raw Mean = %.1f m\nRaw Median = %.0f m\nRaw Skew = %.2f",
    mean(df_clean$arrival_delay), median(df_clean$arrival_delay), e1071::skewness(df_clean$arrival_delay)
  ), bty = "n", cex = 0.85)

  # D: Engineered In-Flight Comfort Score by Class (Boxplot)
  boxplot(inflight_comfort_score ~ class,
    data = df_clean,
    col = c("#BDC3C7", "#5DADE2", "#1B9E77"),
    main = "D. Engineered In-Flight Comfort Score by Class",
    xlab = "Cabin Class Tier", ylab = "Composite Score (1-5 Scale)", outline = FALSE
  )
  grid(nx = NA, ny = NULL, col = "gray88", lty = "dotted")
})

cat("\nScript 01_data_preparation.R completed successfully.\n")
