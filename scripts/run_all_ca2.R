# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      run_all_ca2.R
# Description: Master execution pipeline that runs all analysis scripts
# ==============================================================================

# Stop early unless run from the project root
if (!file.exists("data/airline_passenger_satisfaction.csv")) {
  stop("Please run this script from the project root directory.")
}

source("scripts/console_helpers.R")

banner("STARTING CA 2 DATA ANALYSIS PIPELINE")

total_t0 <- proc.time()

# Run one script with a heading before it and its timing after it
run_stage <- function(script_path, label, i, n) {
  banner(sprintf("[%s] STAGE %d of %d: %s", format(Sys.time(), "%H:%M:%S"), i, n, label))
  t0 <- proc.time()
  source(script_path)
  elapsed <- (proc.time() - t0)[["elapsed"]]
  cat(sprintf("\n>> Stage %d of %d finished in %.2f seconds\n\n", i, n, elapsed))
  elapsed
}

# Run each script in order; the later scripts read files written by the earlier ones
stages <- c(
  "01 Data Cleaning & Imputation"                = "scripts/01_data_preparation.R",
  "02 Normality & Parametric Assumptions"        = "scripts/02_normality_and_assumptions.R",
  "03 Primary Hypothesis Testing (RQ1-RQ5)"      = "scripts/03_hypothesis_testing.R",
  "04 Supplementary Robustness Checks (S1-S8)"   = "scripts/04_supplementary_analysis.R"
)
timings <- vapply(
  seq_along(stages),
  function(i) run_stage(stages[[i]], names(stages)[i], i, length(stages)),
  numeric(1)
)
names(timings) <- names(stages)

total_elapsed <- (proc.time() - total_t0)[["elapsed"]]

banner("ALL CA 2 SCRIPTS COMPLETED SUCCESSFULLY")
cat("Time per stage:\n")
cat(sprintf("  %-46s %6.2f s\n", names(timings), timings), sep = "")
cat(sprintf("  %-46s %6.2f s\n\n", "Total", total_elapsed))
