# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      check_prerequisites.R
# Description: Run by `make` before any analysis script. Checks that
#                1. R is new enough (the scripts use the native pipe |> and
#                   dplyr's .by / pick(), which need R >= 4.1 and dplyr >= 1.1.0)
#                2. every R package the scripts load is installed (and recent enough)
#                3. the input data file is in place
#              With --install it first installs any missing or outdated packages.
# Usage:       Rscript scripts/check_prerequisites.R [--install]
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
do_install <- "--install" %in% args

# Package -> minimum version ("0" means any version). These are the packages
# loaded with library() in scripts 01-04.
required_packages <- c(
  readr = "0", dplyr = "1.1.0", tidyr = "0", stringr = "0",
  purrr = "0", e1071 = "0", psych = "0"
)
min_r_version <- "4.1.0"
data_file <- "data/airline_passenger_satisfaction.csv"

problems <- character()

# 1. R version ---------------------------------------------------------------
if (getRversion() < min_r_version) {
  problems <- c(problems, sprintf("R %s or newer is required (found %s)", min_r_version, getRversion()))
} else {
  cat(sprintf("  [ok] R %s\n", getRversion()))
}

# 2. Packages ----------------------------------------------------------------
# Returns the packages that are missing or older than the minimum version
find_package_problems <- function() {
  bad <- character()
  for (pkg in names(required_packages)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      bad[pkg] <- "not installed"
    } else if (packageVersion(pkg) < required_packages[[pkg]]) {
      bad[pkg] <- sprintf("version %s is too old (need >= %s)", packageVersion(pkg), required_packages[[pkg]])
    }
  }
  bad
}

pkg_problems <- find_package_problems()

if (length(pkg_problems) > 0 && do_install) {
  cat(sprintf("  Installing: %s\n", paste(names(pkg_problems), collapse = ", ")))
  install.packages(names(pkg_problems), repos = "https://cloud.r-project.org")
  pkg_problems <- find_package_problems()
}

for (pkg in names(required_packages)) {
  if (pkg %in% names(pkg_problems)) {
    cat(sprintf("  [!!] %-8s %s\n", pkg, pkg_problems[[pkg]]))
  } else {
    cat(sprintf("  [ok] %-8s %s\n", pkg, as.character(packageVersion(pkg))))
  }
}
if (length(pkg_problems) > 0) {
  problems <- c(problems, sprintf("R packages missing or outdated: %s", paste(names(pkg_problems), collapse = ", ")))
}

# 3. Input data --------------------------------------------------------------
if (!file.exists(data_file)) {
  problems <- c(problems, sprintf("input data not found: %s (run make from the project root)", data_file))
} else {
  cat(sprintf("  [ok] data file %s\n", data_file))
}

# Result ---------------------------------------------------------------------
if (length(problems) > 0) {
  cat("\nPrerequisite check FAILED:\n")
  cat(paste0("  - ", problems, collapse = "\n"), "\n")
  if (!do_install && length(pkg_problems) > 0) cat("\nFix the packages with:  make install-packages\n")
  quit(status = 1)
}
cat("\nAll prerequisites are in place.\n")
