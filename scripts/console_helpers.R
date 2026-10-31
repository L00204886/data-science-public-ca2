# ==============================================================================
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Script:      console_helpers.R
# Description: Small helpers that make the console output of scripts 01-04 and
#              run_all_ca2.R easier to read. Sourced at the top of each script.
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
