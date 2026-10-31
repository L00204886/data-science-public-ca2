# ==============================================================================
# CA2 analysis pipeline
# ------------------------------------------------------------------------------
# Module:      DATA_IT904 - Data Science (2026/27)
# Assessment:  CA 2 - Statistical Data Analysis & Hypothesis Testing
# Author:      Eamon Foy (L00204886)
# Usage (run from the project root):
#   make               show this help
#   make check         check the tools, R version, R packages and input data
#   make install-packages   install any missing R packages, then re-check
#   make run           check, then run all four analysis scripts in order
#   make prep | assumptions | tests | supplementary
#                      check, then run one script (01 runs first if its output
#                      is missing or out of date)
#
# Every target that runs R first runs `make check`, so nothing starts unless the
# prerequisites are in place.
# ==============================================================================

RSCRIPT ?= Rscript

CLEAN_RDS := output/cleaned_airline_satisfaction.rds
DATA_FILE := data/airline_passenger_satisfaction.csv

.DEFAULT_GOAL := help
.PHONY: help check check-tools check-packages install-packages run prep assumptions tests supplementary check-report-tools report

help:
	@echo "CA2 pipeline targets:"
	@echo "  make check             Check tools, R version, R packages and input data"
	@echo "  make install-packages  Install missing R packages, then re-check"
	@echo "  make run               Check, then run scripts 01-04 in order"
	@echo "  make prep              Check, then run 01_data_preparation.R"
	@echo "  make assumptions       Check, then run 02_normality_and_assumptions.R"
	@echo "  make tests             Check, then run 03_hypothesis_testing.R"
	@echo "  make supplementary     Check, then run 04_supplementary_analysis.R"

# ------------------------------ Prerequisites ---------------------------------
check: check-tools check-packages
	@:

# Command-line tools needed to run the analysis
check-tools:
	@echo "Checking tools ..."
	@command -v $(RSCRIPT) >/dev/null 2>&1 || { \
		echo "  [!!] $(RSCRIPT) not found. Install R from https://cran.r-project.org (macOS: brew install r)"; exit 1; }
	@echo "  [ok] $$($(RSCRIPT) --version 2>&1 | head -n 1)"
	@test -f $(DATA_FILE) || { \
		echo "  [!!] $(DATA_FILE) not found. Run make from the project root."; exit 1; }

# R version, R packages (with minimum versions) and the input data
check-packages: check-tools
	@echo ""
	@echo "Checking R packages ..."
	@$(RSCRIPT) scripts/check_prerequisites.R

install-packages: check-tools
	@echo "Installing missing R packages ..."
	@$(RSCRIPT) scripts/check_prerequisites.R --install

# ------------------------------ Analysis scripts ------------------------------
# Script 01 writes the cleaned dataset that scripts 02-04 read. `| check` is an
# order-only prerequisite: the check always runs first but does not force a rebuild.
$(CLEAN_RDS): $(DATA_FILE) scripts/01_data_preparation.R | check
	@$(RSCRIPT) scripts/01_data_preparation.R

run: check
	@echo ""
	@echo "Running the analysis (scripts 01-04) ..."
	@$(RSCRIPT) scripts/run_all_ca2.R

prep: check
	@$(RSCRIPT) scripts/01_data_preparation.R

assumptions: $(CLEAN_RDS) | check
	@$(RSCRIPT) scripts/02_normality_and_assumptions.R

tests: $(CLEAN_RDS) | check
	@$(RSCRIPT) scripts/03_hypothesis_testing.R

supplementary: $(CLEAN_RDS) | check
	@$(RSCRIPT) scripts/04_supplementary_analysis.R


