# CA2: Statistical Analysis and Hypothesis Testing of Airline Passenger Satisfaction

**Module:** DATA_IT904 - Data Science (2026/27)
**Assessment:** CA 2 - Data Analysis (30% of the module)
**Author:** Eamon Foy (L00204886)
**Repository:** [https://github.com/eamonfoy/CA2](https://github.com/eamonfoy/CA2)

This repository holds the R scripts, outputs, tables/figures  for CA 2.
All analysis is done in R. 

## What the assignment covers

The brief asks for a full data analysis of an assigned dataset: clean it, describe
it, choose suitable statistical tests, run them in R and interpret the results. The
dataset is the Kaggle *Airline Passenger Satisfaction* survey (Bhat, 2022), with
**129,880 passengers** and **24 columns**, including 14 service ratings on a 1-5 scale.

Five research questions are tested, each with a different kind of variable:

| RQ  | Question                                                                          | Test used                                     |
| :-- | :-------------------------------------------------------------------------------- | :-------------------------------------------- |
| RQ1 | Do satisfied passengers rate **online boarding** higher?                    | Wilcoxon rank-sum (Welch's t-test as a check) |
| RQ2 | Is **type of travel** (business/personal) linked to satisfaction?           | Chi-square, Cramer's V, odds ratio            |
| RQ3 | Does in-flight **comfort** differ across the three **cabin classes**? | Kruskal-Wallis with Bonferroni post-hoc tests |
| RQ4 | Do dissatisfied passengers have longer **arrival delays**?                  | Wilcoxon rank-sum (Welch's t-test as a check) |
| RQ5 | Are **departure and arrival delay** collinear?                              | Pearson and Spearman correlation, VIF         |

The pipeline follows the report sections in the brief:

1. **Data preparation:** audit missing values and "0 = Not Applicable" ratings,
   impute 393 missing arrival delays by regression, build comfort and delay
   variables, check composite reliability (Cronbach's alpha), and draw exploratory charts.
2. **Statistical testing:** check normality (Shapiro-Wilk, Kolmogorov-Smirnov, Q-Q
   plots) and equal variance (Bartlett, Fligner-Killeen) to justify rank-based tests.
3. **Hypothesis testing and results:** run the five tests and report effect sizes
   alongside p-values, because at this sample size every p-value is tiny.
4. **Robustness checks:** whether travel type is confounded by cabin class, whether
   the delay result survives different imputation and outlier choices, and whether the
   comfort gap is just a flight-length effect.

**High level results**

- Online boarding is the strongest differentiator (rank-biserial r = 0.669).
- Business travellers have 12.44 times the odds of being satisfied, falling to 5.13 once cabin class is controlled for.
- Business class is more comfortable than Economy, but Economy Plus is no better than Economy.
- Arrival delay has only a small effect on satisfaction (r = 0.106).
- Departure and arrival delay are almost the same measurement (r = 0.965, VIF = 14.66).

## Project layout

```
CA2/
├── Makefile                     run the pipeline (see below)
├── data/                        input data, data dictionary and source URL
├── scripts/
│   ├── check_prerequisites.R    checks R version, packages and data file
│   ├── console_helpers.R        headings and table formatting for console output
│   ├── 01_data_preparation.R    cleaning, new variables, descriptive tables, Figures 1-4
│   ├── 02_normality_and_assumptions.R   normality and variance tests, Figure 5
│   ├── 03_hypothesis_testing.R  the five hypothesis tests, Figures 6-8
│   ├── 04_supplementary_analysis.R      robustness checks (S1-S8)
│   └── run_all_ca2.R            runs scripts 01-04 in order
├── output/
│   ├── cleaned_airline_satisfaction.rds   cleaned data used by scripts 02-04
│   ├── tables/                  CSV results
│   └── figures/                 PNG charts used in the report
```

---

## Setup

You need **R** (version 4.1 or newer) and, to use the `Makefile`, **make**. R
packages are installed for you by `make install-packages`.

### macOS

1. **Install R.** Either download the installer from [https://cran.r-project.org/bin/macosx/](https://cran.r-project.org/bin/macosx/)
   (choose the build for your Mac: Apple silicon or Intel), or use Homebrew:
   ```bash
   brew install r
   ```
2. **make** is included with the Xcode command line tools. If `make --version` says
   it is not found, run:
   ```bash
   xcode-select --install
   ```
3. Check both are available:
   ```bash
   Rscript --version
   make --version
   ```

### Windows

1. **Install R** from [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/) and run the installer.
2. **Put `Rscript` on your PATH.** The installer does not always do this. Add the folder
   that contains `Rscript.exe`, for example `C:\Program Files\R\R-4.5.0\bin`, to your
   PATH (Start menu: "Edit the system environment variables" > Environment Variables >
   Path > Edit). Open a **new** terminal afterwards and check:
   ```
   Rscript --version
   ```
3. **Get `make`.** Windows does not include it. Pick one of these:
   - **WSL (recommended):** in PowerShell run `wsl --install`, restart, then install R
     and make *inside* Ubuntu with `sudo apt update && sudo apt install r-base make`.
     Work in the Linux terminal from then on. (Steps 1 and 2 above are then not needed.)
   - **Git Bash + make:** install [Git for Windows](https://git-scm.com/download/win), then
     install make with [Chocolatey](https://chocolatey.org/) (`choco install make`) or
     [Scoop](https://scoop.sh/) (`scoop install make`). Run the commands below from **Git Bash**.
   - **No make at all:** skip `make` and run the two R commands shown under
     "Run without make" below. This works in PowerShell or Command Prompt.

> `make` is only a convenience. Everything it does can be done with `Rscript`.

---

## Run the analysis

Run every command from the **project root** (the folder that contains `Makefile`).

```bash
make                     # list the available targets
make install-packages    # one-off: install any missing R packages
make check               # check R, packages and the data file
make run                 # check, then run scripts 01-04 in order
```

`make run` always starts with the prerequisite check, so nothing runs if R, a
package or the data file is missing. It takes about 6 seconds.

| Command                   | What it does                                                                                  |
| :------------------------ | :-------------------------------------------------------------------------------------------- |
| `make check`            | Checks `Rscript`, the R version, each R package (and its minimum version) and the data file |
| `make install-packages` | Installs missing or too-old R packages, then re-checks                                        |
| `make run`              | Runs all four scripts in order                                                                |
| `make prep`             | Runs only `01_data_preparation.R`                                                           |
| `make assumptions`      | Runs `02_normality_and_assumptions.R` (runs 01 first if its output is missing)              |
| `make tests`            | Runs `03_hypothesis_testing.R` (runs 01 first if needed)                                    |
| `make supplementary`    | Runs `04_supplementary_analysis.R` (runs 01 first if needed)                                |

**R packages used:** `readr`, `dplyr` (1.1.0 or newer), `tidyr`, `stringr`, `purrr`,
`e1071`, `psych`.

### Run without make

```bash
Rscript scripts/check_prerequisites.R --install    # check, and install what is missing
Rscript scripts/run_all_ca2.R                      # run the whole pipeline
```

### Where the results go

- `output/tables/`: CSV tables (hygiene audit, descriptive statistics, normality and
  variance tests, hypothesis test results, robustness checks).
- `output/figures/`: Figures 1-8 used in the report.
- `output/cleaned_airline_satisfaction.rds`: the cleaned dataset used by scripts 02-04.

| Problem                                                         | Fix                                                                                                                       |
| :-------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------ |
| `Rscript not found`                                           | R is not installed, or not on your PATH (see the Windows steps above)                                                     |
| `R packages missing or outdated`                              | Run `make install-packages`                                                                                             |
| `data/airline_passenger_satisfaction.csv not found`           | Run `make` from the project root, not from `scripts/`                                                                 |
| `missing separator` or odd `\r` errors from make on Windows | The Makefile was checked out with Windows line endings. Run `git config core.autocrlf false`, then re-checkout the file |
| Package install fails on Windows asking for a compiler          | Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version and retry                         |

