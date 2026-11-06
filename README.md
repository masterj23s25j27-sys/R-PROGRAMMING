# R-PROGRAMMING: Complete Basic-to-Advanced End-to-End Curriculum

A structured, hands-on **End-to-End R Programming Curriculum** designed to take you from absolute beginner (`Part 01`) to advanced R software engineering, statistical inference, machine learning, time-series forecasting, and production data science pipelines (`Part 06`).

Every part contains **modular, self-contained `.R` scripts** alongside a rich **interactive R Markdown (`.Rmd`) study guide**.

---

## Repository Structure & Learning Roadmap

```text
R-PROGRAMMING/
├── Part_01_Basics/
│   ├── 01_variables_and_datatypes.R
│   ├── 02_data_structures.R
│   ├── 03_control_flow_and_functions.R
│   └── Part_01_Basics_Guide.Rmd
├── Part_02_Data_Wrangling/
│   ├── 01_data_import_export_tidyr.R
│   ├── 02_dplyr_data_manipulation.R
│   ├── 03_strings_factors_dates.R
│   └── Part_02_Data_Wrangling_Guide.Rmd
├── Part_03_Data_Visualization/
│   ├── 01_base_r_plotting.R
│   ├── 02_ggplot2_fundamentals.R
│   ├── 03_advanced_visualizations.R
│   └── Part_03_Visualization_Guide.Rmd
├── Part_04_Statistics_Probability/
│   ├── 01_descriptive_stats_and_distributions.R
│   ├── 02_hypothesis_testing_and_anova.R
│   ├── 03_regression_and_glm.R
│   └── Part_04_Statistics_Guide.Rmd
├── Part_05_Advanced_R/
│   ├── 01_oop_s3_s4_r6.R
│   ├── 02_functional_programming_and_metaprogramming.R
│   ├── 03_error_handling_debugging_performance.R
│   └── Part_05_Advanced_R_Guide.Rmd
└── Part_06_Machine_Learning_Capstone/
    ├── 01_supervised_ml_classification_regression.R
    ├── 02_unsupervised_learning_and_timeseries.R
    ├── 03_end_to_end_capstone_project.R
    └── Part_06_ML_Capstone_Guide.Rmd
```

---

## Part-by-Part Syllabus

### Part 01: R Basics & Fundamentals (`Part_01_Basics/`)
- **`01_variables_and_datatypes.R`**: Variable assignment (`<-`, `=`, `assign`), atomic types (`numeric`, `integer`, `character`, `logical`, `complex`, `raw`), explicit/implicit coercion hierarchy, vectorized operators, and special values (`NA`, `NaN`, `Inf`, `NULL`).
- **`02_data_structures.R`**: 1D Atomic Vectors, Categorical Factors (nominal vs ordered), 2D Matrices & Linear Algebra (`%*%`, `solve`, `det`, `t`), 3D Arrays, Recursive Lists (`[` vs `[[` vs `$`), and Data Frames.
- **`03_control_flow_and_functions.R`**: Conditional branching (`if/else`, vectorized `ifelse()`, `switch()`), pre-allocated loops (`seq_along`), custom functions with default arguments & ellipsis (`...`), and the Base R `apply` family (`apply`, `lapply`, `sapply`, `vapply`, `tapply`).
- **`Part_01_Basics_Guide.Rmd`**: Complete interactive notebook & exercises for Part 01.

### Part 02: Data Wrangling & The Tidyverse (`Part_02_Data_Wrangling/`)
- **`01_data_import_export_tidyr.R`**: Reading/writing CSV, TSV, and native `.rds` serialization (`saveRDS`/`readRDS`), Tidy Data principles, `pivot_longer()`, `pivot_wider()`, `separate()`, and missing value imputation (`replace_na()`).
- **`02_dplyr_data_manipulation.R`**: Core grammar of data manipulation (`filter`, `select`, `arrange`, `mutate`, `case_when`, `across`, `group_by`, `summarise`), window functions (`row_number`, `lag`, `lead`, `cumsum`, `min_rank`), and relational joins (`left_join`, `inner_join`, `anti_join`, `semi_join`).
- **`03_strings_factors_dates.R`**: Regular expressions & log parsing (`stringr` & Base R `grep`/`sub`), categorical factor engineering (`forcats`), and date-time parsing, time zones, and arithmetic (`lubridate`, `Date`, `POSIXct`).
- **`Part_02_Data_Wrangling_Guide.Rmd`**: End-to-end data wrangling R Markdown guide.

### Part 03: Data Visualization & Graphics (`Part_03_Data_Visualization/`)
- **`01_base_r_plotting.R`**: Multi-panel Base R dashboards (`par(mfrow)`), scatter plots with OLS & LOWESS curves, density histograms, notched boxplots with jittered stripcharts, grouped barplots, scatterplot matrices (`pairs`), and 2D topographic contour maps (`filled.contour`).
- **`02_ggplot2_fundamentals.R`**: Layered Grammar of Graphics (`ggplot2`), aesthetics, geometries, confidence ribbons (`geom_smooth`), raincloud-style distributions (`geom_violin` + `geom_boxplot` + `geom_jitter`), and faceted small multiples (`facet_wrap`).
- **`03_advanced_visualizations.R`**: Annotated correlation heatmaps (`geom_tile`), time-series uncertainty fan charts (`geom_ribbon`), and Cleveland dumbbell charts (`geom_segment`).
- **`Part_03_Visualization_Guide.Rmd`**: Visual gallery and chart-selection guide.

### Part 04: Probability, Statistics & Hypothesis Testing (`Part_04_Statistics_Probability/`)
- **`01_descriptive_stats_and_distributions.R`**: Mean vs trimmed mean, median, IQR, MAD, skewness, excess kurtosis, Pearson vs Spearman vs Kendall correlation, R's `d/p/q/r` probability distribution functions, and Monte Carlo verification of the Central Limit Theorem.
- **`02_hypothesis_testing_and_anova.R`**: One-sample, two-sample Welch, and paired `t.test()` with Cohen's $d$, Mann-Whitney-Wilcoxon (`wilcox.test`), A/B proportion testing (`prop.test`), Chi-Square (`chisq.test`), One-Way ANOVA (`aov`) with `TukeyHSD()`, Kruskal-Wallis, and multiple testing correction (`p.adjust`: Bonferroni, Holm, Benjamini-Hochberg FDR).
- **`03_regression_and_glm.R`**: Multiple linear regression (`lm`) with interactions, nested $F$-tests, `AIC`/`BIC`, Cook's distance diagnostics, Logistic & Poisson Generalized Linear Models (`glm`), and non-parametric Bootstrap confidence intervals.
- **`Part_04_Statistics_Guide.Rmd`**: Statistical inference & diagnostic plots notebook.

### Part 05: Advanced R Programming (`Part_05_Advanced_R/`)
- **`01_oop_s3_s4_r6.R`**: S3 classes, constructors, validators, and custom generic dispatch (`UseMethod`); formal S4 classes (`setClass`, typed slots, validity checks, `setGeneric`/`setMethod`); and encapsulated mutable R6 reference classes (`R6Class`).
- **`02_functional_programming_and_metaprogramming.R`**: Closures, function factories, stateful lexical scoping (`<<-`), higher-order functions (`Reduce`, `Filter`, `Map`), `purrr` functional pipelines (`map_dbl`, `possibly`), and Non-Standard Evaluation (`substitute`, `eval`, `quote`, AST inspection).
- **`03_error_handling_debugging_performance.R`**: Condition handling (`tryCatch`), benchmarking vector growth vs pre-allocation vs vectorization (`system.time`), $O(1)$ hash-environment memoisation (`new.env(hash = TRUE)`), and multi-core parallel computing (`parallel::makeCluster`, `parSapply`).
- **`Part_05_Advanced_R_Guide.Rmd`**: Deep-dive guide to OOP, FP, and metaprogramming.

### Part 06: Machine Learning, Time Series & Production Capstone (`Part_06_Machine_Learning_Capstone/`)
- **`01_supervised_ml_classification_regression.R`**: Leakage-free train/test scaling, $K$-fold cross-validation engine, closed-form Ridge ($L_2$) regression tuning, and binary classification evaluation (Confusion Matrix, Precision, Recall, $F_1$, exact ROC-AUC).
- **`02_unsupervised_learning_and_timeseries.R`**: Principal Component Analysis (`prcomp`), $K$-Means Elbow WCSS (`kmeans`), Hierarchical Clustering (`hclust`), STL seasonal decomposition (`stl`), Holt-Winters exponential smoothing, and Seasonal ARIMA (`arima`) with Ljung-Box residual diagnostics.
- **`03_end_to_end_capstone_project.R`**: Complete FinTech Credit Risk pipeline: synthetic data ingestion with missing values, leakage-free imputation & domain feature engineering, model comparison (AIC, ROC-AUC, Brier Score), Risk Decile Calibration Lift Table, and deployable `.rds` model bundle serialization.
- **`Part_06_ML_Capstone_Guide.Rmd`**: Capstone report & visual diagnostics.

---

## Quick Start

### 1. Clone the Repository
```bash
git clone https://github.com/masterj23s25j27-sys/R-PROGRAMMING.git
cd R-PROGRAMMING
```

### 2. Install Recommended Packages (Optional — All scripts also include Base R fallbacks!)
```r
install.packages(c(
  "tidyverse",   # dplyr, tidyr, readr, ggplot2, stringr, forcats, lubridate, purrr
  "R6",          # Encapsulated OOP
  "rmarkdown",   # Rendering .Rmd guides
  "knitr"        # Dynamic tables and report generation
))
```

### 3. Run Any Script or Render Any `.Rmd` Guide
```r
# Run Part 01 Module 01
source("Part_01_Basics/01_variables_and_datatypes.R")

# Render an R Markdown Guide to HTML
rmarkdown::render("Part_01_Basics/Part_01_Basics_Guide.Rmd")
```
