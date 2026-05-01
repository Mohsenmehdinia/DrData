# DrData

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/DrData)](https://CRAN.R-project.org/package=DrData)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**DrData** is a Shiny-based interactive platform for end-to-end data science
workflows — designed for students and practitioners in data science and
artificial intelligence.

## Features

| Module | What it does |
|---|---|
| **Data Import** | CSV, Excel (xlsx/xls), RDS, TXT; built-in example datasets (iris, mtcars, Titanic) |
| **Preprocessing** | Missing value imputation, duplicate removal, column/row dropping, scaling (z-score, min-max, log), one-hot encoding, outlier detection |
| **EDA** | 8 interactive plot types; normality tests (Shapiro-Wilk, KS, Anderson-Darling, Jarque-Bera); Auto-EDA report |
| **Regression** | 8 algorithms: Linear, Ridge, Lasso, Decision Tree, Random Forest, SVM, GBM, Neural Network; interaction terms; full diagnostic plots |
| **Classification** | 8 algorithms: Logistic, Decision Tree, Random Forest, SVM, KNN, Naive Bayes, GBM, Neural Network; ROC curve; confusion matrix |
| **Clustering** | K-Means, Hierarchical, DBSCAN; elbow plot; silhouette score; cluster profiles |

## Installation

### From CRAN (once accepted)

```r
install.packages("DrData")
```

### Development version

```r
# install.packages("remotes")
remotes::install_github("mohsenmehdinia/DrData")
```

## Quick Start

```r
library(DrData)
run_app()
```

Load the built-in `iris` or `mtcars` dataset from the **Data Import** tab to
explore immediately — no data preparation needed.

## Workflow

```
Data Import → Preprocessing → EDA → Regression / Classification / Clustering
```

## License

MIT © 2025 Mohsen Mehdinia
