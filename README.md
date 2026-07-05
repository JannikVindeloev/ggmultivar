---

editor_options: 
  markdown: 
    wrap: 72
---

# ggmultivar: Multivariate Analysis Plots Using ggplot2

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![R-CMD-check](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml)

**ggmultivar** provides a tidyverse-style interface for creating scores and loading plots for multivariate analysis methods including PCA, PLS, sPLS, and sPLS-DA. The package is designed to work seamlessly with both native R data structures and [mixOmics](https://mixomicsteam.github.io/) objects.

## Features

- **Tidyverse-compatible**: All functions follow tidyverse conventions and return tidy data frames
- **ggplot2-based**: Create publication-quality plots using the familiar ggplot2 syntax
- **mixOmics compatibility**: Directly handle mixOmics objects (PCA, PLS, sPLS, sPLS-DA, DIABLO, RGCCA)
- **Comprehensive plotting**: Scores plots, loadings plots, and biplots for all supported methods
- **Flexible customization**: Color, shape, and size points by external variables
- **Statistical annotations**: Automatic variance explained labels, confidence ellipses

## Installation

``` r
# Install from GitHub
devtools::install_github("JannikVindeloev/ggmultivar")

# Or install dependencies first
install.packages(c("ggplot2", "dplyr", "tidyr", "purrr", "tibble", "pls", "mixOmics"))
devtools::install_github("JannikVindeloev/ggmultivar")
```

## Quick Start

### PCA with Native R Data

``` r
library(ggmultivar)

# Perform PCA
pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)

# Create scores plot
pca_scores_plot(pca_result, color_by = mtcars$cyl) +
  ggtitle("PCA Scores Plot - mtcars data")

# Create loadings plot
pca_loadings_plot(pca_result)

# Create biplot (scores + loadings)
pca_biplot(pca_result, color_by = mtcars$cyl)
```

### PLS with Native R Data

``` r
# Simulate data
set.seed(123)
X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)

# Perform PLS
pls_result <- perform_pls(X, Y, n_components = 3)

# Create scores plot
pls_scores_plot(pls_result)

# Create biplot
pls_biplot(pls_result)
```

### With mixOmics Objects

``` r
library(mixOmics)
data(wine)

# PCA with mixOmics
pca_mix <- pca(wine$X, ncomp = 3)
mixomics_scores_plot(pca_mix, color_by = wine$Y)

# PLS with mixOmics
pls_mix <- pls(wine$X, wine$Y, ncomp = 3)
mixomics_biplot(pls_mix)

# sPLS-DA with mixOmics
splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
mixomics_scores_plot(splsda_mix)  # Automatically uses class labels
```

## Function Overview

### Core Analysis Functions

- `perform_pca()`: Perform PCA on a data matrix
- `perform_pls()`: Perform PLS regression on predictor and response matrices

### Plotting Functions

**For PCA:** - `pca_scores_plot()`: Create scores plot - `pca_loadings_plot()`: Create loadings plot\
- `pca_biplot()`: Create combined scores and loadings plot

**For PLS:** - `pls_scores_plot()`: Create scores plot - `pls_loadings_plot()`: Create loadings plot - `pls_biplot()`: Create combined scores and loadings plot

**For mixOmics Objects:** - `mixomics_to_ggmultivar()`: Convert mixOmics object to ggmultivar format - `mixomics_scores_plot()`: Create scores plot directly from mixOmics object - `mixomics_loadings_plot()`: Create loadings plot directly from mixOmics object - `mixomics_biplot()`: Create biplot directly from mixOmics object

### Conversion Functions

- `mixomics_to_ggmultivar()`: Generic conversion for any mixOmics object
- `mixomics_pca_to_ggmultivar()`: Convert mixOmics PCA object
- `mixomics_pls_to_ggmultivar()`: Convert mixOmics PLS object
- `mixomics_spls_to_ggmultivar()`: Convert mixOmics sPLS object
- `mixomics_splsda_to_ggmultivar()`: Convert mixOmics sPLS-DA object

## Plot Customization

All plotting functions accept standard ggplot2 arguments and provide additional customization options:

``` r
# Customize scores plot
pca_scores_plot(pca_result, 
                x_component = 1, 
                y_component = 2,
                color_by = mtcars$cyl,
                shape_by = mtcars$gear,
                size_by = mtcars$hp,
                show_ellipse = TRUE,
                ellipse_level = 0.95,
                show_labels = TRUE,
                label_size = 3,
                theme = theme_bw()) +
  scale_color_brewer(palette = "Set1") +
  ggtitle("Customized PCA Scores Plot")

# Customize loadings plot
pca_loadings_plot(pca_result,
                  show_arrows = TRUE,
                  arrow_length = 0.8,
                  show_circle = TRUE,
                  circle_radius = 1,
                  color_by = abs(pca_result$loadings$loading))
```

## Output Format

All analysis functions return a list with the following components:

- `scores`: Tidy data frame with component scores for each sample
- `loadings`: Tidy data frame with component loadings for each variable
- `explained_variance`: Data frame with variance explained by each component
- Additional method-specific components (e.g., `y_loadings`, `weights`, `r2`)

## Supported Methods

- **PCA**: Principal Component Analysis
- **PLS**: Partial Least Squares Regression
- **sPLS**: Sparse Partial Least Squares
- **sPLS-DA**: Sparse Partial Least Squares Discriminant Analysis
- **DIABLO**: Multi-omics integration (via mixOmics)
- **RGCCA**: Regularized Generalized Canonical Correlation Analysis (via mixOmics)

## Dependencies

- Required: R (\>= 3.6.0), ggplot2, dplyr, tidyr, purrr, tibble
- Suggested: pls, mixOmics, testthat, knitr, rmarkdown, covr

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License
