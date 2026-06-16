# ggmultivar: Multivariate Analysis Plots Using ggplot2 Syntax

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml)

**ggmultivar** provides a ggplot2-like interface for creating scores and loading plots for multivariate analysis methods. It uses S3 method dispatch and custom geoms to create a familiar and extensible plotting system.

## Features

- **ggplot2-like syntax**: `ggmultivar(data, aes(x, y)) + geom_scores() + geom_loadings()`
- **Custom geoms**: `geom_scores()`, `geom_loadings()`, `geom_conf()`, `geom_ellipse()`
- **Faceting**: `facet_components()` for pairwise component layouts
- **S3 method dispatch**: Works with native R objects, mixOmics objects, and custom classes
- **Tidyverse compatible**: All functions follow tidyverse conventions
- **mixOmics integration**: Seamless compatibility with mixOmics objects

## Installation

```r
# Install from GitHub
devtools::install_github("JannikVindeloev/ggmultivar")

# Or install dependencies first
install.packages(c("ggplot2", "dplyr", "tidyr", "purrr", "tibble", "rlang", "pls", "mixOmics"))
devtools::install_github("JannikVindeloev/ggmultivar")
```

## Quick Start

### Basic PCA Plot

```r
library(ggmultivar)

# Perform PCA
pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)

# Create a basic scores plot
ggmultivar(data = pca_result, aes(PC1, PC2)) +
  geom_scores()

# Add loadings and confidence circle
ggmultivar(data = pca_result, aes(PC1, PC2)) +
  geom_scores(aes(color = factor(mtcars$cyl))) +
  geom_loadings() +
  geom_conf()
```

### Customizing the Plot

```r
# Custom scores plot with ellipses
ggmultivar(data = pca_result, aes(PC1, PC2, color = factor(mtcars$cyl))) +
  geom_scores(size = 3, alpha = 0.7) +
  geom_ellipse(level = 0.95, color = "gray50") +
  geom_conf() +
  ggtitle("PCA Scores Plot - mtcars data") +
  theme_minimal()

# Biplot with connected loadings (for time series or spectra)
ggmultivar(data = pca_result, aes(PC1, PC2)) +
  geom_scores(aes(color = factor(mtcars$cyl))) +
  geom_loadings(connect = TRUE, connect_order = colnames(mtcars[, 1:7])) +
  geom_conf()
```

### Faceting Components

```r
# Create a grid of pairwise component plots
pca_result <- perform_pca(mtcars[, 1:7], n_components = 4)

# Manual faceting
ggmultivar(data = pca_result, aes(PC1, PC2)) +
  geom_scores() +
  facet_components(c("PC1", "PC2"), c("PC3", "PC4"))

# Using facet_pairs for all pairwise combinations
facet_pairs(pca_result, ncol = 2)
```

### PLS Analysis

```r
# Simulate data
set.seed(123)
X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)

# Perform PLS
pls_result <- perform_pls(X, Y, n_components = 3)

# Create PLS plot
ggmultivar(data = pls_result, aes(L1, L2)) +
  geom_scores() +
  geom_loadings() +
  geom_conf()
```

## Function Overview

### Main Functions

- `ggmultivar()`: Create a ggmultivar plot object
- `perform_pca()`: Perform PCA on a data matrix
- `perform_pls()`: Perform PLS regression on predictor and response matrices

### Custom Geoms

- `geom_scores()`: Add a layer of score points
- `geom_loadings()`: Add a layer of loading arrows or points
- `geom_conf()`: Add a confidence circle (unit circle)
- `geom_ellipse()`: Add confidence ellipses for grouped data
- `geom_variance()`: Add variance explained labels

### Faceting Functions

- `facet_components()`: Create facet_grid or facet_wrap layout for components
- `facet_pairs()`: Create a matrix of pairwise component plots
- `facet_matrix()`: Create a ggpairs-like matrix of plots

### S3 Methods

- `as_ggmultivar()`: Generic function to convert objects to ggmultivar format
- Methods for: `list`, `pca`, `pls`, `mixOmics::pca`, `mixOmics::pls`, `mixOmics::spls`, `mixOmics::splsda`

## Working with mixOmics Objects

```r
library(mixOmics)
data(wine)

# PCA with mixOmics
pca_mix <- pca(wine$X, ncomp = 3)
ggmultivar(data = pca_mix, aes(Comp1, Comp2)) +
  geom_scores(aes(color = wine$Y)) +
  geom_conf()

# PLS with mixOmics
pls_mix <- pls(wine$X, wine$Y, ncomp = 3)
ggmultivar(data = pls_mix, aes(Comp1, Comp2)) +
  geom_scores() +
  geom_loadings()

# sPLS-DA with mixOmics
splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
ggmultivar(data = splsda_mix, aes(Comp1, Comp2)) +
  geom_scores(aes(color = wine$Y)) +
  geom_ellipse(level = 0.95)
```

## Custom Geom Parameters

### geom_scores()
- `mapping`: Aesthetic mappings
- `size`: Point size (default: 2)
- `shape`: Point shape (default: 19)
- `color`: Point color (default: "black")
- `alpha`: Transparency (default: 1)
- All standard ggplot2 aesthetics

### geom_loadings()
- `as_arrows`: Show as arrows from origin (default: TRUE)
- `arrow_length`: Scaling factor for arrows (default: 1)
- `connect`: Connect loadings with lines (default: FALSE)
- `connect_order`: Order for connecting loadings
- `color`: Loading color (default: "red")
- `size`: Loading point size (default: 1)

### geom_conf()
- `radius`: Circle radius (default: 1)
- `linetype`: Line type (default: "dashed")
- `color`: Circle color (default: "gray50")
- `alpha`: Transparency (default: 1)

### geom_ellipse()
- `level`: Confidence level (default: 0.95)
- All standard stat_ellipse parameters

## Design Philosophy

ggmultivar is designed to feel like a natural extension of ggplot2. The key principles are:

1. **Consistent Interface**: All functions follow ggplot2 conventions
2. **Layered Plotting**: Build plots by adding layers with `+`
3. **S3 Dispatch**: Automatic handling of different object types
4. **Extensibility**: Easy to add new geoms and methods
5. **Tidy Data**: All outputs are tidy data frames

## Dependencies

- Required: R (>= 3.6.0), ggplot2, dplyr, tidyr, purrr, tibble, rlang
- Suggested: pls, mixOmics, testthat, knitr, rmarkdown, covr, patchwork, cowplot, GGally

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License