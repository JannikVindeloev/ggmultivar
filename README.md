# ggmultivar: Multivariate Analysis Plots Using ggplot2

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JannikVindeloev/ggmultivar/actions/workflows/R-CMD-check.yaml)

**ggmultivar** provides a tidyverse-style interface for creating scores and loading plots for multivariate analysis methods. The package is designed to work seamlessly with mixOmics objects and enables layer-by-layer plot customization using ggplot2 syntax.

## Features

- **ggplot2-based**: Create publication-quality plots using the familiar ggplot2 syntax
- **mixOmics compatibility**: Directly handle mixOmics objects (PCA, PLS, sPLS, sPLS-DA, DIABLO, RGCCA)
- **Layer-by-layer customization**: Build plots incrementally with `ggscores()`, `ggloadings()`, and `ggbiplot()`
- **Custom geoms**: Special geoms for confidence intervals and encircling groups
- **Faceting support**: Facet scores and loadings by external variables
- **S3 methods**: Generic functions for extracting scores, loadings, and explained variance

## Installation

```r
# Install from GitHub
devtools::install_github("JannikVindeloev/ggmultivar")

# Or install dependencies first
install.packages(c("ggplot2", "dplyr", "tidyr", "purrr", "tibble", "mixOmics"))
devtools::install_github("JannikVindeloev/ggmultivar")
```

## Quick Start

### Basic Usage with mixOmics

```r
library(ggmultivar)
library(mixOmics)
data(wine)

# PCA example
pca_obj <- pca(wine$X, ncomp = 3)

# Create scores plot
ggscores(pca_obj, color_by = wine$Y) +
  ggtitle("PCA Scores Plot - Wine Data")

# Create loadings plot
ggloadings(pca_obj) +
  ggtitle("PCA Loadings Plot")

# Create biplot (scores + loadings)
ggbiplot(pca_obj, color_by = wine$Y) +
  ggtitle("PCA Biplot")
```

### Layer-by-Layer Customization

```r
# Start with base scores plot
p <- ggscores(pca_obj, color_by = wine$Y)

# Add confidence ellipses
p <- p + geom_encircle(group_by = "color_by")

# Add sample labels
p <- p + geom_sample_labels(size = 2)

# Customize appearance
p <- p + 
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  ggtitle("Customized PCA Scores Plot")

# Display
print(p)
```

### Using Custom Geoms

```r
# Encircle groups with convex hulls
ggscores(pca_obj, color_by = wine$Y) +
  geom_encircle(group_by = "color_by", geom = "hull", fill = "red", alpha = 0.1)

# Add correlation circle to loadings
ggloadings(pca_obj) +
  geom_correlation_circle(radius = 1, color = "blue")

# Add confidence intervals to loadings (requires original data)
ggloadings(pca_obj) +
  geom_confidence_interval()
```

### Faceting

```r
# Facet scores by class
ggscores(pca_obj, facet_by = wine$Y) +
  ggtitle("PCA Scores by Class")

# Facet loadings by variable type (if available)
ggloadings(pca_obj, facet_by = "variable_type")
```

## Function Overview

### Core Plotting Functions

- `ggscores()`: Create scores plot from multivariate analysis object
- `ggloadings()`: Create loadings plot from multivariate analysis object  
- `ggbiplot()`: Create combined scores and loadings plot (biplot)

### S3 Methods for Data Extraction

- `scores()`: Extract scores from mixOmics objects
- `loadings()`: Extract loadings from mixOmics objects
- `explained_variance()`: Extract explained variance from mixOmics objects

### Custom Geoms

- `geom_encircle()`: Add ellipses or convex hulls to encircle groups
- `geom_confidence_interval()`: Add confidence intervals to loading vectors
- `geom_correlation_circle()`: Add correlation circles to biplots
- `geom_sample_labels()`: Add sample labels with intelligent positioning
- `geom_variable_labels()`: Add variable labels with intelligent positioning

## Supported Methods

- **PCA**: Principal Component Analysis
- **PLS**: Partial Least Squares Regression
- **sPLS**: Sparse Partial Least Squares
- **sPLS-DA**: Sparse Partial Least Squares Discriminant Analysis
- **DIABLO**: Multi-omics integration
- **RGCCA**: Regularized Generalized Canonical Correlation Analysis

## Plot Customization Examples

### Customizing Scores Plot

```r
ggscores(pca_obj, 
         x_component = 1, 
         y_component = 2,
         color_by = wine$Y,
         shape_by = wine$Y,
         size_by = rowSums(wine$X),
         show_ellipse = TRUE,
         ellipse_level = 0.95,
         show_labels = TRUE,
         label_size = 3) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15)) +
  theme_minimal() +
  ggtitle("Customized PCA Scores")
```

### Customizing Loadings Plot

```r
ggloadings(pca_obj,
           x_component = 1,
           y_component = 2,
           show_labels = TRUE,
           label_size = 4,
           show_arrows = TRUE,
           arrow_length = 0.8,
           color_by = abs(loadings(pca_obj)$loading),
           circle_radius = 1,
           show_circle = TRUE) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_bw()
```

### Customizing Biplot

```r
ggbiplot(pca_obj,
         x_component = 1,
         y_component = 2,
         color_by = wine$Y,
         show_ellipse = TRUE,
         show_score_labels = FALSE,
         show_loading_labels = TRUE,
         loading_label_size = 4,
         arrow_length = 1,
         circle_radius = 1,
         show_circle = TRUE) +
  geom_encircle(group_by = "color_by", color = "blue") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  ggtitle("Customized PCA Biplot")
```

## Dependencies

- Required: R (>= 3.6.0), ggplot2, dplyr, tidyr, purrr, tibble, stats
- Suggested: testthat, knitr, rmarkdown, covr, mixOmics

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License
