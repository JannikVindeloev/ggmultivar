# ggmultivar Style Guide

This document outlines the coding and documentation conventions for the ggmultivar package.

## R Coding Style

### Base Style: tidyverse

This package follows [tidyverse style](https://style.tidyverse.org/) with the following specifications.

### Indentation

- Use **2 spaces** for indentation
- Never use tabs

### Assignment

- Use `<-` for assignment, not `=`

```r
# Good
x <- 10

# Bad
x = 10
```

### Pipe Operator

- Use the **native pipe** `|>` (R 4.1+), not magrittr's `%>%`

```r
# Good
data |>
  filter(x > 5) |>
  group_by(y) |>
  summarise(mean = mean(z))

# Bad
data %>%
  filter(x > 5) %>%
  group_by(y) %>%
  summarise(mean = mean(z))
```

### Function Definition

- Use `function(x) {` for multi-line functions
- Use `\` for anonymous functions when appropriate

```r
# Multi-line function
my_function <- function(data, x, y) {
  data |>
    select({{ x }}, {{ y }}) |>
    cor.test()
}

# Anonymous function
mtcars |>
  filter(mpg > 20) |>
  sapply(\(x) mean(x, na.rm = TRUE))
```

### Braces

- Opening brace `{` on the same line as function/control statement
- Closing brace `}` on its own line, aligned with the start of the statement

```r
# Good
if (condition) {
  do_something()
}

function(x) {
  x + 1
}

# Bad
if (condition)
{
  do_something()
}

function(x)
{
  x + 1
}
```

### Quotes

- Use **double quotes** `"` for strings

```r
# Good
"This is a string"

# Bad
'This is a string'
```

### Line Length

- Aim for **~80 characters per line** (soft limit)
- Break long pipes into multiple lines

```r
# Good
data |>
  filter(x > 5, y < 10) |>
  group_by(category) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

# Bad (too long on one line)
data |> filter(x > 5, y < 10) |> group_by(category) |> summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
```

### Spacing

- Always put spaces around operators (`+`, `-`, `*`, `/`, `=`, `<-`, etc.)
- Put spaces after commas
- No spaces inside parentheses

```r
# Good
x <- y + z
a <- b * (c - d)
function(x, y, z)

# Bad
x<-y+z
a<-b*(c-d)
function(x,y,z)
```

## Package Preferences

### Core Packages

| Category | Preferred Package | Purpose |
|----------|------------------|---------|
| Data frames | `tibble` | Modern data frame implementation |
| Data manipulation | `dplyr` | Data wrangling and transformation |
| Data reshaping | `tidyr` | Tidy data principles |
| String manipulation | `stringr` | Consistent string operations |
| Functional programming | `purrr` | Functional tools and iteration |
| Graphs/networks | `tidygraph` | Tidy graph manipulation |


### Visualization & Graphics

| Category | Preferred Package | Purpose |
|----------|------------------|---------|
| Static plotting | `ggplot2` | Grammar of Graphics |
| Interactive graphics | `plotly` | Interactive visualizations |
| Web applications | `shiny` | Interactive web apps |
| Graphs graphics | `ggraph` | Grammar of Graphics for graphs |
| Interactive Graphs | `visnetwork` | Interactive D3 network plots |


### Documentation

| Category | Preferred Package | Purpose |
|----------|------------------|---------|
| Function documentation | `roxygen2` | In-line documentation |
| Vignettes | `rmarkdown` | Long-form documentation |

## Documentation Standards

### Function Documentation (roxygen2)

All exported functions must have complete roxygen2 documentation:

```r
#' Title Case Function Name
#'
#' @description A brief description of what the function does.
#'
#' @param data A data frame or tibble. Description of the parameter.
#' @param x A numeric vector. Description of x.
#' @param y A numeric vector. Description of y.
#' @param alpha A numeric value between 0 and 1. The significance level.
#'   Default is 0.05.
#' @return A tibble with columns for estimate, lower, and upper confidence
#'   interval bounds.
#' @examples
#' data(mtcars)
#' my_function(mtcars, mpg, wt)
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
my_function <- function(data, x, y, alpha = 0.05) {
  # Function implementation
}
```

### Vignettes (rmarkdown)

- Use R Markdown (`.Rmd`) files for vignettes
- Include a `vignettes/` directory in the package
- Use YAML header with appropriate metadata

```yaml
---
title: "Getting Started with ggmultivar"
author: "Package Author"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with ggmultivar}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
```

- Include practical examples with real data
- Explain the motivation and use cases
- Use the native pipe `|>` consistently

### Package Documentation

- Maintain a comprehensive `README.md` with:
  - Package overview
  - Installation instructions
  - Basic usage examples
  - Badges for CI, coverage, etc.

- Include a `NEWS.md` file for release notes

## File Organization

### Directory Structure

```
R/                  # Source code
  - functions.R     # Main functions
  - utils.R         # Utility functions
  - data.R          # Data-related functions
man/                # Generated documentation (do not edit manually)
vignettes/          # Vignettes
tests/              # Test files
  - testthat/       # testthat tests
  - testthat.R      # testthat setup
inst/               # Installed files
data/               # Package data
doc/                # Additional documentation
```

### File Naming

- Use lowercase with underscores for file names
- Be descriptive but concise

```
# Good
R/correlation_plots.R
R/data_manipulation.R
tests/testthat/test_correlation.R

# Bad
R/CorrelationPlots.R
R/dataManip.R
tests/testthat/TestCorrelation.R
```

## Testing

- Use `testthat` for unit testing
- Place tests in `tests/testthat/`
- Test file names should mirror the source files they test

```r
# tests/testthat/test_correlation.R
test_that("correlation calculation works", {
  data <- tibble(x = 1:10, y = 1:10)
  result <- my_cor_function(data, x, y)
  expect_equal(result$estimate, 1)
})
```

## Git Workflow

- Use meaningful branch names: `feature/short-description` or `fix/short-description`
- Write clear, concise commit messages
- Use imperative mood in commit messages ("Add feature" not "Added feature")
- Reference issues when applicable

## Examples

### Complete Function Example

```r
#' Calculate correlation with confidence intervals
#'
#' Computes Pearson correlation coefficient with confidence intervals
#' using Fisher's z-transformation.
#'
#' @param data A data frame or tibble containing the variables.
#' @param x, y Numeric vectors to compute correlation between.
#'   These are passed through `{{ }}` and support tidy evaluation.
#' @param alpha Numeric value between 0 and 1. The significance level
#'   for confidence interval calculation. Default is 0.05.
#' @return A tibble with one row containing:
#'   - `estimate`: The correlation coefficient
#'   - `lower`: Lower bound of the confidence interval
#'   - `upper`: Upper bound of the confidence interval
#'   - `p.value`: The p-value for the correlation test
#' @examples
#' library(tibble)
#' data <- tibble(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' cor_with_ci(data, x, y)
#' @export
#' @importFrom dplyr select
#' @importFrom broom tidy
cor_with_ci <- function(data, x, y, alpha = 0.05) {
  data |>
    select({{ x }}, {{ y }}) |>
    cor.test() |>
    tidy() |>
    mutate(
      lower = estimate - qnorm(1 - alpha / 2) * std.error,
      upper = estimate + qnorm(1 - alpha / 2) * std.error
    ) |>
    select(estimate, lower, upper, p.value)
}
```

### Complete Vignette Example

````markdown
---
title: "Visualizing Multivariate Relationships"
author: "Package Author"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Visualizing Multivariate Relationships}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
```

# Introduction

This vignette demonstrates how to use ggmultivar to visualize multivariate relationships.

# Basic Usage

```{r basic-usage}
library(ggmultivar)
library(tibble)
library(ggplot2)

# Create sample data
data <- tibble(
  x = rnorm(100),
  y = rnorm(100),
  z = x + y + rnorm(100, sd = 0.5)
)

# Use ggmultivar functions
result <- cor_with_ci(data, x, y)
print(result)
```

# Advanced Visualization

```{r advanced-plot}
data |>
  ggplot(aes(x = x, y = y, color = z)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
```
````

## References

- [tidyverse style guide](https://style.tidyverse.org/)
- [R Packages book](https://r-pkgs.org/)
- [roxygen2 documentation](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)
- [testthat documentation](https://testthat.r-lib.org/)

## Version History

| Date | Author | Changes |
|------|--------|---------|
| 2024-01-XX | JANVI | Created style guide |
