#' ggmultivar: Multivariate Analysis Plots Using ggplot2
#' 
#' @description 
#' ggmultivar provides a tidyverse-style interface for creating scores and loading plots
#' for multivariate analysis methods including PCA, PLS, sPLS, and sPLS-DA.
#' 
#' The package is designed to work seamlessly with both native R data structures
#' and mixOmics objects, making it easy to integrate into existing workflows.
#' 
#' @name ggmultivar
#' @docType package
#' @examples
#' # PCA example with mtcars
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' pca_scores_plot(pca_result, color_by = mtcars$cyl) +
#'   ggtitle("PCA Scores Plot - mtcars data")
#' 
#' # PLS example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y, n_components = 3)
#' pls_biplot(pls_result)
#' 
#' # With mixOmics objects
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' mixomics_scores_plot(pca_mix, color_by = wine$Y)
#' }
NULL

#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_path 
#'   geom_abline theme_minimal labs xlab ylab ggtitle stat_ellipse
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_df
#' @importFrom tibble tibble as_tibble
#' @useDynLib ggmultivar, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL