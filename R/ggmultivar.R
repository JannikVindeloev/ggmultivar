#' ggmultivar: Multivariate Analysis Plots Using ggplot2
#' 
#' @description 
#' ggmultivar provides a tidyverse-style interface for creating scores and loading plots
#' for multivariate analysis methods. The package is designed to work seamlessly with
#' mixOmics objects, making it easy to create publication-quality plots using ggplot2 syntax.
#' 
#' @name ggmultivar
#' @docType package
#' @examples
#' # With mixOmics objects
#' \dontrun{
#' library(ggmultivar)
#' library(mixOmics)
#' data(wine)
#' 
#' # PCA example
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' # Create scores plot
#' ggscores(pca_obj, color_by = wine$Y) +
#'   geom_encircle(group_by = "color_by") +
#'   ggtitle("PCA Scores Plot - Wine Data")
#' 
#' # Create loadings plot
#' ggloadings(pca_obj) +
#'   geom_correlation_circle() +
#'   ggtitle("PCA Loadings Plot")
#' 
#' # Create biplot
#' ggbiplot(pca_obj, color_by = wine$Y) +
#'   geom_encircle(group_by = "color_by") +
#'   ggtitle("PCA Biplot")
#' 
#' # PLS example
#' pls_obj <- pls(wine$X, wine$Y, ncomp = 3)
#' ggbiplot(pls_obj, color_by = wine$Y)
#' }
NULL

#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_path 
#'   geom_abline theme_minimal labs xlab ylab ggtitle stat_ellipse
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise select bind_rows
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_df
#' @importFrom tibble tibble as_tibble
#' @importFrom stats chull
#' @useDynLib ggmultivar, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
