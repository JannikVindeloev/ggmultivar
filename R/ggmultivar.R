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
#' # With base R data
#' data(iris)
#' 
#' # Create a simple data frame with scores for demonstration
#' scores_data <- data.frame(
#'   sample = rownames(iris),
#'   component = rep(1:2, each = nrow(iris)),
#'   score = c(rnorm(nrow(iris)), rnorm(nrow(iris)))
#' )
#' 
#' # Create scores plot
#' ggscores(scores_data, color_by = iris$Species) +
#'   ggtitle("Scores Plot - Iris Data")
#' 
#' # Create loadings data for demonstration
#' loadings_data <- data.frame(
#'   variable = colnames(iris)[1:4],
#'   component = rep(1:2, each = 2),
#'   loading = rnorm(4)
#' )
#' 
#' # Create loadings plot
#' ggloadings(loadings_data) +
#'   ggtitle("Loadings Plot")
#' 
#' # Create biplot
#' ggbiplot(list(scores = scores_data, loadings = loadings_data)) +
#'   ggtitle("Biplot")
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
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
#'   geom_abline theme_minimal labs xlab ylab ggtitle stat_ellipse ggproto
#' @importFrom dplyr filter mutate arrange group_by ungroup summarise select bind_rows
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_df
#' @importFrom tibble tibble as_tibble
NULL
