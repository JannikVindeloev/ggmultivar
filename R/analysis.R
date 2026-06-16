#' Perform Principal Component Analysis
#' 
#' @description Perform PCA on a data matrix and return results in ggmultivar format.
#' 
#' @param data A numeric data frame or matrix. Rows are observations, columns are variables.
#' @param n_components Number of principal components to compute. Default is 2.
#' @param scale Logical, whether to scale variables to unit variance. Default is TRUE.
#' @param center Logical, whether to center variables by their mean. Default is TRUE.
#' @param sample_names Optional character vector of sample names. If NULL, defaults to Sample_1, Sample_2, etc.
#' @param variable_names Optional character vector of variable names. If NULL, uses column names from data.
#' 
#' @return A list containing:
#' \itemize{
#'   \item scores: Tidy data frame with principal component scores for each sample
#'   \item loadings: Tidy data frame with principal component loadings for each variable
#'   \item explained_variance: Data frame with variance explained by each component
#'   \item sdev: Vector of standard deviations of the principal components
#'   \item rotation: Matrix of variable loadings (rotation matrix)
#'   \item method: Character, "pca"
#'   \item n_components: Number of components computed
#' }
#' 
#' @examples
#' data(mtcars)
#' # Use only numeric columns
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' 
#' # Create a ggmultivar plot
#' ggmultivar(data = pca_result, aes(PC1, PC2)) +
#'   geom_scores() +
#'   geom_conf()
#' 
#' @export
perform_pca <- function(data, n_components = 2, scale = TRUE, center = TRUE, 
                        sample_names = NULL, variable_names = NULL) {
  
  # Validate input
  validate_multivar_data(data, n_components)
  
  # Get variable names if not provided
  if (is.null(variable_names)) {
    if (is.data.frame(data)) {
      variable_names <- colnames(data)
    } else if (is.matrix(data)) {
      variable_names <- colnames(data)
      if (is.null(variable_names)) {
        variable_names <- paste0("V", 1:ncol(data))
      }
    }
  }
  
  # Standardize data
  data_processed <- standardize_data(data, scale = scale)
  
  # Perform PCA using prcomp
  pca_result <- prcomp(data_processed, center = center, scale. = FALSE, 
                       n = n_components)
  
  # Extract scores and loadings
  scores <- pca_result$x[, 1:n_components, drop = FALSE]
  loadings <- pca_result$rotation[, 1:n_components, drop = FALSE]
  
  # Create tidy results
  tidy_results <- create_tidy_results(
    scores = scores,
    loadings = loadings,
    sample_names = sample_names,
    variable_names = variable_names,
    method = "pca"
  )
  
  # Calculate explained variance
  explained_variance <- calculate_explained_variance(pca_result$sdev)
  
  # Return comprehensive results
  structure(
    list(
      scores = tidy_results$scores,
      loadings = tidy_results$loadings,
      explained_variance = explained_variance,
      sdev = pca_result$sdev,
      rotation = pca_result$rotation,
      center = pca_result$center,
      scale = pca_result$scale,
      sample_names = tidy_results$sample_names,
      variable_names = tidy_results$variable_names,
      n_components = n_components,
      method = "pca"
    ),
    class = "pca"
  )
}

#' Perform Partial Least Squares Regression
#' 
#' @description Perform PLS regression on predictor and response matrices and return results in ggmultivar format.
#' 
#' @param X A numeric data frame or matrix of predictors. Rows are observations, columns are variables.
#' @param Y A numeric data frame or matrix of responses. Rows are observations, columns are response variables.
#' @param n_components Number of PLS components to compute. Default is 2.
#' @param scale Logical, whether to scale variables to unit variance. Default is TRUE.
#' @param center Logical, whether to center variables by their mean. Default is TRUE.
#' @param sample_names Optional character vector of sample names. If NULL, defaults to Sample_1, Sample_2, etc.
#' @param x_variable_names Optional character vector of X variable names. If NULL, uses column names from X.
#' @param y_variable_names Optional character vector of Y variable names. If NULL, uses column names from Y.
#' 
#' @return A list containing:
#' \itemize{
#'   \item scores: Tidy data frame with PLS scores for each sample
#'   \item loadings: Tidy data frame with PLS loadings for each X variable
#'   \item y_loadings: Tidy data frame with PLS loadings for each Y variable
#'   \item weights: Tidy data frame with PLS weights for each X variable
#'   \item explained_variance: Data frame with variance explained by each component
#'   \item regression_coefficients: Matrix of regression coefficients
#'   \item r2: R-squared values for each component
#'   \item method: Character, "pls"
#'   \item n_components: Number of components computed
#' }
#' 
#' @examples
#' # Example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y, n_components = 3)
#' 
#' # Create a ggmultivar plot
#' ggmultivar(data = pls_result, aes(L1, L2)) +
#'   geom_scores() +
#'   geom_loadings()
#' 
#' @export
perform_pls <- function(X, Y, n_components = 2, scale = TRUE, center = TRUE,
                         sample_names = NULL, x_variable_names = NULL, y_variable_names = NULL) {
  
  # Validate input
  validate_multivar_data(X, n_components)
  validate_multivar_data(Y, n_components)
  
  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same number of rows (observations)")
  }
  
  # Get variable names if not provided
  if (is.null(x_variable_names)) {
    if (is.data.frame(X)) {
      x_variable_names <- colnames(X)
    } else if (is.matrix(X)) {
      x_variable_names <- colnames(X)
      if (is.null(x_variable_names)) {
        x_variable_names <- paste0("X", 1:ncol(X))
      }
    }
  }
  
  if (is.null(y_variable_names)) {
    if (is.data.frame(Y)) {
      y_variable_names <- colnames(Y)
    } else if (is.matrix(Y)) {
      y_variable_names <- colnames(Y)
      if (is.null(y_variable_names)) {
        y_variable_names <- paste0("Y", 1:ncol(Y))
      }
    }
  }
  
  # Standardize data
  X_processed <- standardize_data(X, scale = scale)
  Y_processed <- standardize_data(Y, scale = scale)
  
  # Check if pls package is available
  if (!requireNamespace("pls", quietly = TRUE)) {
    stop("The 'pls' package is required for PLS analysis. Please install it with: install.packages('pls')")
  }
  
  # Perform PLS using the pls package
  pls_result <- pls::plsr(Y ~ X, ncomp = n_components, data = data.frame(X = X_processed, Y = Y_processed))
  
  # Extract scores (T matrix)
  scores <- pls_result$scores[, 1:n_components, drop = FALSE]
  
  # Extract X loadings (P matrix)
  x_loadings <- pls_result$loadings[, 1:n_components, drop = FALSE]
  
  # Extract Y loadings (Q matrix)
  y_loadings <- pls_result$Yloadings[, 1:n_components, drop = FALSE]
  
  # Extract weights (W matrix)
  weights <- pls_result$weights[, 1:n_components, drop = FALSE]
  
  # Create tidy results for X
  x_tidy_results <- create_tidy_results(
    scores = scores,
    loadings = x_loadings,
    sample_names = sample_names,
    variable_names = x_variable_names,
    method = "pls"
  )
  
  # Create tidy results for Y loadings
  y_loadings_tidy <- as.data.frame(y_loadings) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = y_variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading",
      names_prefix = "L"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pls",
      variable_type = "Y"
    )
  
  # Create tidy results for weights
  weights_tidy <- as.data.frame(weights) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = x_variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "weight",
      names_prefix = "L"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pls"
    )
  
  # Calculate explained variance
  explained_variance <- calculate_pls_explained_variance(pls_result)
  
  # Extract R-squared values
  r2_values <- pls_result$R2
  
  # Return comprehensive results
  structure(
    list(
      scores = x_tidy_results$scores,
      loadings = x_tidy_results$loadings,
      y_loadings = y_loadings_tidy,
      weights = weights_tidy,
      explained_variance = explained_variance,
      regression_coefficients = pls_result$coeffs,
      r2 = r2_values,
      sample_names = x_tidy_results$sample_names,
      x_variable_names = x_tidy_results$variable_names,
      y_variable_names = y_variable_names,
      n_components = n_components,
      method = "pls",
      pls_object = pls_result
    ),
    class = "pls"
  )
}

#' Calculate explained variance for PLS
#' @param pls_result Result from pls::plsr()
#' @return Data frame with explained variance
#' @noRd
calculate_pls_explained_variance <- function(pls_result) {
  # Extract variance explained for X and Y
  x_variance <- pls_result$explvar$X
  y_variance <- pls_result$explvar$Y
  
  n_components <- length(x_variance)
  
  tibble::tibble(
    component = 1:n_components,
    x_variance = x_variance,
    y_variance = y_variance,
    x_variance_percent = x_variance * 100,
    y_variance_percent = y_variance * 100,
    x_cumulative = cumsum(x_variance) * 100,
    y_cumulative = cumsum(y_variance) * 100
  )
}