#' Check if required packages are available
#' @description Helper function to check package availability
#' @param packages Character vector of package names
#' @return Logical vector indicating availability
#' @noRd
check_packages <- function(packages) {
  sapply(packages, function(pkg) requireNamespace(pkg, quietly = TRUE))
}

#' Validate data structure for multivariate analysis
#' @param data Data frame or matrix
#' @param n_components Number of components to validate
#' @return TRUE if valid, throws error otherwise
#' @noRd
validate_multivar_data <- function(data, n_components = 2) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or matrix")
  }
  
  if (ncol(data) < 2) {
    stop("Data must have at least 2 columns (variables)")
  }
  
  if (nrow(data) < 2) {
    stop("Data must have at least 2 rows (observations)")
  }
  
  if (n_components < 1 || n_components > min(nrow(data), ncol(data))) {
    stop(paste("n_components must be between 1 and", min(nrow(data), ncol(data))))
  }
  
  return(TRUE)
}

#' Standardize data (mean center and scale)
#' @param data Data frame or matrix
#' @param scale Logical, whether to scale variables to unit variance
#' @return Standardized data
#' @noRd
standardize_data <- function(data, scale = TRUE) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or matrix")
  }
  
  # Convert to matrix if data frame
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  
  # Mean center
  data_centered <- scale(data, center = TRUE, scale = FALSE)
  
  # Scale if requested
  if (scale) {
    data_centered <- scale(data_centered, center = FALSE, scale = TRUE)
  }
  
  return(data_centered)
}

#' Calculate explained variance percentage
#' @param sdev Standard deviations from PCA
#' @return Data frame with explained variance
#' @noRd
calculate_explained_variance <- function(sdev) {
  total_variance <- sum(sdev^2)
  explained_variance <- (sdev^2) / total_variance * 100
  
  tibble::tibble(
    component = 1:length(sdev),
    variance = sdev^2,
    variance_percent = explained_variance,
    cumulative_variance = cumsum(explained_variance)
  )
}

#' Create tidy data frame from PCA/PLS results
#' @param scores Matrix of scores
#' @param loadings Matrix of loadings
#' @param sample_names Character vector of sample names
#' @param variable_names Character vector of variable names
#' @param method Character, "pca" or "pls"
#' @return List with tidy scores and loadings data frames
#' @noRd
create_tidy_results <- function(scores, loadings, sample_names = NULL, variable_names = NULL, method = "pca") {
  # Default names if not provided
  if (is.null(sample_names)) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  if (is.null(variable_names)) {
    variable_names <- paste0("Var_", 1:ncol(loadings))
  }
  
  # Create tidy scores
  scores_tidy <- as.data.frame(scores) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(sample = sample_names) |
    tidyr::pivot_longer(
      cols = -sample,
      names_to = "component",
      values_to = "score",
      names_prefix = "PC"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = method
    )
  
  # Create tidy loadings
  loadings_tidy <- as.data.frame(loadings) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading",
      names_prefix = "PC"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = method
    )
  
  list(
    scores = scores_tidy,
    loadings = loadings_tidy,
    sample_names = sample_names,
    variable_names = variable_names
  )
}