#' Check if mixOmics package is available
#' @description Helper function to check mixOmics availability
#' @return Logical indicating if mixOmics is available
#' @noRd
check_mixomics <- function() {
  requireNamespace("mixOmics", quietly = TRUE)
}

#' Extract data from mixOmics PCA object
#' 
#' @description Convert a mixOmics PCA object to ggmultivar format.
#' 
#' @param mixomics_pca A PCA object from mixOmics::pca()
#' @param n_components Number of components to extract. Default is 2.
#' 
#' @return A list in the same format as perform_pca()
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' pca_result <- mixomics_to_ggmultivar(pca_mix)
#' pca_scores_plot(pca_result)
#' }
#' 
#' @export
mixomics_to_ggmultivar <- function(mixomics_pca, n_components = 2) {
  
  # Check if mixOmics is available
  if (!check_mixomics()) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(mixomics_pca, "pca")) {
    stop("Input must be a mixOmics PCA object")
  }
  
  if (n_components > mixomics_pca$ncomp) {
    stop(paste("n_components cannot exceed", mixomics_pca$ncomp))
  }
  
  # Extract scores (individuals)
  scores <- mixomics_pca$variates$X[, 1:n_components, drop = FALSE]
  
  # Extract loadings (variables)
  loadings <- mixomics_pca$loadings[, 1:n_components, drop = FALSE]
  
  # Extract sample names
  sample_names <- rownames(mixomics_pca$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract variable names
  variable_names <- rownames(mixomics_pca$loadings)
  if (is.null(variable_names) || all(variable_names == "")) {
    variable_names <- paste0("Var_", 1:ncol(loadings))
  }
  
  # Create tidy results
  tidy_results <- create_tidy_results(
    scores = scores,
    loadings = loadings,
    sample_names = sample_names,
    variable_names = variable_names,
    method = "pca"
  )
  
  # Calculate explained variance
  explained_variance <- calculate_explained_variance(mixomics_pca$eigval[1:n_components])
  
  # Return comprehensive results in ggmultivar format
  list(
    scores = tidy_results$scores,
    loadings = tidy_results$loadings,
    explained_variance = explained_variance,
    sdev = sqrt(mixomics_pca$eigval[1:n_components]),
    rotation = loadings,
    center = mixomics_pca$center,
    scale = mixomics_pca$scale,
    sample_names = tidy_results$sample_names,
    variable_names = tidy_results$variable_names,
    n_components = n_components,
    mixomics_object = mixomics_pca
  )
}

#' Extract data from mixOmics PLS object
#' 
#' @description Convert a mixOmics PLS object to ggmultivar format.
#' 
#' @param mixomics_pls A PLS object from mixOmics::pls()
#' @param n_components Number of components to extract. Default is 2.
#' 
#' @return A list in the same format as perform_pls()
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pls_mix <- pls(wine$X, wine$Y, ncomp = 3)
#' pls_result <- mixomics_pls_to_ggmultivar(pls_mix)
#' pls_scores_plot(pls_result)
#' }
#' 
#' @export
mixomics_pls_to_ggmultivar <- function(mixomics_pls, n_components = 2) {
  
  # Check if mixOmics is available
  if (!check_mixomics()) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(mixomics_pls, "pls")) {
    stop("Input must be a mixOmics PLS object")
  }
  
  if (n_components > mixomics_pls$ncomp) {
    stop(paste("n_components cannot exceed", mixomics_pls$ncomp))
  }
  
  # Extract scores (T matrix)
  scores <- mixomics_pls$variates$X[, 1:n_components, drop = FALSE]
  
  # Extract X loadings (P matrix)
  x_loadings <- mixomics_pls$loadings[, 1:n_components, drop = FALSE]
  
  # Extract Y loadings (Q matrix)
  y_loadings <- mixomics_pls$loadingsY[, 1:n_components, drop = FALSE]
  
  # Extract weights (W matrix)
  weights <- mixomics_pls$weights[, 1:n_components, drop = FALSE]
  
  # Extract sample names
  sample_names <- rownames(mixomics_pls$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract X variable names
  x_variable_names <- rownames(mixomics_pls$loadings)
  if (is.null(x_variable_names) || all(x_variable_names == "")) {
    x_variable_names <- paste0("X_", 1:ncol(x_loadings))
  }
  
  # Extract Y variable names
  y_variable_names <- rownames(mixomics_pls$loadingsY)
  if (is.null(y_variable_names) || all(y_variable_names == "")) {
    y_variable_names <- paste0("Y_", 1:ncol(y_loadings))
  }
  
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
  explained_variance <- calculate_mixomics_pls_explained_variance(mixomics_pls, n_components)
  
  # Extract R-squared values
  r2_values <- mixomics_pls$R2
  
  # Return comprehensive results in ggmultivar format
  list(
    scores = x_tidy_results$scores,
    loadings = x_tidy_results$loadings,
    y_loadings = y_loadings_tidy,
    weights = weights_tidy,
    explained_variance = explained_variance,
    regression_coefficients = mixomics_pls$coeffs,
    r2 = r2_values,
    sample_names = x_tidy_results$sample_names,
    x_variable_names = x_tidy_results$variable_names,
    y_variable_names = y_variable_names,
    n_components = n_components,
    mixomics_object = mixomics_pls
  )
}

#' Calculate explained variance for mixOmics PLS
#' @param mixomics_pls mixOmics PLS object
#' @param n_components Number of components
#' @return Data frame with explained variance
#' @noRd
calculate_mixomics_pls_explained_variance <- function(mixomics_pls, n_components) {
  # Extract variance explained for X and Y
  x_variance <- mixomics_pls$explVar$X[1:n_components]
  y_variance <- mixomics_pls$explVar$Y[1:n_components]
  
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

#' Extract data from mixOmics sPLS object
#' 
#' @description Convert a mixOmics sPLS (sparse PLS) object to ggmultivar format.
#' 
#' @param mixomics_spls A sPLS object from mixOmics::spls()
#' @param n_components Number of components to extract. Default is 2.
#' 
#' @return A list in the same format as perform_pls()
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' spls_mix <- spls(wine$X, wine$Y, ncomp = 3)
#' spls_result <- mixomics_spls_to_ggmultivar(spls_mix)
#' pls_scores_plot(spls_result)
#' }
#' 
#' @export
mixomics_spls_to_ggmultivar <- function(mixomics_spls, n_components = 2) {
  
  # Check if mixOmics is available
  if (!check_mixomics()) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(mixomics_spls, "spls")) {
    stop("Input must be a mixOmics sPLS object")
  }
  
  if (n_components > mixomics_spls$ncomp) {
    stop(paste("n_components cannot exceed", mixomics_spls$ncomp))
  }
  
  # Extract scores (T matrix)
  scores <- mixomics_spls$variates$X[, 1:n_components, drop = FALSE]
  
  # Extract X loadings (P matrix)
  x_loadings <- mixomics_spls$loadings[, 1:n_components, drop = FALSE]
  
  # Extract Y loadings (Q matrix)
  y_loadings <- mixomics_spls$loadingsY[, 1:n_components, drop = FALSE]
  
  # Extract weights (W matrix)
  weights <- mixomics_spls$weights[, 1:n_components, drop = FALSE]
  
  # Extract sample names
  sample_names <- rownames(mixomics_spls$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract X variable names
  x_variable_names <- rownames(mixomics_spls$loadings)
  if (is.null(x_variable_names) || all(x_variable_names == "")) {
    x_variable_names <- paste0("X_", 1:ncol(x_loadings))
  }
  
  # Extract Y variable names
  y_variable_names <- rownames(mixomics_spls$loadingsY)
  if (is.null(y_variable_names) || all(y_variable_names == "")) {
    y_variable_names <- paste0("Y_", 1:ncol(y_loadings))
  }
  
  # Create tidy results for X
  x_tidy_results <- create_tidy_results(
    scores = scores,
    loadings = x_loadings,
    sample_names = sample_names,
    variable_names = x_variable_names,
    method = "spls"
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
      method = "spls",
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
      method = "spls"
    )
  
  # Calculate explained variance
  explained_variance <- calculate_mixomics_pls_explained_variance(mixomics_spls, n_components)
  
  # Extract R-squared values
  r2_values <- mixomics_spls$R2
  
  # Return comprehensive results in ggmultivar format
  list(
    scores = x_tidy_results$scores,
    loadings = x_tidy_results$loadings,
    y_loadings = y_loadings_tidy,
    weights = weights_tidy,
    explained_variance = explained_variance,
    regression_coefficients = mixomics_spls$coeffs,
    r2 = r2_values,
    sample_names = x_tidy_results$sample_names,
    x_variable_names = x_tidy_results$variable_names,
    y_variable_names = y_variable_names,
    n_components = n_components,
    mixomics_object = mixomics_spls
  )
}

#' Extract data from mixOmics sPLS-DA object
#' 
#' @description Convert a mixOmics sPLS-DA object to ggmultivar format.
#' 
#' @param mixomics_splsda A sPLS-DA object from mixOmics::splsda()
#' @param n_components Number of components to extract. Default is 2.
#' 
#' @return A list in the same format as perform_pls() with additional DA-specific information
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
#' splsda_result <- mixomics_splsda_to_ggmultivar(splsda_mix)
#' pls_scores_plot(splsda_result, color_by = wine$Y)
#' }
#' 
#' @export
mixomics_splsda_to_ggmultivar <- function(mixomics_splsda, n_components = 2) {
  
  # Check if mixOmics is available
  if (!check_mixomics()) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(mixomics_splsda, "splsda")) {
    stop("Input must be a mixOmics sPLS-DA object")
  }
  
  if (n_components > mixomics_splsda$ncomp) {
    stop(paste("n_components cannot exceed", mixomics_splsda$ncomp))
  }
  
  # Extract scores (T matrix)
  scores <- mixomics_splsda$variates$X[, 1:n_components, drop = FALSE]
  
  # Extract X loadings (P matrix)
  x_loadings <- mixomics_splsda$loadings[, 1:n_components, drop = FALSE]
  
  # Extract Y loadings (Q matrix) - for sPLS-DA, this might be different
  # In sPLS-DA, Y is typically a factor, so we handle it differently
  if (!is.null(mixomics_splsda$loadingsY)) {
    y_loadings <- mixomics_splsda$loadingsY[, 1:n_components, drop = FALSE]
  } else {
    # If no Y loadings, create dummy matrix
    y_loadings <- matrix(0, nrow = 1, ncol = n_components)
  }
  
  # Extract weights (W matrix)
  weights <- mixomics_splsda$weights[, 1:n_components, drop = FALSE]
  
  # Extract sample names
  sample_names <- rownames(mixomics_splsda$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract X variable names
  x_variable_names <- rownames(mixomics_splsda$loadings)
  if (is.null(x_variable_names) || all(x_variable_names == "")) {
    x_variable_names <- paste0("X_", 1:ncol(x_loadings))
  }
  
  # For sPLS-DA, Y is typically a factor, so we get the class labels
  y_variable_names <- paste0("Class_", 1:ncol(y_loadings))
  
  # Create tidy results for X
  x_tidy_results <- create_tidy_results(
    scores = scores,
    loadings = x_loadings,
    sample_names = sample_names,
    variable_names = x_variable_names,
    method = "splsda"
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
      method = "splsda",
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
      method = "splsda"
    )
  
  # Calculate explained variance
  explained_variance <- calculate_mixomics_pls_explained_variance(mixomics_splsda, n_components)
  
  # Extract R-squared values
  r2_values <- mixomics_splsda$R2
  
  # Extract class information if available
  class_labels <- mixomics_splsda$Y
  
  # Return comprehensive results in ggmultivar format
  list(
    scores = x_tidy_results$scores,
    loadings = x_tidy_results$loadings,
    y_loadings = y_loadings_tidy,
    weights = weights_tidy,
    explained_variance = explained_variance,
    regression_coefficients = if (!is.null(mixomics_splsda$coeffs)) mixomics_splsda$coeffs else NULL,
    r2 = r2_values,
    sample_names = x_tidy_results$sample_names,
    x_variable_names = x_tidy_results$variable_names,
    y_variable_names = y_variable_names,
    n_components = n_components,
    class_labels = class_labels,
    mixomics_object = mixomics_splsda
  )
}

#' Generic function to handle any mixOmics object
#' 
#' @description Automatically detect and convert mixOmics objects to ggmultivar format.
#' 
#' @param mixomics_object A mixOmics object (PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param n_components Number of components to extract. Default is 2.
#' 
#' @return A list in ggmultivar format
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' 
#' # Works with PCA
#' pca_mix <- pca(wine$X, ncomp = 3)
#' pca_result <- mixomics_to_ggmultivar(pca_mix)
#' 
#' # Works with PLS
#' pls_mix <- pls(wine$X, wine$Y, ncomp = 3)
#' pls_result <- mixomics_to_ggmultivar(pls_mix)
#' 
#' # Works with sPLS-DA
#' splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
#' splsda_result <- mixomics_to_ggmultivar(splsda_mix)
#' }
#' 
#' @export
mixomics_to_ggmultivar <- function(mixomics_object, n_components = 2) {
  
  # Check if mixOmics is available
  if (!check_mixomics()) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Determine the type of mixOmics object
  object_class <- class(mixomics_object)
  
  # Dispatch to appropriate conversion function
  if (inherits(mixomics_object, "pca")) {
    return(mixomics_to_ggmultivar(mixomics_object, n_components))
  } else if (inherits(mixomics_object, "pls")) {
    return(mixomics_pls_to_ggmultivar(mixomics_object, n_components))
  } else if (inherits(mixomics_object, "spls")) {
    return(mixomics_spls_to_ggmultivar(mixomics_object, n_components))
  } else if (inherits(mixomics_object, "splsda")) {
    return(mixomics_splsda_to_ggmultivar(mixomics_object, n_components))
  } else if (inherits(mixomics_object, "rcc") || inherits(mixomics_object, "rgcca")) {
    # For DIABLO and RGCCA, use similar approach to PLS
    return(mixomics_pls_to_ggmultivar(mixomics_object, n_components))
  } else {
    stop(paste("Unsupported mixOmics object type:", paste(object_class, collapse = ", ")))
  }
}

#' Create scores plot directly from mixOmics object
#' 
#' @description Create a ggplot2 scores plot directly from a mixOmics object.
#' 
#' @param mixomics_object A mixOmics object (PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color points by. For sPLS-DA, this can be the class labels.
#' @param ... Additional arguments passed to the underlying plot function
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' mixomics_scores_plot(pca_mix)
#' 
#' splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
#' mixomics_scores_plot(splsda_mix, color_by = wine$Y)
#' }
#' 
#' @export
mixomics_scores_plot <- function(mixomics_object, x_component = 1, y_component = 2,
                                  color_by = NULL, ...) {
  
  # Convert mixOmics object to ggmultivar format
  gg_result <- mixomics_to_ggmultivar(mixomics_object, max(x_component, y_component))
  
  # Determine the method type
  method <- gg_result$method
  
  # For sPLS-DA, if color_by is not provided, use class labels if available
  if (is.null(color_by) && !is.null(gg_result$class_labels)) {
    color_by <- gg_result$class_labels
  }
  
  # Create appropriate plot based on method
  if (method %in% c("pca", "mixomics")) {
    return(pca_scores_plot(gg_result, x_component, y_component, 
                           color_by = color_by, ...))
  } else {
    return(pls_scores_plot(gg_result, x_component, y_component, 
                          color_by = color_by, ...))
  }
}

#' Create loadings plot directly from mixOmics object
#' 
#' @description Create a ggplot2 loadings plot directly from a mixOmics object.
#' 
#' @param mixomics_object A mixOmics object (PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param ... Additional arguments passed to the underlying plot function
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' mixomics_loadings_plot(pca_mix)
#' }
#' 
#' @export
mixomics_loadings_plot <- function(mixomics_object, x_component = 1, y_component = 2, ...) {
  
  # Convert mixOmics object to ggmultivar format
  gg_result <- mixomics_to_ggmultivar(mixomics_object, max(x_component, y_component))
  
  # Determine the method type
  method <- gg_result$method
  
  # Create appropriate plot based on method
  if (method %in% c("pca", "mixomics")) {
    return(pca_loadings_plot(gg_result, x_component, y_component, ...))
  } else {
    return(pls_loadings_plot(gg_result, x_component, y_component, ...))
  }
}

#' Create biplot directly from mixOmics object
#' 
#' @description Create a biplot directly from a mixOmics object.
#' 
#' @param mixomics_object A mixOmics object (PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color points by.
#' @param ... Additional arguments passed to the underlying plot function
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' mixomics_biplot(pca_mix)
#' }
#' 
#' @export
mixomics_biplot <- function(mixomics_object, x_component = 1, y_component = 2,
                             color_by = NULL, ...) {
  
  # Convert mixOmics object to ggmultivar format
  gg_result <- mixomics_to_ggmultivar(mixomics_object, max(x_component, y_component))
  
  # Determine the method type
  method <- gg_result$method
  
  # For sPLS-DA, if color_by is not provided, use class labels if available
  if (is.null(color_by) && !is.null(gg_result$class_labels)) {
    color_by <- gg_result$class_labels
  }
  
  # Create appropriate plot based on method
  if (method %in% c("pca", "mixomics")) {
    return(pca_biplot(gg_result, x_component, y_component, 
                      color_by = color_by, ...))
  } else {
    return(pls_biplot(gg_result, x_component, y_component, 
                     color_by = color_by, ...))
  }
}