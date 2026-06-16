#' S3 method for mixOmics PCA objects
#' 
#' @description
#' Convert mixOmics PCA objects to ggmultivar format
#' 
#' @param x mixOmics PCA object
#' @param ... Additional arguments
#' 
#' @return ggmultivar data list
#' 
#' @export
#' @rdname as_ggmultivar
as_ggmultivar.pca <- function(x, ...) {
  
  # Check if mixOmics is available
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(x, "pca")) {
    stop("Input must be a mixOmics PCA object")
  }
  
  # Extract scores (individuals)
  scores <- x$variates$X
  
  # Extract loadings (variables)
  loadings <- x$loadings
  
  # Extract sample names
  sample_names <- rownames(x$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract variable names
  variable_names <- rownames(x$loadings)
  if (is.null(variable_names) || all(variable_names == "")) {
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
      names_prefix = "Comp"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pca"
    )
  
  # Create tidy loadings
  loadings_tidy <- as.data.frame(loadings) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading",
      names_prefix = "Comp"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pca"
    )
  
  # Calculate explained variance
  explained_variance <- calculate_explained_variance(sqrt(x$eigval))
  
  # Create ggmultivar data
  gg_data <- list(
    scores = scores_tidy,
    loadings = loadings_tidy,
    explained_variance = explained_variance,
    sdev = sqrt(x$eigval),
    rotation = loadings,
    center = x$center,
    scale = x$scale,
    sample_names = sample_names,
    variable_names = variable_names,
    n_components = x$ncomp,
    method = "pca",
    original_object = x
  )
  
  # Create plot data with all components
  plot_data <- create_ggmultivar_plot_data(gg_data, "pca")
  
  list(
    plot_data = plot_data,
    scores = gg_data$scores,
    loadings = gg_data$loadings,
    explained_variance = gg_data$explained_variance,
    method = "pca",
    n_components = gg_data$n_components,
    original_object = x
  )
}

#' S3 method for mixOmics PLS objects
#' 
#' @description
#' Convert mixOmics PLS objects to ggmultivar format
#' 
#' @param x mixOmics PLS object
#' @param ... Additional arguments
#' 
#' @return ggmultivar data list
#' 
#' @export
#' @rdname as_ggmultivar
as_ggmultivar.pls <- function(x, ...) {
  
  # Check if mixOmics is available
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(x, "pls")) {
    stop("Input must be a mixOmics PLS object")
  }
  
  # Extract scores (T matrix)
  scores <- x$variates$X
  
  # Extract X loadings (P matrix)
  x_loadings <- x$loadings
  
  # Extract Y loadings (Q matrix)
  y_loadings <- x$loadingsY
  
  # Extract sample names
  sample_names <- rownames(x$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores))
  }
  
  # Extract X variable names
  x_variable_names <- rownames(x$loadings)
  if (is.null(x_variable_names) || all(x_variable_names == "")) {
    x_variable_names <- paste0("X_", 1:ncol(x_loadings))
  }
  
  # Extract Y variable names
  y_variable_names <- rownames(x$loadingsY)
  if (is.null(y_variable_names) || all(y_variable_names == "")) {
    y_variable_names <- paste0("Y_", 1:ncol(y_loadings))
  }
  
  # Create tidy scores
  scores_tidy <- as.data.frame(scores) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(sample = sample_names) |
    tidyr::pivot_longer(
      cols = -sample,
      names_to = "component",
      values_to = "score",
      names_prefix = "Comp"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pls"
    )
  
  # Create tidy X loadings
  x_loadings_tidy <- as.data.frame(x_loadings) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = x_variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading",
      names_prefix = "Comp"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pls",
      variable_type = "X"
    )
  
  # Create tidy Y loadings
  y_loadings_tidy <- as.data.frame(y_loadings) |
    tibble::as_tibble(., .name_repair = "unique") |
    dplyr::mutate(variable = y_variable_names) |
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading",
      names_prefix = "Comp"
    ) |
    dplyr::mutate(
      component = as.numeric(component),
      method = "pls",
      variable_type = "Y"
    )
  
  # Calculate explained variance
  explained_variance <- calculate_mixomics_pls_explained_variance(x)
  
  # Create ggmultivar data
  gg_data <- list(
    scores = scores_tidy,
    loadings = x_loadings_tidy,
    y_loadings = y_loadings_tidy,
    explained_variance = explained_variance,
    regression_coefficients = x$coeffs,
    r2 = x$R2,
    sample_names = sample_names,
    x_variable_names = x_variable_names,
    y_variable_names = y_variable_names,
    n_components = x$ncomp,
    method = "pls",
    original_object = x
  )
  
  # Create plot data with all components
  plot_data <- create_ggmultivar_plot_data(gg_data, "pls")
  
  list(
    plot_data = plot_data,
    scores = gg_data$scores,
    loadings = gg_data$loadings,
    y_loadings = gg_data$y_loadings,
    explained_variance = gg_data$explained_variance,
    method = "pls",
    n_components = gg_data$n_components,
    original_object = x
  )
}

#' S3 method for mixOmics sPLS objects
#' 
#' @description
#' Convert mixOmics sPLS objects to ggmultivar format
#' 
#' @param x mixOmics sPLS object
#' @param ... Additional arguments
#' 
#' @return ggmultivar data list
#' 
#' @export
#' @rdname as_ggmultivar
as_ggmultivar.spls <- function(x, ...) {
  
  # Check if mixOmics is available
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(x, "spls")) {
    stop("Input must be a mixOmics sPLS object")
  }
  
  # Use the same logic as PLS but with spls method
  pls_data <- as_ggmultivar.pls(x, ...)
  pls_data$method <- "spls"
  pls_data
}

#' S3 method for mixOmics sPLS-DA objects
#' 
#' @description
#' Convert mixOmics sPLS-DA objects to ggmultivar format
#' 
#' @param x mixOmics sPLS-DA object
#' @param ... Additional arguments
#' 
#' @return ggmultivar data list
#' 
#' @export
#' @rdname as_ggmultivar
as_ggmultivar.splsda <- function(x, ...) {
  
  # Check if mixOmics is available
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("The 'mixOmics' package is required. Please install it with: install.packages('mixOmics')")
  }
  
  # Validate input
  if (!inherits(x, "splsda")) {
    stop("Input must be a mixOmics sPLS-DA object")
  }
  
  # Use the same logic as PLS but with splsda method
  pls_data <- as_ggmultivar.pls(x, ...)
  pls_data$method <- "splsda"
  
  # Add class labels if available
  if (!is.null(x$Y)) {
    pls_data$class_labels <- x$Y
  }
  
  pls_data
}

#' Calculate explained variance for mixOmics PLS
#' @param x mixOmics PLS object
#' @return Data frame with explained variance
#' @noRd
calculate_mixomics_pls_explained_variance <- function(x) {
  # Extract variance explained for X and Y
  x_variance <- x$explVar$X
  y_variance <- x$explVar$Y
  
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