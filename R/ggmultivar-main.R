#' ggmultivar: Create multivariate analysis plots with ggplot2 syntax
#' 
#' @description
#' ggmultivar creates a base plot object that can be extended with custom geoms
#' for multivariate analysis (PCA, PLS, etc.). It uses S3 method dispatch to handle
#' different types of multivariate objects.
#' 
#' @param data A multivariate analysis object (PCA, PLS, mixOmics object, etc.)
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param ... Additional arguments passed to the underlying plot function
#' 
#' @return A ggmultivar object that can be extended with custom geoms
#' 
#' @examples
#' library(ggmultivar)
#' 
#' # With PCA object
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   geom_conf()
#' 
#' # With mixOmics object
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_mix <- pca(wine$X, ncomp = 3)
#' ggmultivar(data = pca_mix, aes(x = Comp1, y = Comp2)) +
#'   geom_scores(aes(color = wine$Y)) +
#'   geom_loadings()
#' }
#' 
#' @export
#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang inherits_s3
ggmultivar <- function(data, mapping = NULL, ...) {
  
  # Validate input data
  if (missing(data)) {
    stop("data argument is required")
  }
  
  # Convert data to ggmultivar format using S3 dispatch
  gg_data <- as_ggmultivar(data, ...)
  
  # Create base ggplot object
  if (is.null(mapping)) {
    mapping <- aes(x = .data[[".ggmultivar_x"]], y = .data[[".ggmultivar_y"]])
  }
  
  # Create the initial plot with the processed data
  p <- ggplot2::ggplot(gg_data$plot_data, mapping) +
    ggplot2::theme_minimal()
  
  # Add ggmultivar class and store metadata
  class(p) <- c("ggmultivar", class(p))
  attr(p, "ggmultivar_data") <- gg_data
  attr(p, "ggmultivar_original") <- data
  
  return(p)
}

#' Convert object to ggmultivar format
#' 
#' @description
#' S3 generic function to convert various multivariate objects to ggmultivar format
#' 
#' @param x Object to convert
#' @param ... Additional arguments
#' 
#' @return List with plot_data and metadata for ggmultivar
#' 
#' @export
#' @rdname as_ggmultivar
as_ggmultivar <- function(x, ...) {
  UseMethod("as_ggmultivar")
}

#' @export
#' @rdname as_ggmultivar
as_ggmultivar.default <- function(x, ...) {
  stop(paste("No as_ggmultivar method for object of class", class(x)))
}

#' @export
#' @rdname as_ggmultivar
as_ggmultivar.list <- function(x, ...) {
  # Handle ggmultivar results from perform_pca, perform_pls, etc.
  if (!all(c("scores", "loadings", "explained_variance") %in% names(x))) {
    stop("List must contain scores, loadings, and explained_variance components")
  }
  
  # Extract method type
  method <- ifelse("method" %in% names(x), x$method, "pca")
  
  # Create plot data with all components
  plot_data <- create_ggmultivar_plot_data(x, method)
  
  list(
    plot_data = plot_data,
    scores = x$scores,
    loadings = x$loadings,
    explained_variance = x$explained_variance,
    method = method,
    n_components = x$n_components,
    original_object = x
  )
}

#' @export
#' @rdname as_ggmultivar
as_ggmultivar.pca <- function(x, ...) {
  # Handle ggmultivar PCA results (from perform_pca)
  if (!all(c("scores", "loadings", "explained_variance", "method") %in% names(x))) {
    stop("PCA object must contain scores, loadings, explained_variance, and method components")
  }
  
  # Create plot data with all components
  plot_data <- create_ggmultivar_plot_data(x, x$method)
  
  list(
    plot_data = plot_data,
    scores = x$scores,
    loadings = x$loadings,
    explained_variance = x$explained_variance,
    method = x$method,
    n_components = x$n_components,
    original_object = x
  )
}

#' @export
#' @rdname as_ggmultivar
as_ggmultivar.pls <- function(x, ...) {
  # Handle ggmultivar PLS results (from perform_pls)
  if (!all(c("scores", "loadings", "explained_variance", "method") %in% names(x))) {
    stop("PLS object must contain scores, loadings, explained_variance, and method components")
  }
  
  # Create plot data with all components
  plot_data <- create_ggmultivar_plot_data(x, x$method)
  
  list(
    plot_data = plot_data,
    scores = x$scores,
    loadings = x$loadings,
    y_loadings = if (!is.null(x$y_loadings)) x$y_loadings else NULL,
    explained_variance = x$explained_variance,
    method = x$method,
    n_components = x$n_components,
    original_object = x
  )
}

#' Create ggmultivar plot data
#' 
#' @param x Multivariate analysis result
#' @param method Method type (pca, pls, etc.)
#' @return Data frame with all components for plotting
#' @noRd
create_ggmultivar_plot_data <- function(x, method = "pca") {
  
  # Get all scores components
  scores_wide <- x$scores |
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = paste0(toupper(method))
    )
  
  # Get all loadings components
  loadings_wide <- x$loadings |
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = paste0(toupper(method), "L")
    )
  
  # Combine scores and loadings
  plot_data <- scores_wide |
    dplyr::full_join(loadings_wide, by = character(0))
  
  # Add metadata
  plot_data$.ggmultivar_method <- method
  plot_data$.ggmultivar_n_components <- x$n_components
  
  # Set default x and y components based on method
  if (method == "pca") {
    plot_data$.ggmultivar_x <- plot_data$PC1
    plot_data$.ggmultivar_y <- plot_data$PC2
  } else if (method == "pls") {
    plot_data$.ggmultivar_x <- plot_data$L1
    plot_data$.ggmultivar_y <- plot_data$L2
  } else {
    # Try to find the first two components
    comp_cols <- grep(paste0(toupper(method)), names(plot_data), value = TRUE)
    comp_cols <- comp_cols[!grepl("\.ggmultivar", comp_cols)]
    if (length(comp_cols) >= 2) {
      plot_data$.ggmultivar_x <- plot_data[[comp_cols[1]]]
      plot_data$.ggmultivar_y <- plot_data[[comp_cols[2]]]
    } else {
      plot_data$.ggmultivar_x <- plot_data[[paste0(toupper(method), "1")]]
      plot_data$.ggmultivar_y <- plot_data[[paste0(toupper(method), "2")]]
    }
  }
  
  return(plot_data)
}

#' Check if object is a ggmultivar object
#' 
#' @param x Object to check
#' @return Logical
#' @noRd
is_ggmultivar <- function(x) {
  inherits(x, "ggmultivar")
}

#' Get ggmultivar data from a ggmultivar object
#' 
#' @param p ggmultivar object
#' @return ggmultivar data list
#' @noRd
get_ggmultivar_data <- function(p) {
  if (!is_ggmultivar(p)) {
    stop("Object is not a ggmultivar plot")
  }
  attr(p, "ggmultivar_data")
}

#' Get original data from a ggmultivar object
#' 
#' @param p ggmultivar object
#' @return Original data object
#' @noRd
get_ggmultivar_original <- function(p) {
  if (!is_ggmultivar(p)) {
    stop("Object is not a ggmultivar plot")
  }
  attr(p, "ggmultivar_original")
}