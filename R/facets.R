#' Create facet layout for pairwise components
#' 
#' @description
#' facet_components creates a facet_grid or facet_wrap layout that shows
#' all pairwise combinations of components.
#' 
#' @param x_component Character vector of x-axis components to facet by
#' @param y_component Character vector of y-axis components to facet by
#' @param scales Character, "free", "fixed", "free_x", or "free_y" (default: "fixed")
#' @param type Character, "grid" or "wrap" (default: "grid")
#' @param ncol,nrow For facet_wrap, number of columns/rows
#' @param ... Additional arguments passed to facet_grid() or facet_wrap()
#' 
#' @return A faceting specification
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 4)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   facet_components(c("PC1", "PC2"), c("PC3", "PC4"))
#' 
#' # Using facet_wrap
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   facet_components(type = "wrap", ncol = 2)
#' 
#' @export
#' @importFrom ggplot2 facet_grid facet_wrap vars
facet_components <- function(x_component = NULL, y_component = NULL, 
                             scales = "fixed", type = "grid", 
                             ncol = NULL, nrow = NULL, ...) {
  
  # If no components specified, try to extract from the ggmultivar data
  # This is a bit tricky since we're not in the ggplot context yet
  # For now, we'll create a default facet specification
  
  if (is.null(x_component) && is.null(y_component)) {
    # Default to first 4 components in a grid
    x_component <- c("PC1", "PC2")
    y_component <- c("PC3", "PC4")
  }
  
  # Ensure x_component and y_component are character vectors
  x_component <- as.character(x_component)
  y_component <- as.character(y_component)
  
  if (type == "grid") {
    return(ggplot2::facet_grid(
      rows = ggplot2::vars(y_component),
      cols = ggplot2::vars(x_component),
      scales = scales,
      ...
    ))
  } else if (type == "wrap") {
    # For facet_wrap, we need to combine all components
    all_components <- c(x_component, y_component)
    return(ggplot2::facet_wrap(
      facets = ggplot2::vars(all_components),
      ncol = ncol,
      nrow = nrow,
      scales = scales,
      ...
    ))
  } else {
    stop("type must be either 'grid' or 'wrap'")
  }
}

#' Create pairwise component plots
#' 
#' @description
#' facet_pairs creates a matrix of plots showing all pairwise combinations of components.
#' This is a convenience function that creates multiple ggmultivar plots.
#' 
#' @param data Multivariate analysis object
#' @param components Character vector of component names to include
#' @param geom_layers List of geom layers to add to each plot
#' @param ncol Number of columns in the layout (default: NULL, auto-detect)
#' @param ... Additional arguments passed to each ggmultivar call
#' 
#' @return A list of ggplot objects arranged in a matrix
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 4)
#' pairs <- facet_pairs(pca_result, components = c("PC1", "PC2", "PC3", "PC4"))
#' 
#' @export
facet_pairs <- function(data, components = NULL, geom_layers = NULL, 
                       ncol = NULL, ...) {
  
  # Convert data to ggmultivar format
  gg_data <- as_ggmultivar(data)
  
  # Get component names if not provided
  if (is.null(components)) {
    method_prefix <- paste0(toupper(gg_data$method))
    components <- grep(method_prefix, names(gg_data$plot_data), value = TRUE)
    components <- components[!grepl("\.ggmultivar", components)]
  }
  
  # Create all pairwise combinations
  n_components <- length(components)
  if (n_components < 2) {
    stop("At least 2 components are required")
  }
  
  # Create pairwise combinations
  pairs <- combn(components, 2, simplify = FALSE)
  
  # Create plots for each pair
  plots <- lapply(pairs, function(pair) {
    x_comp <- pair[1]
    y_comp <- pair[2]
    
    p <- ggmultivar(data = data, aes_string(paste("x =", x_comp, ", y =", y_comp))) +
      ggplot2::ggtitle(paste(x_comp, "vs", y_comp))
    
    # Add geom layers if provided
    if (!is.null(geom_layers)) {
      for (geom_layer in geom_layers) {
        p <- p + geom_layer
      }
    } else {
      # Add default geoms
      p <- p + geom_scores()
    }
    
    p
  })
  
  # Arrange plots in a matrix
  if (!is.null(ncol)) {
    # Use cowplot or patchwork to arrange plots
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(patchwork::wrap_plots(plots, ncol = ncol))
    } else if (requireNamespace("cowplot", quietly = TRUE)) {
      return(cowplot::plot_grid(plotlist = plots, ncol = ncol))
    } else {
      warning("patchwork or cowplot package required for plot arrangement")
      return(plots)
    }
  } else {
    return(plots)
  }
}

#' Create a matrix of component plots with shared axes
#' 
#' @description
#' facet_matrix creates a matrix of plots showing all pairwise combinations
#' of components with shared axes.
#' 
#' @param data Multivariate analysis object
#' @param components Character vector of component names to include
#' @param geom_layers List of geom layers to add to each plot
#' @param diagonal Character, what to show on the diagonal: "none", "hist", "density"
#' @param upper Character, what to show in upper triangle: "none", "scatter", "cor"
#' @param lower Character, what to show in lower triangle: "none", "scatter", "cor"
#' @param ... Additional arguments
#' 
#' @return A ggpairs-like plot matrix
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 4)
#' facet_matrix(pca_result, components = c("PC1", "PC2", "PC3", "PC4"))
#' 
#' @export
facet_matrix <- function(data, components = NULL, geom_layers = NULL, 
                        diagonal = "hist", upper = "scatter", lower = "cor", ...) {
  
  # Convert data to ggmultivar format
  gg_data <- as_ggmultivar(data)
  
  # Get component names if not provided
  if (is.null(components)) {
    method_prefix <- paste0(toupper(gg_data$method))
    components <- grep(method_prefix, names(gg_data$plot_data), value = TRUE)
    components <- components[!grepl("\.ggmultivar", components)]
  }
  
  n_components <- length(components)
  if (n_components < 2) {
    stop("At least 2 components are required")
  }
  
  # Check if GGally is available for ggpairs
  if (requireNamespace("GGally", quietly = TRUE)) {
    # Extract the scores data
    scores_data <- gg_data$scores |
      tidyr::pivot_wider(
        names_from = component,
        values_from = score,
        names_prefix = paste0(toupper(gg_data$method))
      )
    
    # Select only the requested components
    if (!is.null(components)) {
      scores_data <- scores_data[, c("sample", components), drop = FALSE]
    }
    
    # Create ggpairs plot
    return(GGally::ggpairs(
      scores_data,
      columns = 2:ncol(scores_data),
      upper = list(continuous = upper),
      lower = list(continuous = lower),
      diag = list(continuous = diagonal),
      ...
    ))
  } else {
    warning("GGally package required for facet_matrix. Using facet_pairs instead.")
    return(facet_pairs(data, components, geom_layers, ...))
  }
}