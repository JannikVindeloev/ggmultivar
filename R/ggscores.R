#' Create Scores Plot
#' 
#' @description Create a ggplot2 scores plot from multivariate analysis results.
#' This function creates the base plot and returns a ggplot object that can be
#' further customized with additional geoms and layers.
#' 
#' @param data A data frame with scores (must contain columns: sample, component, score)
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color points by. Can be a vector or column name.
#' @param shape_by Optional variable to shape points by. Can be a vector or column name.
#' @param size_by Optional variable to size points by. Can be a vector or column name.
#' @param facet_by Optional variable to facet by. Can be a vector or column name.
#' @param show_ellipse Logical, whether to show confidence ellipses. Default is FALSE.
#' @param ellipse_level Confidence level for ellipses (0-1). Default is 0.95.
#' @param show_labels Logical, whether to show sample labels. Default is FALSE.
#' @param label_size Size of sample labels. Default is 3.
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param explained_variance Optional data frame with variance explained information.
#' @param method Character, method type for default labels ("pca", "pls", etc.). Default is "pca".
#' @param ... Additional arguments passed to geom_point()
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' # Basic scores plot
#' ggscores(pca_obj) +
#'   ggtitle("PCA Scores")
#' 
#' # With coloring by class
#' ggscores(pca_obj, color_by = wine$Y) +
#'   geom_encircle(group_by = "color_by")
#' 
#' # With faceting
#' ggscores(pca_obj, facet_by = wine$Y)
#' }
#' 
#' @export
ggscores <- function(data, x_component = 1, y_component = 2,
                     color_by = NULL, shape_by = NULL, size_by = NULL,
                     facet_by = NULL, show_ellipse = FALSE, ellipse_level = 0.95,
                     show_labels = FALSE, label_size = 3,
                     xlab = NULL, ylab = NULL, explained_variance = NULL,
                     method = "pca", ...) {
  
  # If data is a mixOmics object, extract scores
  if (inherits(data, c("pca", "pls", "spls", "splsda", "rgcca", "diablo"))) {
    scores_data <- scores(data, components = c(x_component, y_component))
    if (is.null(explained_variance) && inherits(data, c("pca", "pls", "spls", "splsda", "rgcca", "diablo"))) {
      explained_variance <- explained_variance(data)
    }
  } else if (is.data.frame(data)) {
    scores_data <- data
  } else {
    stop("data must be a data frame or a multivariate analysis object")
  }
  
  # Ensure we have the required columns
  if (!all(c("sample", "component", "score") %in% colnames(scores_data))) {
    stop("Data must contain columns: sample, component, score")
  }
  
  # Filter to selected components
  scores_subset <- scores_data |>
    dplyr::filter(component %in% c(x_component, y_component))
  
  # Pivot to wide format for plotting
  scores_wide <- scores_subset |>
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "PC"
    )
  
  # Add aesthetics columns if provided
  if (!is.null(color_by)) {
    if (is.character(color_by) && length(color_by) == 1) {
      # color_by is a column name
      if (color_by %in% colnames(scores_wide)) {
        scores_wide[[paste0("color_var_", color_by)]] <- scores_wide[[color_by]]
        color_by <- paste0("color_var_", color_by)
      } else {
        stop(paste("Column", color_by, "not found in data"))
      }
    } else if (is.vector(color_by)) {
      # color_by is a vector
      if (length(color_by) != nrow(scores_wide)) {
        stop("color_by must have the same length as the number of samples")
      }
      scores_wide$color_var <- color_by
      color_by <- "color_var"
    }
  }
  
  if (!is.null(shape_by)) {
    if (is.character(shape_by) && length(shape_by) == 1) {
      if (shape_by %in% colnames(scores_wide)) {
        scores_wide[[paste0("shape_var_", shape_by)]] <- as.factor(scores_wide[[shape_by]])
        shape_by <- paste0("shape_var_", shape_by)
      } else {
        stop(paste("Column", shape_by, "not found in data"))
      }
    } else if (is.vector(shape_by)) {
      if (length(shape_by) != nrow(scores_wide)) {
        stop("shape_by must have the same length as the number of samples")
      }
      scores_wide$shape_var <- as.factor(shape_by)
      shape_by <- "shape_var"
    }
  }
  
  if (!is.null(size_by)) {
    if (is.character(size_by) && length(size_by) == 1) {
      if (size_by %in% colnames(scores_wide)) {
        scores_wide[[paste0("size_var_", size_by)]] <- scores_wide[[size_by]]
        size_by <- paste0("size_var_", size_by)
      } else {
        stop(paste("Column", size_by, "not found in data"))
      }
    } else if (is.vector(size_by)) {
      if (length(size_by) != nrow(scores_wide)) {
        stop("size_by must have the same length as the number of samples")
      }
      scores_wide$size_var <- size_by
      size_by <- "size_var"
    }
  }
  
  if (!is.null(facet_by)) {
    if (is.character(facet_by) && length(facet_by) == 1) {
      if (facet_by %in% colnames(scores_wide)) {
        scores_wide[[paste0("facet_var_", facet_by)]] <- as.factor(scores_wide[[facet_by]])
        facet_by <- paste0("facet_var_", facet_by)
      } else {
        stop(paste("Column", facet_by, "not found in data"))
      }
    } else if (is.vector(facet_by)) {
      if (length(facet_by) != nrow(scores_wide)) {
        stop("facet_by must have the same length as the number of samples")
      }
      scores_wide$facet_var <- as.factor(facet_by)
      facet_by <- "facet_var"
    }
  }
  
  # Create base aesthetic mapping
  aes_mapping <- ggplot2::aes(
    x = .data[[paste0("PC", x_component)]],
    y = .data[[paste0("PC", y_component)]]
  )
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    aes_mapping <- aes_mapping + ggplot2::aes(color = .data[[color_by]])
  }
  
  # Add shape aesthetic if provided
  if (!is.null(shape_by)) {
    aes_mapping <- aes_mapping + ggplot2::aes(shape = .data[[shape_by]])
  }
  
  # Add size aesthetic if provided
  if (!is.null(size_by)) {
    aes_mapping <- aes_mapping + ggplot2::aes(size = .data[[size_by]])
  }
  
  # Create base plot
  p <- ggplot2::ggplot(scores_wide, aes_mapping) +
    ggplot2::geom_point(...)
  
  # Add ellipses if requested and color_by is provided
  if (show_ellipse && !is.null(color_by)) {
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(group = .data[[color_by]]),
      level = ellipse_level,
      fill = NA,
      color = "gray50",
      linetype = "dashed"
    )
  }
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sample),
      size = label_size,
      vjust = -0.5,
      hjust = 0.5,
      check_overlap = TRUE
    )
  }
  
  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_by]]))
  }
  
  # Set axis labels
  if (is.null(xlab)) {
    if (!is.null(explained_variance) && x_component <= nrow(explained_variance)) {
      if (method == "pca") {
        xlab <- paste0("PC", x_component, " (", 
                       round(explained_variance$variance_percent[x_component], 1), "%)")
      } else {
        xlab <- paste0("LV", x_component, " (", 
                       round(explained_variance$x_variance_percent[x_component], 1), "% X var)")
      }
    } else {
      if (method == "pca") {
        xlab <- paste0("PC", x_component)
      } else {
        xlab <- paste0("LV", x_component)
      }
    }
  }
  
  if (is.null(ylab)) {
    if (!is.null(explained_variance) && y_component <= nrow(explained_variance)) {
      if (method == "pca") {
        ylab <- paste0("PC", y_component, " (", 
                       round(explained_variance$variance_percent[y_component], 1), "%)")
      } else {
        ylab <- paste0("LV", y_component, " (", 
                       round(explained_variance$x_variance_percent[y_component], 1), "% X var)")
      }
    } else {
      if (method == "pca") {
        ylab <- paste0("PC", y_component)
      } else {
        ylab <- paste0("LV", y_component)
      }
    }
  }
  
  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  return(p)
}
