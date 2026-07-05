#' Create Loadings Plot
#' 
#' @description Create a ggplot2 loadings plot from multivariate analysis results.
#' This function creates the base plot and returns a ggplot object that can be
#' further customized with additional geoms and layers.
#' 
#' @param data A data frame with loadings (must contain columns: variable, component, loading)
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param show_labels Logical, whether to show variable labels. Default is TRUE.
#' @param label_size Size of variable labels. Default is 4.
#' @param show_arrows Logical, whether to show arrows from origin to loadings. Default is TRUE.
#' @param arrow_length Scaling factor for arrow length. Default is 1.
#' @param color_by Optional variable to color loadings by. Can be a vector or column name.
#' @param circle_radius Radius of the unit circle. Default is 1.
#' @param show_circle Logical, whether to show unit circle. Default is TRUE.
#' @param facet_by Optional variable to facet by. Can be a vector or column name.
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param method Character, method type for default labels ("pca", "pls", etc.). Default is "pca".
#' @param ... Additional arguments passed to geom_text() or geom_segment()
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' # Create sample loadings data
#' loadings_data <- data.frame(
#'   variable = paste0("Var_", 1:10),
#'   component = rep(1:2, each = 10),
#'   loading = rnorm(20)
#' )
#' 
#' # Basic loadings plot
#' ggloadings(loadings_data) +
#'   ggtitle("Basic Loadings Plot")
#' 
#' # With coloring by loading magnitude
#' ggloadings(loadings_data, color_by = abs(loadings_data$loading))
#' 
#' # With faceting
#' loadings_data$variable_type <- rep(c("X", "Y"), each = 5)
#' ggloadings(loadings_data, facet_by = "variable_type")
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' # Basic loadings plot
#' ggloadings(pca_obj) +
#'   ggtitle("PCA Loadings")
#' 
#' # With coloring by loading magnitude
#' ggloadings(pca_obj, color_by = abs(loadings(pca_obj)$loading))
#' 
#' # With faceting
#' ggloadings(pca_obj, facet_by = "variable_type")
#' }
#' 
#' @export
ggloadings <- function(
    data,
    x_component = 1,
    y_component = 2,
    show_labels = TRUE,
    label_size = 4,
    show_arrows = TRUE,
    arrow_length = 1,
    color_by = NULL,
    circle_radius = 1,
    show_circle = TRUE,
    facet_by = NULL,
    xlab = NULL,
    ylab = NULL,
    method = "pca",
    ...
) {
  
  # If data is a mixOmics object, extract loadings
  if (inherits(data, c("pca", "pls", "spls", "splsda", "rgcca", "diablo"))) {
    loadings_data <- loadings(data, components = c(x_component, y_component))
  } else if (is.data.frame(data)) {
    loadings_data <- data
  } else {
    stop("data must be a data frame or a multivariate analysis object")
  }
  
  # Ensure we have the required columns
  if (!all(c("variable", "component", "loading") %in% colnames(loadings_data))) {
    stop("Data must contain columns: variable, component, loading")
  }
  
  # Filter to selected components
  loadings_subset <- loadings_data |>
    filter(component %in% c(x_component, y_component))
  
  # Pivot to wide format for plotting
  loadings_wide <- loadings_subset |>
    pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "PC"
    )
  
  # Add scaled coordinates for plotting
  loadings_wide <- loadings_wide |>
    mutate(
      x_scaled = .data[[paste0("PC", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("PC", y_component)]] * arrow_length
    )
  
  # Add aesthetics columns if provided
  if (!is.null(color_by)) {
    if (is.character(color_by) && length(color_by) == 1) {
      if (color_by %in% colnames(loadings_wide)) {
        loadings_wide[[paste0("color_var_", color_by)]] <- loadings_wide[[color_by]]
        color_by <- paste0("color_var_", color_by)
      } else {
        stop(paste("Column", color_by, "not found in data"))
      }
    } else if (is.vector(color_by)) {
      if (length(color_by) != nrow(loadings_wide)) {
        stop("color_by must have the same length as the number of variables")
      }
      loadings_wide$color_var <- color_by
      color_by <- "color_var"
    }
  }
  
  if (!is.null(facet_by)) {
    if (is.character(facet_by) && length(facet_by) == 1) {
      if (facet_by %in% colnames(loadings_wide)) {
        loadings_wide[[paste0("facet_var_", facet_by)]] <- as.factor(loadings_wide[[facet_by]])
        facet_by <- paste0("facet_var_", facet_by)
      } else {
        stop(paste("Column", facet_by, "not found in data"))
      }
    } else if (is.vector(facet_by)) {
      if (length(facet_by) != nrow(loadings_wide)) {
        stop("facet_by must have the same length as the number of variables")
      }
      loadings_wide$facet_var <- as.factor(facet_by)
      facet_by <- "facet_var"
    }
  }
  
  # Create base plot
  p <- ggplot(loadings_wide, 
                       aes(x = x_scaled, y = y_scaled))
  
  # Add arrows if requested
  if (show_arrows) {
    p <- p + geom_segment(
      aes(x = 0, y = 0, xend = x_scaled, yend = y_scaled),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "gray50",
      ...
    )
  }
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    p <- p + aes(color = .data[[color_by]])
  }
  
  # Add labels if requested
  if (show_labels) {
    p <- p + geom_text(
      aes(label = variable),
      size = label_size,
      vjust = ifelse(loadings_wide$y_scaled > 0, -0.5, 1.5),
      hjust = ifelse(loadings_wide$x_scaled > 0, -0.1, 1.1),
      ...
    )
  }
  
  # Add unit circle if requested
  if (show_circle) {
    circle_data <- data.frame(
      theta = seq(0, 2 * pi, length.out = 100),
      x = circle_radius * cos(seq(0, 2 * pi, length.out = 100)),
      y = circle_radius * sin(seq(0, 2 * pi, length.out = 100))
    )
    p <- p + geom_path(
      data = circle_data,
      aes(x = x, y = y),
      color = "gray70",
      linetype = "dashed"
    )
  }
  
  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + facet_wrap(vars(.data[[facet_by]]))
  }
  
  # Set axis labels
  if (is.null(xlab)) {
    if (method == "pca") {
      xlab <- paste0("PC", x_component, " loadings")
    } else {
      xlab <- paste0("LV", x_component, " loadings")
    }
  }
  
  if (is.null(ylab)) {
    if (method == "pca") {
      ylab <- paste0("PC", y_component, " loadings")
    } else {
      ylab <- paste0("LV", y_component, " loadings")
    }
  }
  
  p <- p +
    xlab(xlab) +
    ylab(ylab) +
    xlim(-circle_radius * 1.1, circle_radius * 1.1) +
    ylim(-circle_radius * 1.1, circle_radius * 1.1)
  
  return(p)
}
