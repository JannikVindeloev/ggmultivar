#' Create Biplot (Scores + Loadings)
#' 
#' @description Create a ggplot2 biplot showing both scores and loadings.
#' This function creates the base plot and returns a ggplot object that can be
#' further customized with additional geoms and layers.
#' 
#' @param data A multivariate analysis object or a list containing scores and loadings data frames
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color scores by. Can be a vector or column name.
#' @param show_ellipse Logical, whether to show confidence ellipses. Default is FALSE.
#' @param ellipse_level Confidence level for ellipses (0-1). Default is 0.95.
#' @param show_score_labels Logical, whether to show sample labels. Default is FALSE.
#' @param show_loading_labels Logical, whether to show variable labels. Default is TRUE.
#' @param loading_label_size Size of variable labels. Default is 4.
#' @param score_label_size Size of sample labels. Default is 3.
#' @param arrow_length Scaling factor for loading arrows. Default is 1.
#' @param circle_radius Radius of the unit circle. Default is 1.
#' @param show_circle Logical, whether to show unit circle. Default is TRUE.
#' @param facet_by Optional variable to facet by. Can be a vector or column name.
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param explained_variance Optional data frame with variance explained information.
#' @param method Character, method type for default labels ("pca", "pls", etc.). Default is "pca".
#' @param ... Additional arguments passed to geoms
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' # Create sample data
#' scores_data <- data.frame(
#'   sample = paste0("Sample_", 1:50),
#'   component = rep(1:2, each = 50),
#'   score = c(rnorm(50), rnorm(50))
#' )
#' 
#' loadings_data <- data.frame(
#'   variable = paste0("Var_", 1:10),
#'   component = rep(1:2, each = 10),
#'   loading = rnorm(20)
#' )
#' 
#' # Basic biplot
#' ggbiplot(list(scores = scores_data, loadings = loadings_data)) +
#'   ggtitle("Basic Biplot")
#' 
#' # With coloring by a grouping variable
#' color_vec <- sample(letters[1:3], 50, replace = TRUE)
#' ggbiplot(list(scores = scores_data, loadings = loadings_data),
#'          color_by = color_vec) +
#'   geom_encircle(group_by = "color_by")
#' 
#' # With faceting
#' facet_vec <- sample(letters[1:2], 50, replace = TRUE)
#' ggbiplot(list(scores = scores_data, loadings = loadings_data),
#'          facet_by = facet_vec)
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' # Basic biplot
#' ggbiplot(pca_obj) +
#'   ggtitle("PCA Biplot")
#' 
#' # With coloring by class
#' ggbiplot(pca_obj, color_by = wine$Y) +
#'   geom_encircle(group_by = "color_by")
#' 
#' # With faceting
#' ggbiplot(pca_obj, facet_by = wine$Y)
#' }
#' 
#' @export
ggbiplot <- function(
    data,
    x_component = 1,
    y_component = 2,
    color_by = NULL,
    show_ellipse = FALSE,
    ellipse_level = 0.95,
    show_score_labels = FALSE,
    show_loading_labels = TRUE,
    loading_label_size = 4,
    score_label_size = 3,
    arrow_length = 1,
    circle_radius = 1,
    show_circle = TRUE,
    facet_by = NULL,
    xlab = NULL,
    ylab = NULL,
    explained_variance = NULL,
    method = "pca",
    ...
) {
  
  # If data is a mixOmics object, extract scores and loadings
  if (inherits(data, c("pca", "pls", "spls", "splsda", "rgcca", "diablo"))) {
    scores_data <- scores(data, components = c(x_component, y_component))
    loadings_data <- loadings(data, components = c(x_component, y_component))
    if (is.null(explained_variance)) {
      explained_variance <- explained_variance(data)
    }
  } else if (is.list(data)) {
    if (!all(c("scores", "loadings") %in% names(data))) {
      stop("data list must contain 'scores' and 'loadings' elements")
    }
    scores_data <- data$scores
    loadings_data <- data$loadings
  } else if (is.data.frame(data)) {
    stop("For data frames, use ggscores() or ggloadings() separately")
  } else {
    stop("data must be a multivariate analysis object or a list with scores and loadings")
  }
  
  # Extract scores for selected components
  scores_subset <- scores_data |>
    dplyr::filter(component %in% c(x_component, y_component))
  
  # Pivot scores to wide format
  scores_wide <- scores_subset |>
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "PC"
    )
  
  # Extract loadings for selected components
  loadings_subset <- loadings_data |>
    dplyr::filter(component %in% c(x_component, y_component))
  
  # Pivot loadings to wide format and add scaled coordinates
  loadings_wide <- loadings_subset |>
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "PC"
    ) |>
    dplyr::mutate(
      x_scaled = .data[[paste0("PC", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("PC", y_component)]] * arrow_length
    )
  
  # Add aesthetics columns to scores if provided
  if (!is.null(color_by)) {
    if (is.character(color_by) && length(color_by) == 1) {
      if (color_by %in% colnames(scores_wide)) {
        scores_wide[[paste0("color_var_", color_by)]] <- scores_wide[[color_by]]
        color_by <- paste0("color_var_", color_by)
      } else {
        stop(paste("Column", color_by, "not found in data"))
      }
    } else if (is.vector(color_by)) {
      if (length(color_by) != nrow(scores_wide)) {
        stop("color_by must have the same length as the number of samples")
      }
      scores_wide$color_var <- color_by
      color_by <- "color_var"
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
  
  # Create base aesthetic mapping for scores
  aes_mapping <- ggplot2::aes(
    x = .data[[paste0("PC", x_component)]],
    y = .data[[paste0("PC", y_component)]]
  )
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    aes_mapping <- aes_mapping + ggplot2::aes(color = .data[[color_by]])
  }
  
  # Create base plot with scores
  p <- ggplot2::ggplot(scores_wide, aes_mapping) +
    ggplot2::geom_point(size = 2, ...)
  
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
  
  # Add score labels if requested
  if (show_score_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sample),
      size = score_label_size,
      vjust = -0.5,
      hjust = 0.5,
      check_overlap = TRUE
    )
  }
  
  # Add loading arrows
  p <- p + ggplot2::geom_segment(
    data = loadings_wide,
    ggplot2::aes(x = 0, y = 0, xend = x_scaled, yend = y_scaled),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
    color = "red",
    ...
  )
  
  # Add loading labels if requested
  if (show_loading_labels) {
    p <- p + ggplot2::geom_text(
      data = loadings_wide,
      ggplot2::aes(x = x_scaled, y = y_scaled, label = variable),
      size = loading_label_size,
      color = "red",
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
    p <- p + ggplot2::geom_path(
      data = circle_data,
      ggplot2::aes(x = x, y = y),
      color = "gray70",
      linetype = "dashed"
    )
  }
  
  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_by]]))
  }
  
  # Set axis labels with variance explained
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
