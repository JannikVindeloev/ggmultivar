#' Perform Principal Component Analysis
#' 
#' @description Perform PCA on a data matrix and return results in tidy format.
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
#' }
#' 
#' @examples
#' data(mtcars)
#' # Use only numeric columns
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' head(pca_result$scores)
#' head(pca_result$loadings)
#' pca_result$explained_variance
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
    n_components = n_components
  )
}

#' Create PCA Scores Plot
#' 
#' @description Create a ggplot2 scores plot from PCA results.
#' 
#' @param pca_result Result from perform_pca() function
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color points by. Can be a vector of the same length as samples.
#' @param shape_by Optional variable to shape points by. Can be a vector of the same length as samples.
#' @param size_by Optional variable to size points by. Can be a vector of the same length as samples.
#' @param show_ellipse Logical, whether to show confidence ellipses. Default is FALSE.
#' @param ellipse_level Confidence level for ellipses (0-1). Default is 0.95.
#' @param show_labels Logical, whether to show sample labels. Default is FALSE.
#' @param label_size Size of sample labels. Default is 3.
#' @param title Character vector of length 2 for axis titles. Default is c("PC1", "PC2").
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param theme ggplot2 theme to apply. Default is theme_minimal().
#' @param ... Additional arguments passed to geom_point()
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' data(mtcars)
#' pca_result <- perform_pca(mtcars[, 1:7])
#' pca_scores_plot(pca_result) +
#'   ggtitle("PCA Scores Plot")
#' 
#' # Color by cylinder
#' pca_scores_plot(pca_result, color_by = mtcars$cyl)
#' 
#' @export
pca_scores_plot <- function(pca_result, x_component = 1, y_component = 2,
                            color_by = NULL, shape_by = NULL, size_by = NULL,
                            show_ellipse = FALSE, ellipse_level = 0.95,
                            show_labels = FALSE, label_size = 3,
                            title = NULL, xlab = NULL, ylab = NULL,
                            theme = ggplot2::theme_minimal(), ...) {
  
  # Extract scores for selected components
  scores_subset <- pca_result$scores |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "PC"
    )
  
  # Create base plot
  p <- ggplot2::ggplot(scores_subset, ggplot2::aes(x = .data[[paste0("PC", x_component)]], 
                                                   y = .data[[paste0("PC", y_component)]])) +
    ggplot2::geom_point(...)
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    if (length(color_by) != nrow(scores_subset)) {
      stop("color_by must have the same length as the number of samples")
    }
    scores_subset$color_var <- color_by
    p <- p + ggplot2::aes(color = color_var)
  }
  
  # Add shape aesthetic if provided
  if (!is.null(shape_by)) {
    if (length(shape_by) != nrow(scores_subset)) {
      stop("shape_by must have the same length as the number of samples")
    }
    scores_subset$shape_var <- as.factor(shape_by)
    p <- p + ggplot2::aes(shape = shape_var)
  }
  
  # Add size aesthetic if provided
  if (!is.null(size_by)) {
    if (length(size_by) != nrow(scores_subset)) {
      stop("size_by must have the same length as the number of samples")
    }
    scores_subset$size_var <- size_by
    p <- p + ggplot2::aes(size = size_var)
  }
  
  # Add ellipses if requested
  if (show_ellipse && !is.null(color_by)) {
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(group = color_var),
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
  
  # Set axis labels
  if (is.null(xlab)) {
    xlab <- paste0("PC", x_component, " (", 
                   round(pca_result$explained_variance$variance_percent[x_component], 1), "%)")
  }
  if (is.null(ylab)) {
    ylab <- paste0("PC", y_component, " (", 
                   round(pca_result$explained_variance$variance_percent[y_component], 1), "%)")
  }
  
  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  # Add title if provided
  if (!is.null(title)) {
    if (length(title) == 2) {
      p <- p + ggplot2::labs(title = title[1], subtitle = title[2])
    } else {
      p <- p + ggplot2::ggtitle(title)
    }
  }
  
  # Apply theme
  p <- p + theme
  
  return(p)
}

#' Create PCA Loadings Plot
#' 
#' @description Create a ggplot2 loadings plot from PCA results.
#' 
#' @param pca_result Result from perform_pca() function
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param show_labels Logical, whether to show variable labels. Default is TRUE.
#' @param label_size Size of variable labels. Default is 4.
#' @param show_arrows Logical, whether to show arrows from origin to loadings. Default is TRUE.
#' @param arrow_length Scaling factor for arrow length. Default is 1.
#' @param color_by Optional variable to color loadings by. Can be a vector of the same length as variables.
#' @param circle_radius Radius of the unit circle. Default is 1.
#' @param show_circle Logical, whether to show unit circle. Default is TRUE.
#' @param title Character vector of length 2 for axis titles. Default is NULL.
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param theme ggplot2 theme to apply. Default is theme_minimal().
#' @param ... Additional arguments passed to geom_text() or geom_segment()
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' data(mtcars)
#' pca_result <- perform_pca(mtcars[, 1:7])
#' pca_loadings_plot(pca_result)
#' 
#' # Color loadings by absolute value
#' pca_loadings_plot(pca_result, color_by = abs(pca_result$loadings$loading))
#' 
#' @export
pca_loadings_plot <- function(pca_result, x_component = 1, y_component = 2,
                              show_labels = TRUE, label_size = 4,
                              show_arrows = TRUE, arrow_length = 1,
                              color_by = NULL, circle_radius = 1,
                              show_circle = TRUE, title = NULL,
                              xlab = NULL, ylab = NULL,
                              theme = ggplot2::theme_minimal(), ...) {
  
  # Extract loadings for selected components
  loadings_subset <- pca_result$loadings |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "PC"
    )
  
  # Scale loadings by arrow_length
  loadings_subset <- loadings_subset |
    dplyr::mutate(
      x_scaled = .data[[paste0("PC", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("PC", y_component)]] * arrow_length
    )
  
  # Create base plot
  p <- ggplot2::ggplot(loadings_subset, 
                       ggplot2::aes(x = x_scaled, y = y_scaled))
  
  # Add arrows if requested
  if (show_arrows) {
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = x_scaled, yend = y_scaled),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
      color = "gray50",
      ...
    )
  }
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    if (length(color_by) != nrow(loadings_subset)) {
      stop("color_by must have the same length as the number of variables")
    }
    loadings_subset$color_var <- color_by
    p <- p + ggplot2::aes(color = color_var)
  }
  
  # Add labels if requested
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = variable),
      size = label_size,
      vjust = ifelse(loadings_subset$y_scaled > 0, -0.5, 1.5),
      hjust = ifelse(loadings_subset$x_scaled > 0, -0.1, 1.1),
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
  
  # Set axis labels
  if (is.null(xlab)) {
    xlab <- paste0("PC", x_component, " loadings")
  }
  if (is.null(ylab)) {
    ylab <- paste0("PC", y_component, " loadings")
  }
  
  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::xlim(-circle_radius * 1.1, circle_radius * 1.1) +
    ggplot2::ylim(-circle_radius * 1.1, circle_radius * 1.1)
  
  # Add title if provided
  if (!is.null(title)) {
    if (length(title) == 2) {
      p <- p + ggplot2::labs(title = title[1], subtitle = title[2])
    } else {
      p <- p + ggplot2::ggtitle(title)
    }
  }
  
  # Apply theme
  p <- p + theme
  
  return(p)
}

#' Create Combined PCA Scores and Loadings Plot (Biplot)
#' 
#' @description Create a biplot showing both PCA scores and loadings.
#' 
#' @param pca_result Result from perform_pca() function
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color scores by.
#' @param show_ellipse Logical, whether to show confidence ellipses. Default is FALSE.
#' @param ellipse_level Confidence level for ellipses. Default is 0.95.
#' @param show_score_labels Logical, whether to show sample labels. Default is FALSE.
#' @param show_loading_labels Logical, whether to show variable labels. Default is TRUE.
#' @param loading_label_size Size of variable labels. Default is 4.
#' @param score_label_size Size of sample labels. Default is 3.
#' @param arrow_length Scaling factor for loading arrows. Default is 1.
#' @param circle_radius Radius of the unit circle. Default is 1.
#' @param show_circle Logical, whether to show unit circle. Default is TRUE.
#' @param title Character vector for plot title. Default is NULL.
#' @param theme ggplot2 theme to apply. Default is theme_minimal().
#' @param ... Additional arguments passed to geoms
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' data(mtcars)
#' pca_result <- perform_pca(mtcars[, 1:7])
#' pca_biplot(pca_result, color_by = mtcars$cyl)
#' 
#' @export
pca_biplot <- function(pca_result, x_component = 1, y_component = 2,
                        color_by = NULL, show_ellipse = FALSE, ellipse_level = 0.95,
                        show_score_labels = FALSE, show_loading_labels = TRUE,
                        loading_label_size = 4, score_label_size = 3,
                        arrow_length = 1, circle_radius = 1, show_circle = TRUE,
                        title = NULL, theme = ggplot2::theme_minimal(), ...) {
  
  # Extract scores for selected components
  scores_subset <- pca_result$scores |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "PC"
    )
  
  # Extract loadings for selected components
  loadings_subset <- pca_result$loadings |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "PC"
    ) |
    dplyr::mutate(
      x_scaled = .data[[paste0("PC", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("PC", y_component)]] * arrow_length
    )
  
  # Create base plot with scores
  p <- ggplot2::ggplot(scores_subset, 
                       ggplot2::aes(x = .data[[paste0("PC", x_component)]], 
                                   y = .data[[paste0("PC", y_component)]])) +
    ggplot2::geom_point(size = 2, ...)
  
  # Add color aesthetic if provided
  if (!is.null(color_by)) {
    if (length(color_by) != nrow(scores_subset)) {
      stop("color_by must have the same length as the number of samples")
    }
    scores_subset$color_var <- color_by
    p <- p + ggplot2::aes(color = color_var)
  }
  
  # Add ellipses if requested
  if (show_ellipse && !is.null(color_by)) {
    p <- p + ggplot2::stat_ellipse(
      ggplot2::aes(group = color_var),
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
    data = loadings_subset,
    ggplot2::aes(x = 0, y = 0, xend = x_scaled, yend = y_scaled),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
    color = "red",
    ...
  )
  
  # Add loading labels if requested
  if (show_loading_labels) {
    p <- p + ggplot2::geom_text(
      data = loadings_subset,
      ggplot2::aes(x = x_scaled, y = y_scaled, label = variable),
      size = loading_label_size,
      color = "red",
      vjust = ifelse(loadings_subset$y_scaled > 0, -0.5, 1.5),
      hjust = ifelse(loadings_subset$x_scaled > 0, -0.1, 1.1),
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
  
  # Set axis labels with variance explained
  xlab <- paste0("PC", x_component, " (", 
                 round(pca_result$explained_variance$variance_percent[x_component], 1), "%)")
  ylab <- paste0("PC", y_component, " (", 
                 round(pca_result$explained_variance$variance_percent[y_component], 1), "%)")
  
  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  # Add title if provided
  if (!is.null(title)) {
    if (length(title) == 2) {
      p <- p + ggplot2::labs(title = title[1], subtitle = title[2])
    } else {
      p <- p + ggplot2::ggtitle(title)
    }
  }
  
  # Apply theme
  p <- p + theme
  
  return(p)
}