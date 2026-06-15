#' Perform Partial Least Squares Regression
#' 
#' @description Perform PLS regression on predictor and response matrices and return results in tidy format.
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
#' }
#' 
#' @examples
#' # Example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y, n_components = 3)
#' head(pls_result$scores)
#' head(pls_result$loadings)
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
    pls_object = pls_result
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

#' Create PLS Scores Plot
#' 
#' @description Create a ggplot2 scores plot from PLS results.
#' 
#' @param pls_result Result from perform_pls() function
#' @param x_component X-axis component number. Default is 1.
#' @param y_component Y-axis component number. Default is 2.
#' @param color_by Optional variable to color points by. Can be a vector of the same length as samples.
#' @param shape_by Optional variable to shape points by. Can be a vector of the same length as samples.
#' @param size_by Optional variable to size points by. Can be a vector of the same length as samples.
#' @param show_ellipse Logical, whether to show confidence ellipses. Default is FALSE.
#' @param ellipse_level Confidence level for ellipses (0-1). Default is 0.95.
#' @param show_labels Logical, whether to show sample labels. Default is FALSE.
#' @param label_size Size of sample labels. Default is 3.
#' @param title Character vector of length 2 for axis titles. Default is NULL.
#' @param xlab,ylab Custom axis labels. If NULL, uses default.
#' @param theme ggplot2 theme to apply. Default is theme_minimal().
#' @param ... Additional arguments passed to geom_point()
#' 
#' @return A ggplot2 object
#' 
#' @examples
#' # Example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y)
#' pls_scores_plot(pls_result)
#' 
#' @export
pls_scores_plot <- function(pls_result, x_component = 1, y_component = 2,
                            color_by = NULL, shape_by = NULL, size_by = NULL,
                            show_ellipse = FALSE, ellipse_level = 0.95,
                            show_labels = FALSE, label_size = 3,
                            title = NULL, xlab = NULL, ylab = NULL,
                            theme = ggplot2::theme_minimal(), ...) {
  
  # Extract scores for selected components
  scores_subset <- pls_result$scores |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "L"
    )
  
  # Create base plot
  p <- ggplot2::ggplot(scores_subset, ggplot2::aes(x = .data[[paste0("L", x_component)]], 
                                                   y = .data[[paste0("L", y_component)]])) +
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
    xlab <- paste0("LV", x_component, " (", 
                   round(pls_result$explained_variance$x_variance_percent[x_component], 1), "% X var)")
  }
  if (is.null(ylab)) {
    ylab <- paste0("LV", y_component, " (", 
                   round(pls_result$explained_variance$x_variance_percent[y_component], 1), "% X var)")
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

#' Create PLS Loadings Plot
#' 
#' @description Create a ggplot2 loadings plot from PLS results.
#' 
#' @param pls_result Result from perform_pls() function
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
#' # Example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y)
#' pls_loadings_plot(pls_result)
#' 
#' @export
pls_loadings_plot <- function(pls_result, x_component = 1, y_component = 2,
                              show_labels = TRUE, label_size = 4,
                              show_arrows = TRUE, arrow_length = 1,
                              color_by = NULL, circle_radius = 1,
                              show_circle = TRUE, title = NULL,
                              xlab = NULL, ylab = NULL,
                              theme = ggplot2::theme_minimal(), ...) {
  
  # Extract loadings for selected components
  loadings_subset <- pls_result$loadings |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "L"
    )
  
  # Scale loadings by arrow_length
  loadings_subset <- loadings_subset |
    dplyr::mutate(
      x_scaled = .data[[paste0("L", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("L", y_component)]] * arrow_length
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
    xlab <- paste0("LV", x_component, " loadings")
  }
  if (is.null(ylab)) {
    ylab <- paste0("LV", y_component, " loadings")
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

#' Create Combined PLS Scores and Loadings Plot (Biplot)
#' 
#' @description Create a biplot showing both PLS scores and loadings.
#' 
#' @param pls_result Result from perform_pls() function
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
#' # Example with simulated data
#' set.seed(123)
#' X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
#' Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
#' pls_result <- perform_pls(X, Y)
#' pls_biplot(pls_result)
#' 
#' @export
pls_biplot <- function(pls_result, x_component = 1, y_component = 2,
                        color_by = NULL, show_ellipse = FALSE, ellipse_level = 0.95,
                        show_score_labels = FALSE, show_loading_labels = TRUE,
                        loading_label_size = 4, score_label_size = 3,
                        arrow_length = 1, circle_radius = 1, show_circle = TRUE,
                        title = NULL, theme = ggplot2::theme_minimal(), ...) {
  
  # Extract scores for selected components
  scores_subset <- pls_result$scores |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "L"
    )
  
  # Extract loadings for selected components
  loadings_subset <- pls_result$loadings |
    dplyr::filter(component == x_component | component == y_component) |
    tidyr::pivot_wider(
      names_from = component,
      values_from = loading,
      names_prefix = "L"
    ) |
    dplyr::mutate(
      x_scaled = .data[[paste0("L", x_component)]] * arrow_length,
      y_scaled = .data[[paste0("L", y_component)]] * arrow_length
    )
  
  # Create base plot with scores
  p <- ggplot2::ggplot(scores_subset, 
                       ggplot2::aes(x = .data[[paste0("L", x_component)]], 
                                   y = .data[[paste0("L", y_component)]])) +
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
  xlab <- paste0("LV", x_component, " (", 
                 round(pls_result$explained_variance$x_variance_percent[x_component], 1), "% X var)")
  ylab <- paste0("LV", y_component, " (", 
                 round(pls_result$explained_variance$x_variance_percent[y_component], 1), "% X var)")
  
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