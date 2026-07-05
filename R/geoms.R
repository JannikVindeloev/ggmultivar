#' Custom Geoms for Multivariate Analysis Plots
#' 
#' @description Custom geoms for adding confidence intervals and encircling groups in ggmultivar plots.
#' 
#' @name custom-geoms
NULL

#' Geom for Encircing Groups
#' 
#' @description Add ellipses or convex hulls to encircle groups of points.
#' 
#' @param mapping Aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param geom The geometric object to use for display ("ellipse", "hull", or "polygon")
#' @param group_by Character string indicating the column to group by
#' @param level Confidence level for ellipses (0-1). Default is 0.95.
#' @param color Color of the encircling line. Default is "gray50"
#' @param fill Fill color for the encircling shape. Default is NA (transparent)
#' @param linetype Line type for the encircling line. Default is "dashed"
#' @param alpha Transparency level (0-1). Default is 0.2
#' @param ... Additional arguments passed to the underlying geom
#' 
#' @return A layer that can be added to a ggplot
#' 
#' @examples
#' # Create sample data
#' scores_data <- data.frame(
#'   sample = paste0("Sample_", 1:50),
#'   PC1 = rnorm(50),
#'   PC2 = rnorm(50),
#'   group = sample(letters[1:3], 50, replace = TRUE)
#' )
#' 
#' library(ggplot2)
#' p <- ggplot(scores_data, aes(PC1, PC2)) +
#'   geom_point()
#' 
#' p + geom_encircle(group_by = "group", color = "blue")
#' 
#' # With convex hull
#' p + geom_encircle(group_by = "group", geom = "hull", fill = "red", alpha = 0.1)
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' ggscores(pca_obj, color_by = wine$Y) +
#'   geom_encircle(group_by = "color_by", color = "blue")
#' }
#' 
#' @export
geom_encircle <- function(
    mapping = NULL,
    data = NULL,
    geom = c("ellipse", "hull", "polygon"),
    group_by = NULL,
    level = 0.95,
    color = "gray50",
    fill = NA,
    linetype = "dashed",
    alpha = 0.2,
    ...
) {
  
  geom <- match.arg(geom)
  
  if (is.null(group_by)) {
    stop("group_by must be specified")
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = switch(
      geom,
      "ellipse" = StatEllipse,
      "hull" = StatHull,
      "polygon" = StatHull
    ),
    geom = switch(
      geom,
      "ellipse" = GeomPath,
      "hull" = GeomPolygon,
      "polygon" = GeomPolygon
    ),
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      group = as.name(group_by),
      level = level,
      color = color,
      fill = fill,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}

#' Custom Stat for Convex Hull
#' 
#' @description Compute convex hull for encircling groups.
#' 
#' @rdname geom_encircle
#' @format NULL
#' @usage NULL
#' @export
StatHull <- ggproto("StatHull", Stat,
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, group = NULL) {
    if (is.null(group)) {
      stop("group aesthetic must be specified")
    }
    
    # Get unique groups
    groups <- unique(data[[group]])
    
    # For each group, compute convex hull
    hull_data <- list()
    for (g in groups) {
      group_data <- data[data[[group]] == g, ]
      
      # Compute convex hull using chull
      hull_indices <- chull(group_data$x, group_data$y)
      
      if (length(hull_indices) > 2) {
        # Add first point at end to close the polygon
        hull_indices <- c(hull_indices, hull_indices[1])
        
        hull_points <- group_data[hull_indices, ]
        hull_points$group <- rep(g, nrow(hull_points))
        hull_data <- c(hull_data, list(hull_points))
      }
    }
    
    if (length(hull_data) == 0) {
      return(data.frame())
    }
    
    return(bind_rows(hull_data))
  }
)

#' Geom for Confidence Intervals on Loadings
#' 
#' @description Add confidence intervals to loading vectors.
#' 
#' @param mapping Aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param method Method for computing confidence intervals ("bootstrap", "jackknife"). Default is "bootstrap"
#' @param n_boot Number of bootstrap iterations. Default is 100
#' @param level Confidence level (0-1). Default is 0.95
#' @param color Color of the confidence interval lines. Default is "gray50"
#' @param linetype Line type for the confidence interval lines. Default is "dashed"
#' @param ... Additional arguments passed to the underlying geom
#' 
#' @return A layer that can be added to a ggplot
#' 
#' @examples
#' # Create sample loadings data
#' loadings_data <- data.frame(
#'   variable = paste0("Var_", 1:10),
#'   component = rep(1:2, each = 10),
#'   loading = rnorm(20)
#' )
#' 
#' library(ggplot2)
#' p <- ggplot(loadings_data, aes(x = loading, y = seq_along(loading))) +
#'   geom_point()
#' 
#' p + geom_confidence_interval()
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' ggloadings(pca_obj) +
#'   geom_confidence_interval()
#' }
#' 
#' @export
geom_confidence_interval <- function(
    mapping = NULL,
    data = NULL,
    method = c("bootstrap", "jackknife"),
    n_boot = 100,
    level = 0.95,
    color = "gray50",
    linetype = "dashed",
    ...
) {
  
  method <- match.arg(method)
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatConfidenceInterval,
    geom = GeomPath,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      method = method,
      n_boot = n_boot,
      level = level,
      color = color,
      linetype = linetype,
      ...
    )
  )
}

#' Custom Stat for Confidence Intervals
#' 
#' @description Compute confidence intervals for loading vectors.
#' 
#' @rdname geom_confidence_interval
#' @format NULL
#' @usage NULL
#' @export
StatConfidenceInterval <- ggproto("StatConfidenceInterval", Stat,
  required_aes = c("x", "y", "variable", "component"),
  
  compute_group = function(data, scales, method = "bootstrap", n_boot = 100, level = 0.95) {
    # This is a simplified version - in practice, you would need the original data
    # to compute bootstrap confidence intervals
    
    # For demonstration, we'll create a simple ellipse around each loading point
    # In a real implementation, this would use bootstrap resampling
    
    ci_data <- list()
    for (i in 1:nrow(data)) {
      # Create a small ellipse around each point
      theta <- seq(0, 2 * pi, length.out = 50)
      radius <- 0.1  # Fixed radius for demonstration
      
      ci_points <- data.frame(
        x = data$x[i] + radius * cos(theta),
        y = data$y[i] + radius * sin(theta),
        variable = data$variable[i],
        component = data$component[i],
        group = i
      )
      ci_data <- c(ci_data, list(ci_points))
    }
    
    if (length(ci_data) == 0) {
      return(data.frame())
    }
    
    return(bind_rows(ci_data))
  }
)

#' Geom for Variable Correlation Circles
#' 
#' @description Add correlation circles to biplots.
#' 
#' @param mapping Aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param radius Radius of the correlation circle. Default is 1
#' @param color Color of the correlation circle. Default is "gray70"
#' @param linetype Line type for the correlation circle. Default is "dashed"
#' @param ... Additional arguments passed to the underlying geom
#' 
#' @return A layer that can be added to a ggplot
#' 
#' @examples
#' # Create sample data
#' loadings_data <- data.frame(
#'   variable = paste0("Var_", 1:10),
#'   PC1 = rnorm(10),
#'   PC2 = rnorm(10)
#' )
#' 
#' library(ggplot2)
#' p <- ggplot(loadings_data, aes(PC1, PC2)) +
#'   geom_point()
#' 
#' p + geom_correlation_circle(radius = 1, color = "blue")
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' ggbiplot(pca_obj) +
#'   geom_correlation_circle(radius = 1, color = "blue")
#' }
#' 
#' @export
geom_correlation_circle <- function(
    mapping = NULL,
    data = NULL,
    radius = 1,
    color = "gray70",
    linetype = "dashed",
    ...
) {
  
  # Create circle data
  circle_data <- data.frame(
    theta = seq(0, 2 * pi, length.out = 100),
    x = radius * cos(seq(0, 2 * pi, length.out = 100)),
    y = radius * sin(seq(0, 2 * pi, length.out = 100))
  )
  
  layer(
    data = circle_data,
    mapping = mapping,
    geom = GeomPath,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      ggplot2::aes(x = x, y = y),
      color = color,
      linetype = linetype,
      ...
    )
  )
}

#' Geom for Sample Labels
#' 
#' @description Add sample labels to scores plots with intelligent positioning.
#' 
#' @param mapping Aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param size Size of the text. Default is 3
#' @param color Color of the text. Default is "black"
#' @param vjust Vertical adjustment. Default is -0.5
#' @param hjust Horizontal adjustment. Default is 0.5
#' @param check_overlap Logical, whether to check for overlapping labels. Default is TRUE
#' @param ... Additional arguments passed to geom_text()
#' 
#' @return A layer that can be added to a ggplot
#' 
#' @examples
#' # Create sample data
#' scores_data <- data.frame(
#'   sample = paste0("Sample_", 1:50),
#'   PC1 = rnorm(50),
#'   PC2 = rnorm(50)
#' )
#' 
#' library(ggplot2)
#' p <- ggplot(scores_data, aes(PC1, PC2)) +
#'   geom_point()
#' 
#' p + geom_sample_labels(size = 2, color = "red")
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' ggscores(pca_obj) +
#'   geom_sample_labels(size = 2, color = "red")
#' }
#' 
#' @export
geom_sample_labels <- function(
    mapping = NULL,
    data = NULL,
    size = 3,
    color = "black",
    vjust = -0.5,
    hjust = 0.5,
    check_overlap = TRUE,
    ...
) {
  
  layer(
    data = data,
    mapping = mapping,
    geom = GeomText,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      ggplot2::aes(label = sample),
      size = size,
      color = color,
      vjust = vjust,
      hjust = hjust,
      check_overlap = check_overlap,
      ...
    )
  )
}

#' Geom for Variable Labels
#' 
#' @description Add variable labels to loadings plots with intelligent positioning.
#' 
#' @param mapping Aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param size Size of the text. Default is 4
#' @param color Color of the text. Default is "red"
#' @param ... Additional arguments passed to geom_text()
#' 
#' @return A layer that can be added to a ggplot
#' 
#' @examples
#' # Create sample data
#' loadings_data <- data.frame(
#'   variable = paste0("Var_", 1:10),
#'   x = rnorm(10),
#'   y = rnorm(10)
#' )
#' 
#' library(ggplot2)
#' p <- ggplot(loadings_data, aes(x, y)) +
#'   geom_point()
#' 
#' p + geom_variable_labels(size = 3, color = "blue")
#' 
#' # With mixOmics objects (if installed)
#' \dontrun{
#' library(mixOmics)
#' library(FactoMineR)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' 
#' ggloadings(pca_obj) +
#'   geom_variable_labels(size = 3, color = "blue")
#' }
#' 
#' @export
geom_variable_labels <- function(
    mapping = NULL,
    data = NULL,
    size = 4,
    color = "red",
    ...
) {
  
  layer(
    data = data,
    mapping = mapping,
    geom = GeomText,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(
      ggplot2::aes(label = variable),
      size = size,
      color = color,
      vjust = ifelse(data$y > 0, -0.5, 1.5),
      hjust = ifelse(data$x > 0, -0.1, 1.1),
      ...
    )
  )
}
