#' Add scores layer to ggmultivar plot
#' 
#' @description
#' geom_scores adds a layer of points representing the scores (sample projections)
#' in the multivariate analysis.
#' 
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param data Data frame to use, or NULL to use the plot's data
#' @param stat Statistic to use, or NULL
#' @param position Position adjustment, or NULL
#' @param ... Additional arguments passed to geom_point()
#' @param na.rm Logical, whether to remove NA values
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' 
#' @return A ggplot2 layer
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores(aes(color = factor(mtcars$cyl)))
#' 
#' @export
#' @importFrom ggplot2 geom_point layer
geom_scores <- function(mapping = NULL, data = NULL, stat = NULL, position = NULL,
                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  
  # Create the layer
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomScores,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomScores ggproto object
#' 
#' @format NULL
#' @usage NULL
#' @export
GeomScores <- ggplot2::ggproto(
  "GeomScores", ggplot2::GeomPoint,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    shape = 19,
    size = 2,
    color = "black",
    fill = NA,
    alpha = 1,
    stroke = 0.5
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    # Filter out NA values if requested
    if (na.rm) {
      data <- data[complete.cases(data), ]
    }
    
    # Use the parent's draw_panel method
    ggplot2::GeomPoint$draw_panel(data, panel_params, coord, na.rm)
  }
)

#' Add loadings layer to ggmultivar plot
#' 
#' @description
#' geom_loadings adds a layer of arrows or points representing the variable loadings.
#' 
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param data Data frame to use, or NULL to use the plot's data
#' @param stat Statistic to use, or NULL
#' @param position Position adjustment, or NULL
#' @param ... Additional arguments passed to geom_segment() or geom_point()
#' @param na.rm Logical, whether to remove NA values
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' @param as_arrows Logical, whether to show loadings as arrows from origin (default: TRUE)
#' @param arrow_length Scaling factor for arrow length (default: 1)
#' @param connect Logical, whether to connect loadings with lines (for time series, spectra)
#' @param connect_order Character vector specifying the order for connecting loadings
#' 
#' @return A ggplot2 layer
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   geom_loadings()
#' 
#' # With custom arrow length
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   geom_loadings(arrow_length = 0.8)
#' 
#' @export
#' @importFrom ggplot2 geom_segment geom_point layer
geom_loadings <- function(mapping = NULL, data = NULL, stat = NULL, position = NULL,
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         as_arrows = TRUE, arrow_length = 1, connect = FALSE, connect_order = NULL) {
  
  # Store parameters in the layer
  params <- list(
    na.rm = na.rm,
    as_arrows = as_arrows,
    arrow_length = arrow_length,
    connect = connect,
    connect_order = connect_order,
    ...
  )
  
  # Create the layer
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLoadings,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' GeomLoadings ggproto object
#' 
#' @format NULL
#' @usage NULL
#' @export
GeomLoadings <- ggplot2::ggproto(
  "GeomLoadings", ggplot2::Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    color = "red",
    size = 1,
    alpha = 1,
    linetype = 1
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE, as_arrows = TRUE, 
                       arrow_length = 1, connect = FALSE, connect_order = NULL) {
    
    # Filter out NA values if requested
    if (na.rm) {
      data <- data[complete.cases(data), ]
    }
    
    # Scale coordinates by arrow_length
    data$x <- data$x * arrow_length
    data$y <- data$y * arrow_length
    
    # Transform data to device coordinates
    coords <- coord$transform(data, panel_params)
    
    # Draw arrows from origin to loadings
    if (as_arrows && nrow(coords) > 0) {
      # Create origin points
      origins <- data.frame(
        x = 0,
        y = 0,
        group = coords$group,
        stringsAsFactors = FALSE
      )
      origin_coords <- coord$transform(origins, panel_params)
      
      # Draw arrows
      ggplot2::grid::arrow(
        x0 = origin_coords$x,
        y0 = origin_coords$y,
        x1 = coords$x,
        y1 = coords$y,
        gp = ggplot2::grid::gpar(
          col = coords$colour,
          lwd = coords$size * .pt,
          lty = coords$linetype,
          alpha = coords$alpha
        )
      )
    }
    
    # Draw points at loading positions
    ggplot2::grid::points(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = ggplot2::grid::gpar(
        col = coords$colour,
        cex = coords$size * .pt,
        fill = coords$fill,
        alpha = coords$alpha
      )
    )
    
    # Connect loadings if requested
    if (connect && !is.null(connect_order) && nrow(coords) > 1) {
      # Order the coordinates according to connect_order
      if (length(connect_order) == nrow(coords)) {
        coords <- coords[connect_order, ]
      }
      
      ggplot2::grid::polyline(
        x = coords$x,
        y = coords$y,
        gp = ggplot2::grid::gpar(
          col = coords$colour[1],
          lwd = coords$size[1] * .pt,
          lty = coords$linetype[1],
          alpha = coords$alpha[1]
        )
      )
    }
  }
)

#' Add confidence circle layer to ggmultivar plot
#' 
#' @description
#' geom_conf adds a confidence circle (unit circle) to the plot, typically used
#' for loadings plots to show the correlation circle.
#' 
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param data Data frame to use, or NULL to use the plot's data
#' @param stat Statistic to use, or NULL
#' @param position Position adjustment, or NULL
#' @param ... Additional arguments passed to geom_path()
#' @param na.rm Logical, whether to remove NA values
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' @param radius Radius of the confidence circle (default: 1)
#' @param linetype Line type for the circle (default: "dashed")
#' @param color Color of the circle (default: "gray50")
#' @param alpha Alpha transparency (default: 1)
#' 
#' @return A ggplot2 layer
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   geom_loadings() +
#'   geom_conf()
#' 
#' @export
#' @importFrom ggplot2 geom_path layer
geom_conf <- function(mapping = NULL, data = NULL, stat = NULL, position = NULL,
                       ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                       radius = 1, linetype = "dashed", color = "gray50", alpha = 1) {
  
  # Create circle data
  circle_data <- data.frame(
    x = radius * cos(seq(0, 2 * pi, length.out = 100)),
    y = radius * sin(seq(0, 2 * pi, length.out = 100))
  )
  
  # Create the layer with circle data
  layer(
    data = circle_data,
    mapping = mapping,
    stat = stat,
    geom = GeomConf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linetype = linetype,
      color = color,
      alpha = alpha,
      ...
    )
  )
}

#' GeomConf ggproto object
#' 
#' @format NULL
#' @usage NULL
#' @export
GeomConf <- ggplot2::ggproto(
  "GeomConf", ggplot2::GeomPath,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    color = "gray50",
    linetype = "dashed",
    size = 0.5,
    alpha = 1
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    # Filter out NA values if requested
    if (na.rm) {
      data <- data[complete.cases(data), ]
    }
    
    # Transform data to device coordinates
    coords <- coord$transform(data, panel_params)
    
    # Draw the circle
    if (nrow(coords) > 1) {
      ggplot2::grid::polyline(
        x = coords$x,
        y = coords$y,
        gp = ggplot2::grid::gpar(
          col = coords$colour[1],
          lwd = coords$size[1] * .pt,
          lty = coords$linetype[1],
          alpha = coords$alpha[1]
        )
      )
    }
  }
)

#' Add confidence ellipse layer to ggmultivar plot
#' 
#' @description
#' geom_ellipse adds confidence ellipses to the plot, typically used for
#' grouping samples in scores plots.
#' 
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param data Data frame to use, or NULL to use the plot's data
#' @param stat Statistic to use, or NULL
#' @param position Position adjustment, or NULL
#' @param ... Additional arguments passed to stat_ellipse()
#' @param na.rm Logical, whether to remove NA values
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' @param level Confidence level for the ellipse (default: 0.95)
#' 
#' @return A ggplot2 layer
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2, color = factor(mtcars$cyl))) +
#'   geom_scores() +
#'   geom_ellipse(level = 0.95)
#' 
#' @export
#' @importFrom ggplot2 stat_ellipse layer
geom_ellipse <- function(mapping = NULL, data = NULL, stat = "ellipse", position = NULL,
                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, level = 0.95) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      level = level,
      ...
    )
  )
}

#' Add variance explained labels to ggmultivar plot
#' 
#' @description
#' geom_variance adds labels showing the percentage of variance explained
#' by each component.
#' 
#' @param mapping Aesthetic mappings created with aes(), or NULL
#' @param data Data frame to use, or NULL to use the plot's data
#' @param stat Statistic to use, or NULL
#' @param position Position adjustment, or NULL
#' @param ... Additional arguments passed to geom_text()
#' @param na.rm Logical, whether to remove NA values
#' @param show.legend Logical, whether to show legend
#' @param inherit.aes Logical, whether to inherit aesthetics
#' @param size Text size (default: 3)
#' @param color Text color (default: "gray50")
#' @param x X position for the label (default: "left")
#' @param y Y position for the label (default: "top")
#' @param hjust Horizontal justification (default: 0)
#' @param vjust Vertical justification (default: 1)
#' 
#' @return A ggplot2 layer
#' 
#' @examples
#' library(ggmultivar)
#' pca_result <- perform_pca(mtcars[, 1:7], n_components = 3)
#' ggmultivar(data = pca_result, aes(x = PC1, y = PC2)) +
#'   geom_scores() +
#'   geom_variance()
#' 
#' @export
#' @importFrom ggplot2 geom_text layer
geom_variance <- function(mapping = NULL, data = NULL, stat = NULL, position = NULL,
                         ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         size = 3, color = "gray50", x = "left", y = "top", 
                         hjust = 0, vjust = 1) {
  
  # This geom will be handled specially by the ggmultivar system
  # It extracts variance information from the ggmultivar data
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVariance,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      size = size,
      color = color,
      x = x,
      y = y,
      hjust = hjust,
      vjust = vjust,
      ...
    )
  )
}

#' GeomVariance ggproto object
#' 
#' @format NULL
#' @usage NULL
#' @export
GeomVariance <- ggplot2::ggproto(
  "GeomVariance", ggplot2::GeomText,
  
  required_aes = character(0),
  
  default_aes = ggplot2::aes(
    label = "",
    x = 0,
    y = 0,
    size = 3,
    color = "gray50",
    alpha = 1,
    angle = 0,
    hjust = 0,
    vjust = 1,
    family = "",
    fontface = 1,
    lineheight = 1
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE, 
                       size = 3, color = "gray50", x = "left", y = "top",
                       hjust = 0, vjust = 1) {
    
    # This will be handled by a stat instead
    # For now, just draw the default text
    ggplot2::GeomText$draw_panel(data, panel_params, coord, na.rm)
  }
)

#' Stat for variance labels
#' 
#' @format NULL
#' @usage NULL
#' @export
StatVariance <- ggplot2::ggproto(
  "StatVariance", ggplot2::Stat,
  
  required_aes = c("x", "y"),
  
  compute_layer = function(data, params, layout) {
    # Get the ggmultivar data from the plot
    # This is a bit tricky since we're in a stat context
    # For now, we'll return the data as-is and handle variance in the geom
    data
  }
)