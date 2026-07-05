#' S3 Methods for Multivariate Analysis Objects
#' 
#' @description S3 methods for extracting scores and loadings from multivariate analysis objects
#' (primarily mixOmics objects) in a format suitable for ggmultivar plotting functions.
#' 
#' @name s3-methods
NULL

#' Extract Scores from Multivariate Analysis Object
#' 
#' @description Generic function to extract scores from multivariate analysis objects.
#' 
#' @param object A multivariate analysis object (mixOmics PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param components Vector of component indices to extract. Default is 1:2.
#' @param ... Additional arguments for specific methods
#' 
#' @return A tidy data frame with columns: sample, component, score
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' scores <- scores(pca_obj)
#' head(scores)
#' }
#' 
#' @export
scores <- function(object, components = 1:2, ...) {
  UseMethod("scores")
}

#' @export
#' @rdname scores
scores.default <- function(object, components = 1:2, ...) {
  stop("No scores method available for this object type")
}

#' @export
#' @rdname scores
scores.pca <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics PCA object
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PCA objects")
  }
  
  n_comp <- min(max(components), object$ncomp)
  components <- components[components <= n_comp]
  
  scores_mat <- object$variates$X[, components, drop = FALSE]
  
  # Get sample names
  sample_names <- rownames(object$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores_mat))
  }
  
  # Convert to tidy format
  scores_tidy <- as.data.frame(scores_mat) |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::mutate(sample = sample_names) |>
    tidyr::pivot_longer(
      cols = -sample,
      names_to = "component",
      values_to = "score"
    ) |>
    dplyr::mutate(
      component = as.numeric(gsub("X\\.", "", component)),
      .before = 1
    )
  
  return(scores_tidy)
}

#' @export
#' @rdname scores
scores.pls <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics PLS object
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PLS objects")
  }
  
  n_comp <- min(max(components), object$ncomp)
  components <- components[components <= n_comp]
  
  scores_mat <- object$variates$X[, components, drop = FALSE]
  
  # Get sample names
  sample_names <- rownames(object$variates$X)
  if (is.null(sample_names) || all(sample_names == "")) {
    sample_names <- paste0("Sample_", 1:nrow(scores_mat))
  }
  
  # Convert to tidy format
  scores_tidy <- as.data.frame(scores_mat) |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::mutate(sample = sample_names) |>
    tidyr::pivot_longer(
      cols = -sample,
      names_to = "component",
      values_to = "score"
    ) |>
    dplyr::mutate(
      component = as.numeric(gsub("X\\.", "", component)),
      .before = 1
    )
  
  return(scores_tidy)
}

#' @export
#' @rdname scores
scores.spls <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics sPLS object
  scores.pls(object, components, ...)
}

#' @export
#' @rdname scores
scores.splsda <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics sPLS-DA object
  scores.pls(object, components, ...)
}

#' @export
#' @rdname scores
scores.rgcca <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics RGCCA object
  scores.pls(object, components, ...)
}

#' @export
#' @rdname scores
scores.diablo <- function(object, components = 1:2, ...) {
  # Extract scores from mixOmics DIABLO object
  scores.pls(object, components, ...)
}

#' Extract Loadings from Multivariate Analysis Object
#' 
#' @description Generic function to extract loadings from multivariate analysis objects.
#' 
#' @param object A multivariate analysis object (mixOmics PCA, PLS, sPLS, sPLS-DA, etc.)
#' @param components Vector of component indices to extract. Default is 1:2.
#' @param type Character, "X" for X loadings or "Y" for Y loadings (for PLS-like methods). Default is "X".
#' @param ... Additional arguments for specific methods
#' 
#' @return A tidy data frame with columns: variable, component, loading
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' loadings <- loadings(pca_obj)
#' head(loadings)
#' }
#' 
#' @export
loadings <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  UseMethod("loadings")
}

#' @export
#' @rdname loadings
loadings.default <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  stop("No loadings method available for this object type")
}

#' @export
#' @rdname loadings
loadings.pca <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  type <- match.arg(type)
  
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PCA objects")
  }
  
  n_comp <- min(max(components), object$ncomp)
  components <- components[components <= n_comp]
  
  # For PCA, we only have X loadings
  loadings_mat <- object$loadings[, components, drop = FALSE]
  
  # Get variable names
  variable_names <- rownames(object$loadings)
  if (is.null(variable_names) || all(variable_names == "")) {
    variable_names <- paste0("Var_", 1:ncol(loadings_mat))
  }
  
  # Convert to tidy format
  loadings_tidy <- as.data.frame(loadings_mat) |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::mutate(variable = variable_names) |>
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading"
    ) |>
    dplyr::mutate(
      component = as.numeric(gsub("comp\\.", "", component)),
      .before = 1
    )
  
  return(loadings_tidy)
}

#' @export
#' @rdname loadings
loadings.pls <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  type <- match.arg(type)
  
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PLS objects")
  }
  
  n_comp <- min(max(components), object$ncomp)
  components <- components[components <= n_comp]
  
  if (type == "X") {
    loadings_mat <- object$loadings[, components, drop = FALSE]
    variable_names <- rownames(object$loadings)
  } else {
    loadings_mat <- object$loadingsY[, components, drop = FALSE]
    variable_names <- rownames(object$loadingsY)
  }
  
  if (is.null(variable_names) || all(variable_names == "")) {
    variable_names <- paste0("Var_", 1:ncol(loadings_mat))
  }
  
  # Convert to tidy format
  loadings_tidy <- as.data.frame(loadings_mat) |>
    tibble::as_tibble(.name_repair = "unique") |>
    dplyr::mutate(variable = variable_names) |>
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "component",
      values_to = "loading"
    ) |>
    dplyr::mutate(
      component = as.numeric(gsub("comp\\.", "", component)),
      .before = 1,
      variable_type = type
    )
  
  return(loadings_tidy)
}

#' @export
#' @rdname loadings
loadings.spls <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  loadings.pls(object, components, type, ...)
}

#' @export
#' @rdname loadings
loadings.splsda <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  loadings.pls(object, components, type, ...)
}

#' @export
#' @rdname loadings
loadings.rgcca <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  loadings.pls(object, components, type, ...)
}

#' @export
#' @rdname loadings
loadings.diablo <- function(object, components = 1:2, type = c("X", "Y"), ...) {
  loadings.pls(object, components, type, ...)
}

#' Extract Explained Variance from Multivariate Analysis Object
#' 
#' @description Generic function to extract explained variance from multivariate analysis objects.
#' 
#' @param object A multivariate analysis object
#' @param ... Additional arguments for specific methods
#' 
#' @return A data frame with explained variance information
#' 
#' @examples
#' \dontrun{
#' library(mixOmics)
#' data(wine)
#' pca_obj <- pca(wine$X, ncomp = 3)
#' var_exp <- explained_variance(pca_obj)
#' print(var_exp)
#' }
#' 
#' @export
explained_variance <- function(object, ...) {
  UseMethod("explained_variance")
}

#' @export
#' @rdname explained_variance
explained_variance.default <- function(object, ...) {
  stop("No explained_variance method available for this object type")
}

#' @export
#' @rdname explained_variance
explained_variance.pca <- function(object, ...) {
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PCA objects")
  }
  
  eigval <- object$eigval
  total_var <- sum(eigval)
  
  tibble::tibble(
    component = 1:length(eigval),
    variance = eigval,
    variance_percent = eigval / total_var * 100,
    cumulative_variance = cumsum(eigval / total_var * 100)
  )
}

#' @export
#' @rdname explained_variance
explained_variance.pls <- function(object, ...) {
  if (!requireNamespace("mixOmics", quietly = TRUE)) {
    stop("mixOmics package required for PLS objects")
  }
  
  n_comp <- object$ncomp
  
  tibble::tibble(
    component = 1:n_comp,
    x_variance = object$explVar$X[1:n_comp],
    y_variance = object$explVar$Y[1:n_comp],
    x_variance_percent = object$explVar$X[1:n_comp] * 100,
    y_variance_percent = object$explVar$Y[1:n_comp] * 100,
    x_cumulative = cumsum(object$explVar$X[1:n_comp] * 100),
    y_cumulative = cumsum(object$explVar$Y[1:n_comp] * 100)
  )
}

#' @export
#' @rdname explained_variance
explained_variance.spls <- function(object, ...) {
  explained_variance.pls(object, ...)
}

#' @export
#' @rdname explained_variance
explained_variance.splsda <- function(object, ...) {
  explained_variance.pls(object, ...)
}

#' @export
#' @rdname explained_variance
explained_variance.rgcca <- function(object, ...) {
  explained_variance.pls(object, ...)
}

#' @export
#' @rdname explained_variance
explained_variance.diablo <- function(object, ...) {
  explained_variance.pls(object, ...)
}
