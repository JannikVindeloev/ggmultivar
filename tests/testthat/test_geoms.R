context("Custom Geoms")

# Test geom_encircle
test_that("geom_encircle creates a layer", {
  # Create mock data
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100),
    group = sample(letters[1:3], 50, replace = TRUE)
  )
  
  # Pivot to wide format
  scores_wide <- scores_data |>
    tidyr::pivot_wider(
      names_from = component,
      values_from = score,
      names_prefix = "PC"
    )
  
  layer <- geom_encircle(data = scores_wide, group_by = "group")
  expect_is(layer, "Layer")
})

# Test geom_encircle with different geom types
test_that("geom_encircle works with different geom types", {
  scores_wide <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    PC1 = rnorm(50),
    PC2 = rnorm(50),
    group = sample(letters[1:3], 50, replace = TRUE)
  )
  
  # Test ellipse
  layer_ellipse <- geom_encircle(data = scores_wide, group_by = "group", geom = "ellipse")
  expect_is(layer_ellipse, "Layer")
  
  # Test hull
  layer_hull <- geom_encircle(data = scores_wide, group_by = "group", geom = "hull")
  expect_is(layer_hull, "Layer")
  
  # Test polygon
  layer_polygon <- geom_encircle(data = scores_wide, group_by = "group", geom = "polygon")
  expect_is(layer_polygon, "Layer")
})

# Test geom_correlation_circle
test_that("geom_correlation_circle creates a layer", {
  layer <- geom_correlation_circle()
  expect_is(layer, "Layer")
})

# Test geom_correlation_circle with custom parameters
test_that("geom_correlation_circle works with custom parameters", {
  layer <- geom_correlation_circle(radius = 2, color = "blue", linetype = "solid")
  expect_is(layer, "Layer")
})

# Test geom_sample_labels
test_that("geom_sample_labels creates a layer", {
  scores_wide <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    PC1 = rnorm(50),
    PC2 = rnorm(50)
  )
  
  layer <- geom_sample_labels(data = scores_wide)
  expect_is(layer, "Layer")
})

# Test geom_sample_labels with custom parameters
test_that("geom_sample_labels works with custom parameters", {
  scores_wide <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    PC1 = rnorm(50),
    PC2 = rnorm(50)
  )
  
  layer <- geom_sample_labels(data = scores_wide, size = 4, color = "red")
  expect_is(layer, "Layer")
})

# Test geom_variable_labels
test_that("geom_variable_labels creates a layer", {
  loadings_wide <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    x_scaled = rnorm(10),
    y_scaled = rnorm(10)
  )
  
  layer <- geom_variable_labels(data = loadings_wide)
  expect_is(layer, "Layer")
})

# Test geom_variable_labels with custom parameters
test_that("geom_variable_labels works with custom parameters", {
  loadings_wide <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    x_scaled = rnorm(10),
    y_scaled = rnorm(10)
  )
  
  layer <- geom_variable_labels(data = loadings_wide, size = 5, color = "blue")
  expect_is(layer, "Layer")
})

# Test error handling
test_that("geom_encircle throws error without group_by", {
  scores_wide <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    PC1 = rnorm(50),
    PC2 = rnorm(50)
  )
  
  expect_error(geom_encircle(data = scores_wide), "group_by must be specified")
})
