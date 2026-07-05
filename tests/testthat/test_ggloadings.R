context("ggloadings Functions")

# Test ggloadings with data frame input
test_that("ggloadings works with data frame input", {
  # Create mock loadings data
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggloadings(loadings_data)
  expect_is(plot, "ggplot")
})

# Test ggloadings with color_by parameter
test_that("ggloadings works with color_by", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  color_vec <- abs(rnorm(10))
  plot <- ggloadings(loadings_data, color_by = color_vec)
  expect_is(plot, "ggplot")
})

# Test ggloadings with show_arrows parameter
test_that("ggloadings works with show_arrows", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggloadings(loadings_data, show_arrows = FALSE)
  expect_is(plot, "ggplot")
})

# Test ggloadings with show_circle parameter
test_that("ggloadings works with show_circle", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggloadings(loadings_data, show_circle = FALSE)
  expect_is(plot, "ggplot")
})

# Test ggloadings with show_labels parameter
test_that("ggloadings works with show_labels", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggloadings(loadings_data, show_labels = FALSE)
  expect_is(plot, "ggplot")
})

# Test ggloadings with facet_by parameter
test_that("ggloadings works with facet_by", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20),
    variable_type = rep(c("X", "Y"), each = 5)
  )
  
  plot <- ggloadings(loadings_data, facet_by = "variable_type")
  expect_is(plot, "ggplot")
})

# Test error handling
test_that("ggloadings throws error for invalid input", {
  expect_error(ggloadings("not_a_data_frame"), "data must be a data frame")
  
  invalid_data <- data.frame(a = 1:10, b = 1:10)
  expect_error(ggloadings(invalid_data), "Data must contain columns")
})

# Test ggloadings with custom components
test_that("ggloadings works with custom components", {
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:3, each = 10),
    loading = rnorm(30)
  )
  
  plot <- ggloadings(loadings_data, x_component = 1, y_component = 3)
  expect_is(plot, "ggplot")
})
