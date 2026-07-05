context("ggscores Functions")

# Test ggscores with data frame input
test_that("ggscores works with data frame input", {
  # Create mock scores data
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  plot <- ggscores(scores_data)
  expect_is(plot, "ggplot")
})

# Test ggscores with color_by parameter
test_that("ggscores works with color_by", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  color_vec <- sample(letters[1:3], 50, replace = TRUE)
  plot <- ggscores(scores_data, color_by = color_vec)
  expect_is(plot, "ggplot")
})

# Test ggscores with facet_by parameter
test_that("ggscores works with facet_by", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  facet_vec <- sample(letters[1:3], 50, replace = TRUE)
  plot <- ggscores(scores_data, facet_by = facet_vec)
  expect_is(plot, "ggplot")
})

# Test ggscores with show_ellipse parameter
test_that("ggscores works with show_ellipse", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  color_vec <- sample(letters[1:3], 50, replace = TRUE)
  plot <- ggscores(scores_data, color_by = color_vec, show_ellipse = TRUE)
  expect_is(plot, "ggplot")
})

# Test ggscores with show_labels parameter
test_that("ggscores works with show_labels", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  plot <- ggscores(scores_data, show_labels = TRUE)
  expect_is(plot, "ggplot")
})

# Test error handling
test_that("ggscores throws error for invalid input", {
  expect_error(ggscores("not_a_data_frame"), "data must be a data frame")
  
  invalid_data <- data.frame(a = 1:10, b = 1:10)
  expect_error(ggscores(invalid_data), "Data must contain columns")
})

# Test ggscores with custom components
test_that("ggscores works with custom components", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:3, each = 50),
    score = rnorm(150)
  )
  
  plot <- ggscores(scores_data, x_component = 1, y_component = 3)
  expect_is(plot, "ggplot")
})
