context("ggbiplot Functions")

# Test ggbiplot with list input
test_that("ggbiplot works with list input", {
  # Create mock data
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggbiplot(list(scores = scores_data, loadings = loadings_data))
  expect_is(plot, "ggplot")
})

# Test ggbiplot with color_by parameter
test_that("ggbiplot works with color_by", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  color_vec <- sample(letters[1:3], 50, replace = TRUE)
  plot <- ggbiplot(list(scores = scores_data, loadings = loadings_data), 
                   color_by = color_vec)
  expect_is(plot, "ggplot")
})

# Test ggbiplot with show_ellipse parameter
test_that("ggbiplot works with show_ellipse", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  color_vec <- sample(letters[1:3], 50, replace = TRUE)
  plot <- ggbiplot(list(scores = scores_data, loadings = loadings_data), 
                   color_by = color_vec, show_ellipse = TRUE)
  expect_is(plot, "ggplot")
})

# Test ggbiplot with show_loading_labels parameter
test_that("ggbiplot works with show_loading_labels", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggbiplot(list(scores = scores_data, loadings = loadings_data), 
                   show_loading_labels = FALSE)
  expect_is(plot, "ggplot")
})

# Test ggbiplot with show_circle parameter
test_that("ggbiplot works with show_circle", {
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  plot <- ggbiplot(list(scores = scores_data, loadings = loadings_data), 
                   show_circle = FALSE)
  expect_is(plot, "ggplot")
})

# Test error handling
test_that("ggbiplot throws error for invalid input", {
  expect_error(ggbiplot("not_a_list"), "data must be a multivariate analysis object")
  
  invalid_list <- list(scores = data.frame(a = 1:10))
  expect_error(ggbiplot(invalid_list), "data list must contain")
})
