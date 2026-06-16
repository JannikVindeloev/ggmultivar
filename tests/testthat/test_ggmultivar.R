context("ggmultivar Main Functions")

# Test ggmultivar function
test_that("ggmultivar creates a ggmultivar object", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2))
  
  expect_true(is_ggmultivar(p))
  expect_true(inherits(p, "ggplot"))
  expect_true(inherits(p, "ggmultivar"))
})

# Test ggmultivar with default aesthetics
test_that("ggmultivar works with default aesthetics", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result)
  
  expect_true(is_ggmultivar(p))
})

# Test as_ggmultivar S3 dispatch
test_that("as_ggmultivar works with pca objects", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  gg_data <- as_ggmultivar(pca_result)
  
  expect_is(gg_data, "list")
  expect_is(gg_data$plot_data, "data.frame")
  expect_true("PC1" %in% names(gg_data$plot_data))
  expect_true("PC2" %in% names(gg_data$plot_data))
})

# Test get_ggmultivar_data
test_that("get_ggmultivar_data retrieves stored data", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2))
  
  gg_data <- get_ggmultivar_data(p)
  
  expect_is(gg_data, "list")
  expect_is(gg_data$plot_data, "data.frame")
})

# Test get_ggmultivar_original
test_that("get_ggmultivar_original retrieves original object", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2))
  
  original <- get_ggmultivar_original(p)
  
  expect_identical(original, pca_result)
})

# Test is_ggmultivar
test_that("is_ggmultivar correctly identifies ggmultivar objects", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2))
  regular_plot <- ggplot2::ggplot(data_mat, ggplot2::aes(1, 2))
  
  expect_true(is_ggmultivar(p))
  expect_false(is_ggmultivar(regular_plot))
})

# Test error handling
test_that("ggmultivar throws error for missing data", {
  expect_error(ggmultivar(), "data argument is required")
})

# Test as_ggmultivar error handling
test_that("as_ggmultivar throws error for unsupported objects", {
  expect_error(as_ggmultivar("not_a_valid_object"), "No as_ggmultivar method for object of class")
})