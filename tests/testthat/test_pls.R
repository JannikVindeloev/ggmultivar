context("PLS Functions")

# Test perform_pls function
test_that("perform_pls works with matrix input", {
  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
  
  # Skip test if pls package is not available
  skip_if_not(installed.packages()["pls"] >= "1.0")
  
  result <- perform_pls(X, Y, n_components = 3)
  
  expect_is(result, "list")
  expect_is(result$scores, "data.frame")
  expect_is(result$loadings, "data.frame")
  expect_is(result$y_loadings, "data.frame")
  expect_is(result$weights, "data.frame")
  expect_is(result$explained_variance, "data.frame")
  expect_equal(nrow(result$scores), 50)
  expect_equal(nrow(result$loadings), 10)
  expect_equal(result$n_components, 3)
})

# Test error handling for PLS
test_that("perform_pls throws error for mismatched dimensions", {
  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  Y <- matrix(rnorm(40 * 2), nrow = 40, ncol = 2)  # Different number of rows
  
  expect_error(perform_pls(X, Y), "X and Y must have the same number of rows")
})

# Test pls_scores_plot function
test_that("pls_scores_plot returns ggplot object", {
  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
  
  skip_if_not(installed.packages()["pls"] >= "1.0")
  
  result <- perform_pls(X, Y, n_components = 2)
  plot <- pls_scores_plot(result)
  
  expect_is(plot, "ggplot")
})

# Test pls_loadings_plot function
test_that("pls_loadings_plot returns ggplot object", {
  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
  
  skip_if_not(installed.packages()["pls"] >= "1.0")
  
  result <- perform_pls(X, Y, n_components = 2)
  plot <- pls_loadings_plot(result)
  
  expect_is(plot, "ggplot")
})

# Test pls_biplot function
test_that("pls_biplot returns ggplot object", {
  set.seed(123)
  X <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  Y <- matrix(rnorm(50 * 2), nrow = 50, ncol = 2)
  
  skip_if_not(installed.packages()["pls"] >= "1.0")
  
  result <- perform_pls(X, Y, n_components = 2)
  plot <- pls_biplot(result)
  
  expect_is(plot, "ggplot")
})