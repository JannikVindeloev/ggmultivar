context("PCA Functions")

# Test perform_pca function
test_that("perform_pca works with matrix input", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  result <- perform_pca(data_mat, n_components = 3)
  
  expect_is(result, "list")
  expect_is(result$scores, "data.frame")
  expect_is(result$loadings, "data.frame")
  expect_is(result$explained_variance, "data.frame")
  expect_equal(nrow(result$scores), 50)
  expect_equal(nrow(result$loadings), 10)
  expect_equal(result$n_components, 3)
})

# Test perform_pca with data frame input
test_that("perform_pca works with data frame input", {
  set.seed(123)
  data_df <- as.data.frame(matrix(rnorm(50 * 10), nrow = 50, ncol = 10))
  colnames(data_df) <- paste0("Var_", 1:10)
  
  result <- perform_pca(data_df, n_components = 2)
  
  expect_is(result, "list")
  expect_equal(result$variable_names, colnames(data_df))
})

# Test pca_scores_plot function
test_that("pca_scores_plot returns ggplot object", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  result <- perform_pca(data_mat, n_components = 2)
  plot <- pca_scores_plot(result)
  
  expect_is(plot, "ggplot")
})

# Test pca_loadings_plot function
test_that("pca_loadings_plot returns ggplot object", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  result <- perform_pca(data_mat, n_components = 2)
  plot <- pca_loadings_plot(result)
  
  expect_is(plot, "ggplot")
})

# Test pca_biplot function
test_that("pca_biplot returns ggplot object", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  result <- perform_pca(data_mat, n_components = 2)
  plot <- pca_biplot(result)
  
  expect_is(plot, "ggplot")
})

# Test with color_by parameter
test_that("pca_scores_plot works with color_by", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  color_vec <- sample(letters[1:3], 50, replace = TRUE)
  
  result <- perform_pca(data_mat, n_components = 2)
  plot <- pca_scores_plot(result, color_by = color_vec)
  
  expect_is(plot, "ggplot")
})

# Test error handling
test_that("perform_pca throws error for invalid input", {
  expect_error(perform_pca("not_a_matrix"), "Data must be a data frame or matrix")
  expect_error(perform_pca(matrix(1, 1, 1)), "Data must have at least 2 columns")
  expect_error(perform_pca(matrix(1, 2, 2), n_components = 3), "n_components must be between 1 and")
})