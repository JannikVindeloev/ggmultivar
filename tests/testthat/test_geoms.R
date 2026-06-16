context("Custom Geoms")

# Test geom_scores
test_that("geom_scores creates a layer", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores()
  
  expect_true(is_ggmultivar(p))
  # Check that the layer was added
  expect_true(length(p$layers) >= 1)
})

# Test geom_scores with custom aesthetics
test_that("geom_scores works with custom aesthetics", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores(size = 3, color = "red", alpha = 0.5)
  
  expect_true(is_ggmultivar(p))
})

# Test geom_loadings
test_that("geom_loadings creates a layer", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores() +
    geom_loadings()
  
  expect_true(is_ggmultivar(p))
  expect_true(length(p$layers) >= 2)
})

# Test geom_loadings with custom parameters
test_that("geom_loadings works with custom parameters", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores() +
    geom_loadings(as_arrows = FALSE, arrow_length = 0.5, color = "blue")
  
  expect_true(is_ggmultivar(p))
})

# Test geom_conf
test_that("geom_conf creates a layer", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores() +
    geom_conf()
  
  expect_true(is_ggmultivar(p))
  expect_true(length(p$layers) >= 2)
})

# Test geom_conf with custom parameters
test_that("geom_conf works with custom parameters", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2)) +
    geom_scores() +
    geom_conf(radius = 0.8, color = "blue", linetype = "solid")
  
  expect_true(is_ggmultivar(p))
})

# Test geom_ellipse
test_that("geom_ellipse creates a layer", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  # Create a grouping variable
  groups <- sample(letters[1:3], 50, replace = TRUE)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2, color = groups)) +
    geom_scores() +
    geom_ellipse(level = 0.95)
  
  expect_true(is_ggmultivar(p))
  expect_true(length(p$layers) >= 2)
})

# Test geom_ellipse with custom parameters
test_that("geom_ellipse works with custom parameters", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  groups <- sample(letters[1:3], 50, replace = TRUE)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2, color = groups)) +
    geom_scores() +
    geom_ellipse(level = 0.90, color = "red")
  
  expect_true(is_ggmultivar(p))
})

# Test combined geoms
test_that("multiple geoms can be combined", {
  set.seed(123)
  data_mat <- matrix(rnorm(50 * 10), nrow = 50, ncol = 10)
  
  pca_result <- perform_pca(data_mat, n_components = 3)
  
  groups <- sample(letters[1:3], 50, replace = TRUE)
  
  p <- ggmultivar(data = pca_result, aes(PC1, PC2, color = groups)) +
    geom_scores() +
    geom_loadings() +
    geom_conf() +
    geom_ellipse(level = 0.95)
  
  expect_true(is_ggmultivar(p))
  expect_true(length(p$layers) >= 4)
})