context("mixOmics Compatibility")

# Test mixOmics conversion functions
# These tests will be skipped if mixOmics is not available

# Test mixomics_to_ggmultivar with PCA
test_that("mixomics_to_ggmultivar works with PCA object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  pca_mix <- pca(wine$X, ncomp = 3)
  result <- mixomics_to_ggmultivar(pca_mix, n_components = 2)
  
  expect_is(result, "list")
  expect_is(result$scores, "data.frame")
  expect_is(result$loadings, "data.frame")
  expect_is(result$explained_variance, "data.frame")
  expect_equal(result$n_components, 2)
})

# Test mixomics_to_ggmultivar with PLS
test_that("mixomics_to_ggmultivar works with PLS object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  pls_mix <- pls(wine$X, wine$Y, ncomp = 3)
  result <- mixomics_to_ggmultivar(pls_mix, n_components = 2)
  
  expect_is(result, "list")
  expect_is(result$scores, "data.frame")
  expect_is(result$loadings, "data.frame")
  expect_is(result$y_loadings, "data.frame")
  expect_is(result$weights, "data.frame")
})

# Test mixomics_to_ggmultivar with sPLS-DA
test_that("mixomics_to_ggmultivar works with sPLS-DA object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  splsda_mix <- splsda(wine$X, wine$Y, ncomp = 3)
  result <- mixomics_to_ggmultivar(splsda_mix, n_components = 2)
  
  expect_is(result, "list")
  expect_is(result$scores, "data.frame")
  expect_is(result$loadings, "data.frame")
  expect_not_null(result$class_labels)  # sPLS-DA should have class labels
})

# Test mixomics_scores_plot function
test_that("mixomics_scores_plot returns ggplot object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  pca_mix <- pca(wine$X, ncomp = 3)
  plot <- mixomics_scores_plot(pca_mix)
  
  expect_is(plot, "ggplot")
})

# Test mixomics_loadings_plot function
test_that("mixomics_loadings_plot returns ggplot object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  pca_mix <- pca(wine$X, ncomp = 3)
  plot <- mixomics_loadings_plot(pca_mix)
  
  expect_is(plot, "ggplot")
})

# Test mixomics_biplot function
test_that("mixomics_biplot returns ggplot object", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  library(mixOmics)
  data(wine)
  
  pca_mix <- pca(wine$X, ncomp = 3)
  plot <- mixomics_biplot(pca_mix)
  
  expect_is(plot, "ggplot")
})

# Test error handling for invalid mixOmics objects
test_that("mixomics_to_ggmultivar throws error for invalid input", {
  skip_if_not(installed.packages()["mixOmics"] >= "1.0")
  
  expect_error(mixomics_to_ggmultivar("not_a_mixomics_object"), "Unsupported mixOmics object type")
})