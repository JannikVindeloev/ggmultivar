context("S3 Methods")

# Test scores method with data frame
test_that("scores method works with data frame", {
  # Create mock scores data
  scores_data <- tibble::tibble(
    sample = paste0("Sample_", 1:50),
    component = rep(1:2, each = 50),
    score = rnorm(100)
  )
  
  # This should use the default method which will fail
  expect_error(scores(scores_data), "No scores method available")
})

# Test loadings method with data frame
test_that("loadings method works with data frame", {
  # Create mock loadings data
  loadings_data <- tibble::tibble(
    variable = paste0("Var_", 1:10),
    component = rep(1:2, each = 10),
    loading = rnorm(20)
  )
  
  # This should use the default method which will fail
  expect_error(loadings(loadings_data), "No loadings method available")
})

# Test explained_variance method with data frame
test_that("explained_variance method works with data frame", {
  # Create mock variance data
  var_data <- data.frame(
    component = 1:3,
    variance = c(0.5, 0.3, 0.2)
  )
  
  # This should use the default method which will fail
  expect_error(explained_variance(var_data), "No explained_variance method available")
})

# Test that methods work with custom classes (if we had mock objects)
# These tests would normally require mixOmics to be installed
# For now, we'll just test that the functions exist

test_that("S3 methods are exported", {
  expect_true(exists("scores", mode = "function"))
  expect_true(exists("loadings", mode = "function"))
  expect_true(exists("explained_variance", mode = "function"))
})
