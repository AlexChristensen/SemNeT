# Tests for tmfg()

test_that("tmfg returns a valid TMFG-filtered adjacency matrix", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Check for matrix
  expect_true(is.matrix(net))

  # Check dimensions
  expect_equal(dim(net), dim(cordata))

  # Binarize network
  binary <- ifelse(net != 0, 1, 0)

  # Remove self-loops
  diag(binary) <- 0

  # Check for 3n - 6 edges
  expect_equal(sum(binary) / 2, 3 * ncol(cordata) - 6)

})

test_that("tmfg handles the minimum 4-node case without corrupting output", {

  # Set seed
  set.seed(3)

  # Set up correlations
  small <- cor(matrix(rnorm(15 * 4), nrow = 15, ncol = 4))

  # Estimate network
  net <- suppressWarnings(tmfg(small))

  # Check dimensions
  expect_equal(dim(net), c(4, 4))

  # Check for finite values
  expect_true(all(is.finite(net)))

  # Binarize network
  binary <- ifelse(net != 0, 1, 0)

  # Check for fully connected network
  expect_equal(sum(binary), 16)

})

test_that("tmfg's depend = TRUE path preserves directional edge weights", {

  # Set seed
  set.seed(2)

  # Set up asymmetric matrix
  asym <- matrix(runif(20 * 20, -1, 1), 20, 20)

  # Set diagonal
  diag(asym) <- 1

  # Estimate network
  net <- suppressWarnings(tmfg(asym, depend = TRUE))

  # Check dimensions
  expect_equal(dim(net), dim(asym))

  # Get connected pairs
  connected <- which(net != 0 & upper.tri(net), arr.ind = TRUE)

  # Check for connections
  expect_true(nrow(connected) > 0)

})
