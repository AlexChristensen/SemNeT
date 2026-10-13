# Tests for cc()

test_that("cc matches a hand-computed value for a fully-connected 4-node network", {

  # Set up complete network
  A <- matrix(1, 4, 4)

  # Remove self-loops
  diag(A) <- 0

  # Check clustering
  expect_equal(as.numeric(cc(A)), 1)

})

test_that("cc matches a hand-computed value for a ring network (no triangles)", {

  # Set up network
  A <- matrix(0, 4, 4)

  # Add first edge
  A[1, 2] <- A[2, 1] <- 1

  # Add second edge
  A[2, 3] <- A[3, 2] <- 1

  # Add third edge
  A[3, 4] <- A[4, 3] <- 1

  # Add fourth edge
  A[4, 1] <- A[1, 4] <- 1

  # Check clustering
  expect_equal(as.numeric(cc(A)), 0)

})

test_that("cc's weighted formula matches a hand-derived value, including signed weights", {

  # Set up weighted triangle
  A <- matrix(c(0, 2, 4, 2, 0, 8, 4, 8, 0), 3, 3)

  # Compute clustering
  result <- cc(A, weighted = TRUE)

  # Check clustering of nodes
  expect_equal(as.numeric(attr(result, "node")), c(.5, .5, .5))

  # Copy network
  A_signed <- A

  # Make one weight negative
  A_signed[1, 3] <- A_signed[3, 1] <- -4

  # Compute clustering
  result_signed <- cc(A_signed, weighted = TRUE)

  # Check for no missing values
  expect_false(anyNA(attr(result_signed, "node")))

  # Check clustering of nodes
  expect_equal(as.numeric(attr(result_signed, "node")), c(-.5, -.5, -.5))

})

test_that("cc computes weighted clustering coefficients", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Compute clustering
  weighted_cc <- cc(net, weighted = TRUE)

  # Check for numeric
  expect_true(is.numeric(as.numeric(weighted_cc)))

  # Check for non-negative
  expect_true(as.numeric(weighted_cc) >= 0)

})

test_that("cc counts nodes with fewer than two neighbors as 0 unless na.rm = TRUE", {

  # Set up network
  A <- matrix(0, 5, 5)

  # Add triangle
  A[1, 2] <- A[2, 1] <- A[2, 3] <- A[3, 2] <- A[1, 3] <- A[3, 1] <- 1

  # Add tail
  A[3, 4] <- A[4, 3] <- A[4, 5] <- A[5, 4] <- 1

  # Compute clustering with zeros
  with_zeros <- cc(A)

  # Check clustering
  expect_equal(as.numeric(with_zeros), (1 + 1 + 1 / 3 + 0 + 0) / 5)

  # Check clustering of last node
  expect_equal(as.numeric(attr(with_zeros, "node"))[5], 0)

  # Compute clustering without undefined
  without <- cc(A, na.rm = TRUE)

  # Check clustering
  expect_equal(as.numeric(without), (1 + 1 + 1 / 3 + 0) / 4)

  # Check for missing last node
  expect_true(is.na(attr(without, "node")[5]))

  # Check for error
  expect_error(cc(A, na.rm = "yes"), "cc")

})

test_that("cc does not round its per-node values", {

  # Set up network
  A <- matrix(0, 6, 6)

  # Add triangle edges
  A[1, 2] <- A[2, 1] <- A[1, 3] <- A[3, 1] <- A[1, 4] <- A[4, 1] <- A[2, 3] <- A[3, 2] <- 1

  # Add tail edges
  A[4, 5] <- A[5, 4] <- A[4, 6] <- A[6, 4] <- 1

  # Check unrounded clustering
  expect_equal(as.numeric(attr(cc(A), "node"))[1], 1 / 3, tolerance = 1e-12)

})

test_that("weighted cc does not depend on the scale of the weights", {

  # Set seed
  set.seed(2)

  # Set up network
  A <- matrix(runif(36), 6, 6)

  # Make network symmetric
  A <- (A + t(A)) / 2

  # Remove self-loops
  diag(A) <- 0

  # Check scale invariance
  expect_equal(cc(A, weighted = TRUE), cc(A * 10, weighted = TRUE))

  # Check upper bound
  expect_lte(cc(A * 10, weighted = TRUE), 1)

  # Binarize network
  B <- ifelse(A > .5, 1, 0)

  # Check binary network
  expect_equal(cc(B, weighted = TRUE), cc(B))

})
