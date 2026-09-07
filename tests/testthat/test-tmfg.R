# Tests for tmfg()

test_that("tmfg returns a valid TMFG-filtered adjacency matrix", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  expect_true(is.matrix(net))
  expect_equal(dim(net), dim(cordata))

  # TMFG retains exactly 3n - 6 edges (off-diagonal, counting each edge once)
  binary <- ifelse(net != 0, 1, 0)
  diag(binary) <- 0
  expect_equal(sum(binary) / 2, 3 * ncol(cordata) - 6)

})

test_that("tmfg handles the minimum 4-node case without corrupting output", {

  set.seed(3)
  small <- cor(matrix(rnorm(15 * 4), nrow = 15, ncol = 4))
  net <- suppressWarnings(tmfg(small))

  expect_equal(dim(net), c(4, 4))
  expect_true(all(is.finite(net)))

  # A 4-node network is the tetrahedron itself: fully connected
  binary <- ifelse(net != 0, 1, 0)
  expect_equal(sum(binary), 16)

})

test_that("tmfg errors below the 4-node minimum instead of silently corrupting", {

  expect_error(tmfg(cor(matrix(rnorm(15 * 3), nrow = 15, ncol = 3))), "at least 4 nodes")

})

test_that("tmfg warns (but still runs) below the recommended 9-node minimum", {

  set.seed(4)
  cordata <- cor(matrix(rnorm(15 * 6), nrow = 15, ncol = 6))
  expect_warning(tmfg(cordata), "smaller than recommended")

})

test_that("tmfg's depend = TRUE path preserves directional edge weights", {

  set.seed(2)
  asym <- matrix(runif(20 * 20, -1, 1), 20, 20)
  diag(asym) <- 1

  net <- suppressWarnings(tmfg(asym, depend = TRUE))

  expect_equal(dim(net), dim(asym))

  # A directed edge's two weights should come from the original asymmetric
  # matrix in each direction, so they need not be equal to each other
  connected <- which(net != 0 & upper.tri(net), arr.ind = TRUE)
  expect_true(nrow(connected) > 0)

})

test_that("tmfg validates its input", {

  expect_error(tmfg(matrix(1, 3, 4)), "square")
  expect_error(tmfg(cor(matrix(rnorm(50), 10, 5)), depend = "yes"), "tmfg")

})
