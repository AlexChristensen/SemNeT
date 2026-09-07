# Tests for cc()

test_that("cc matches a hand-computed value for a fully-connected 4-node network", {

  # Complete graph: every node's neighborhood is itself fully connected -> CC = 1
  A <- matrix(1, 4, 4)
  diag(A) <- 0

  expect_equal(as.numeric(cc(A)), 1)

})

test_that("cc matches a hand-computed value for a ring network (no triangles)", {

  # 4-node ring: no two neighbors of any node are themselves connected -> CC = 0
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[2, 3] <- A[3, 2] <- 1
  A[3, 4] <- A[4, 3] <- 1
  A[4, 1] <- A[1, 4] <- 1

  expect_equal(as.numeric(cc(A)), 0)

})

test_that("cc validates the input before mutating it", {

  # A non-square matrix must error before the diagonal is ever touched
  expect_error(cc(matrix(1, 3, 4)), "square")

})

test_that("cc attaches per-node clustering coefficients without changing its numeric value", {

  A <- matrix(1, 4, 4)
  diag(A) <- 0

  result <- cc(A)
  expect_equal(as.numeric(result), 1)
  expect_length(attr(result, "node"), 4)
  expect_true(all(attr(result, "node") == 1))

})

test_that("cc computes weighted clustering coefficients", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  weighted_cc <- cc(net, weighted = TRUE)
  expect_true(is.numeric(as.numeric(weighted_cc)))
  expect_true(as.numeric(weighted_cc) >= 0)

})
