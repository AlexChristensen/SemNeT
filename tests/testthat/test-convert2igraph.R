# Tests for convert2igraph()

test_that("convert2igraph produces the same network as the qgraph-based round trip", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  A <- suppressWarnings(tmfg(cordata))

  net <- convert2igraph(A)

  expect_s3_class(net, "igraph")
  expect_equal(igraph::vcount(net), ncol(A))
  expect_equal(igraph::ecount(net), (3 * ncol(A) - 6))

})

test_that("convert2igraph drops nodes with zero strength", {

  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  diag(A) <- 1

  # Nodes 3 and 4 have no connections at all (besides the diagonal)
  net <- convert2igraph(A)
  expect_equal(igraph::vcount(net), 2)

})

test_that("convert2igraph validates its input", {

  expect_error(convert2igraph(matrix(1, 3, 4)), "square")
  expect_error(convert2igraph(list(1, 2)), "convert2igraph")

})
