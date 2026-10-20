# Tests for convert2igraph()

test_that("convert2igraph removes nodes without edges, not nodes with canceling signs", {

  # Set up signed network: node d has edges of +1 and -1
  A <- matrix(0, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  A["a", "d"] <- A["d", "a"] <- 1
  A["b", "d"] <- A["d", "b"] <- -1

  # Convert
  g <- convert2igraph(A)

  # Check nodes and edges
  expect_equal(igraph::vcount(g), 3)
  expect_equal(igraph::ecount(g), 2)

  # Set up network with isolate
  B <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  B["b", "c"] <- B["c", "b"] <- 1

  # Check isolate removed
  expect_identical(igraph::V(convert2igraph(B))$name, c("b", "c"))

  # Check single edge keeps matrix form
  expect_equal(igraph::vcount(convert2igraph(B[2:3, 2:3])), 2)

  # Check asymmetric input is rejected
  C <- B
  C["b", "c"] <- 2
  expect_error(convert2igraph(C), "symmetric")

})
