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

test_that("cc is the average of the local coefficients by default; na.rm leaves undefined nodes out", {

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

  # Check that default is the average
  expect_equal(as.numeric(with_zeros), as.numeric(cc(A, type = "average")))

  # Check clustering of last node
  expect_equal(as.numeric(attr(with_zeros, "node"))[5], 0)

  # Compute clustering without undefined
  without <- cc(A, na.rm = TRUE)

  # Check clustering
  expect_equal(as.numeric(without), (1 + 1 + 1 / 3 + 0) / 4)

  # Check for missing last node
  expect_true(is.na(attr(without, "node")[5]))

  # Check against {igraph}
  graph <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  expect_equal(as.numeric(with_zeros), igraph::transitivity(graph, type = "average", isolates = "zero"))

  # Check for error
  expect_error(cc(A, na.rm = "yes"), "cc")

  # Check for error with unknown type
  expect_error(cc(A, type = "local"), "arg")

})

test_that("cc's global type is transitivity; na.rm only changes the per-node values", {

  # Set up network
  A <- matrix(0, 5, 5)

  # Add triangle
  A[1, 2] <- A[2, 1] <- A[2, 3] <- A[3, 2] <- A[1, 3] <- A[3, 1] <- 1

  # Add tail
  A[3, 4] <- A[4, 3] <- A[4, 5] <- A[5, 4] <- 1

  # Compute global clustering
  global <- cc(A, type = "global")

  # Check clustering (3 x triangles / connected triples = 3 / 6)
  expect_equal(as.numeric(global), 3 / 6)

  # Check that the average of the local coefficients is a different quantity
  expect_false(isTRUE(all.equal(as.numeric(global), as.numeric(cc(A)))))

  # Compute global clustering without undefined nodes
  without <- cc(A, na.rm = TRUE, type = "global")

  # Check clustering is unchanged
  expect_equal(as.numeric(without), as.numeric(global))

  # Check for missing last node
  expect_true(is.na(attr(without, "node")[5]))

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

test_that("cc's global type matches igraph's transitivity and differs from the average local coefficient", {

  # Set seed
  set.seed(4)

  # Loop over random graphs
  for(i in 1:25){

    # Generate graph
    graph <- igraph::sample_gnp(30, runif(1, .1, .5))
    A <- as.matrix(igraph::as_adjacency_matrix(graph))

    # Check global transitivity
    expect_equal(as.numeric(cc(A, type = "global")), igraph::transitivity(graph, type = "global"))

    # Check average of local coefficients
    expect_equal(as.numeric(cc(A)), igraph::transitivity(graph, type = "average", isolates = "zero"))

  }

  # Set up a hub: a triangle-rich clique with many pendant leaves on one of its nodes
  A <- matrix(0, 12, 12)
  A[1:4, 1:4] <- 1
  A[1, 5:12] <- 1
  A <- pmax(A, t(A))
  diag(A) <- 0

  # Check that the global coefficient is not the Watts-Strogatz average
  global <- as.numeric(cc(A, type = "global"))
  expect_equal(global, igraph::transitivity(igraph::graph_from_adjacency_matrix(A, mode = "undirected"), type = "global"))
  expect_false(isTRUE(all.equal(global, as.numeric(cc(A)))))

})

test_that("cc is undefined without connected triples and rejects asymmetric networks", {

  # Set up matching
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- A[3, 4] <- A[4, 3] <- 1

  # Check for undefined global clustering
  expect_true(is.nan(as.numeric(cc(A, type = "global"))))

  # Check for zero average clustering
  expect_equal(as.numeric(cc(A)), 0)

  # Check for undefined average clustering without undefined nodes
  expect_true(is.nan(as.numeric(cc(A, na.rm = TRUE))))

  # Set up asymmetric network
  A[1, 2] <- 0

  # Check for error
  expect_error(cc(A), "symmetric")

})

test_that("weighted global cc is the ratio of weighted triangles to connected triples", {

  # Set up weighted network with a pendant node
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[1, 3] <- A[3, 1] <- .5
  A[2, 3] <- A[3, 2] <- .25
  A[3, 4] <- A[4, 3] <- 1

  # Scale by the largest weight and take cube roots
  W <- A^(1 / 3)

  # Compute by hand
  expected <- sum(diag(W %*% W %*% W)) / sum(rowSums(A != 0) * (rowSums(A != 0) - 1))

  # Check ratio
  expect_equal(as.numeric(cc(A, weighted = TRUE, type = "global")), expected)

})
