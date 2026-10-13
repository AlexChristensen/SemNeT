# Tests for aspl()

test_that("aspl matches a hand-computed value on a small connected ring network", {

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

  # Check ASPL
  expect_equal(as.numeric(aspl(A)), 4 / 3)

  # Check arithmetic ASPL
  expect_equal(as.numeric(aspl(A, type = "arithmetic")), 4 / 3)

  # Check against {igraph}
  expect_equal(as.numeric(aspl(A)), igraph::mean_distance(igraph::graph_from_adjacency_matrix(A, "undirected")))

  # Get distances from {igraph}
  d <- igraph::distances(igraph::graph_from_adjacency_matrix(A, "undirected"))

  # Check harmonic ASPL
  expect_equal(as.numeric(aspl(A, type = "harmonic")), 1.2)

  # Check harmonic ASPL against distances
  expect_equal(as.numeric(aspl(A, type = "harmonic")), 1 / mean(1 / d[upper.tri(d)]))

})

test_that("aspl excludes unreachable pairs by default instead of masking them as adjacent", {

  # Set up network
  A <- matrix(0, 8, 8)

  # Add first clique
  A[1:4, 1:4] <- 1

  # Add second clique
  A[5:8, 5:8] <- 1

  # Set diagonal
  diag(A) <- 1

  # Check ASPL
  expect_equal(as.numeric(aspl(A)), 1)

  # Check harmonic ASPL
  expect_equal(as.numeric(aspl(A, type = "harmonic")), 7 / 3)

  # Check network without edges
  expect_identical(as.numeric(aspl(matrix(0, 3, 3), type = "harmonic")), Inf)

})

test_that("aspl's weighted distances match an independent (igraph) reference", {

  # Skip without {igraph}
  skip_if_not_installed("igraph")

  # Set seed
  set.seed(42)

  # Set number of nodes
  n <- 8

  # Initialize network
  A <- matrix(0, n, n)

  # Sample weights
  A[upper.tri(A)] <- sample(c(0, 0, 0.2, 0.5, 0.8, 1), n * (n - 1) / 2, replace = TRUE)

  # Make network symmetric
  A <- A + t(A)

  # Remove self-loops
  diag(A) <- 0

  # Loop over consecutive nodes
  for(i in 1:(n - 1)){

    # Check for missing edge
    if(A[i, i + 1] == 0){

      # Connect nodes
      A[i, i + 1] <- A[i + 1, i] <- 0.5

    }

  }

  # Compute distances
  D_semnet <- distance(A, weighted = TRUE)

  # Convert to {igraph}
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)

  # Compute distances in {igraph}
  D_igraph <- igraph::distances(g, weights = 1 / igraph::E(g)$weight)

  # Check distances
  expect_equal(unname(D_semnet), unname(D_igraph), tolerance = 1e-8)

})

test_that("aspl's harmonic mean is never larger than the arithmetic mean distance", {

  # Initialize network
  A <- matrix(0, 12, 12)

  # Loop over nodes
  for(i in 1:12){

    # Get next node
    j <- if(i == 12) 1 else i + 1

    # Connect nodes
    A[i, j] <- A[j, i] <- 1

  }

  # Add chords
  A[1, 5] <- A[5, 1] <- A[3, 9] <- A[9, 3] <- A[6, 11] <- A[11, 6] <- 1

  # Convert to {igraph}
  graph <- igraph::graph_from_adjacency_matrix(A, "undirected")

  # Check for connected network
  expect_true(igraph::is_connected(graph))

  # Check ordering
  expect_lt(as.numeric(aspl(A, type = "harmonic")), as.numeric(aspl(A)))

  # Check ASPL against {igraph}
  expect_equal(as.numeric(aspl(A)), igraph::mean_distance(graph))

})

test_that("aspl's per-node averages are each node's distances to the others, also for directed input", {

  # Set up directed network
  A <- matrix(0, 3, 3)

  # Add edges
  A[1, 2] <- A[2, 3] <- 1

  # Compute ASPL
  result <- aspl(A)

  # Check node averages
  expect_equal(unname(attr(result, "node")), c(1.5, 1, NaN))

  # Check ASPL
  expect_equal(as.numeric(result), mean(c(1.5, 1)))

})
