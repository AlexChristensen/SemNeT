# Tests for q()

test_that("q's compiled Louvain is reproducible via 'seed', independent of R's own RNG state", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Compute modularity
  q1 <- q(net, seed = 42)

  # Set different seed
  set.seed(999)

  # Compute modularity
  q2 <- q(net, seed = 42)

  # Reset seed
  set.seed(NULL)

  # Compute modularity
  q3 <- q(net, seed = 42)

  # Check first and second
  expect_equal(q1, q2)

  # Check first and third
  expect_equal(q1, q3)

  # Set seed
  set.seed(123)

  # Store random state
  state_before <- .Random.seed

  # Compute modularity
  q(net, seed = 42)

  # Check that random state is unchanged
  expect_identical(.Random.seed, state_before)

})

test_that("q's modularity is the modularity of the partition it found (checked against igraph)", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Remove self-loops
  diag(net) <- 0

  # Find communities
  communities <- louvain(abs(net), seed = 5)

  # Convert to {igraph}
  graph <- igraph::graph_from_adjacency_matrix(abs(net), mode = "undirected", weighted = TRUE)

  # Check modularity
  expect_equal(
    q(net, seed = 5),
    igraph::modularity(graph, communities$membership, weights = igraph::E(graph)$weight)
  )

})

test_that("q gives the textbook modularity of two triangles joined by an edge", {

  # Set up network
  net <- matrix(0, 6, 6)

  # Add triangles and bridge
  net[1, 2] <- net[2, 3] <- net[1, 3] <- net[4, 5] <- net[5, 6] <- net[4, 6] <- net[3, 4] <- 1

  # Make network symmetric
  net <- net + t(net)

  # Check modularity
  expect_equal(q(net, seed = 1), 5 / 14)

})

test_that("a network without edges has no defined modularity", {

  # Check for undefined modularity
  expect_true(is.nan(q(matrix(0, 4, 4))))

})

test_that("signed modularity of a network without negative edges is the ordinary modularity of the partition", {

  # Set up network
  net <- matrix(0, 4, 4)

  # Add first edge
  net[1, 2] <- 2

  # Add second edge
  net[3, 4] <- 1

  # Make network symmetric
  net <- net + t(net)

  # Convert to {igraph}
  graph <- igraph::graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE)

  # Check modularity
  expect_equal(
    signed_modularity(net, c(1, 1, 2, 2)),
    igraph::modularity(graph, c(1, 1, 2, 2), weights = igraph::E(graph)$weight)
  )

})

test_that("signed modularity of a mixed-sign network matches Gomez et al.'s (2009) formula", {

  # Set up network
  net <- matrix(0, 4, 4)

  # Add first positive edge
  net[1, 2] <- 2

  # Add second positive edge
  net[3, 4] <- 1

  # Add negative edge
  net[1, 3] <- -1

  # Make network symmetric
  net <- net + t(net)

  # Check modularity
  expect_equal(signed_modularity(net, c(1, 1, 2, 2)), 11 / 24)

  # Convert weights to {igraph}
  graph_of <- function(weights) igraph::graph_from_adjacency_matrix(weights, mode = "undirected", weighted = TRUE)

  # Get positive edges
  positive <- ifelse(net > 0, net, 0)

  # Get absolute negative edges
  negative <- ifelse(net < 0, -net, 0)

  # Compute modularity of weights
  q_of <- function(weights){

    # Convert to {igraph}
    graph <- graph_of(weights)

    # Return modularity
    igraph::modularity(graph, c(1, 1, 2, 2), weights = igraph::E(graph)$weight)

  }

  # Check against formula
  expect_equal(
    signed_modularity(net, c(1, 1, 2, 2)),
    sum(positive) / (sum(positive) + sum(negative)) * q_of(positive) -
      sum(negative) / (sum(positive) + sum(negative)) * q_of(negative)
  )

  # Check network with only negative edges
  expect_equal(signed_modularity(-negative, c(1, 1, 2, 2)), -q_of(negative))

})

test_that("q ignores the diagonal (e.g. TMFG's diag = 1 self-loop convention)", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Copy network
  net_no_diag <- net

  # Remove self-loops
  diag(net_no_diag) <- 0

  # Check modularity
  expect_equal(q(net, seed = 42), q(net_no_diag, seed = 42))

})

test_that("q's signed = TRUE option scores the same partition differently", {

  # Set seed
  set.seed(1)

  # Set up correlations
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))

  # Estimate network
  net <- suppressWarnings(tmfg(cordata))

  # Compute unsigned modularity
  unsigned <- q(net, seed = 42, signed = FALSE)

  # Compute signed modularity
  signed <- q(net, seed = 42, signed = TRUE)

  # Check for numeric
  expect_true(is.numeric(signed))

  # Check that they differ
  expect_false(isTRUE(all.equal(unsigned, signed)))

})
