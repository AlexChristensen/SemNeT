# Tests for aspl()

test_that("aspl matches a hand-computed value on a small connected ring network", {

  # 4-node ring: every node is 1 step from 2 neighbors and 2 steps from the opposite node
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[2, 3] <- A[3, 2] <- 1
  A[3, 4] <- A[4, 3] <- 1
  A[4, 1] <- A[1, 4] <- 1

  # Each node's distances to the other 3: 1, 1, 2 -> mean = 4/3
  expect_equal(as.numeric(aspl(A)), 4 / 3)

})

test_that("aspl excludes unreachable pairs instead of masking them as adjacent", {

  # Two disconnected 4-node cliques: every node is 1 step from its 3 clique-mates
  # and has no path at all to the other clique
  A <- matrix(0, 8, 8)
  A[1:4, 1:4] <- 1
  A[5:8, 5:8] <- 1
  diag(A) <- 1

  # Every node's only reachable neighbors are all at distance 1
  expect_equal(as.numeric(aspl(A)), 1)

})

test_that("aspl attaches per-node aspl and eccentricity without changing its numeric value", {

  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[2, 3] <- A[3, 2] <- 1
  A[3, 4] <- A[4, 3] <- 1
  A[4, 1] <- A[1, 4] <- 1

  result <- aspl(A)
  expect_equal(as.numeric(result), 4 / 3)
  expect_length(attr(result, "node"), 4)
  expect_length(attr(result, "eccentricity"), 4)

})

test_that("aspl's weighted distances match an independent (igraph) reference", {

  # `distance(A, weighted = TRUE)` is a from-scratch Dijkstra implementation
  # with no prior correctness test (only a type-validation one existed) --
  # verified here against igraph::distances() on a random weighted network,
  # using SemNeT's own distance = 1/weight convention (Rubinov & Sporns, 2010)
  skip_if_not_installed("igraph")

  set.seed(42)
  n <- 8
  A <- matrix(0, n, n)
  A[upper.tri(A)] <- sample(c(0, 0, 0.2, 0.5, 0.8, 1), n * (n - 1) / 2, replace = TRUE)
  A <- A + t(A)
  diag(A) <- 0
  for(i in 1:(n - 1)){
    if(A[i, i + 1] == 0){
      A[i, i + 1] <- A[i + 1, i] <- 0.5
    }
  }

  D_semnet <- distance(A, weighted = TRUE)

  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
  D_igraph <- igraph::distances(g, weights = 1 / igraph::E(g)$weight)

  expect_equal(unname(D_semnet), unname(D_igraph), tolerance = 1e-8)

})

test_that("aspl validates its input", {

  expect_error(aspl(matrix(1, 3, 4)), "square")
  expect_error(aspl(matrix(1, 3, 3), weighted = "yes"), "aspl")

})
