# Tests for random_walk()

# Ring network ----
# Updated 13.10.2026
ring_network <- function(n)
{

  # Initialize network
  A <- matrix(0, n, n)

  # Loop over nodes
  for(i in seq_len(n)){

    # Get next node
    j <- if(i == n) 1 else i + 1

    # Connect nodes
    A[i, j] <- A[j, i] <- 1

  }

  # Return network
  A

}

# Complete network ----
# Updated 13.10.2026
complete_network <- function(n)
{

  # Initialize network
  A <- matrix(1, n, n)

  # Remove self-loops
  diag(A) <- 0

  # Return network
  A

}

test_that("random_walk's row-normalized transition matrices match a hand-looped construction", {

  # Set up network
  A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)

  # Get degrees
  degree <- colSums(A)

  # Initialize transitions
  hand_looped <- matrix(0, 3, 3)

  # Compute transitions by hand
  for(i in 1:3) for(j in 1:3) hand_looped[i, j] <- A[i, j] / degree[i]

  # Check transitions
  expect_equal(unname(A / degree), hand_looped)

})

test_that("random_walk's steps = 1 doesn't corrupt the starting node", {

  # Set seed
  set.seed(1)

  # Set up first correlations
  cordata1 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))

  # Set up second correlations
  cordata2 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))

  # Estimate first network
  net1 <- suppressWarnings(tmfg(cordata1))

  # Estimate second network
  net2 <- suppressWarnings(tmfg(cordata2))

  # Perform random walk
  result <- suppressMessages(random_walk(net1, net2, reps = 1, steps = 1, iter = 20, cores = 1))

  # Check similarity of first network
  expect_equal(unname(result$long[1, "M.sim.A"]), 1)

  # Check similarity of second network
  expect_equal(unname(result$long[1, "M.sim.B"]), 1)

})

test_that("a network's 5-unique-node statistic does not depend on the other network's walk", {

  # Set seed
  set.seed(1)

  # Perform walk against ring
  with_ring <- suppressMessages(random_walk(ring_network(30), ring_network(30), reps = 6, steps = 60, iter = 1500))

  # Set seed
  set.seed(1)

  # Perform walk against path
  with_path <- suppressMessages(random_walk(ring_network(30), complete_network(3), reps = 6, steps = 60, iter = 1500))

  # Check that statistic is similar
  expect_lt(abs(with_ring$long[1, "M.sim5.A"] - with_path$long[1, "M.sim5.A"]), .03)

  # Check for no constant index column
  expect_false("g5ind" %in% colnames(with_ring$long))

})

test_that("a statistic without variation gives an infinite d for a real difference", {

  # Perform walk
  result <- suppressMessages(random_walk(complete_network(2), complete_network(3), reps = 5, steps = 50, iter = 50))

  # Check difference
  expect_equal(unname(result$long[1, "diff.uniq"]), -1)

  # Check effect
  expect_equal(unname(result$long[1, "d.uniq"]), -Inf)

  # Check direction
  expect_equal(result$short[["Direction (Unique Nodes)"]][1], "B > A")

  # Perform walk of equal networks
  same <- suppressMessages(random_walk(complete_network(3), complete_network(3), reps = 5, steps = 50, iter = 50))

  # Check zero effect
  expect_equal(unname(same$long[1, "d.uniq"]), 0)

  # Check negligible direction
  expect_equal(same$short[["Direction (Unique Nodes)"]][1], "negligible")

})

test_that("random_walk copes with isolated nodes and a single walk", {

  # Set up network
  with_isolate <- ring_network(5)

  # Add isolated node
  with_isolate <- rbind(cbind(with_isolate, 0), 0)

  # Perform walk
  result <- suppressMessages(random_walk(with_isolate, ring_network(5), reps = 2, steps = 5, iter = 40))

  # Check for no missing values
  expect_false(anyNA(result$long))

  # Perform single walk
  single <- suppressMessages(random_walk(ring_network(5), ring_network(6), reps = 1, steps = 5, iter = 1))

  # Check for undefined direction
  expect_true(all(single$short[["Direction (Unique Nodes)"]] == "undefined"))

  # Check for error without edges
  expect_error(random_walk(matrix(0, 4, 4), ring_network(5)), "at least one edge")

})

test_that("random_walk's cumulative transitions end at exactly one and results have plain row names", {

  # Set up a weighted-looking network whose row sums round below one
  A <- matrix(0, 7, 7)
  A[1, 2:7] <- 1
  A[2:7, 1] <- 1

  # Get transitions
  cumulative <- cumulative_transitions(A / rowSums(A), A)

  # Check that every row with neighbors reaches exactly one at its last neighbor
  expect_true(all(cumulative[, 7] == 1))
  expect_true(all(cumulative[2:7, 1] == 1))

  # Check that no probability is placed past the last neighbor before it
  expect_equal(cumulative[2, 1], 1)

  # Set up ring network
  ring <- function(n){
    net <- matrix(0, n, n)
    net[cbind(1:n, c(2:n, 1))] <- 1
    net + t(net)
  }

  # Check one level of steps
  one_level <- suppressMessages(random_walk(ring(8), ring(10), reps = 1, steps = 5, iter = 10))
  expect_identical(rownames(one_level$short), "1")

})
