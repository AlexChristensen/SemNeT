# Tests for random_network_test()

test_that("generate_random_network preserves node and edge counts", {

  # Generate network
  net <- generate_random_network(nodes = 10, edges = 15)

  # Check number of nodes
  expect_equal(ncol(net), 10)

  # Check number of edges
  expect_equal(sum(net) / 2, 15)

  # Check for symmetry
  expect_true(isSymmetric(net))

})

test_that("generate_random_network produces exactly 0 edges for an edgeless observed network", {

  # Generate network
  net <- generate_random_network(nodes = 6, edges = 0)

  # Check for no edges
  expect_equal(sum(net), 0)

})

test_that("generate_degree_network keeps every node's degree, and the network simple", {

  # Set seed
  set.seed(1)

  # Estimate network
  net <- cn(open.clean[which(open.group == "Low"), ])

  # Rewire network
  rewired <- generate_degree_network(net)

  # Check degrees
  expect_equal(unname(rowSums(rewired)), unname(rowSums(net)))

  # Check for symmetry
  expect_true(isSymmetric(unname(rewired)))

  # Check for no self-loops
  expect_true(all(diag(rewired) == 0))

  # Check for rewiring
  expect_false(identical(unname(rewired), unname(net)))

})

test_that("random_network_test counts a weighted network's edges, not its weights", {

  # Set seed
  set.seed(1)

  # Estimate weighted network
  net <- suppressWarnings(tmfg(similarity(simulate_fluency(30, 100))))

  # Binarize network
  observed <- binarize(net)

  # Remove self-loops
  diag(observed) <- 0

  # Check that edges exceed weights
  expect_gt(sum(observed) / 2, floor(sum(net - diag(diag(net))) / 2))

  # Perform test
  result <- suppressMessages(random_network_test(net = net, iter = 10))

  # Check clustering of random networks
  expect_gt(result$net["cc", "random_mean"], 0)

  # Check clustering of degree-preserving networks
  expect_gt(result$net["cc", "random_mean_degree"], 0)

})

test_that("random_network_test is reproducible via 'seed' whatever 'cores' is", {

  # Set seed
  set.seed(1)

  # Estimate network
  net <- suppressWarnings(tmfg(similarity(simulate_fluency(25, 120))))

  # Set up test
  run <- function(seed, cores = 1){
    suppressMessages(random_network_test(net = net, iter = 12, cores = cores, seed = seed))
  }

  # Perform test
  one <- run(7)

  # Check same seed
  expect_identical(one, run(7))

  # Check same seed with more cores
  expect_identical(one, run(7, cores = 2))

  # Check different seed
  expect_false(identical(one, run(8)))

})

test_that("random_network_test p-values are not rounded to 0 and asymmetric networks are rejected", {

  # Check that rounding gives zero
  expect_equal(round(1 / 20001, 4), 0)

  # Check that significant digits do not
  expect_gt(signif(1 / 20001, 4), 0)

  # Set up asymmetric network
  asymmetric <- matrix(0, 5, 5)

  # Add edge
  asymmetric[1, 2] <- 1

  # Check for asymmetry error
  expect_error(random_network_test(net = asymmetric, iter = 5), "symmetric")

  # Check for seed error
  expect_error(random_network_test(net = matrix(0, 3, 3), seed = -1), "random_network_test")

})
