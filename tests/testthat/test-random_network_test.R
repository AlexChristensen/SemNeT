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

  # Set up a strongly clustered network (two cliques bridged by one edge)
  clique <- matrix(1, 10, 10)
  net <- rbind(cbind(clique, matrix(0, 10, 10)), cbind(matrix(0, 10, 10), clique))
  diag(net) <- 0
  net[10, 11] <- net[11, 10] <- 1

  # Check that the smallest p-value is positive (and not rounded to 0)
  result <- suppressMessages(random_network_test(net = net, iter = 100, seed = 1))$net
  expect_true(all(result$p_value > 0))
  expect_true(all(result$p_value_degree > 0))

  # Set up asymmetric network
  asymmetric <- matrix(0, 5, 5)

  # Add edge
  asymmetric[1, 2] <- 1

  # Check for asymmetry error
  expect_error(random_network_test(net = asymmetric, iter = 5), "symmetric")

  # Check for seed error
  expect_error(random_network_test(net = matrix(0, 3, 3), seed = -1), "random_network_test")

})

test_that("random_network_test uses separate random networks for each input and a harmonic ASPL option", {

  # Set up two identical networks
  net <- matrix(0, 12, 12)
  net[cbind(1:11, 2:12)] <- 1
  net <- net + t(net)

  # Test with a seed
  result <- suppressMessages(random_network_test(one = net, two = net, iter = 20, seed = 3))

  # Check that identical networks do not receive identical random networks
  expect_false(identical(result$one$random_mean, result$two$random_mean))

  # Check that results are reproducible
  again <- suppressMessages(random_network_test(one = net, two = net, iter = 20, seed = 3))
  expect_identical(result, again)

  # Check harmonic type
  harmonic <- suppressMessages(random_network_test(one = net, iter = 20, seed = 3, type = "harmonic"))
  expect_false(identical(harmonic$one$random_mean[1], result$one$random_mean[1]))
  expect_error(random_network_test(one = net, type = "geometric"), "arg")

})

test_that("random_network_significance leaves undefined draws out and warns", {

  # Set up draws with an undefined value
  draws <- rbind(aspl = c(1, 2, 3, NaN), cc = c(.1, .2, .3, .4), q = c(.5, .6, .7, .8))

  # Check for warning (once for each null model)
  expect_warning(expect_warning(
    result <- random_network_significance(c(aspl = 2, cc = .25, q = .6), draws, draws), "undefined"
  ), "undefined")

  # Check that mean ignores undefined draw and p-value is not at its minimum
  expect_equal(result$random_mean[1], 2)
  expect_gt(result$p_value[1], 1 / 5)

})

test_that("random_network_test passes 'cc_type' on to cc", {

  # Set seed
  set.seed(1)

  # Estimate network
  net <- suppressWarnings(tmfg(similarity(simulate_fluency(25, 120))))

  # Perform test with average clustering
  average <- suppressMessages(random_network_test(net = net, iter = 10, seed = 3))

  # Perform test with global clustering
  global <- suppressMessages(random_network_test(net = net, iter = 10, seed = 3, cc_type = "global"))

  # Check that ASPL is unchanged
  expect_equal(average$net["aspl", ], global$net["aspl", ])

  # Check that clustering differs
  expect_false(isTRUE(all.equal(average$net["cc", "random_mean"], global$net["cc", "random_mean"])))

  # Check for error with unknown type
  expect_error(random_network_test(net = net, iter = 2, cc_type = "local"), "arg")

})
