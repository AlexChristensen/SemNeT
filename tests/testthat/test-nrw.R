# Tests for nrw()

test_that("nrw's num vs. prop thresholding produce consistent, monotonic networks", {

  # Get data
  data <- open.clean[which(open.group == "Low"), ]

  # Estimate loose network
  loose <- nrw(data, type = "prop", threshold = 0.01)

  # Estimate strict network
  strict <- nrw(data, type = "prop", threshold = 0.1)

  # Check for subset of vocabulary
  expect_true(all(rownames(strict) %in% rownames(loose)))

})

test_that("nrw accepts an ordered numeric response matrix (the fixed >= 1 vs. == 1 bug)", {

  # Get data
  data <- open.clean[which(open.group == "Low"), ]

  # Get response order
  ordered_numeric <- responses2binary(data)$order

  # Check for no error
  expect_no_error(nrw(ordered_numeric))

})

test_that("nrw counts each pair once per participant (not once from each end)", {

  # Set up responses
  responses <- rbind(c("a", "b", "c"), c("a", "b", "d"))

  # Check number threshold
  expect_equal(sort(rownames(nrw(responses, threshold = 2))), c("a", "b"))

  # Check number threshold above counts
  expect_equal(dim(nrw(responses, threshold = 3)), c(0, 0))

  # Check proportion threshold of everyone
  expect_equal(sort(rownames(nrw(responses, type = "prop", threshold = 1))), c("a", "b"))

  # Check proportion threshold of half
  expect_equal(sort(rownames(nrw(responses, type = "prop", threshold = .5))), c("a", "b", "c", "d"))

  # Set up repeated responses
  repeated <- rbind(c("a", "b", "a", "b", "b"), c("c", "d", "e", "f", "g"))

  # Check repeated pairs are counted once
  expect_equal(dim(nrw(repeated, threshold = 2)), c(0, 0))

  # Check self-pairs are not counted
  expect_equal(unname(diag(nrw(repeated))), rep(0, 7))

})

test_that("nrw reproduces Lerner et al.'s (2009) Figure 1 example", {

  # Set up responses (5 nodes, 6 edges, CC = 0.6, ASPL = 1.4)
  responses <- rbind(
    c("cat", "dog", "elephant", NA),
    c("cat", "dog", "horse", "elephant"),
    c("dog", "cat", "elephant", "moose")
  )

  # Estimate network
  net <- nrw(responses)

  # Check number of nodes
  expect_equal(nrow(net), 5)

  # Check number of edges
  expect_equal(sum(net) / 2, 6)

  # Check average degree
  expect_equal(unname(sum(net) / nrow(net)), 2.4)

  # Check clustering coefficient (Lerner et al. report the average of the nodes' coefficients)
  expect_equal(as.numeric(cc(net)), 0.6)

  # Check global clustering coefficient (3 x triangles / connected triples)
  expect_equal(as.numeric(cc(net, type = "global")), 6 / 11)

  # Check average shortest path length
  expect_equal(as.numeric(aspl(net)), 1.4)

  # Check average distance of horse
  expect_equal(unname(attr(aspl(net), "node")["horse"]), 1.5)

  # Check average distance of moose
  expect_equal(unname(attr(aspl(net), "node")["moose"]), 1.75)

  # Check average distance of elephant
  expect_equal(unname(attr(aspl(net), "node")["elephant"]), 1)

})

test_that("nrw keeps a one-node network a matrix when a threshold drops everything else", {

  # Set up responses
  responses <- rbind(c("a", "b"), c("a", "b"), c("c", NA))

  # Estimate network
  net <- nrw(responses, threshold = 2)

  # Check for matrix
  expect_true(is.matrix(net))

  # Check dimensions
  expect_equal(dim(net), c(2, 2))

})
