# Tests for semantic_network_measures()

test_that("semantic_network_measures passes na.rm on to cc", {

  # Set up network
  A <- matrix(0, 5, 5)

  # Add triangle
  A[1, 2] <- A[2, 1] <- A[2, 3] <- A[3, 2] <- A[1, 3] <- A[3, 1] <- 1

  # Add tail
  A[3, 4] <- A[4, 3] <- A[4, 5] <- A[5, 4] <- 1

  # Check default
  expect_equal(unname(semantic_network_measures(A, measures = "cc")), as.numeric(cc(A)))

  # Check removal of missing
  expect_equal(unname(semantic_network_measures(A, measures = "cc", na.rm = TRUE)), as.numeric(cc(A, na.rm = TRUE)))

  # Check that removal raises the average of the local coefficients
  expect_gt(
    semantic_network_measures(A, measures = "cc", na.rm = TRUE),
    semantic_network_measures(A, measures = "cc")
  )

  # Check that removal does not change the global clustering coefficient
  expect_equal(
    semantic_network_measures(A, measures = "cc", na.rm = TRUE, cc_type = "global"),
    semantic_network_measures(A, measures = "cc", cc_type = "global")
  )

  # Check that ASPL is unaffected
  expect_equal(
    semantic_network_measures(A, measures = "aspl", na.rm = TRUE),
    semantic_network_measures(A, measures = "aspl")
  )

  # Check for error
  expect_error(semantic_network_measures(A, na.rm = "yes"), "semantic_network_measures")

})

test_that("semantic_network_measures passes 'signed' through to q (weighted networks only)", {

  # Set up network
  net <- matrix(0, 6, 6)

  # Add communities
  net[1, 2] <- net[2, 3] <- net[1, 3] <- net[4, 5] <- net[5, 6] <- net[4, 6] <- 1

  # Add negative edge between communities
  net[3, 4] <- -1

  # Make network symmetric
  net <- net + t(net)

  # Compute unsigned modularity
  unsigned <- semantic_network_measures(net, measures = "q", weighted = TRUE, seed = 1)

  # Compute signed modularity
  signed <- semantic_network_measures(net, measures = "q", weighted = TRUE, seed = 1, signed = TRUE)

  # Check signed modularity
  expect_equal(unname(signed), q(net, seed = 1, signed = TRUE))

  # Check unsigned modularity
  expect_equal(unname(unsigned), q(net, seed = 1))

  # Check that they differ
  expect_false(isTRUE(all.equal(unname(unsigned), unname(signed))))

  # Check for warning without weights
  expect_warning(
    semantic_network_measures(net, measures = "q", seed = 1, signed = TRUE), "no effect"
  )

  # Check for no warning without modularity
  expect_no_warning(semantic_network_measures(net, measures = c("aspl", "cc"), signed = TRUE))

  # Check that clustering is unaffected
  expect_equal(
    semantic_network_measures(net, measures = "cc", weighted = TRUE, signed = TRUE),
    semantic_network_measures(net, measures = "cc", weighted = TRUE)
  )

  # Check for error
  expect_error(semantic_network_measures(net, signed = "yes"), "signed|logical")

})

test_that("semantic_network_measures passes 'type' through to aspl", {

  # Set up network
  net <- matrix(0, 6, 6)

  # Add two disconnected triangles
  net[1, 2] <- net[2, 3] <- net[1, 3] <- net[4, 5] <- net[5, 6] <- net[4, 6] <- 1

  # Make network symmetric
  net <- net + t(net)

  # Compute arithmetic ASPL
  arithmetic <- semantic_network_measures(net, measures = "aspl")

  # Compute harmonic ASPL
  harmonic <- semantic_network_measures(net, measures = "aspl", type = "harmonic")

  # Check arithmetic ASPL
  expect_equal(unname(arithmetic), as.numeric(aspl(net)))

  # Check value of arithmetic ASPL
  expect_equal(unname(arithmetic), 1)

  # Check harmonic ASPL
  expect_equal(unname(harmonic), as.numeric(aspl(net, type = "harmonic")))

  # Check ordering
  expect_gt(harmonic, arithmetic)

  # Check explicit arithmetic
  expect_equal(semantic_network_measures(net, measures = "aspl", type = "arithmetic"), arithmetic)

  # Check that clustering is unaffected
  expect_equal(
    semantic_network_measures(net, measures = "cc", type = "harmonic"),
    semantic_network_measures(net, measures = "cc")
  )

  # Check for unknown type
  expect_error(semantic_network_measures(net, type = "geometric"), "arg")

  # Check for non-character type
  expect_error(semantic_network_measures(net, type = 1), "character|type")

})

test_that("semantic_network_measures passes 'cc_type' through to cc", {

  # Set up network
  A <- matrix(0, 5, 5)

  # Add triangle
  A[1, 2] <- A[2, 1] <- A[2, 3] <- A[3, 2] <- A[1, 3] <- A[3, 1] <- 1

  # Add tail
  A[3, 4] <- A[4, 3] <- A[4, 5] <- A[5, 4] <- 1

  # Check average clustering
  expect_equal(unname(semantic_network_measures(A, measures = "cc")), as.numeric(cc(A)))

  # Check explicit average clustering
  expect_equal(semantic_network_measures(A, measures = "cc", cc_type = "average"), semantic_network_measures(A, measures = "cc"))

  # Check global clustering
  expect_equal(unname(semantic_network_measures(A, measures = "cc", cc_type = "global")), as.numeric(cc(A, type = "global")))

  # Check that other measures are unaffected
  expect_equal(
    semantic_network_measures(A, measures = "aspl", cc_type = "global"),
    semantic_network_measures(A, measures = "aspl")
  )

  # Check for unknown type
  expect_error(semantic_network_measures(A, cc_type = "local"), "arg")

  # Check for non-character type
  expect_error(semantic_network_measures(A, cc_type = 1), "character|cc_type")

})
