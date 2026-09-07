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

test_that("aspl validates its input", {

  expect_error(aspl(matrix(1, 3, 4)), "square")
  expect_error(aspl(matrix(1, 3, 3), weighted = "yes"), "aspl")

})
