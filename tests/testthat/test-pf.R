# Tests for pf()

# Paulsen example ----
# Updated 13.10.2026
paulsen_example <- function()
{

  # Return ordered responses of three participants
  rbind(
    c("cat", "dog", "monkey", "giraffe", NA, NA, NA),
    c("cat", "tiger", "lion", "dog", "snake", "mouse", "goat"),
    c("cat", "mouse", "giraffe", "zebra", NA, NA, NA)
  )

}

test_that("pf's distance reproduces Paulsen et al.'s (1996) worked example", {

  # Get response order
  ordered <- responses2binary(paulsen_example())$order

  # Compute distances
  D <- paulsen_distance(ordered)

  # Name distances
  rownames(D) <- colnames(D) <- colnames(ordered)

  # Check distance of cat and dog
  expect_equal(D["cat", "dog"], 3 / 2^2 * (1 / 4 + 3 / 7))

  # Check rounded distance of cat and dog
  expect_equal(round(D["cat", "dog"], 2), 0.51)

  # Check distance of cat and giraffe
  expect_equal(D["cat", "giraffe"], 3 / 2^2 * (3 / 4 + 2 / 4))

  # Check distance of cat and zebra
  expect_equal(D["cat", "zebra"], 3 / 1^2 * (3 / 4))

  # Check infinite distance of dog and zebra
  expect_equal(D["dog", "zebra"], Inf)

  # Check for symmetry
  expect_equal(D, t(D))

  # Check for zero diagonal
  expect_equal(unname(diag(D)), rep(0, nrow(D)))

})

test_that("a repeated response is positioned as responses2binary() does (its last occurrence)", {

  # Get response order
  ordered <- responses2binary(matrix(c("a", "b", "a", "c"), nrow = 1))$order

  # Compute distances
  D <- paulsen_distance(ordered)

  # Check distance of a and c
  expect_equal(D[1, 3], 1 / 4)

  # Check distance of a and b
  expect_equal(D[1, 2], 1 / 4)

  # Check distance of b and c
  expect_equal(D[2, 3], 2 / 4)

})

test_that("only responses somebody gave are nodes", {

  # Get response order
  ordered <- responses2binary(paulsen_example())$order

  # Add response nobody gave
  ordered <- cbind(ordered, never = 0)

  # Check for removal
  expect_false("never" %in% rownames(pf(ordered)))

  # Check network
  expect_equal(pf(ordered), pf(paulsen_example()))

})

test_that("the pathfinder links are the union of all minimum spanning trees", {

  # Set up triangle of ties
  triangle <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), nrow = 3)

  # Check that ties are kept
  expect_equal(sum(pathfinder_links(triangle)) / 2, 3)

  # Set up chain
  chain <- matrix(c(0, 1, 3, 1, 0, 1, 3, 1, 0), nrow = 3)

  # Check that long direct link is dropped
  expect_equal(pathfinder_links(chain)[1, 3], 0)

  # Check number of links
  expect_equal(sum(pathfinder_links(chain)) / 2, 2)

  # Set up zero distance
  zero <- matrix(c(0, 0, 2, 0, 0, 2, 2, 2, 0), nrow = 3)

  # Check that zero distance is linked
  expect_equal(pathfinder_links(zero)[1, 2], 1)

  # Set up infinite distances
  disconnected <- matrix(c(0, 1, Inf, 1, 0, Inf, Inf, Inf, 0), nrow = 3)

  # Check that infinite distances are not linked
  expect_equal(sum(pathfinder_links(disconnected)) / 2, 1)

  # Set seed
  set.seed(4)

  # Loop over sizes
  for(n in c(6, 12)){

    # Set up random distances
    D <- matrix(sample(c(1:5, Inf), n^2, replace = TRUE, prob = c(rep(.17, 5), .15)), n, n)

    # Make distances symmetric
    D[lower.tri(D)] <- t(D)[lower.tri(D)]

    # Set self-distances
    diag(D) <- 0

    # Copy distances
    minimax <- D

    # Loop over intermediate nodes
    for(k in 1:n){

      # Loop over first nodes
      for(i in 1:n){

        # Loop over second nodes
        for(j in 1:n){

          # Update minimax path distance
          minimax[i, j] <- min(minimax[i, j], max(minimax[i, k], minimax[k, j]))

        }

      }

    }

    # Get expected links
    expected <- ifelse(is.finite(D) & D <= minimax & row(D) != col(D), 1, 0)

    # Check links
    expect_equal(pathfinder_links(D), expected)

  }

})

test_that("pf gives the same network for raw responses and an ordered numeric matrix", {

  # Get raw responses
  raw <- paulsen_example()

  # Set missing responses to empty
  raw[is.na(raw)] <- ""

  # Check against response order
  expect_equal(pf(raw), pf(responses2binary(raw)$order))

  # Check against missing responses
  expect_equal(pf(raw), pf(paulsen_example()))

})

test_that("pf links words that never co-occur only through other words", {

  # Estimate network
  net <- pf(paulsen_example())

  # Check for no direct link
  expect_equal(net["dog", "zebra"], 0)

  # Check that every node is linked
  expect_true(all(rowSums(net) > 0))

})
