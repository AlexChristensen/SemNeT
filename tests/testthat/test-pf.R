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

  # Check network of response order
  expect_equal(pf(ordered, method = "paulsen"), pf(paulsen_example(), method = "paulsen"))

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

  # Loop over methods
  for(method in c("chebyshev", "paulsen")){

    # Check against response order
    expect_equal(pf(raw, method = method), pf(responses2binary(raw)$order, method = method))

    # Check against missing responses
    expect_equal(pf(raw, method = method), pf(paulsen_example(), method = method))

  }

})

test_that("pf links words that never co-occur only through other words", {

  # Estimate network
  net <- pf(paulsen_example(), method = "paulsen")

  # Check for no direct link
  expect_equal(net["dog", "zebra"], 0)

  # Check that every node is linked
  expect_true(all(rowSums(net) > 0))

})

test_that("pf defaults to the Chebyshev distance of co-occurrence profiles", {

  # Get binary responses
  binary <- responses2binary(paulsen_example())$binary

  # Get co-occurrences
  both <- crossprod(binary)

  # Remove self co-occurrences
  diag(both) <- 0

  # Get expected distances
  expected <- as.matrix(dist(both, method = "maximum"))

  # Check distances
  expect_equal(unname(cooccurrence_distance(binary > 0, "chebyshev")), unname(expected))

  # Check default
  expect_equal(pf(paulsen_example()), pf(paulsen_example(), method = "chebyshev"))

  # Check weights of links
  net <- pf(paulsen_example())

  # Check distances of links
  expect_equal(unname(attr(net, "weighted")[net == 1]), unname(expected[net == 1]))

})

test_that("pf accepts a binary matrix for every method but 'paulsen'", {

  # Get binary responses
  binary <- responses2binary(paulsen_example())$binary

  # Loop over methods
  for(method in c("chebyshev", "euclidean", "manhattan", "cosine", "hamming", "jaccard")){

    # Check against raw responses
    expect_equal(pf(binary, method = method), pf(paulsen_example(), method = method))

  }

  # Check that response order is needed
  expect_error(pf(binary, method = "paulsen"), "strictly binary")

})

test_that("every method gives a symmetric binary network with distances as weights", {

  # Get example data
  data <- open.clean[1:60, ]

  # Loop over methods
  for(method in c("chebyshev", "euclidean", "manhattan", "cosine", "hamming", "jaccard", "paulsen")){

    # Estimate network
    net <- pf(data, method = method)

    # Check network
    expect_true(isSymmetric(unname(net)))
    expect_true(all(net %in% c(0, 1)))
    expect_true(all(diag(net) == 0))
    expect_true(sum(net) > 0)

    # Get weights
    weighted <- attr(net, "weighted")

    # Check weights
    expect_true(all(weighted[net == 0] == 0))
    expect_true(all(weighted >= 0))
    expect_true(all(is.finite(weighted)))

  }

})

test_that("co-occurrence distances are computed as documented", {

  # Set up presence of three responses
  given <- cbind(a = c(1, 1, 0, 0), b = c(1, 1, 1, 0), c = c(0, 0, 1, 1)) > 0

  # Get Jaccard distances
  jaccard <- cooccurrence_distance(given, "jaccard")

  # Check shared participants (2 of 3)
  expect_equal(jaccard[1, 2], 1 - 2 / 3)

  # Check no shared participants
  expect_equal(jaccard[1, 3], Inf)

  # Get co-occurrences
  both <- crossprod(1 * given)

  # Remove self co-occurrences
  diag(both) <- 0

  # Check Manhattan
  expect_equal(cooccurrence_distance(given, "manhattan")[1, 2], sum(abs(both[1, ] - both[2, ])))

  # Check Euclidean
  expect_equal(cooccurrence_distance(given, "euclidean")[1, 3], sqrt(sum((both[1, ] - both[3, ])^2)))

  # Check Hamming
  expect_equal(cooccurrence_distance(given, "hamming")[1, 2], sum(both[1, ] != both[2, ]))

  # Check cosine
  expect_equal(cooccurrence_distance(given, "cosine")[2, 3], 1 - cosine(both[2, ], both[3, ]))

  # Set up response given alone (empty profile)
  alone <- cbind(given, d = c(0, 0, 0, 0))
  alone[1, 4] <- 1
  alone[1, 1:3] <- 0

  # Check undefined cosine is infinite
  expect_equal(cooccurrence_distance(alone > 0, "cosine")[4, 1], Inf)

})

test_that("pf checks 'method'", {

  # Check for unknown method
  expect_error(pf(paulsen_example(), method = "unknown"))

  # Check for non-character method
  expect_error(pf(paulsen_example(), method = 1), "character")

})

test_that("pf checks 'q' and 'r'", {

  # Check for q below two
  expect_error(pf(paulsen_example(), q = 1), "whole number")

  # Check for fractional q
  expect_error(pf(paulsen_example(), q = 2.5), "whole number")

  # Check for non-numeric q
  expect_error(pf(paulsen_example(), q = "a"), "numeric")

  # Check for r below one
  expect_error(pf(paulsen_example(), r = 0.5), "at least 1")

  # Check for vector r
  expect_error(pf(paulsen_example(), r = c(1, 2)))

  # Check that Inf and NULL are n - 1
  expect_equal(pf(paulsen_example(), q = Inf), pf(paulsen_example()))
  expect_equal(pf(paulsen_example(), q = 1000), pf(paulsen_example(), q = NULL))

})

test_that("the default q and r are computed with MST-Pathfinder", {

  # Set seed
  set.seed(5)

  # Loop over sizes
  for(n in c(5, 9)){

    # Set up random distances with ties and infinite values
    D <- matrix(sample(c(0, 1:4, Inf), n^2, replace = TRUE, prob = c(.1, rep(.2, 4), .1)), n, n)

    # Make distances symmetric
    D[lower.tri(D)] <- t(D)[lower.tri(D)]

    # Set self-distances
    diag(D) <- 0

    # Check that general definition agrees
    expect_equal(general_pathfinder_links(D, n - 1, Inf), mst_pathfinder_links(D))

    # Check dispatch
    expect_equal(pathfinder_links(D, n - 1, Inf), mst_pathfinder_links(D))

  }

})

test_that("general pathfinder links follow the definition for any q and r", {

  # Set seed
  set.seed(6)

  # Set up distances
  n <- 7
  D <- matrix(runif(n^2, .5, 5), n, n)
  D <- (D + t(D)) / 2
  diag(D) <- 0

  # Loop over q
  for(q in c(2, 3, 6)){

    # Loop over r
    for(r in c(1, 2, 3, Inf)){

      # Initialize best path lengths with direct distances
      best <- D

      # Initialize paths of exactly k links
      paths <- D

      # Loop over path lengths
      for(k in seq_len(q)[-1]){

        # Initialize next paths
        following <- matrix(Inf, n, n)

        # Loop over node pairs and intermediate nodes
        for(i in 1:n) for(j in 1:n) for(m in setdiff(1:n, j)){

          # Compute path length
          length <- if(is.infinite(r)) max(paths[i, m], D[m, j]) else (paths[i, m]^r + D[m, j]^r)^(1 / r)

          # Keep shortest path
          following[i, j] <- min(following[i, j], length)

        }

        # Update paths
        paths <- following

        # Update best paths
        best <- pmin(best, paths)

      }

      # Get expected links
      expected <- ifelse(D <= best + 1e-9 & row(D) != col(D), 1, 0)

      # Check links
      expect_equal(general_pathfinder_links(D, q, r), expected)

    }

  }

})

test_that("larger q and r keep fewer links, and r = 1 keeps every link of a metric", {

  # Get example data
  data <- open.clean[1:60, ]

  # Estimate networks of increasing q
  links_q <- vapply(c(2, 3, 5, Inf), function(q) sum(pf(data, method = "euclidean", q = q)), numeric(1))

  # Check that links do not increase with q
  expect_true(all(diff(links_q) <= 0))

  # Estimate networks of increasing r
  links_r <- vapply(c(2, 3, Inf), function(r) sum(pf(data, method = "euclidean", q = 2, r = r)), numeric(1))

  # Check that links do not increase with r
  expect_true(all(diff(links_r) <= 0))

  # Loop over metrics
  for(method in c("chebyshev", "euclidean", "manhattan", "hamming", "jaccard")){

    # Estimate network
    net <- pf(data, method = method, r = 1)

    # Get finite distances
    finite <- is.finite(
      cooccurrence_distance(responses2binary(data)$binary > 0, method)
    ) & row(net) != col(net)

    # Check that every finite link is kept
    expect_equal(sum(net), sum(finite))

  }

})
