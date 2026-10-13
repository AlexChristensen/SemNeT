# Tests for the generic helpers in R/helpers.R

test_that("distance matches a hand-computed shortest-path matrix on a ring network", {

  # Set up network
  A <- matrix(0, 4, 4)

  # Add first edge
  A[1, 2] <- A[2, 1] <- 1

  # Add second edge
  A[2, 3] <- A[3, 2] <- 1

  # Add third edge
  A[3, 4] <- A[4, 3] <- 1

  # Add fourth edge
  A[4, 1] <- A[1, 4] <- 1

  # Compute distances
  D <- distance(A)

  # Check diagonal
  expect_equal(unname(diag(D)), c(0, 0, 0, 0))

  # Check neighbors
  expect_equal(unname(D[1, 2]), 1)

  # Check opposite nodes
  expect_equal(unname(D[1, 3]), 2)

})

test_that("distance leaves unreachable pairs as Inf instead of masking them as 0", {

  # Set up network
  A <- matrix(0, 4, 4)

  # Add first pair
  A[1, 2] <- A[2, 1] <- 1

  # Add second pair
  A[3, 4] <- A[4, 3] <- 1

  # Compute distances
  D <- distance(A)

  # Check first unreachable pair
  expect_true(is.infinite(D[1, 3]))

  # Check second unreachable pair
  expect_true(is.infinite(D[2, 4]))

  # Check reachable pair
  expect_equal(unname(D[1, 2]), 1)

})

test_that("bad_response replaces empty/placeholder responses with NA", {

  # Check placeholders
  expect_equal(bad_response(c("dog", "", " ", "NA", NA, "cat")), c("dog", NA, NA, NA, NA, "cat"))

  # Check no responses
  expect_equal(bad_response(character(0)), NA)

})

test_that("responses2binary and binary2responses round-trip a response matrix", {

  # Set up responses
  raw <- matrix(c("dog", "cat", "cat", "", "dog", "bird"), nrow = 2, ncol = 3, byrow = TRUE)

  # Convert to binary
  converted <- responses2binary(raw)

  # Check class
  expect_s3_class(converted, "responses2binary")

  # Check vocabulary
  expect_equal(sort(colnames(converted$binary)), c("bird", "cat", "dog"))

  # Check for binary values
  expect_true(all(converted$binary %in% c(0, 1)))

  # Convert back to responses
  back <- binary2responses(converted)

  # Get responses of first participant
  said_row_1 <- na.omit(back[1, ])

  # Check responses
  expect_true(all(c("dog", "cat") %in% said_row_1))

})

test_that("binary2responses handles a fully-empty (all-zero) response matrix", {

  # Set up empty responses
  empty <- matrix(0, nrow = 3, ncol = 5, dimnames = list(paste0("p", 1:3), paste0("w", 1:5)))

  # Convert to responses
  result <- binary2responses(empty)

  # Check columns
  expect_equal(ncol(result), 0)

  # Check rows
  expect_equal(nrow(result), 3)

  # Check row names
  expect_equal(row.names(result), paste0("p", 1:3))

})

test_that("cosine matches hand-computed similarity", {

  # Set up first vector
  x <- c(1, 0, 1, 0)

  # Set up second vector
  y <- c(1, 1, 0, 0)

  # Check cosine of vectors
  expect_equal(cosine(x, y), 0.5)

  # Set up matrix
  m <- matrix(c(1, 0, 1, 0, 1, 1, 0, 0), 4, 2)

  # Compute cosine of columns
  similarity_matrix <- cosine(m)

  # Check diagonal
  expect_equal(unname(diag(similarity_matrix)), c(1, 1))

  # Check cosine of columns
  expect_equal(unname(similarity_matrix[1, 2]), 0.5)

})

test_that("cohens_d matches a hand-computed effect size", {

  # Set up first sample
  sample_1 <- c(1, 2, 3, 4, 5)

  # Set up second sample
  sample_2 <- c(3, 4, 5, 6, 7)

  # Check effect size
  expect_equal(cohens_d(sample_1, sample_2), abs(mean(sample_1) - mean(sample_2)) / sd(sample_1))

  # Check that order does not matter
  expect_equal(cohens_d(sample_1, sample_2), cohens_d(sample_2, sample_1))

})

test_that("reproducible seeds and shuffles depend only on the seed", {

  # Check same seed
  expect_identical(reproducible_seeds(5, 1), reproducible_seeds(5, 1))

  # Check different seed
  expect_false(identical(reproducible_seeds(5, 1), reproducible_seeds(5, 2)))

  # Check unique seeds
  expect_length(unique(reproducible_seeds(100, 1)), 100)

  # Check shuffle with same seed
  expect_identical(shuffle(1:20, size = 5, seed = 3), shuffle(1:20, size = 5, seed = 3))

  # Check shuffle elements
  expect_setequal(shuffle(1:20, seed = 3), 1:20)

  # Check shuffle size
  expect_length(shuffle(1:20, size = 5, seed = 3), 5)

  # Resample with replacement
  drawn <- shuffle_replace(1:20, seed = 3)

  # Check resample with same seed
  expect_identical(drawn, shuffle_replace(1:20, seed = 3))

  # Check resample size
  expect_length(drawn, 20)

  # Check resample elements
  expect_true(all(drawn %in% 1:20))

})

test_that("seed = 0 asks for random results, like NULL, while any other seed is reproducible", {

  # Check no seed
  expect_true(random_seed(NULL))

  # Check zero seed
  expect_true(random_seed(0))

  # Check nonzero seed
  expect_false(random_seed(1))

  # Check fractional seed
  expect_false(random_seed(.5))

  # Check seed for compiled code
  expect_equal(compiled_seed(7), 7)

  # Check no seed for compiled code
  expect_equal(compiled_seed(NULL), 0)

  # Check zero seed for compiled code
  expect_equal(compiled_seed(0), 0)

  # Check same seed
  expect_identical(reproducible_seeds(5, 1), reproducible_seeds(5, 1))

  # Check different seed
  expect_false(identical(reproducible_seeds(5, 1), reproducible_seeds(5, 2)))

  # Check zero seed gives fresh seeds
  expect_false(identical(reproducible_seeds(5, 0), reproducible_seeds(5, 0)))

  # Check no seed gives fresh seeds
  expect_false(identical(reproducible_seeds(5, NULL), reproducible_seeds(5, NULL)))

  # Check zero seed gives fresh resamples
  expect_false(identical(shuffle_replace(1:50, seed = 0), shuffle_replace(1:50, seed = 0)))

  # Set seed
  set.seed(1)

  # Estimate network
  net <- suppressWarnings(tmfg(cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))))

  # Check modularity with same seed
  expect_identical(q(net, seed = 3), q(net, seed = 3))

  # Check message for random seed
  expect_message(
    bootstrap_SemNeT(low = open.clean[1:30, ], iter = 2, method = "nrw", seed = 0), "not be reproducible"
  )

})

test_that("cohens_d pools the standard deviations by degrees of freedom", {

  # Set seed
  set.seed(1)

  # Sample first group
  sample_1 <- rnorm(10, 0, 1)

  # Sample second group
  sample_2 <- rnorm(200, .5, 3)

  # Compute pooled standard deviation
  pooled_sd <- sqrt((9 * var(sample_1) + 199 * var(sample_2)) / 208)

  # Check effect size
  expect_equal(cohens_d(sample_1, sample_2), abs(mean(sample_1) - mean(sample_2)) / pooled_sd)

  # Compute t statistic
  t_statistic <- unname(t.test(sample_1, sample_2, var.equal = TRUE)$statistic)

  # Check against t statistic
  expect_equal(cohens_d(sample_1, sample_2), abs(t_statistic) * sqrt(1 / 10 + 1 / 200))

})

test_that("binary2responses reads a plain ordered numeric matrix by response rank", {

  # Set up ordered responses
  ordered <- matrix(c(2, 1, 0, 0, 3, 1), nrow = 2, byrow = TRUE)

  # Name responses
  colnames(ordered) <- c("a", "b", "c")

  # Convert to responses
  back <- binary2responses(ordered)

  # Check first participant
  expect_equal(unname(back[1, 1:2]), c("b", "a"))

  # Check second participant
  expect_equal(unname(back[2, 1:2]), c("c", "b"))

  # Set up binary responses
  binary <- matrix(c(1, 1, 0, 0, 1, 1), nrow = 2, byrow = TRUE)

  # Name responses
  colnames(binary) <- c("a", "b", "c")

  # Check column order
  expect_equal(unname(binary2responses(binary)[1, 1:2]), c("a", "b"))

})

test_that("cohens_d handles groups of one and samples without variation", {

  # Check group of one
  expect_equal(cohens_d(5, c(1, 2, 3)), abs(5 - 2) / 1)

  # Check no variation and no difference
  expect_equal(cohens_d(c(2, 2, 2), c(2, 2)), 0)

  # Check no variation and a difference
  expect_equal(cohens_d(c(2, 2, 2), c(3, 3)), Inf)

  # Check nothing to pool
  expect_true(is.na(cohens_d(1, 2)))

  # Check empty sample
  expect_true(is.na(cohens_d(numeric(0), c(1, 2))))

  # Set up first sample
  x <- c(1, 4, 2, 8, 5)

  # Set up second sample
  y <- c(3, 9, 7)

  # Check against t statistic
  expect_equal(cohens_d(x, y), abs(unname(t.test(x, y, var.equal = TRUE)$statistic)) * sqrt(1 / 5 + 1 / 3))

})
