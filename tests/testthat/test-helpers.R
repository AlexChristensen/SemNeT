# Tests for the generic, cross-cutting helpers in R/helpers.R

test_that("get_object_type identifies shapes correctly", {

  expect_equal(get_object_type(matrix(1:4, 2, 2)), "matrix")
  expect_equal(get_object_type(data.frame(x = 1:2)), "data.frame")
  expect_equal(get_object_type(list(1, 2)), "list")
  expect_equal(get_object_type(1:5), "vector")
  expect_equal(get_object_type(factor(c("a", "b"))), "factor")
  expect_equal(get_object_type(array(1:8, dim = c(2, 2, 2))), "array")

})

test_that("object_error only errors on unexpected types", {

  expect_silent(object_error(matrix(1, 1, 1), "matrix", "test_fn"))
  expect_error(object_error(list(1), "matrix", "test_fn"), "test_fn")

})

test_that("typeof_error collapses integer/double into numeric", {

  expect_silent(typeof_error(1L, "numeric", "test_fn"))
  expect_silent(typeof_error(1.5, "numeric", "test_fn"))
  expect_error(typeof_error("a", "numeric", "test_fn"), "test_fn")

})

test_that("length_error checks against a set of allowed lengths", {

  expect_silent(length_error(1:3, 3, "test_fn"))
  expect_silent(length_error(1, c(1, 2), "test_fn"))
  expect_error(length_error(1:3, 2, "test_fn"), "test_fn")

})

test_that("range_error checks minimum and maximum bounds", {

  expect_silent(range_error(c(0, 0.5, 1), c(0, 1), "test_fn"))
  expect_error(range_error(c(-1, 0.5), c(0, 1), "test_fn"), "minimum")
  expect_error(range_error(c(0.5, 2), c(0, 1), "test_fn"), "maximum")

})

test_that("swiftelse matches ifelse for both scalar and vector conditions", {

  expect_equal(swiftelse(TRUE, "yes", "no"), "yes")
  expect_equal(swiftelse(FALSE, "yes", "no"), "no")
  expect_equal(swiftelse(c(TRUE, FALSE, TRUE), 1:3, 4:6), ifelse(c(TRUE, FALSE, TRUE), 1:3, 4:6))

})

test_that("adjacency_matrix_error validates and coerces adjacency matrices", {

  square_matrix <- matrix(1, 3, 3)
  expect_equal(adjacency_matrix_error(square_matrix, "test_fn"), square_matrix)

  # data.frame input is coerced to a matrix
  coerced <- adjacency_matrix_error(as.data.frame(square_matrix), "test_fn")
  expect_true(is.matrix(coerced))

  # non-square input errors
  expect_error(adjacency_matrix_error(matrix(1, 3, 4), "test_fn"), "square")

  # non-symmetric input only errors when symmetry is required
  asymmetric <- matrix(c(1, 0, 1, 1), 2, 2)
  expect_silent(adjacency_matrix_error(asymmetric, "test_fn"))
  expect_error(adjacency_matrix_error(asymmetric, "test_fn", require_symmetric = TRUE), "symmetric")

  # a 0x0 matrix passes the square check vacuously -- must be rejected explicitly
  expect_error(adjacency_matrix_error(matrix(numeric(0), nrow = 0, ncol = 0), "test_fn"), "at least 1 node")

})

test_that("response_matrix_error validates response matrices without guessing", {

  binary_matrix <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_equal(response_matrix_error(binary_matrix, "test_fn"), binary_matrix)

  character_matrix <- matrix(c("dog", "cat", "cat", "dog"), 2, 2)
  expect_equal(response_matrix_error(character_matrix, "test_fn"), character_matrix)

  # non-binary numeric input errors instead of being silently reinterpreted
  count_matrix <- matrix(c(0, 2, 1, 0), 2, 2)
  expect_error(response_matrix_error(count_matrix, "test_fn"), "Binarize")

  # empty input errors
  expect_error(response_matrix_error(matrix(nrow = 0, ncol = 0), "test_fn"), "row")

})

test_that("ordered_response_matrix_error accepts responses/ordered numeric and rejects binary", {

  # Character responses pass through untouched
  raw <- matrix(c("dog", "cat", "cat", "dog"), 2, 2)
  expect_equal(ordered_response_matrix_error(raw, "test_fn"), raw)

  # Ordered numeric (values above 1) is accepted -- this is the fixed
  # `>= 1` vs. `== 1` distinction that `nrw()` originally got backwards
  ordered <- matrix(c(1, 2, 0, 1), 2, 2)
  expect_equal(ordered_response_matrix_error(ordered, "test_fn"), ordered)

  # Strictly binary numeric has already discarded response order
  binary <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(ordered_response_matrix_error(binary, "test_fn"), "discarded response order")

  # empty input errors
  expect_error(ordered_response_matrix_error(matrix(nrow = 0, ncol = 0), "test_fn"), "row")

})

test_that("binarize converts any nonzero entry to 1 and preserves dimnames", {

  A <- matrix(c(0, -2, 3, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  binary <- binarize(A)

  expect_equal(unname(binary), matrix(c(0, 1, 1, 0), 2, 2))
  expect_equal(dimnames(binary), dimnames(A))

})

test_that("distance matches a hand-computed shortest-path matrix on a ring network", {

  # 4-node ring: neighbors at distance 1, opposite node at distance 2
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[2, 3] <- A[3, 2] <- 1
  A[3, 4] <- A[4, 3] <- 1
  A[4, 1] <- A[1, 4] <- 1

  D <- distance(A)

  expect_equal(unname(diag(D)), c(0, 0, 0, 0))
  expect_equal(unname(D[1, 2]), 1)
  expect_equal(unname(D[1, 3]), 2)

})

test_that("distance leaves unreachable pairs as Inf instead of masking them as 0", {

  # Two disconnected 2-node pairs
  A <- matrix(0, 4, 4)
  A[1, 2] <- A[2, 1] <- 1
  A[3, 4] <- A[4, 3] <- 1

  D <- distance(A)

  expect_true(is.infinite(D[1, 3]))
  expect_true(is.infinite(D[2, 4]))
  expect_equal(unname(D[1, 2]), 1)

})

test_that("bad_response replaces empty/placeholder responses with NA", {

  expect_equal(bad_response(c("dog", "", " ", "NA", NA, "cat")), c("dog", NA, NA, NA, NA, "cat"))
  expect_equal(bad_response(character(0)), NA)

})

test_that("responses_to_binary and binary_to_responses round-trip a response matrix", {

  raw <- matrix(c("dog", "cat", "cat", "", "dog", "bird"), nrow = 2, ncol = 3, byrow = TRUE)

  converted <- responses_to_binary(raw)

  expect_s3_class(converted, "responses_to_binary")
  expect_equal(sort(colnames(converted$binary)), c("bird", "cat", "dog"))
  expect_true(all(converted$binary %in% c(0, 1)))

  # Every word actually said appears somewhere in the round-tripped output
  back <- binary_to_responses(converted)
  said_row_1 <- na.omit(back[1, ])
  expect_true(all(c("dog", "cat") %in% said_row_1))

})

test_that("binary_to_responses handles a fully-empty (all-zero) response matrix", {

  # Real bug fixed: `1:max_responses` became `1:0 = c(1, 0)` when every
  # participant said nothing, mis-sizing `response_names` against a
  # 0-column output matrix and erroring on `colnames<-`
  empty <- matrix(0, nrow = 3, ncol = 5, dimnames = list(paste0("p", 1:3), paste0("w", 1:5)))

  result <- binary_to_responses(empty)

  expect_equal(ncol(result), 0)
  expect_equal(nrow(result), 3)
  expect_equal(row.names(result), paste0("p", 1:3))

})

test_that("cosine matches hand-computed similarity", {

  x <- c(1, 0, 1, 0)
  y <- c(1, 1, 0, 0)

  # By hand: dot = 1, ||x|| = sqrt(2), ||y|| = sqrt(2) -> cos = 1/2
  expect_equal(cosine(x, y), 0.5)

  # The matrix form's diagonal is always 1 (each column is identical to itself)
  m <- matrix(c(1, 0, 1, 0, 1, 1, 0, 0), 4, 2)
  similarity_matrix <- cosine(m)
  expect_equal(unname(diag(similarity_matrix)), c(1, 1))
  expect_equal(unname(similarity_matrix[1, 2]), 0.5)

})

test_that("cohens_d matches a hand-computed effect size", {

  # Two samples with equal spread: d = |mean diff| / sd, no pooling ambiguity
  sample_1 <- c(1, 2, 3, 4, 5)
  sample_2 <- c(3, 4, 5, 6, 7)

  expect_equal(cohens_d(sample_1, sample_2), abs(mean(sample_1) - mean(sample_2)) / sd(sample_1))

  # Order-independence: d is reported as an absolute value
  expect_equal(cohens_d(sample_1, sample_2), cohens_d(sample_2, sample_1))

})

test_that("parallel_process runs sequentially and restores the prior future plan", {

  plan_before <- future::plan()

  result <- parallel_process(5, function(i) i^2, cores = 1, progress = FALSE)
  expect_equal(unlist(result), (1:5)^2)

  # The plan in effect after the call must match what was active before it
  expect_equal(class(future::plan()), class(plan_before))

})

test_that("parallel_process validates its arguments", {

  expect_error(parallel_process(0, function(i) i), "parallel_process")
  expect_error(parallel_process(5, function(i) i, cores = 0), "parallel_process")
  expect_error(parallel_process(5, "not a function"), "parallel_process")

})
