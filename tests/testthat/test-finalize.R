# Tests for finalize()

test_that("finalize keeps only responses given by enough participants", {

  data <- matrix(
    c(1, 1, 1, 1, 0, 0, 0, 0, 0), # dog: 3 cases, cat: 1 case, bird: 0 cases
    nrow = 3, ncol = 3, dimnames = list(NULL, c("dog", "cat", "bird"))
  )

  result <- finalize(data, minimum_cases = 2)

  expect_equal(colnames(result), "dog")

})

test_that("finalize matches the original on real fixture data", {

  final <- finalize(open.binary, minimum_cases = 2)

  expect_true(all(colSums(open.binary)[colnames(final)] >= 2))
  expect_true(all(colSums(open.binary)[setdiff(colnames(open.binary), colnames(final))] < 2))

})

test_that("finalize errors instead of silently returning a 0-column matrix", {

  data <- matrix(c(0, 1, 0, 1), nrow = 2, ncol = 2, dimnames = list(NULL, c("dog", "cat")))
  expect_error(finalize(data, minimum_cases = 5), "no response")

})

test_that("finalize validates its input", {

  expect_error(finalize(open.binary, minimum_cases = -1), "finalize")
  expect_error(finalize(list(1, 2)), "finalize")

})

test_that("finalize gives a clear error on raw (not-yet-binarized) word responses", {

  # Real bug fixed: raw responses passed `response_matrix_error()`'s general
  # check (which also accepts raw content, for `pf()`/`tmfg()`'s sake), but
  # `finalize()`'s own body assumes numeric 0/1 content and crashed with a
  # confusing base-R error ("'x' must be numeric or complex") instead
  raw <- matrix(c("dog", "cat", "dog", "bird"), nrow = 2)
  expect_error(finalize(raw), "binary")

})
