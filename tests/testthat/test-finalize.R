# Tests for finalize()

test_that("finalize keeps only responses given by enough participants", {

  # Set up data (dog: 3 cases, cat: 1 case, bird: 0 cases)
  data <- matrix(
    c(1, 1, 1, 1, 0, 0, 0, 0, 0),
    nrow = 3, ncol = 3, dimnames = list(NULL, c("dog", "cat", "bird"))
  )

  # Finalize data
  result <- finalize(data, minimum_cases = 2)

  # Check for only dog
  expect_equal(colnames(result), "dog")

})

test_that("finalize matches the original on real fixture data", {

  # Finalize data
  final <- finalize(open.binary, minimum_cases = 2)

  # Check kept responses
  expect_true(all(colSums(open.binary)[colnames(final)] >= 2))

  # Check removed responses
  expect_true(all(colSums(open.binary)[setdiff(colnames(open.binary), colnames(final))] < 2))

})
