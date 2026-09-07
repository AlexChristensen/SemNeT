# Tests for similarity()

test_that("similarity defaults to cosine and matches the cosine() helper", {

  data <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
  expect_equal(similarity(data), cosine(data))
  expect_equal(similarity(data, method = "cosine"), cosine(data))

})

test_that("similarity always returns a plain matrix, regardless of method", {

  data <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)

  for(method in c("angular", "cor", "cosine", "euclid", "faith", "jaccard", "phi", "rr")){
    expect_true(is.matrix(similarity(data, method = method)))
  }

})

test_that("similarity's contingency-table methods match a hand-computed value", {

  # Two columns: co-occur once (row 1), column 1 only (row 2), column 2 only (row 3), neither (row 4)
  # a = 1, b = 1, c = 1, d = 1
  data <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0), nrow = 4, ncol = 2)

  jaccard <- similarity(data, method = "jaccard")
  expect_equal(unname(jaccard[1, 2]), 1 / (1 + 1 + 1))

  rr <- similarity(data, method = "rr")
  expect_equal(unname(rr[1, 2]), 1 / 4)

  faith <- similarity(data, method = "faith")
  expect_equal(unname(faith[1, 2]), (1 + 0.5 * 1) / 4)

})

test_that("similarity's cor and phi methods agree with stats::cor", {

  data <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
  expect_equal(unname(similarity(data, method = "cor")), unname(cor(data)))
  expect_equal(unname(similarity(data, method = "phi")), unname(cor(data)))

})

test_that("similarity validates its input", {

  expect_error(similarity(list(1, 2)), "similarity")
  expect_error(similarity(matrix(c("a", "b", "c", "d"), 2, 2)), "similarity")

})
