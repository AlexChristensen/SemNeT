# Tests for similarity()

test_that("similarity defaults to cosine and matches the cosine() helper", {

  # Set up data
  data <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)

  # Check default
  expect_equal(similarity(data), cosine(data))

  # Check cosine
  expect_equal(similarity(data, method = "cosine"), cosine(data))

})

test_that("similarity's euclid is a similarity (1 / (1 + distance)), not a distance", {

  # Set up data
  data <- cbind(a = c(1, 1, 0, 0), b = c(1, 1, 0, 0), c = c(0, 0, 1, 1))

  # Compute similarity
  euclid <- similarity(data, method = "euclid")

  # Check identical columns
  expect_equal(euclid["a", "b"], 1)

  # Check different columns
  expect_equal(euclid["a", "c"], 1 / (1 + 2))

  # Check diagonal
  expect_equal(unname(diag(euclid)), c(1, 1, 1))

  # Check ordering
  expect_lt(euclid["a", "c"], euclid["a", "b"])

})

test_that("similarity's contingency-table methods match a hand-computed value", {

  # Set up data (a = 1, b = 1, c = 1, d = 1)
  data <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0), nrow = 4, ncol = 2)

  # Compute Jaccard
  jaccard <- similarity(data, method = "jaccard")

  # Check Jaccard
  expect_equal(unname(jaccard[1, 2]), 1 / (1 + 1 + 1))

  # Compute RR
  rr <- similarity(data, method = "rr")

  # Check RR
  expect_equal(unname(rr[1, 2]), 1 / 4)

  # Compute Faith
  faith <- similarity(data, method = "faith")

  # Check Faith
  expect_equal(unname(faith[1, 2]), (1 + 0.5 * 1) / 4)

})

test_that("similarity's cor and phi methods agree with stats::cor", {

  # Set up data
  data <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)

  # Check correlation
  expect_equal(unname(similarity(data, method = "cor")), unname(cor(data)))

  # Check phi
  expect_equal(unname(similarity(data, method = "phi")), unname(cor(data)))

})
