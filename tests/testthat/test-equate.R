# Tests for equate()

test_that("equate reduces two groups to their true shared vocabulary", {

  # Set up first group
  mat1 <- matrix(1, 2, 3, dimnames = list(NULL, c("dog", "cat", "bird")))

  # Set up second group
  mat2 <- matrix(1, 2, 3, dimnames = list(NULL, c("dog", "fish", "snake")))

  # Equate groups
  result <- equate(mat1 = mat1, mat2 = mat2)

  # Check first group
  expect_equal(colnames(result$mat1), "dog")

  # Check second group
  expect_equal(colnames(result$mat2), "dog")

})

test_that("equate returns every group in identical column order", {

  # Set up first group
  m1 <- matrix(1, 2, 4, dimnames = list(NULL, c("dog", "cat", "bird", "fish")))

  # Set up second group
  m2 <- matrix(1, 2, 4, dimnames = list(NULL, c("bird", "dog", "cat", "snake")))

  # Set up third group
  m3 <- matrix(1, 2, 3, dimnames = list(NULL, c("cat", "dog", "elephant")))

  # Equate groups
  result <- equate(a = m1, b = m2, c = m3)

  # Check first and second groups
  expect_equal(colnames(result$a), colnames(result$b))

  # Check second and third groups
  expect_equal(colnames(result$b), colnames(result$c))

  # Check shared responses
  expect_equal(sort(colnames(result$a)), c("cat", "dog"))

})

test_that("equate preserves each group's own row data after subsetting", {

  # Set up first group
  m1 <- matrix(c(1, 0, 1, 1), 2, 2, dimnames = list(NULL, c("dog", "cat")))

  # Set up second group
  m2 <- matrix(c(0, 1, 1, 1), 2, 2, dimnames = list(NULL, c("dog", "cat")))

  # Equate groups
  result <- equate(m1 = m1, m2 = m2)

  # Check first group
  expect_equal(result$m1, m1[, c("cat", "dog")])

  # Check second group
  expect_equal(result$m2, m2[, c("cat", "dog")])

})
