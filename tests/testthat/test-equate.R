# Tests for equate()

test_that("equate reduces two groups to their true shared vocabulary", {

  # Real bug in the original: comparing vocabulary SIZE instead of actual set
  # membership meant two groups of the same size never got equated at all,
  # even when their actual words barely overlapped
  mat1 <- matrix(1, 2, 3, dimnames = list(NULL, c("dog", "cat", "bird")))
  mat2 <- matrix(1, 2, 3, dimnames = list(NULL, c("dog", "fish", "snake")))

  result <- equate(mat1 = mat1, mat2 = mat2)

  expect_equal(colnames(result$mat1), "dog")
  expect_equal(colnames(result$mat2), "dog")

})

test_that("equate returns every group in identical column order", {

  m1 <- matrix(1, 2, 4, dimnames = list(NULL, c("dog", "cat", "bird", "fish")))
  m2 <- matrix(1, 2, 4, dimnames = list(NULL, c("bird", "dog", "cat", "snake")))
  m3 <- matrix(1, 2, 3, dimnames = list(NULL, c("cat", "dog", "elephant")))

  result <- equate(a = m1, b = m2, c = m3)

  expect_equal(colnames(result$a), colnames(result$b))
  expect_equal(colnames(result$b), colnames(result$c))
  expect_equal(sort(colnames(result$a)), c("cat", "dog"))

})

test_that("equate preserves each group's own row data after subsetting", {

  # equate() returns a consistent (sorted) column order across every group,
  # which need not match a given group's own original column order
  m1 <- matrix(c(1, 0, 1, 1), 2, 2, dimnames = list(NULL, c("dog", "cat")))
  m2 <- matrix(c(0, 1, 1, 1), 2, 2, dimnames = list(NULL, c("dog", "cat")))

  result <- equate(m1 = m1, m2 = m2)

  expect_equal(result$m1, m1[, c("cat", "dog")])
  expect_equal(result$m2, m2[, c("cat", "dog")])

})

test_that("equate errors when nothing is shared across all groups", {

  m1 <- matrix(1, 2, 1, dimnames = list(NULL, "dog"))
  m2 <- matrix(1, 2, 1, dimnames = list(NULL, "cat"))

  expect_error(equate(m1 = m1, m2 = m2), "nothing to equate")

})

test_that("equate requires at least two named groups", {

  m1 <- matrix(1, 2, 1, dimnames = list(NULL, "dog"))

  expect_error(equate(m1 = m1), "at least two")
  expect_error(equate(m1, matrix(1, 2, 1, dimnames = list(NULL, "cat"))), "named")

})

test_that("equate requires named columns to equate on", {

  m1 <- matrix(1, 2, 2)
  m2 <- matrix(1, 2, 2, dimnames = list(NULL, c("dog", "cat")))

  expect_error(equate(m1 = m1, m2 = m2), "column names")

})

test_that("equate rejects two groups sharing the same name", {

  # Real bug fixed: two groups named identically collapsed onto one entry in
  # the returned list, since `equated_groups[[name]]`/`$name` access only
  # returns the first match of a duplicate name -- silently discarding the
  # second group's equated matrix
  m1 <- matrix(1, 2, 1, dimnames = list(NULL, "dog"))
  m2 <- matrix(1, 2, 1, dimnames = list(NULL, "dog"))

  expect_error(equate(low = m1, low = m2), "unique name")

})
