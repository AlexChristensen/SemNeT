# Tests for nrw()

test_that("nrw produces a valid, symmetric, binary semantic network", {

  data <- open.clean[which(open.group == "Low"), ]
  net <- nrw(data)

  expect_true(is.matrix(net))
  expect_true(isSymmetric(unname(net)))
  expect_true(all(net %in% c(0, 1)))

})

test_that("nrw's num vs. prop thresholding produce consistent, monotonic networks", {

  data <- open.clean[which(open.group == "Low"), ]

  loose <- nrw(data, type = "prop", threshold = 0.01)
  strict <- nrw(data, type = "prop", threshold = 0.1)

  # A stricter threshold can only retain a subset of the vocabulary
  expect_true(all(rownames(strict) %in% rownames(loose)))

})

test_that("nrw accepts an ordered numeric response matrix (the fixed >= 1 vs. == 1 bug)", {

  data <- open.clean[which(open.group == "Low"), ]
  ordered_numeric <- responses_to_binary(data)$order

  expect_no_error(nrw(ordered_numeric))

})

test_that("nrw rejects an already-binarized response matrix (no order information left)", {

  binary <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(nrw(binary), "discarded response order")

})

test_that("nrw validates its threshold argument", {

  data <- open.clean[which(open.group == "Low"), ]

  expect_error(nrw(data, threshold = -1), "nrw")
  expect_error(nrw(data, type = "prop", threshold = 1.5), "nrw")

})
