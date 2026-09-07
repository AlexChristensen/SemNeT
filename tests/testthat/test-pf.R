# Tests for pf()

test_that("pf produces a valid, symmetric, binary network from a response matrix", {

  net <- pf(open.binary)

  expect_true(is.matrix(net))
  expect_true(isSymmetric(unname(net)))
  expect_true(all(net %in% c(0, 1)))

})

test_that("pf attaches the pre-binarization weighted network instead of discarding it", {

  net <- pf(open.binary)
  weighted <- attr(net, "weighted")

  expect_false(is.null(weighted))
  expect_equal(dim(weighted), dim(net))

  # Every binary edge must correspond to a nonzero weighted edge, and vice versa
  expect_equal(unname(net), unname(ifelse(weighted != 0, 1, 0)), ignore_attr = TRUE)

})

test_that("pf accepts raw character responses as well as an already-binary matrix", {

  raw <- open.clean[which(open.group == "Low"), ]
  expect_no_error(pf(raw))

})

test_that("pf validates its input", {

  expect_error(pf(list(1, 2)), "pf")

})
