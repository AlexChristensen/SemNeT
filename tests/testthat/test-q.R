# Tests for q()

test_that("q returns a plausible modularity value", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  modularity <- q(net, seed = 1)

  expect_true(is.numeric(modularity))
  expect_true(modularity >= 0 && modularity <= 1)

})

test_that("q's compiled Louvain is reproducible via 'seed', independent of R's own RNG state", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  q1 <- q(net, seed = 42)

  set.seed(999)
  q2 <- q(net, seed = 42)

  set.seed(NULL)
  q3 <- q(net, seed = 42)

  expect_equal(q1, q2)
  expect_equal(q1, q3)

})

test_that("q ignores the diagonal (e.g. TMFG's diag = 1 self-loop convention)", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  net_no_diag <- net
  diag(net_no_diag) <- 0

  expect_equal(q(net, seed = 42), q(net_no_diag, seed = 42))

})

test_that("q's signed = TRUE option scores the same partition differently", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  unsigned <- q(net, seed = 42, signed = FALSE)
  signed <- q(net, seed = 42, signed = TRUE)

  expect_true(is.numeric(signed))
  expect_false(isTRUE(all.equal(unsigned, signed)))

})

test_that("q validates its input", {

  expect_error(q(matrix(1, 3, 4)), "square")
  expect_error(q(list(1, 2)), "q")
  expect_error(q(matrix(1, 3, 3), resolution = -1), "q")
  expect_error(q(matrix(1, 3, 3), seed = c(1, 2)), "q")
  expect_error(q(matrix(1, 3, 3), signed = "yes"), "q")

})
