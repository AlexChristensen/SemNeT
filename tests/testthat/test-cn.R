# Tests for cn()

test_that("cn produces a valid, symmetric, binary semantic network", {

  data <- open.clean[which(open.group == "Low"), ]

  net <- cn(data)

  expect_true(is.matrix(net))
  expect_true(isSymmetric(unname(net)))
  expect_true(all(net %in% c(0, 1)))

})

test_that("cn's enrich = TRUE option only adds edges, never removes them", {

  data <- open.clean[which(open.group == "Low"), ]

  base_net <- cn(data)
  enriched_net <- cn(data, enrich = TRUE)

  # Both networks are estimated over the same (largest-component) vocabulary
  expect_equal(dim(base_net), dim(enriched_net))
  expect_true(all(base_net[base_net == 1] == enriched_net[base_net == 1]))
  expect_true(sum(enriched_net) >= sum(base_net))

})

test_that("cn rejects an already-binarized response matrix (no order information left)", {

  binary <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_error(cn(binary), "discarded response order")

})

test_that("cn validates its numeric arguments", {

  data <- open.clean[which(open.group == "Low"), ]

  expect_error(cn(data, window = 0), "cn")
  expect_error(cn(data, alpha = 1.5), "cn")
  expect_error(cn(data, enrich = "yes"), "cn")

})

test_that("stat_cooccur doesn't crash when the vocabulary has 0 or 1 words", {

  # A single shared word across all participants -- `1:(n_words - 1)` would
  # otherwise be `1:0` (i.e. `c(1, 0)`, not empty), sending the inner range
  # `(i + 1):n_words` out of the 1x1 adjacency matrix's bounds
  one_word <- matrix(c("dog", "dog", "dog"), nrow = 3, ncol = 1)
  expect_equal(unname(stat_cooccur(one_word)), matrix(0, 1, 1))

})

test_that("rel_freq handles a single-participant dataset without erroring", {

  # With exactly 1 participant, each per-word `rowSums(...) > 0` collapses to
  # a length-1 logical, which `sapply()` alone would return as a plain
  # vector instead of a (1 x vocabulary) matrix -- breaking `colMeans()`
  one_participant <- matrix(c("dog", "cat"), nrow = 1, ncol = 2)
  result <- rel_freq(one_participant)

  expect_length(result, 2)
  expect_true(all(result == 1))

})
