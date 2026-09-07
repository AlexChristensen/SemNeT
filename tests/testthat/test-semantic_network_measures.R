# Tests for semantic_network_measures()

test_that("semantic_network_measures returns all three measures by default, correctly named", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  result <- semantic_network_measures(net, seed = 2)

  expect_named(result, c("aspl", "cc", "q"))
  expect_equal(unname(result["aspl"]), as.numeric(aspl(binarize(net))))
  expect_equal(unname(result["cc"]), as.numeric(cc(binarize(net))))

  # 'weighted = FALSE' (the default) binarizes before *every* measure,
  # including 'q' -- so the comparison must run 'q' on the binarized
  # network too, not the original signed one. Compiled Louvain's randomness
  # depends only on 'seed' now (not R's `set.seed()`), so this only holds
  # when the same explicit seed is used both times
  expect_equal(unname(result["q"]), as.numeric(q(binarize(net), seed = 2)))

})

test_that("semantic_network_measures respects a subset request, in the order given", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  result <- semantic_network_measures(net, measures = c("q", "cc"))

  expect_named(result, c("q", "cc"))
  expect_length(result, 2)

})

test_that("semantic_network_measures validates its input", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 20), nrow = 15, ncol = 20))
  net <- suppressWarnings(tmfg(cordata))

  expect_error(semantic_network_measures(net, measures = "not_a_measure"), "semantic_network_measures")
  expect_error(semantic_network_measures(net, weighted = "yes"), "semantic_network_measures")

})
