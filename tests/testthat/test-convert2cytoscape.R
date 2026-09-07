# Tests for convert2cytoscape()

test_that("convert2cytoscape produces a sparse edge list with no zero-weight edges", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net <- suppressWarnings(tmfg(cordata))

  cyto <- convert2cytoscape(net)

  expect_equal(colnames(cyto), c("nodeTO", "nodeFROM", "nodeWEIGHT"))
  expect_true(all(as.numeric(cyto[, "nodeWEIGHT"]) != 0))

  # TMFG keeps 3n - 6 edges; the sparse list has each edge listed both ways
  expect_equal(nrow(cyto), 2 * (3 * ncol(net) - 6))

})

test_that("convert2cytoscape validates its input", {

  expect_error(convert2cytoscape(matrix(1, 3, 4)), "square")

})
