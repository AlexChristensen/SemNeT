# Tests for compare_networks()

make_test_network <- function(seed, nodes = 10) {
  set.seed(seed)
  cordata <- cor(matrix(rnorm(15 * nodes), nrow = 15, ncol = nodes))
  suppressWarnings(tmfg(cordata))
}

test_that("compare_networks runs without error for two networks", {

  net1 <- make_test_network(1)
  net2 <- make_test_network(2)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(compare_networks(one = net1, two = net2))

})

test_that("compare_networks runs without error for more than two networks", {

  net1 <- make_test_network(1)
  net2 <- make_test_network(2)
  net3 <- make_test_network(3)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(compare_networks(one = net1, two = net2, three = net3))

})

test_that("compare_networks's placement = 'match' works when networks share node counts", {

  net1 <- make_test_network(1)
  net2 <- make_test_network(2)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(compare_networks(one = net1, two = net2, placement = "match"))

})

test_that("compare_networks rejects placement = 'match' when node counts differ", {

  net1 <- make_test_network(1, nodes = 10)
  net2 <- make_test_network(2, nodes = 15)

  expect_error(compare_networks(one = net1, two = net2, placement = "match"), "same number of nodes")

})

test_that("compare_networks defaults title to the names given in ...", {

  net1 <- make_test_network(1)
  net2 <- make_test_network(2)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  # Missing 'title' shouldn't error -- it should default to c("one", "two")
  expect_no_error(compare_networks(one = net1, two = net2))

})

test_that("compare_networks validates its input", {

  net1 <- make_test_network(1)
  net2 <- make_test_network(2)

  expect_error(compare_networks(one = net1), "at least two")
  expect_error(compare_networks(net1, net2), "named")
  expect_error(compare_networks(one = net1, one = net2), "unique name")
  expect_error(compare_networks(one = net1, two = net2, title = "not a list"), "compare_networks")
  expect_error(compare_networks(one = net1, two = net2, config = 123), "compare_networks")
  expect_error(compare_networks(one = net1, two = net2, weighted = "yes"), "compare_networks")
  expect_error(compare_networks(one = net1, two = net2, qgraph_args = "not a list"), "compare_networks")

})
