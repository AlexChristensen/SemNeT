# Tests for random_network_test()

test_that("random_network_test returns a named list of correctly-shaped results", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  net_low <- cn(low)
  net_high <- cn(high)

  result <- suppressMessages(random_network_test(low = net_low, high = net_high, iter = 10, cores = 1))

  expect_named(result, c("low", "high"))
  expect_true(is.data.frame(result$low))
  expect_equal(rownames(result$low), c("aspl", "cc", "q"))
  expect_equal(colnames(result$low), c("p_value", "random_mean", "random_sd"))

})

test_that("random_network_test requires every network to be named", {

  set.seed(1)
  net <- suppressWarnings(tmfg(cor(matrix(rnorm(150), 15, 10))))

  expect_error(random_network_test(net, iter = 5), "named")

})

test_that("random_network_test rejects two networks sharing the same name", {

  # Real bug fixed: `Map()` names the returned list after `networks`, so two
  # networks sharing a name produced a duplicate list name, and `$`/`[[`
  # access then silently returned only the first network's result
  set.seed(1)
  net <- suppressWarnings(tmfg(cor(matrix(rnorm(150), 15, 10))))

  expect_error(random_network_test(low = net, low = net, iter = 5), "unique name")

})

test_that("generate_random_network preserves node and edge counts", {

  net <- generate_random_network(nodes = 10, edges = 15)

  expect_equal(ncol(net), 10)
  expect_equal(sum(net) / 2, 15)
  expect_true(isSymmetric(net))

})

test_that("generate_random_network produces exactly 0 edges for an edgeless observed network", {

  # `1:edges` with `edges == 0` is `c(1, 0)`, not empty -- since a 0 index is
  # silently dropped rather than erroring, that used to select 1 edge
  # instead of the correct 0
  net <- generate_random_network(nodes = 6, edges = 0)

  expect_equal(sum(net), 0)

})

test_that("random_network_test validates its iter and cores arguments", {

  net <- suppressWarnings(tmfg(cor(matrix(rnorm(150), 15, 10))))

  expect_error(random_network_test(a = net, iter = 0), "random_network_test")
  expect_error(random_network_test(a = net, cores = 0), "random_network_test")

})
