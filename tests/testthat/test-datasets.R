# Smoke tests for the bundled example datasets
# These are copied verbatim from the original package and should never change shape

test_that("bundled datasets load with their expected shapes", {

  # animals.freq is a length-367 frequency vector
  expect_length(animals.freq, 367)

  # net.high and net.low are 160x160 adjacency matrices
  expect_equal(dim(net.high), c(160, 160))
  expect_equal(dim(net.low), c(160, 160))

  # open.clean, open.binary, and open.group are index-matched companions
  expect_equal(nrow(open.clean), 516)
  expect_equal(nrow(open.binary), 516)
  expect_length(open.group, 516)

  # one.result and two.result are pre-computed bootSemNeT-style lists
  expect_type(one.result, "list")
  expect_type(two.result, "list")

  # vignette.plots is a list of pre-rendered ggplot objects
  expect_type(vignette.plots, "list")

})
