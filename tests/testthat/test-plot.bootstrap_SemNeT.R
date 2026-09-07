# Tests for plot.bootstrap_SemNeT()

test_that("plot.bootstrap_SemNeT runs without error for the default and a single measure", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]
  boot_result <- suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 8, cores = 1))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot(boot_result))
  expect_no_error(plot(boot_result, measures = "aspl"))
  expect_no_error(plot(boot_result, measures = c("cc", "q")))

})

test_that("plot.bootstrap_SemNeT validates its input", {

  expect_error(plot.bootstrap_SemNeT(list()), "bootstrap_SemNeT")

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]
  boot_result <- suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 5, cores = 1))

  expect_error(plot(boot_result, measures = "not_a_measure"), "plot.bootstrap_SemNeT")

})
