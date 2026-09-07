# Tests for R/legacy.R -- the pre-2.0.0 exported names, kept working as
# thin wrappers around their new-named replacements

test_that("legacy_dot_names captures explicit names, falling back to deparsed expressions", {

  f <- function(...) legacy_dot_names(substitute(list(...)))

  low <- 1
  high <- 2

  expect_equal(f(low, high), c("low", "high"))
  expect_equal(f(a = low, high), c("a", "high"))
  expect_equal(f(), character(0))

})

test_that("TMFG/ASPL/CC/Q legacy wrappers are identical to their new-named replacements", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net_new <- suppressWarnings(tmfg(cordata))
  net_old <- suppressWarnings(TMFG(cordata))

  expect_identical(net_old, net_new)
  expect_identical(ASPL(net_old), aspl(net_new))
  expect_identical(CC(net_old), cc(net_new))

  # q()'s modularity is only reproducible via an explicit `seed` -- the
  # legacy Q() wrapper has no such argument (matching the original's
  # signature), so just check it produces a plausible, unseeded value
  q_old <- Q(net_old)
  expect_true(is.numeric(q_old) && length(q_old) == 1)

})

test_that("CN/NRW/PF legacy wrappers are identical to their new-named replacements", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]

  expect_identical(suppressWarnings(CN(low)), suppressWarnings(cn(low)))
  expect_identical(suppressWarnings(NRW(low)), suppressWarnings(nrw(low)))
  expect_identical(suppressWarnings(PF(low)), suppressWarnings(pf(low)))

})

test_that("sim.fluency legacy wrapper requires nodes/cases, like the original", {

  expect_error(sim.fluency())

  set.seed(1)
  expect_identical(sim.fluency(20, 30), {set.seed(1); simulate_fluency(nodes = 20, cases = 30)})

})

test_that("semnetmeas legacy wrapper matches semantic_network_measures, renamed to uppercase", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net <- suppressWarnings(tmfg(cordata))

  old <- semnetmeas(net)
  new <- semantic_network_measures(net)

  expect_named(old, c("ASPL", "CC", "Q"))
  expect_equal(unname(old), unname(new))

})

test_that("response.analysis legacy wrapper captures names from bare expressions", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  old <- response.analysis(low, high)
  new <- response_analysis(low = low, high = high)

  expect_identical(old, new)
  expect_true(any(grepl("low", names(old$total), fixed = TRUE)))

})

test_that("randwalk legacy wrapper labels output from bare expressions, including `$`-split names", {

  set.seed(1)
  cordata1 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  cordata2 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net1 <- suppressWarnings(tmfg(cordata1))
  net2 <- suppressWarnings(tmfg(cordata2))

  result <- suppressMessages(randwalk(net1, net2, reps = 2, steps = 5, iter = 20))

  expect_true(is.matrix(result$long))
  expect_true(all(c("M.uniq.net1", "M.uniq.net2") %in% colnames(result$long)))

  data <- list(one = net1)
  result_dollar <- suppressMessages(randwalk(data$one, net2, reps = 2, steps = 5, iter = 20))

  expect_true(is.matrix(result_dollar$long))
  expect_true("M.uniq.one" %in% colnames(result_dollar$long))
  expect_false(any(grepl("data", colnames(result_dollar$long), fixed = TRUE)))

})

test_that("randnet.test legacy wrapper matches random_network_test", {

  set.seed(1)
  net_low <- suppressWarnings(cn(open.clean[which(open.group == "Low"), ]))
  net_high <- suppressWarnings(cn(open.clean[which(open.group == "High"), ]))

  old <- suppressMessages(randnet.test(low = net_low, high = net_high, iter = 5, cores = 1))
  new <- suppressMessages(random_network_test(low = net_low, high = net_high, iter = 5, cores = 1))

  expect_named(old, c("low", "high"))
  expect_equal(names(old), names(new))

})

test_that("bootSemNeT legacy wrapper works with no method/type given, unlike the original", {

  # The original had no usable default for either argument -- `switch()`/
  # `if(type == ...)` on an unresolved multi-value default would error.
  # The legacy wrapper now falls through cleanly to the new function's own
  # defaults ("tmfg", "case") instead
  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(bootSemNeT(low, high, iter = 3, cores = 1))

  expect_s3_class(result, "bootSemNeT")
  expect_s3_class(result, "bootstrap_SemNeT")
  expect_named(result$groups, c("low", "high"))
  expect_equal(result$meta$method, "tmfg")
  expect_equal(result$meta$type, "case")

})

test_that("bootSemNeT legacy wrapper translates methodArgs$minCase into minimum_cases", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(bootSemNeT(
    low, high, method = "TMFG", type = "case", iter = 3, cores = 1,
    methodArgs = list(minCase = 3)
  ))

  expect_s3_class(result, "bootstrap_SemNeT")

})

test_that("bootSemNeT legacy wrapper supports node-type bootstrapping via an old-style method name", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ][, colnames(low)]

  result <- suppressMessages(bootSemNeT(
    low, high, method = "NRW", type = "node", prop = .5, iter = 3, cores = 1,
    methodArgs = list(threshold = 1)
  ))

  expect_equal(result$meta$method, "nrw")
  expect_equal(result$meta$type, "node")

})

test_that("test.bootSemNeT legacy wrapper consumes a legacy bootSemNeT result", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  boot_result <- suppressMessages(bootSemNeT(low, high, method = "TMFG", type = "case", iter = 5, cores = 1))
  tested <- test.bootSemNeT(boot_result)

  expect_s3_class(tested, "bootstrap_test_SemNeT")
  expect_named(tested$results, "Case")
  expect_named(tested$results$Case, c("aspl", "cc", "q"))

})

test_that("plot.bootSemNeT legacy wrapper dispatches through plot() and supports relabeling groups", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  boot_result <- suppressMessages(bootSemNeT(low, high, method = "TMFG", type = "case", iter = 5, cores = 1))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot(boot_result))
  expect_no_error(plot(boot_result, groups = c("Low group", "High group"), measures = c("ASPL", "Q")))

})

test_that("compare_nets legacy wrapper matches compare_networks", {

  net1 <- suppressWarnings(tmfg(cor(matrix(rnorm(150), 15, 10))))
  net2 <- suppressWarnings(tmfg(cor(matrix(rnorm(150), 15, 10))))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(compare_nets(net1, net2))
  expect_no_error(compare_nets(net1, net2, placement = "match"))

})
