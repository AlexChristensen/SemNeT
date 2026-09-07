# Tests for bootstrap_SemNeT()

test_that("bootstrap_SemNeT returns the locked schema for a case-resampled tmfg bootstrap", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(
    bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 8, cores = 1, keep_networks = TRUE)
  )

  expect_s3_class(result, "bootstrap_SemNeT")
  expect_named(result, c("groups", "meta"))
  expect_named(result$groups, c("low", "high"))
  expect_named(result$groups$low, c("networks", "measures", "edges", "summary"))
  expect_length(result$groups$low$networks, 8)
  expect_equal(dim(result$groups$low$measures), c(3, 8))
  expect_equal(row.names(result$groups$low$measures), c("aspl", "cc", "q"))
  expect_length(result$groups$low$edges, 8)
  expect_equal(row.names(result$groups$low$summary), c("aspl", "cc", "q"))
  expect_null(result$meta$prop)
  expect_equal(result$meta$type, "case")
  expect_equal(result$meta$method, "tmfg")

})

test_that("bootstrap_SemNeT keeps 'networks' NULL by default, with 'edges' always populated", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  # Real bug fixed: every bootstrapped network (an N x N matrix, `iter` of
  # them per group) used to be retained unconditionally, even though the
  # only documented downstream consumer (`bootstrap_test_SemNeT()`'s ANCOVA
  # covariate) only ever needs each network's edge count
  result <- suppressMessages(
    bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 6, cores = 1)
  )

  expect_null(result$groups$low$networks)
  expect_null(result$groups$high$networks)
  expect_length(result$groups$low$edges, 6)
  expect_true(all(result$groups$low$edges >= 0))

})

test_that("bootstrap_SemNeT accepts raw word responses for tmfg and binarizes them", {

  set.seed(2)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_no_error(
    suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 5, cores = 1))
  )

})

test_that("bootstrap_SemNeT's tmfg case-resampling equates every group to the same nodes each iteration", {

  set.seed(3)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(
    bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 6, cores = 1, keep_networks = TRUE)
  )

  matched <- vapply(seq_len(6), function(i){
    ncol(result$groups$low$networks[[i]]) == ncol(result$groups$high$networks[[i]])
  }, logical(1))

  expect_true(all(matched))

})

test_that("bootstrap_SemNeT's node-drop type keeps the requested proportion of nodes", {

  set.seed(4)
  low <- finalize(open.binary[which(open.group == "Low"), ], 2)
  high <- finalize(open.binary[which(open.group == "High"), ], 2)
  equated <- equate(low = low, high = high)

  result <- suppressMessages(
    bootstrap_SemNeT(low = equated$low, high = equated$high, method = "tmfg", type = "node", prop = .5, iter = 5, cores = 1, keep_networks = TRUE)
  )

  expect_equal(result$meta$prop, .5)
  expect_true(all(vapply(result$groups$low$networks, ncol, numeric(1)) == floor(ncol(equated$low) * .5)))

})

test_that("bootstrap_SemNeT works with cn, nrw, and pf on raw response data", {

  set.seed(5)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_no_error(suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "cn", iter = 4, cores = 1)))
  expect_no_error(suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "pf", iter = 4, cores = 1)))
  expect_no_error(suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "nrw", iter = 4, cores = 1, method_args = list(threshold = 1))))

})

test_that("bootstrap_SemNeT rejects a binary matrix for cn/nrw, which need response order", {

  low_binary <- open.binary[which(open.group == "Low"), ]
  high_binary <- open.binary[which(open.group == "High"), ]

  expect_error(bootstrap_SemNeT(low = low_binary, high = high_binary, method = "cn"), "discarded response order")

})

test_that("bootstrap_SemNeT validates its input", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_error(bootstrap_SemNeT(low, high), "named")

  # Real bug fixed: two groups sharing a name silently collapsed onto one
  # entry (`samples[[group_name]]`/`results[[group_name]]` are both keyed by
  # name), computing one group's results twice under two different labels
  # instead of erroring
  expect_error(bootstrap_SemNeT(low = low, low = high), "unique name")

  expect_error(bootstrap_SemNeT(low = low, high = high, method = "nrw", method_args = list(thresold = 1)), "not used by")
  expect_error(bootstrap_SemNeT(low = low, high = high[, 1:20], type = "node"), "same number of nodes")
  expect_error(bootstrap_SemNeT(low = low, prop = 1.5), "bootstrap_SemNeT")
  expect_error(bootstrap_SemNeT(low = low, iter = 0), "bootstrap_SemNeT")

})
