# Tests for permutation_SemNeT()

test_that("permutation_SemNeT returns the expected schema for tmfg", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 15, cores = 1, keep_networks = TRUE))

  expect_s3_class(result, "permutation_SemNeT")
  expect_named(result, c("result", "networks", "method", "differences"))
  expect_equal(nrow(result$result), 1)
  expect_equal(row.names(result$result), "aspl")
  expect_named(result$result, c("low", "high", "p_value", "alternative"))
  expect_length(result$networks$network_1, 15)
  expect_length(result$networks$network_2, 15)
  expect_length(result$differences, 15)

})

test_that("permutation_SemNeT keeps 'networks' NULL by default", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  # Real bug fixed: every permuted sample's estimated networks (2 x `iter`
  # of them) used to be materialized and retained unconditionally, even
  # though only `previous =` reuse ever needs them again
  result <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1))

  expect_null(result$networks)
  expect_length(result$differences, 10)

})

test_that("permutation_SemNeT's 'previous' reuse requires the original call kept its networks", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  first <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 6, cores = 1))

  expect_error(
    suppressMessages(permutation_SemNeT(previous = first, measure = "cc", cores = 1)),
    "keep_networks = TRUE"
  )

})

test_that("permutation_SemNeT's first (unpermuted) entry gives the observed statistic", {

  set.seed(2)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1, keep_networks = TRUE))

  observed <- as.numeric(aspl(result$networks$network_1[[1]])) - as.numeric(aspl(result$networks$network_2[[1]]))
  expect_equal(unname(result$differences[1]), observed)

})

test_that("permutation_SemNeT reuses a previous result's networks for a different measure", {

  set.seed(3)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  first <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1, keep_networks = TRUE))
  second <- suppressMessages(permutation_SemNeT(previous = first, measure = "cc", cores = 1))

  expect_identical(first$networks, second$networks)
  expect_equal(row.names(second$result), "cc")
  expect_equal(names(second$result)[1:2], names(first$result)[1:2])

})

test_that("permutation_SemNeT works with cn, nrw, and pf", {

  set.seed(4)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_no_error(suppressMessages(permutation_SemNeT(low, high, method = "cn", measure = "q", iter = 6, cores = 1)))
  expect_no_error(suppressMessages(permutation_SemNeT(low, high, method = "pf", measure = "cc", iter = 6, cores = 1)))
  expect_no_error(suppressMessages(permutation_SemNeT(low, high, method = "nrw", measure = "aspl", iter = 6, cores = 1, method_args = list(threshold = 1))))

})

test_that("permutation_SemNeT validates its input", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_error(permutation_SemNeT(sample_1 = low), "must both be provided")
  expect_error(permutation_SemNeT(low, high, method = "nrw", method_args = list(thresold = 1)), "not used by")
  expect_error(permutation_SemNeT(previous = list()), "permutation_SemNeT")
  expect_error(permutation_SemNeT(low, high, iter = 0), "permutation_SemNeT")

})
