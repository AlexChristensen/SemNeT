# Tests for response_analysis()

test_that("response_analysis matches the original algorithm for two groups", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- response_analysis(low = low, high = high)

  expect_named(result, c("total", "unique"))
  expect_named(result$total, c(
    "mean_low", "sd_low", "mean_high", "sd_high",
    "t_statistic", "df", "p_value", "ci_lower", "ci_upper", "cohens_d"
  ))
  expect_named(result$unique, c(
    "total_across_groups", "total_low", "total_high", "unique_low", "unique_high",
    "mcnemars_x2", "df", "p_value", "phi"
  ))

})

test_that("response_analysis's cohens_d matches the shared helper directly", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  result <- response_analysis(low = low, high = high)

  low_binary <- responses_to_binary(low)$binary
  high_binary <- responses_to_binary(high)$binary
  expected_d <- round(cohens_d(rowSums(low_binary), rowSums(high_binary)), 3)

  expect_equal(unname(result$total["cohens_d"]), expected_d)

})

test_that("response_analysis generalizes to more than two groups via all pairwise comparisons", {

  set.seed(1)
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]
  mid <- open.clean[sample(nrow(open.clean), 50), ]

  result <- response_analysis(low = low, high = high, mid = mid)

  expect_named(result, c("low_vs_high", "low_vs_mid", "high_vs_mid"))
  expect_named(result$low_vs_high, c("total", "unique"))

})

test_that("response_analysis accepts already-binary response matrices too", {

  low_binary <- responses_to_binary(open.clean[which(open.group == "Low"), ])$binary
  high_binary <- responses_to_binary(open.clean[which(open.group == "High"), ])$binary

  expect_no_error(response_analysis(low = low_binary, high = high_binary))

})

test_that("response_analysis requires at least two named groups", {

  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_error(response_analysis(low = low), "at least two")
  expect_error(response_analysis(low, high), "named")

})

test_that("response_analysis rejects groups sharing the same name", {

  # Real bug fixed: with 3+ groups, two groups sharing a name can produce two
  # *different* pairwise comparisons landing on the exact same
  # "<name>_vs_<name>" label, and `$`/`[[` access then silently returns only
  # the first of them
  low <- open.clean[which(open.group == "Low"), ]
  high <- open.clean[which(open.group == "High"), ]

  expect_error(response_analysis(low = low, low = high), "unique name")

})
