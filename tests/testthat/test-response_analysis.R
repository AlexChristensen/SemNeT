# Tests for response_analysis()

test_that("response_analysis matches the original algorithm for two groups", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform analysis
  result <- response_analysis(low = low, high = high)

  # Check result
  expect_named(result, c("total", "unique", "vocabulary"))

  # Check totals
  expect_named(result$total, c(
    "mean_low", "sd_low", "mean_high", "sd_high",
    "t_statistic", "df", "p_value", "ci_lower", "ci_upper", "cohens_d"
  ))

  # Check uniqueness
  expect_named(result$unique, c(
    "total_across_groups", "total_low", "total_high", "unique_low", "unique_high",
    "mcnemars_x2", "df", "p_value", "phi"
  ))

})

test_that("response_analysis's cohens_d matches the shared helper directly", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform analysis
  result <- response_analysis(low = low, high = high)

  # Convert low group to binary
  low_binary <- responses2binary(low)$binary

  # Convert high group to binary
  high_binary <- responses2binary(high)$binary

  # Compute effect size
  expected_d <- round(cohens_d(rowSums(low_binary), rowSums(high_binary)), 3)

  # Check effect size
  expect_equal(unname(result$total["cohens_d"]), expected_d)

})

test_that("response_analysis generalizes to more than two groups via all pairwise comparisons", {

  # Set seed
  set.seed(1)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Get middle group
  mid <- open.clean[sample(nrow(open.clean), 50), ]

  # Perform analysis
  result <- response_analysis(low = low, high = high, mid = mid)

  # Check comparisons
  expect_named(result, c("low_vs_high", "low_vs_mid", "high_vs_mid"))

  # Check result of comparison
  expect_named(result$low_vs_high, c("total", "unique", "vocabulary"))

})

test_that("response_analysis accepts already-binary response matrices too", {

  # Convert low group to binary
  low_binary <- responses2binary(open.clean[which(open.group == "Low"), ])$binary

  # Convert high group to binary
  high_binary <- responses2binary(open.clean[which(open.group == "High"), ])$binary

  # Check for no error
  expect_no_error(response_analysis(low = low_binary, high = high_binary))

})

test_that("rarefied_richness is Hurlbert's expected number of distinct responses", {

  # Check four responses given once
  expect_equal(rarefied_richness(c(1, 1, 1, 1), 4, 2), 2)

  # Check responses given by one, two, and all participants
  expect_equal(rarefied_richness(c(1, 2, 4), 4, 2), (1 - 3 / 6) + (1 - 1 / 6) + 1)

  # Check full sample
  expect_equal(rarefied_richness(c(1, 2, 4, 0), 4, 4), 3)

})

test_that("response_analysis compares vocabularies at the smaller group's size", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get small high group
  high <- open.clean[which(open.group == "High"), ][1:100, ]

  # Compute vocabulary results
  vocabulary <- response_analysis(low = low, high = high)$vocabulary

  # Check rarefied size
  expect_equal(unname(vocabulary["rarefied_n"]), 100)

  # Check lower bound of Jaccard
  expect_gte(vocabulary["jaccard"], 0)

  # Check upper bound of Jaccard
  expect_lte(vocabulary["jaccard"], 1)

  # Get observed richness of low group
  observed_low <- ncol(responses2binary(low)$binary)

  # Check rarefied richness
  expect_lte(vocabulary[["rarefied_low"]], observed_low)

  # Check rarefied difference
  expect_equal(vocabulary[["rarefied_diff"]], round(vocabulary[["rarefied_low"]] - vocabulary[["rarefied_high"]], 3), tolerance = 1e-3)

})

test_that("a row-subset of an already-binary matrix does not count never-given responses", {

  # Get rows of low group
  low_rows <- which(open.group == "Low")

  # Get rows of high group
  high_rows <- which(open.group == "High")

  # Perform analysis on raw responses
  from_raw <- response_analysis(low = open.clean[low_rows, ], high = open.clean[high_rows, ])

  # Convert to binary
  binary <- responses2binary(open.clean)$binary

  # Perform analysis on binary responses
  from_binary <- response_analysis(low = binary[low_rows, ], high = binary[high_rows, ])

  # Check uniqueness
  expect_equal(from_binary$unique, from_raw$unique)

  # Check vocabulary
  expect_equal(from_binary$vocabulary, from_raw$vocabulary)

  # Check for unique responses
  expect_gt(from_binary$unique[["unique_low"]], 0)

})

test_that("response_analysis does not fail when the t-test or McNemar's test is undefined", {

  # Set up first group
  one <- matrix(c("a", "b", "a", "b", "b", "a"), nrow = 3, byrow = TRUE)

  # Set up second group
  two <- matrix(c("a", "b", "b", "a"), nrow = 2, byrow = TRUE)

  # Perform analysis
  result <- response_analysis(one = one, two = two)

  # Check for undefined t-statistic
  expect_true(is.na(result$total[["t_statistic"]]))

  # Check for undefined p-value
  expect_true(is.na(result$total[["p_value"]]))

  # Check for zero effect
  expect_equal(result$total[["cohens_d"]], 0)

  # Check for undefined McNemar statistic
  expect_true(is.na(result$unique[["mcnemars_x2"]]))

  # Check for undefined McNemar p-value
  expect_true(is.na(result$unique[["p_value"]]))

  # Check for complete overlap
  expect_equal(unname(result$vocabulary["jaccard"]), 1)

})
