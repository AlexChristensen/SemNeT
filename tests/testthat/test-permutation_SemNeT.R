# Tests for permutation_SemNeT()

test_that("permutation_SemNeT's first (unpermuted) entry gives the observed statistic", {

  # Set seed
  set.seed(2)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform permutation test
  result <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1, keep_networks = TRUE))

  # Compute observed difference
  observed <- as.numeric(aspl(result$networks$network_1[[1]])) - as.numeric(aspl(result$networks$network_2[[1]]))

  # Check observed difference
  expect_equal(unname(result$differences[1]), observed)

})

test_that("permutation_SemNeT reuses a previous result's networks for a different measure", {

  # Set seed
  set.seed(3)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform first permutation test
  first <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 10, cores = 1, keep_networks = TRUE))

  # Perform second permutation test
  second <- suppressMessages(permutation_SemNeT(previous = first, measure = "cc", cores = 1))

  # Check networks
  expect_identical(first$networks, second$networks)

  # Check measure
  expect_equal(row.names(second$result), "cc")

  # Check names of samples
  expect_equal(names(second$result)[1:2], names(first$result)[1:2])

})

test_that("permutation_SemNeT reports no minimal detectable effect and stores alpha", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform permutation test
  result <- permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 30, cores = 1, seed = 1, keep_networks = TRUE)

  # Get differences
  d <- result$differences

  # Check that there is no minimal detectable effect
  expect_null(result$mde)

  # Check significance level
  expect_equal(result$alpha, .05)

  # Perform stricter test
  stricter <- permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 30, cores = 1, seed = 1, alpha = .01)

  # Check that significance level is stored
  expect_equal(stricter$alpha, .01)

  # Check that p-value is unchanged
  expect_equal(stricter$result$p_value, result$result$p_value)

  # Check that removed argument is rejected
  expect_error(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 30, cores = 1, seed = 1, power = .9), "unused argument")

  # Check printed output
  expect_output(print(result), "null_range")
  expect_false(any(grepl("MDE", capture.output(print(result)))))

  # Check result column
  expect_output(print(result), "result")

  # Reuse permuted networks
  reused <- permutation_SemNeT(previous = result, measure = "aspl", cores = 1)

  # Check significance level
  expect_equal(reused$alpha, result$alpha)

})

test_that("permutation_SemNeT tests several measures at once, matching one-at-a-time runs", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Set measures
  measures <- c("aspl", "cc", "q")

  # Perform permutation test
  all_measures <- permutation_SemNeT(low, high, method = "tmfg", measure = measures, iter = 20, cores = 1, seed = 1)

  # Check measures
  expect_equal(row.names(all_measures$result), measures)

  # Check columns
  expect_named(all_measures$result, c("low", "high", "p_value", "alternative"))

  # Check dimensions of differences
  expect_equal(dim(all_measures$differences), c(20, 3))

  # Check names of differences
  expect_equal(colnames(all_measures$differences), measures)

  # Check diagnostics
  expect_named(all_measures$diagnostics, c(
    "aspl_1", "aspl_2", "cc_1", "cc_2", "q_1", "q_2", "nodes_1", "nodes_2", "edges_1", "edges_2"
  ))

  # Loop over measures
  for(name in measures){

    # Perform permutation test of measure
    single <- permutation_SemNeT(low, high, method = "tmfg", measure = name, iter = 20, cores = 1, seed = 1)

    # Check differences
    expect_equal(unname(all_measures$differences[, name]), single$differences)

    # Check result
    expect_equal(all_measures$result[name, ], single$result)

  }

  # Perform permutation test of one measure
  single <- permutation_SemNeT(low, high, method = "tmfg", measure = "cc", iter = 20, cores = 1, seed = 1)

  # Check that differences are a vector
  expect_null(dim(single$differences))

  # Check diagnostics
  expect_named(single$diagnostics, c("value_1", "value_2", "nodes_1", "nodes_2", "edges_1", "edges_2"))

  # Reuse networks for subset of measures
  reused <- permutation_SemNeT(previous = all_measures, measure = c("aspl", "q"), cores = 1)

  # Check measures
  expect_equal(row.names(reused$result), c("aspl", "q"))

  # Check differences
  expect_equal(reused$differences, all_measures$differences[, c("aspl", "q")])

  # Capture printed output
  printed <- paste(capture.output(print(all_measures)), collapse = "\n")

  # Check header
  expect_match(printed, "Permutation test of group differences")

  # Check note on adjustment
  expect_match(printed, "not adjusted for the number of\\s+(\\|\\s+)?measures")

  # Check rows
  expect_equal(sum(grepl("^ +(ASPL|CC|Q) ", strsplit(printed, "\n")[[1]])), 3)

  # Check for error with unknown measure
  expect_error(permutation_SemNeT(low, high, measure = c("aspl", "nope")), "must only contain")

})

test_that("permutation_SemNeT reports per-permutation values and network sizes, including on reuse", {

  # Set seed
  set.seed(3)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform permutation test
  result <- suppressMessages(permutation_SemNeT(low, high, method = "tmfg", measure = "aspl", iter = 8, cores = 1, keep_networks = TRUE))

  # Get diagnostics
  diagnostics <- result$diagnostics

  # Check class
  expect_s3_class(diagnostics, "data.frame")

  # Check rows
  expect_equal(nrow(diagnostics), 8)

  # Check columns
  expect_named(diagnostics, c("value_1", "value_2", "nodes_1", "nodes_2", "edges_1", "edges_2"))

  # Check differences
  expect_equal(diagnostics$value_1 - diagnostics$value_2, result$differences)

  # Check nodes
  expect_equal(diagnostics$nodes_1, vapply(result$networks$network_1, nrow, numeric(1)))

  # Check edges
  expect_equal(diagnostics$edges_2, unname(vapply(result$networks$network_2, function(n) network_counts(n)["edges"], numeric(1))))

  # Reuse networks
  reused <- suppressMessages(permutation_SemNeT(previous = result, measure = "cc", cores = 1))

  # Check nodes of reused result
  expect_equal(reused$diagnostics$nodes_1, diagnostics$nodes_1)

  # Check edges of reused result
  expect_equal(reused$diagnostics$edges_2, diagnostics$edges_2)

  # Check values of reused result
  expect_equal(reused$diagnostics$value_1, vapply(result$networks$network_1, cc, numeric(1)))

})

test_that("permutation_SemNeT is reproducible given a seed, regardless of cores", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Set up permutation test
  run <- function(seed, cores = 1, measure = "q"){
    suppressMessages(permutation_SemNeT(
      low, high, measure = measure, iter = 8, cores = cores, seed = seed, keep_networks = TRUE
    ))
  }

  # Perform permutation test
  one <- run(1)

  # Check same seed
  expect_identical(one$differences, run(1)$differences)

  # Check more cores
  expect_identical(one$differences, run(1, cores = 2)$differences)

  # Check different seed
  expect_false(identical(one$differences, run(2)$differences))

  # Check stored seed
  expect_identical(one$seed, 1)

  # Set seed
  set.seed(99)

  # Check independence of R's random state
  expect_identical(one$differences, run(1)$differences)

  # Reuse networks
  reused <- suppressMessages(permutation_SemNeT(previous = one, measure = "q"))

  # Check inherited seed
  expect_identical(reused$seed, 1)

  # Check differences
  expect_identical(reused$differences, one$differences)

  # Check message without seed
  expect_message(
    permutation_SemNeT(low, high, method = "pf", iter = 2), "not be reproducible"
  )

})

test_that("the two-sided permutation p-value has three documented definitions", {

  # Set up null centred away from zero
  null <- c(-0.1, 0.20, 0.25, 0.30, 0.30, 0.35, 0.40, 0.45, 0.50, 0.60)

  # Compute p-value
  p <- function(alternative, observed = null[1]){
    permutation_p_value(c(observed, null[-1]), observed, alternative)
  }

  # Check distance from zero
  expect_equal(p("two_tailed"), 1)

  # Get center of null
  centre <- mean(null)

  # Check distance from center
  expect_equal(p("two_tailed_centered"), mean(abs(null - centre) >= abs(null[1] - centre)))

  # Check that it is below half
  expect_lt(p("two_tailed_centered"), .5)

  # Check less
  expect_equal(p("less"), 0.1)

  # Check greater
  expect_equal(p("greater"), 1)

  # Check doubled
  expect_equal(p("two_tailed_doubled"), 0.2)

  # Check doubled cap
  expect_equal(p("two_tailed_doubled", observed = 0.30), 1)

  # Shift null
  shifted <- null + 5

  # Check that one-sided p-value is unchanged
  expect_equal(
    permutation_p_value(shifted, shifted[1], "greater"), permutation_p_value(null, null[1], "greater")
  )

  # Set up ties with floating-point noise
  noisy <- c(0.1 + 0.2, 0.3, 0.3, 0.9)

  # Check exact comparison
  expect_equal(mean(noisy >= noisy[1]), 1 / 2)

  # Check tolerance
  expect_equal(permutation_p_value(noisy, noisy[1], "greater"), 1)

  # Set up symmetric null
  symmetric <- c(0.5, -0.5, 0.1, -0.1, 0.2, -0.2, 0.3, -0.3)

  # Check agreement of tails and center
  expect_equal(
    permutation_p_value(symmetric, symmetric[1], "two_tailed"),
    permutation_p_value(symmetric, symmetric[1], "two_tailed_centered")
  )

  # Check agreement of tails and doubled
  expect_equal(
    permutation_p_value(symmetric, symmetric[1], "two_tailed"),
    permutation_p_value(symmetric, symmetric[1], "two_tailed_doubled")
  )

  # Check missing observed value
  expect_true(is.na(permutation_p_value(c(NA, 1, 2), NA_real_, "two_tailed")))

})

test_that("each two-sided option runs end to end and has its own minimal detectable effect", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Set up permutation test
  run <- function(alternative){
    permutation_SemNeT(
      low, high, method = "tmfg", measure = "aspl", iter = 20, cores = 1, seed = 1, alternative = alternative
    )
  }

  # Perform default test
  default <- run("two_tailed")

  # Perform centered test
  centered <- run("two_tailed_centered")

  # Perform doubled test
  doubled <- run("two_tailed_doubled")

  # Get differences
  d <- default$differences

  # Check default p-value
  expect_equal(default$result$p_value, signif(permutation_p_value(d, d[1], "two_tailed"), 3))

  # Check centered p-value
  expect_equal(centered$result$p_value, signif(permutation_p_value(d, d[1], "two_tailed_centered"), 3))

  # Check doubled p-value
  expect_equal(doubled$result$p_value, signif(permutation_p_value(d, d[1], "two_tailed_doubled"), 3))

  # Check alternative
  expect_match(centered$result$alternative, "!=")

  # Check that permutations are the same
  expect_identical(default$differences, centered$differences)

  # Check for error with unknown alternative
  expect_error(run("both"), "arg")

})

test_that("undefined permuted differences are reported, and small p-values are not rounded to 0", {

  # Set up differences with a missing value
  differences <- c(0.5, NA, 0.1, -0.2, 0.4)

  # Check warning
  expect_warning(
    p <- permutation_p_value(differences, 0.5, "greater"), "1 of 5 permutations"
  )

  # Check p-value
  expect_equal(p, 1 / 4)

  # Check that significant digits keep value
  expect_gt(signif(1 / 10001, 3), 0)

  # Check that rounding gives zero
  expect_equal(round(1 / 10001, 3), 0)

})

test_that("permutation pools samples with different widths or vocabularies by what the columns mean", {

  # Set up wide sample
  wide <- rbind(c("a", "b", "c", "d"), c("b", "c", "d", "e"))

  # Set up narrow sample
  narrow <- rbind(c("a", "b"), c("c", "d"), c("a", "e"))

  # Align samples
  aligned <- align_samples(wide, narrow)

  # Check width of first sample
  expect_equal(ncol(aligned$sample_1), 4)

  # Check width of second sample
  expect_equal(ncol(aligned$sample_2), 4)

  # Check padding
  expect_true(all(is.na(aligned$sample_2[, 3:4])))

  # Check pooling
  expect_equal(dim(rbind(aligned$sample_1, aligned$sample_2)), c(5, 4))

  # Set up first numeric sample
  one <- matrix(c(1, 0, 1, 1, 1, 0), nrow = 2, dimnames = list(NULL, c("x", "y", "z")))

  # Set up second numeric sample
  two <- matrix(c(1, 1, 0, 1, 1, 0), nrow = 2, dimnames = list(NULL, c("x", "z", "w")))

  # Align samples
  aligned <- align_samples(one, two)

  # Check responses of first sample
  expect_equal(colnames(aligned$sample_1), c("x", "y", "z", "w"))

  # Check responses of second sample
  expect_equal(colnames(aligned$sample_2), c("x", "y", "z", "w"))

  # Check response missing in first sample
  expect_equal(unname(aligned$sample_1[, "w"]), c(0, 0))

  # Check response missing in second sample
  expect_equal(unname(aligned$sample_2[, "y"]), c(0, 0))

  # Check response given in second sample
  expect_equal(unname(aligned$sample_2[, "z"]), unname(two[, "z"]))

  # Reorder responses
  reordered <- one[, c("z", "x", "y")]

  # Align samples
  aligned <- align_samples(one, reordered)

  # Check order of responses
  expect_identical(colnames(aligned$sample_2), colnames(aligned$sample_1))

  # Check values of responses
  expect_equal(unname(aligned$sample_2[, "x"]), unname(one[, "x"]))

  # Check for error with unnamed columns
  expect_error(align_samples(unname(one), unname(two)[, 1:2]), "column names")

  # Check for error with mixed samples
  expect_error(align_samples(one, wide), "both")

  # Copy sample
  duplicated_names <- one

  # Duplicate name
  colnames(duplicated_names)[2] <- "x"

  # Check for error with duplicate names
  expect_error(align_samples(duplicated_names, two), "unique")

})

test_that("permutation_SemNeT accepts raw samples of different widths", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get narrow high group
  high <- open.clean[which(open.group == "High"), 1:15]

  # Check for no error
  expect_no_error(suppressMessages(
    permutation_SemNeT(low, high, method = "nrw", measure = "cc", iter = 4, cores = 1, seed = 1)
  ))

})

test_that("permutation_SemNeT drops responses everyone gave when similarity is a correlation", {

  # Set seed
  set.seed(5)

  # Set up binary samples that share a response everyone gave
  make_sample <- function(n){
    mat <- matrix(rbinom(n * 20, 1, .4), nrow = n, ncol = 20, dimnames = list(NULL, paste0("w", 1:20)))
    cbind(mat, everyone = 1)
  }

  # Perform permutation test
  result <- suppressMessages(permutation_SemNeT(
    make_sample(30), make_sample(30), method = "tmfg", sim = "cor", measure = "aspl", iter = 5, seed = 1
  ))

  # Check that all permutations were estimated
  expect_true(all(is.finite(result$differences)))
  expect_true(all(result$diagnostics$nodes_1 == 20))

  # Check that zero is not a valid minimum
  expect_error(finalize(make_sample(10), minimum_cases = 0), "minimum value")

})

test_that("permutation_SemNeT's observed Q matches bootstrap_SemNeT's, which seeds every iteration's groups alike", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform bootstrap
  boot <- suppressMessages(bootstrap_SemNeT(
    low = low, high = high, method = "tmfg", type = "case", minimum_cases = 2, iter = 4, seed = 42
  ))

  # Perform permutation test
  perm <- suppressMessages(permutation_SemNeT(
    low, high, method = "tmfg", minimum_cases = 2, measure = c("aspl", "cc", "q"), iter = 4, seed = 42
  ))

  # Get bootstrap observed differences
  observed <- boot$groups$low$observed$measures - boot$groups$high$observed$measures

  # Check observed differences
  expect_equal(unname(perm$differences[1, ]), unname(observed[colnames(perm$differences)]))

  # Check permutations do not all share the observed seed
  expect_false(isTRUE(all.equal(perm$differences[2, "q"], perm$differences[1, "q"])))

  # Check that reuse of networks keeps the observed seed
  reused <- suppressMessages(permutation_SemNeT(
    sample_1 = low, sample_2 = high, method = "tmfg", measure = "q", previous = perm
  ))
  expect_equal(unname(reused$differences), unname(perm$differences[, "q"]))

  # Check the shared seed of groups in an iteration
  expect_identical(permutation_seeds(5, 42)[1], 42)
  expect_identical(permutation_seeds(5, 42)[-1], reproducible_seeds(5, 42)[-1])
  expect_identical(permutation_seeds(5, NULL)[1] == 42, FALSE)

})
