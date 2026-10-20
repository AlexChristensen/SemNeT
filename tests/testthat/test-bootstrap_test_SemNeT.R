# Tests for bootstrap_test_SemNeT()

# Make fake bootstrap ----
# Updated 13.10.2026
make_fake_boot <- function(values, type = "case", observed = "median")
{

  # Loop over groups
  groups <- lapply(names(values), function(group_name){

    # Get ASPL of group
    aspl <- values[[group_name]]

    # Set up measures
    measures <- rbind(aspl = aspl, cc = aspl / 10, q = aspl / 100)

    # Get observed ASPL
    obs <- if(identical(observed, "median")){

      # Use median
      stats::median(aspl, na.rm = TRUE)

    }else if(is.list(observed)){

      # Use given value
      observed[[group_name]]

    }else{

      # Use no value
      NA

    }

    # Return group
    list(
      networks = NULL, measures = measures, edges = rep(10, length(aspl)), nodes = rep(20, length(aspl)),
      observed = if(is.na(obs)) NULL else list(measures = c(aspl = obs, cc = obs / 10, q = obs / 100), edges = 10, nodes = 20),
      summary = NULL
    )

  })

  # Name groups
  names(groups) <- names(values)

  # Set up bootstrap
  boot <- list(
    groups = groups,
    meta = list(prop = if(type == "node") .5 else NULL, iter = length(values[[1]]), type = type, method = "nrw")
  )

  # Set class
  class(boot) <- "bootstrap_SemNeT"

  # Return bootstrap
  return(boot)

}

test_that("bootstrap_test_SemNeT returns a percentile CI table of within-iteration contrasts", {

  # Set seed
  set.seed(1)

  # Sample first group
  a <- rnorm(2000, mean = 2.0, sd = .1)

  # Sample second group
  b <- rnorm(2000, mean = 2.3, sd = .1)

  # Make bootstrap
  boot <- make_fake_boot(list(low = a, high = b))

  # Perform tests
  result <- bootstrap_test_SemNeT(boot, measures = c("aspl", "cc", "q"), ci_type = "percentile")

  # Check class
  expect_s3_class(result, "bootstrap_test_SemNeT")

  # Check names
  expect_named(result, c("test", "measures", "groups", "results", "meta", "ci_level", "adjust", "ci_type", "legacy"))

  # Check for no legacy tests
  expect_false(result$legacy)

  # Check conditions
  expect_named(result$results, "Case")

  # Check measures
  expect_named(result$results$Case, c("aspl", "cc", "q"))

  # Check elements of measure
  expect_named(result$results$Case$aspl, c("table", "differences"))

  # Get table
  table <- result$results$Case$aspl$table

  # Check rows
  expect_equal(nrow(table), 1)

  # Check columns
  expect_named(table, c(
    "term", "contrast", "mean_1", "se_1", "mean_2", "se_2", "observed_1", "observed_2", "mean_diff", "observed_diff", "bias", "se", "bias_se", "z",
    "ci_lower", "ci_upper", "p_value", "n_boot", "direction"
  ))

  # Compute differences
  d <- a - b

  # Check bias relative to standard error
  expect_equal(table$bias_se, abs(mean(d) - table$observed_diff) / sd(d))

  # Check contrast
  expect_equal(table$contrast, "low - high")

  # Check mean difference
  expect_equal(table$mean_diff, mean(d))

  # Check standard error
  expect_equal(table$se, sd(d))

  # Check lower bound
  expect_equal(table$ci_lower, unname(quantile(d, .025)))

  # Check upper bound
  expect_equal(table$ci_upper, unname(quantile(d, .975)))

  # Check number of samples
  expect_equal(table$n_boot, 2000)

  # Check direction
  expect_equal(table$direction, "low < high")

  # Check for no p-value
  expect_true(is.na(table$p_value))

  # Check dimensions of differences
  expect_equal(dim(result$results$Case$aspl$differences), c(2000, 1))

})

test_that("bootstrap_test_SemNeT uses normal intervals by default", {

  # Set seed
  set.seed(1)

  # Sample first group
  a <- rnorm(2000, mean = 2.0, sd = .1)

  # Sample second group
  b <- rnorm(2000, mean = 2.3, sd = .1)

  # Make bootstrap
  boot <- make_fake_boot(list(low = a, high = b))

  # Perform tests
  result <- bootstrap_test_SemNeT(boot, measures = "aspl")

  # Check interval type
  expect_equal(result$ci_type, "normal")

  # Get table
  table <- result$results$Case$aspl$table

  # Compute differences
  d <- a - b

  # Get observed difference
  observed <- table$observed_diff

  # Check lower bound
  expect_lt(abs(table$ci_lower - (observed + qnorm(.025) * sd(d))), 1e-3)

  # Check upper bound
  expect_lt(abs(table$ci_upper - (observed + qnorm(.975) * sd(d))), 1e-3)

  # Check p-value
  expect_lt(abs(table$p_value - 2 * pnorm(-abs(observed) / sd(d))), 1e-3)

  # Check direction
  expect_equal(table$direction, "low < high")

  # Check Wald statistic
  expect_lt(abs(table$z - observed / sd(d)), 1e-2)

  # Check p-value from Wald statistic
  expect_lt(abs(table$p_value - 2 * pnorm(-abs(table$z))), 1e-3)

  # Check for no Wald statistic with percentile intervals
  expect_true(is.na(bootstrap_test_SemNeT(boot, measures = "aspl", ci_type = "percentile")$results$Case$aspl$table$z))

  # Perform tests with higher level
  wide <- bootstrap_test_SemNeT(boot, measures = "aspl", ci_level = .99)$results$Case$aspl$table

  # Check lower bound
  expect_lt(wide$ci_lower, table$ci_lower)

  # Check upper bound
  expect_gt(wide$ci_upper, table$ci_upper)

  # Make bootstrap without observed statistics
  boot_no_obs <- make_fake_boot(list(low = a, high = b), observed = NA)

  # Check message
  expect_message(
    table_no_obs <- bootstrap_test_SemNeT(boot_no_obs, measures = "aspl")$results$Case$aspl$table,
    "no observed"
  )

  # Check for missing statistics
  expect_true(all(is.na(table_no_obs[, c("z", "ci_lower", "ci_upper", "p_value")])))

  # Check standard error
  expect_equal(table_no_obs$se, sd(d))

})

test_that("normal-interval p-values agree with whether the interval excludes 0", {

  # Set seed
  set.seed(3)

  # Make bootstrap
  boot <- make_fake_boot(list(
    a = rnorm(1000, 2.00, .1), b = rnorm(1000, 2.02, .1), c = rnorm(1000, 2.05, .1)
  ))

  # Loop over adjustments
  for(adjust in c("none", "bonferroni")){

    # Loop over levels
    for(level in c(.8, .9, .95, .99)){

      # Get table
      table <- bootstrap_test_SemNeT(boot, measures = "aspl", ci_level = level, adjust = adjust)$results$Case$aspl$table

      # Identify intervals excluding zero
      excludes_zero <- table$ci_lower > 0 | table$ci_upper < 0

      # Check agreement with p-value
      expect_equal(table$p_value < 1 - level, excludes_zero)

    }

  }

})

test_that("bootstrap_test_SemNeT intervals do not shrink as the number of bootstrap iterations grows", {

  # Set seed
  set.seed(2)

  # Sample first group
  a <- rnorm(500, mean = 2.00, sd = .1)

  # Sample second group
  b <- rnorm(500, mean = 2.01, sd = .1)

  # Make small bootstrap
  small <- make_fake_boot(list(a = a, b = b))

  # Make large bootstrap
  large <- make_fake_boot(list(a = rep(a, 4), b = rep(b, 4)))

  # Perform tests on small bootstrap
  table_small <- bootstrap_test_SemNeT(small, measures = "aspl")$results$Case$aspl$table

  # Perform tests on large bootstrap
  table_large <- bootstrap_test_SemNeT(large, measures = "aspl")$results$Case$aspl$table

  # Check standard error
  expect_equal(table_small$se, table_large$se, tolerance = .01)

  # Check lower bound
  expect_equal(table_small$ci_lower, table_large$ci_lower, tolerance = .01)

  # Check upper bound
  expect_equal(table_small$ci_upper, table_large$ci_upper, tolerance = .01)

  # Perform legacy tests on small bootstrap
  legacy_small <- suppressWarnings(bootstrap_test_SemNeT(small, measures = "aspl", legacy = TRUE))$results$Case$aspl$table

  # Perform legacy tests on large bootstrap
  legacy_large <- suppressWarnings(bootstrap_test_SemNeT(large, measures = "aspl", legacy = TRUE))$results$Case$aspl$table

  # Check that legacy statistic grows
  expect_equal(abs(legacy_large$t_statistic / legacy_small$t_statistic), 2, tolerance = .01)

})

test_that("bootstrap_test_SemNeT tests every pairwise contrast for 3+ groups", {

  # Set seed
  set.seed(3)

  # Make bootstrap
  boot <- make_fake_boot(list(one = rnorm(300, 1, .1), two = rnorm(300, 2, .1), three = rnorm(300, 3, .1)))

  # Get table
  table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Check contrasts
  expect_equal(table$contrast, c("one - two", "one - three", "two - three"))

  # Check directions
  expect_true(all(table$direction != "n.s."))

})

test_that("bootstrap_test_SemNeT's 'bonferroni' widens intervals and scales p-values", {

  # Set seed
  set.seed(4)

  # Make bootstrap
  boot <- make_fake_boot(list(one = rnorm(1000, 1, .5), two = rnorm(1000, 1.1, .5), three = rnorm(1000, 1.2, .5)))

  # Get unadjusted table
  none <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Get adjusted table
  bonferroni <- bootstrap_test_SemNeT(boot, measures = "aspl", adjust = "bonferroni")$results$Case$aspl$table

  # Check lower bounds
  expect_true(all(bonferroni$ci_lower <= none$ci_lower))

  # Check upper bounds
  expect_true(all(bonferroni$ci_upper >= none$ci_upper))

  # Check p-values
  expect_equal(bonferroni$p_value, pmin(1, none$p_value * 3), tolerance = 1e-8)

})

test_that("bootstrap_test_SemNeT supports 'ci_level'", {

  # Set seed
  set.seed(5)

  # Make bootstrap
  boot <- make_fake_boot(list(a = rnorm(1000), b = rnorm(1000)))

  # Get narrow table
  narrow <- bootstrap_test_SemNeT(boot, measures = "aspl", ci_level = .8)$results$Case$aspl$table

  # Get wide table
  wide <- bootstrap_test_SemNeT(boot, measures = "aspl", ci_level = .99)$results$Case$aspl$table

  # Check lower bound
  expect_true(wide$ci_lower < narrow$ci_lower)

  # Check upper bound
  expect_true(wide$ci_upper > narrow$ci_upper)

})

test_that("bootstrap_test_SemNeT ignores missing bootstrap values pairwise and reports n_boot", {

  # Set seed
  set.seed(6)

  # Sample first group
  a <- rnorm(100)

  # Sample second group
  b <- rnorm(100)

  # Set missing values
  a[1:10] <- NA

  # Make bootstrap
  boot <- make_fake_boot(list(a = a, b = b))

  # Get table
  table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Check number of samples
  expect_equal(table$n_boot, 90)

  # Check lower bound
  expect_false(is.na(table$ci_lower))

})

test_that("bootstrap_test_SemNeT tests main effects of a multi-factor design and interactions via a formula", {

  # Set seed
  set.seed(8)

  # Set number of samples
  n <- 400

  # Sample high-high group
  hihi <- rnorm(n, 1)

  # Sample high-low group
  hilo <- rnorm(n, 2)

  # Sample low-high group
  lohi <- rnorm(n, 3)

  # Sample low-low group
  lolo <- rnorm(n, 5)

  # Make bootstrap
  boot <- make_fake_boot(list(hihi = hihi, hilo = hilo, lohi = lohi, lolo = lolo))

  # Set up design
  groups_df <- matrix(
    c("high", "high", "high", "low", "low", "high", "low", "low"),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("hihi", "hilo", "lohi", "lolo"), c("gf", "caq"))
  )

  # Test main effects
  main <- bootstrap_test_SemNeT(boot, groups = groups_df, measures = "aspl")$results$Case$aspl

  # Check terms
  expect_equal(main$table$term, c("gf", "caq"))

  # Check contrasts
  expect_equal(main$table$contrast, c("high - low", "high - low"))

  # Check first main effect
  expect_equal(main$table$mean_diff[1], mean((hihi + hilo) / 2 - (lohi + lolo) / 2))

  # Check second main effect
  expect_equal(main$table$mean_diff[2], mean((hihi + lohi) / 2 - (hilo + lolo) / 2))

  # Test with interaction
  full <- bootstrap_test_SemNeT(boot, groups = groups_df, formula = "y ~ gf * caq", measures = "aspl")$results$Case$aspl

  # Check terms
  expect_equal(full$table$term, c("gf", "caq", "gf:caq"))

  # Check interaction
  expect_equal(full$table$mean_diff[3], mean((hihi - hilo) - (lohi - lolo)))

  # Check for no mean of side
  expect_true(is.na(full$table$mean_1[3]))

  # Check dimensions of differences
  expect_equal(dim(full$differences), c(n, 3))

})

test_that("bootstrap_test_SemNeT's custom formula only replaces the standalone 'y' outcome placeholder", {

  # Set seed
  set.seed(9)

  # Make bootstrap
  boot <- make_fake_boot(list(hihi = rnorm(50), hilo = rnorm(50), lohi = rnorm(50), lolo = rnorm(50)))

  # Set up design
  groups_df <- matrix(
    c("high", "high", "high", "low", "low", "high", "low", "low"),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("hihi", "hilo", "lohi", "lolo"), c("gf", "yob"))
  )

  # Get table
  table <- bootstrap_test_SemNeT(boot, formula = "y ~ gf*yob", groups = groups_df, measures = "aspl")$results$Case$aspl$table

  # Check terms
  expect_true(all(c("gf", "yob", "gf:yob") %in% table$term))

})

test_that("bootstrap_test_SemNeT reports no minimal detectable effect and adjusts intervals", {

  # Set seed
  set.seed(22)

  # Sample first group
  a <- rnorm(1000, 2.0, .1)

  # Sample second group
  b <- rnorm(1000, 2.1, .1)

  # Make bootstrap
  boot <- make_fake_boot(list(a = a, b = b))

  # Compute differences
  d <- a - b

  # Get table
  table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Check that there is no minimal detectable effect
  expect_false("mde" %in% names(table))

  # Make bootstrap of three groups
  three <- make_fake_boot(list(a = a, b = b, c = rnorm(1000, 2.2, .1)))

  # Get adjusted table
  bonferroni <- bootstrap_test_SemNeT(three, measures = "aspl", adjust = "bonferroni")$results$Case$aspl$table

  # Get unadjusted table
  unadjusted <- bootstrap_test_SemNeT(three, measures = "aspl")$results$Case$aspl$table

  # Check that adjusted intervals are wider
  expect_true(all(bonferroni$ci_upper - bonferroni$ci_lower > unadjusted$ci_upper - unadjusted$ci_lower))

  # Check that percentile intervals have no minimal detectable effect
  expect_false("mde" %in% names(bootstrap_test_SemNeT(boot, measures = "aspl", ci_type = "percentile")$results$Case$aspl$table))

})

test_that("bootstrap differences that do not vary give an undefined test, not p = 0", {

  # Make bootstrap
  boot <- make_fake_boot(list(a = rep(2, 20), b = rep(3, 20)), observed = list(a = 2, b = 3))

  # Check warning
  expect_warning(
    result <- bootstrap_test_SemNeT(boot, measures = "aspl"), "do not vary"
  )

  # Get table
  table <- result$results$Case$aspl$table

  # Check p-value
  expect_true(is.na(table$p_value))

  # Check Wald statistic
  expect_true(is.na(table$z))

  # Check lower bound
  expect_true(is.na(table$ci_lower))

  # Check direction
  expect_equal(table$direction, "undefined")

  # Check standard error
  expect_equal(table$se, 0)

})

test_that("a very large |z| gives a tiny positive p-value, not exactly 0", {

  # Set seed
  set.seed(9)

  # Make bootstrap
  boot <- make_fake_boot(list(low = rnorm(500, 2, .05), high = rnorm(500, 3, .05)))

  # Get table
  table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Check Wald statistic
  expect_gt(abs(table$z), 9)

  # Check p-value above zero
  expect_gt(table$p_value, 0)

  # Check p-value
  expect_equal(table$p_value, 2 * pnorm(-abs(table$z)))

})

test_that("stored statistics are not rounded, so they agree exactly with each other", {

  # Set seed
  set.seed(8)

  # Make bootstrap
  boot <- make_fake_boot(list(low = rnorm(500, 2.0123, .0337), high = rnorm(500, 2.0771, .0341)))

  # Get table
  table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table

  # Check Wald statistic
  expect_equal(table$z, table$observed_diff / table$se)

  # Check p-value
  expect_equal(table$p_value, 2 * pnorm(-abs(table$z)))

  # Check lower bound
  expect_equal(table$ci_lower, table$observed_diff + qnorm(.025) * table$se)

  # Check upper bound
  expect_equal(table$ci_upper, table$observed_diff + qnorm(.975) * table$se)

  # Check that standard error is not rounded
  expect_false(isTRUE(all.equal(table$se, round(table$se, 4), tolerance = 0)))

  # Capture printed output
  printed <- paste(capture.output(print(bootstrap_test_SemNeT(boot, measures = "aspl"))), collapse = "\n")

  # Check printed output
  expect_match(printed, "ASPL")

  # Check observed group values
  expect_equal(table$observed_1, unname(boot$groups$low$observed$measures["aspl"]))
  expect_equal(table$observed_2, unname(boot$groups$high$observed$measures["aspl"]))
  expect_equal(table$observed_1 - table$observed_2, table$observed_diff)

  # Check that groups are shown in the printed table
  expect_match(printed, "low\\s+high")
  expect_match(printed, format_number(table$observed_1), fixed = TRUE)

})
