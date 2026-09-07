# Tests for bootstrap_test_SemNeT()

make_two_group_boot <- function(seed = 1, iter = 20){
  set.seed(seed)
  low <- finalize(open.binary[which(open.group == "Low"), ], 2)
  high <- finalize(open.binary[which(open.group == "High"), ], 2)
  equated <- equate(low = low, high = high)
  suppressMessages(
    bootstrap_SemNeT(low = equated$low, high = equated$high, method = "tmfg", type = "node", prop = .5, iter = iter, cores = 1)
  )
}

make_three_group_boot <- function(seed = 1, iter = 20){
  set.seed(seed)
  mk <- function(s){ set.seed(s); m <- simulate_fluency(25, cases = 150); colnames(m) <- paste0("w", 1:25); m }
  suppressMessages(
    bootstrap_SemNeT(one = mk(seed + 1), two = mk(seed + 2), three = mk(seed + 3), method = "tmfg", type = "node", prop = .6, iter = iter, cores = 1)
  )
}

test_that("bootstrap_test_SemNeT's t-test returns the consistent schema and can be requested for multiple measures at once", {

  boot_result <- make_two_group_boot()

  # Real bug fixed: the original's `match.arg(measures)` (missing
  # `several.ok = TRUE`) errored on its own documented default of all 3
  # measures at once
  result <- bootstrap_test_SemNeT(boot_result, test = "t-test", measures = c("aspl", "cc", "q"))

  expect_s3_class(result, "bootstrap_test_SemNeT")
  expect_named(result, c("test", "measures", "groups", "results"))
  expect_named(result$results, "Proportion (0.5)")
  expect_named(result$results$`Proportion (0.5)`, c("aspl", "cc", "q"))
  expect_named(result$results$`Proportion (0.5)`$aspl, c("table", "post_hoc", "adjusted_means"))
  expect_null(result$results$`Proportion (0.5)`$aspl$post_hoc)
  expect_null(result$results$`Proportion (0.5)`$aspl$adjusted_means)
  expect_equal(nrow(result$results$`Proportion (0.5)`$aspl$table), 1)

})

test_that("bootstrap_test_SemNeT's t-test matches a direct t.test/cohens_d on the same bootstrapped values", {

  boot_result <- make_two_group_boot()

  result <- bootstrap_test_SemNeT(boot_result, test = "t-test", measures = "aspl")
  table <- result$results$`Proportion (0.5)`$aspl$table

  low_values <- boot_result$groups$low$measures["aspl", ]
  high_values <- boot_result$groups$high$measures["aspl", ]

  expected <- t.test(low_values, high_values, var.equal = TRUE)

  expect_equal(table$t_statistic, round(unname(expected$statistic), 3))
  expect_equal(table$cohens_d, round(cohens_d(low_values, high_values), 3))

})

test_that("bootstrap_test_SemNeT disambiguates two `...` objects that would otherwise share a condition label", {

  # Real bug fixed: two `bootstrap_SemNeT` objects of the same `type`/`prop`
  # (or two case-resampled objects) both got the label "Proportion (0.5)" (or
  # "Case"). `names(results) <-` then created a duplicate list name, and `$`/
  # `[[` access -- the documented way to read a result out -- silently
  # returned only the first match, making the second object's results
  # unreachable with no error
  boot_a <- make_two_group_boot(seed = 1)
  boot_b <- make_two_group_boot(seed = 2)

  result <- bootstrap_test_SemNeT(boot_a, boot_b, test = "t-test", measures = "aspl")

  expect_length(result$results, 2)
  expect_length(unique(names(result$results)), 2)

  # Both entries are actually reachable, and aren't the same result reused
  table_1 <- result$results[[1]]$aspl$table
  table_2 <- result$results[[2]]$aspl$table
  expect_false(isTRUE(all.equal(table_1$t_statistic, table_2$t_statistic)))

})

test_that("bootstrap_test_SemNeT's ANOVA works for 3+ groups and reports a Tukey post-hoc", {

  boot_result <- make_three_group_boot()

  result <- bootstrap_test_SemNeT(boot_result, test = "ANOVA", measures = "aspl")
  entry <- result$results$`Proportion (0.6)`$aspl

  expect_true(all(c("term", "sumsq", "df", "statistic", "p.value", "partial_eta_squared") %in% names(entry$table)))
  expect_true("Group" %in% entry$table$term)
  expect_false(is.na(entry$table$partial_eta_squared[entry$table$term == "Group"]))
  expect_true(is.na(entry$table$partial_eta_squared[entry$table$term == "Residuals"]))
  expect_equal(nrow(entry$post_hoc), 3) # 3 pairwise comparisons among 3 groups
  expect_equal(nrow(entry$adjusted_means), 3)

})

test_that("bootstrap_test_SemNeT's ANCOVA is allowed with only two groups and excludes the covariate's eta-squared", {

  boot_result <- make_two_group_boot()

  result <- bootstrap_test_SemNeT(boot_result, test = "ANCOVA", measures = "aspl")
  table <- result$results$`Proportion (0.5)`$aspl$table

  expect_true("Edges" %in% table$term)
  expect_true(is.na(table$partial_eta_squared[table$term == "Edges"]))
  expect_false(is.na(table$partial_eta_squared[table$term == "Group"]))

})

test_that("bootstrap_test_SemNeT's ANCOVA with 3+ groups reports a Tukey post-hoc despite the continuous covariate", {

  # `TukeyHSD()` on a model with a continuous covariate (Edges) alongside the
  # factor of interest (Group) only returns comparisons for the factor --
  # confirms `anova_table()`'s `TukeyHSD(model)[[factor_names]]` indexing
  # doesn't break when a covariate is also in the model
  boot_result <- make_three_group_boot()

  result <- bootstrap_test_SemNeT(boot_result, test = "ANCOVA", measures = "aspl")
  entry <- result$results$`Proportion (0.6)`$aspl

  expect_true("Edges" %in% entry$table$term)
  expect_true("Group" %in% entry$table$term)
  expect_equal(nrow(entry$post_hoc), 3) # 3 pairwise comparisons among 3 groups

})

test_that("bootstrap_test_SemNeT supports a multi-factor design via a custom formula", {

  set.seed(5)
  mk <- function(s){ set.seed(s); m <- simulate_fluency(25, cases = 120); colnames(m) <- paste0("w", 1:25); m }
  boot4 <- suppressMessages(bootstrap_SemNeT(
    hihi = mk(1), hilo = mk(2), lohi = mk(3), lolo = mk(4),
    method = "tmfg", type = "node", prop = .5, iter = 15, cores = 1
  ))

  groups_df <- matrix(
    c("high", "high", "high", "low", "low", "high", "low", "low"),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("hihi", "hilo", "lohi", "lolo"), c("gf", "caq"))
  )

  result <- bootstrap_test_SemNeT(boot4, test = "ANOVA", formula = "y ~ gf*caq", groups = groups_df, measures = "aspl")
  table <- result$results$`Proportion (0.5)`$aspl$table

  expect_true(all(c("gf", "caq", "gf:caq") %in% table$term))
  expect_equal(nrow(result$results$`Proportion (0.5)`$aspl$adjusted_means), 4)

})

test_that("bootstrap_test_SemNeT's additive multi-factor default (no formula) reports every factor's adjusted means", {

  # Real bug fixed: with no interaction term in the model (the documented
  # default for a multi-column 'groups' with no 'formula'), picking the
  # "last" effects::allEffects() term silently reported only one factor's
  # adjusted means and dropped the other entirely
  set.seed(5)
  mk <- function(s){ set.seed(s); m <- simulate_fluency(25, cases = 120); colnames(m) <- paste0("w", 1:25); m }
  boot4 <- suppressMessages(bootstrap_SemNeT(
    hihi = mk(1), hilo = mk(2), lohi = mk(3), lolo = mk(4),
    method = "tmfg", type = "node", prop = .5, iter = 15, cores = 1
  ))

  groups_df <- matrix(
    c("high", "high", "high", "low", "low", "high", "low", "low"),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("hihi", "hilo", "lohi", "lolo"), c("gf", "caq"))
  )

  result <- bootstrap_test_SemNeT(boot4, test = "ANOVA", groups = groups_df, measures = "aspl")
  table <- result$results$`Proportion (0.5)`$aspl$table
  adjusted_means <- result$results$`Proportion (0.5)`$aspl$adjusted_means

  expect_true(all(c("gf", "caq") %in% table$term))
  expect_false("gf:caq" %in% table$term)
  expect_type(adjusted_means, "list")
  expect_named(adjusted_means, c("gf", "caq"))
  expect_true(all(vapply(adjusted_means, is.data.frame, logical(1))))

})

test_that("bootstrap_test_SemNeT's custom formula only replaces the standalone 'y' outcome placeholder", {

  # Real bug fixed: `gsub("y", "Measure", formula)` corrupted any factor name
  # that merely *contained* a "y" (e.g. "yob"), not just the placeholder --
  # a word-boundary match fixes this
  set.seed(5)
  mk <- function(s){ set.seed(s); m <- simulate_fluency(25, cases = 120); colnames(m) <- paste0("w", 1:25); m }
  boot4 <- suppressMessages(bootstrap_SemNeT(
    hihi = mk(1), hilo = mk(2), lohi = mk(3), lolo = mk(4),
    method = "tmfg", type = "node", prop = .5, iter = 15, cores = 1
  ))

  groups_df <- matrix(
    c("high", "high", "high", "low", "low", "high", "low", "low"),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("hihi", "hilo", "lohi", "lolo"), c("gf", "yob"))
  )

  result <- bootstrap_test_SemNeT(boot4, test = "ANOVA", formula = "y ~ gf*yob", groups = groups_df, measures = "aspl")
  table <- result$results$`Proportion (0.5)`$aspl$table

  expect_true(all(c("gf", "yob", "gf:yob") %in% table$term))

})

test_that("bootstrap_test_SemNeT validates its input", {

  boot_result <- make_two_group_boot()
  boot_result_bad_class <- boot_result
  class(boot_result_bad_class) <- "not_a_boot_object"

  expect_error(bootstrap_test_SemNeT(boot_result_bad_class), "bootstrap_SemNeT")
  expect_error(bootstrap_test_SemNeT(boot_result, measures = c("aspl", "not_a_measure")), "bootstrap_test_SemNeT")

  three_group <- make_three_group_boot()
  groups_df <- matrix(c("a", "b"), ncol = 2, dimnames = list("one", c("g1", "g2")))
  expect_error(
    bootstrap_test_SemNeT(three_group, test = "t-test", groups = matrix(c("a", "a", "b", "b", "c", "c"), ncol = 2, dimnames = list(c("one", "two", "three"), c("g1", "g2")))),
    "multi-factor"
  )
  expect_error(bootstrap_test_SemNeT(boot_result, test = "ANOVA"), "2 levels")

})
