#' @title Statistical Tests for \code{\link{bootstrap_SemNeT}}
#'
#' @description Computes statistical tests comparing groups' bootstrapped
#' network measures from one or more \code{\link{bootstrap_SemNeT}} results
#'
#' @param ... One or more \code{bootstrap_SemNeT} objects from
#' \code{\link{bootstrap_SemNeT}}, all describing the same groups. More than
#' one is only useful for \code{type = "node"} results at different values
#' of \code{prop}, compared side by side
#'
#' @param test Character.
#' Type of statistical test: \code{"t-test"} (every pairwise comparison,
#' single-factor designs only), \code{"ANOVA"}, or \code{"ANCOVA"} (controls
#' for each bootstrapped network's number of edges as a covariate, via
#' \code{\link[car]{Anova}} type II/III sums of squares). Defaults to
#' \code{"t-test"}
#'
#' @param measures Character.
#' One or more of \code{"aspl"}, \code{"cc"}, \code{"q"}.
#' Defaults to all three
#'
#' @param formula Character.
#' \strong{Only} for \code{test = "ANOVA"}/\code{"ANCOVA"} with a multi-factor
#' \code{groups} design. A formula with the outcome written as \code{"y"},
#' e.g. \code{formula = "y ~ gf * caq"} for a two-way ANOVA. Defaults to
#' \code{NULL} (every column of \code{groups}, additively or as the single
#' factor's own levels)
#'
#' @param groups Matrix or data frame.
#' One row per group (matched to \code{bootstrap_SemNeT}'s group names by
#' row name if present, otherwise by the order groups were originally
#' entered into \code{bootstrap_SemNeT}), one column per grouping/design
#' variable. Defaults to \code{NULL} (a single factor using each group's own
#' name as its level)
#'
#' @return Returns a list of class \code{"bootstrap_test_SemNeT"}:
#' \item{test}{The \code{test} that was run}
#' \item{measures}{The \code{measures} that were tested}
#' \item{groups}{The (possibly defaulted) \code{groups} design matrix used}
#' \item{results}{A named list, one entry per \code{bootstrap_SemNeT} object
#' in \code{...} (named \code{"Case"}, or \code{"Proportion (<prop>)"} for
#' \code{type = "node"} results), each a named list with one entry per
#' \code{measures}, each a list of: \code{table} (the test's own statistics
#' table), \code{post_hoc} (a Tukey HSD table, only for \code{"ANOVA"}/
#' \code{"ANCOVA"} with a single factor of more than two levels, otherwise
#' \code{NULL}), and \code{adjusted_means} (only for \code{"ANOVA"}/
#' \code{"ANCOVA"}; a single data frame for a one-factor design or a
#' user-supplied interaction \code{formula}, otherwise -- an additive,
#' multi-column \code{groups} design with no \code{formula} -- a named list
#' of one data frame per factor; \code{NULL} for \code{"t-test"})}
#'
#' @examples
#' # Get openness data
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Bootstrap networks
#' boot_result <- bootstrap_SemNeT(
#'   low = low, high = high, iter = 100, cores = 2,
#'   type = "case", method = "tmfg"
#' )
#'
#' # Compute tests
#' bootstrap_test_SemNeT(boot_result)
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats aov as.formula t.test sd TukeyHSD
#' @importFrom utils combn
#'
#' @export
# Statistical tests for bootstrap_SemNeT() results
# Updated 06.09.2026
bootstrap_test_SemNeT <- function(
  ..., test = c("t-test", "ANOVA", "ANCOVA"),
  measures = c("aspl", "cc", "q"),
  formula = NULL, groups = NULL
)
{

  # Validate and coerce input before any computation begins
  validated <- bootstrap_test_SemNeT_errors(list(...), test, measures, formula, groups)
  boot_objects <- validated$boot_objects
  groups_matrix <- validated$groups_matrix

  # `match.arg()` picks the *first* listed choice when missing, which is why
  # "t-test" is listed first above -- it's the documented default. `measures`
  # needs `several.ok = TRUE`: the original omitted it, which meant asking
  # for more than one measure at once (its own documented default!) errored
  test <- match.arg(test)
  measures <- match.arg(measures, c("aspl", "cc", "q"), several.ok = TRUE)

  # One label per `bootstrap_SemNeT` object in `...` -- "Case" for a
  # case-resampled result, or its own node-drop proportion otherwise.
  # `make.unique()` guards against two objects landing on the same label
  # (e.g. two case-resampled objects, or two node-drop objects at an
  # identical `prop`) -- without it, `names(results) <-` would assign a
  # duplicate name, and `$`/`[[` access (the documented way to read a
  # result out of this list) silently returns only the first match,
  # making the second object's results unreachable with no error at all
  condition_labels <- vapply(boot_objects, function(boot_object){
    if(boot_object$meta$type == "node"){
      paste0("Proportion (", boot_object$meta$prop, ")")
    }else{
      "Case"
    }
  }, character(1))
  condition_labels <- make.unique(condition_labels, sep = " #")

  results <- lapply(boot_objects, function(boot_object){

    measure_results <- lapply(measures, function(measure){
      test_one_measure(boot_object, measure, test, formula, groups_matrix)
    })

    names(measure_results) <- measures

    return(measure_results)

  })
  names(results) <- condition_labels

  output <- list(test = test, measures = measures, groups = groups_matrix, results = results)

  class(output) <- "bootstrap_test_SemNeT"

  return(output)

}

#' @noRd
# One measure's test result for one `bootstrap_SemNeT` object -- always
# returns the same 3 named elements regardless of 'test', so callers never
# need to branch on which test was run to know what's available
# Updated 06.09.2026
test_one_measure <- function(boot_object, measure, test, formula, groups_matrix)
{

  frame <- build_measure_frame(boot_object, measure, groups_matrix)

  if(test == "t-test"){
    return(list(table = pairwise_t_test_table(frame), post_hoc = NULL, adjusted_means = NULL))
  }

  return(anova_table(frame, test, formula, groups_matrix))

}

#' @noRd
# One measure's bootstrapped values (plus each network's edge count, for
# ANCOVA), long-format across every group, with the requested grouping
# variable(s) attached
# Updated 06.09.2026
build_measure_frame <- function(boot_object, measure, groups_matrix)
{

  group_names <- names(boot_object$groups)

  rows <- lapply(group_names, function(group_name){

    values <- boot_object$groups[[group_name]]$measures[measure, ]

    # Each bootstrapped network's edge count -- the ANCOVA covariate.
    # `bootstrap_SemNeT()` always computes this at generation time (regardless
    # of its 'keep_networks' setting), so it's read directly here rather than
    # recomputed from the (possibly absent) full networks themselves
    edges <- boot_object$groups[[group_name]]$edges

    data.frame(Measure = values, Edges = edges, Name = group_name, stringsAsFactors = FALSE)

  })

  frame <- do.call(rbind, rows)

  # Group-mean-center the edge count within each group (Bell, Jones, &
  # Fairbrother, 2018, on the dangers of not doing so). A group whose edge
  # count never varies (e.g. every `pf()` network has exactly n - 1 edges)
  # has nothing to center -- `scale()` on a zero-variance vector produces
  # `NaN`, so it's zeroed out directly instead
  for(group_name in group_names){

    rows_in_group <- which(frame$Name == group_name)

    if(length(unique(frame$Edges[rows_in_group])) == 1){
      frame$Edges[rows_in_group] <- 0
    }else{
      frame$Edges[rows_in_group] <- scale(frame$Edges[rows_in_group])
    }

  }

  # Attach the grouping/design variable(s), matched to each row by group name
  group_variables <- as.data.frame(groups_matrix[frame$Name, , drop = FALSE], stringsAsFactors = TRUE)

  return(cbind(frame[, c("Measure", "Edges")], group_variables))

}

#' @noRd
# Every pairwise t-test across a single factor's levels
# Updated 06.09.2026
pairwise_t_test_table <- function(frame)
{

  factor_name <- colnames(frame)[3]
  factor_values <- as.character(frame[[factor_name]])
  levels <- unique(factor_values)

  pairs <- combn(levels, 2)

  rows <- lapply(seq_len(ncol(pairs)), function(pair){

    one <- pairs[1, pair]
    two <- pairs[2, pair]

    values_1 <- frame$Measure[factor_values == one]
    values_2 <- frame$Measure[factor_values == two]

    test_result <- t.test(values_1, values_2, var.equal = TRUE)

    mean_1 <- mean(values_1, na.rm = TRUE)
    mean_2 <- mean(values_2, na.rm = TRUE)

    # An absolute effect size, matching every other test in this package
    # (`response_analysis()`, `cohens_d()`) -- direction is reported
    # separately instead of via a signed value
    direction <- if(test_result$p.value > .05){
      "n.s."
    }else if(mean_1 > mean_2){
      paste(one, ">", two)
    }else{
      paste(one, "<", two)
    }

    data.frame(
      group_1 = one, group_2 = two,
      mean_1 = round(mean_1, 3), sd_1 = round(sd(values_1, na.rm = TRUE), 3),
      mean_2 = round(mean_2, 3), sd_2 = round(sd(values_2, na.rm = TRUE), 3),
      t_statistic = round(unname(test_result$statistic), 3),
      df = round(unname(test_result$parameter), 3),
      p_value = signif(test_result$p.value, 3),
      ci_lower = round(test_result$conf.int[1], 3), ci_upper = round(test_result$conf.int[2], 3),
      cohens_d = round(cohens_d(values_1, values_2), 3),
      direction = direction,
      stringsAsFactors = FALSE
    )

  })

  result <- do.call(rbind, rows)
  row.names(result) <- NULL

  return(result)

}

#' @noRd
# ANOVA/ANCOVA table, adjusted means, and (single-factor, >2 levels) Tukey
# HSD post-hoc comparisons for one measure
# Updated 06.09.2026
anova_table <- function(frame, test, formula, groups_matrix)
{

  factor_names <- colnames(groups_matrix)

  # ANOVA doesn't control for the edge-count covariate -- only ANCOVA does
  if(test == "ANOVA"){
    frame$Edges <- NULL
  }

  # A user-supplied formula (multi-factor designs) always takes precedence;
  # otherwise every grouping variable enters additively (a single factor
  # design is just that one term)
  if(is.null(formula)){
    model_formula <- paste("Measure ~", paste(factor_names, collapse = " + "))
  }else{
    # Word-boundary match -- a plain `gsub("y", ...)` would also corrupt any
    # factor name that merely *contains* a "y" (e.g. "yob", "type"), not just
    # the standalone outcome placeholder
    model_formula <- gsub("\\by\\b", "Measure", formula)
  }

  # The edge-count covariate always enters first, additively
  if(test == "ANCOVA"){
    sides <- strsplit(model_formula, "~")[[1]]
    model_formula <- paste0(sides[1], "~ Edges +", sides[2])
  }

  model <- aov(as.formula(model_formula), data = frame)

  # Type III sums of squares for a genuine multi-factor (interaction)
  # design, type II otherwise -- matches the original's own choice
  anova_type <- if(length(factor_names) > 1) "III" else "II"
  anova_result <- car::Anova(model, type = anova_type)

  # Partial eta squared for every real effect term (everything except the
  # residual row, and the covariate row for ANCOVA) -- computed by term
  # *name* rather than the original's fragile positional index-dropping,
  # which silently produced no eta-squared at all for a single-factor plain
  # ANOVA (there, the only real term sat at the position the original
  # always dropped)
  residual_row <- which(row.names(anova_result) == "Residuals")
  residual_sum_sq <- anova_result$`Sum Sq`[residual_row]
  partial_eta_squared <- anova_result$`Sum Sq` / (anova_result$`Sum Sq` + residual_sum_sq)
  partial_eta_squared[residual_row] <- NA
  if(test == "ANCOVA"){
    partial_eta_squared[row.names(anova_result) == "Edges"] <- NA
  }

  anova_table <- as.data.frame(broom::tidy(anova_result), stringsAsFactors = FALSE)
  anova_table$partial_eta_squared <- round(partial_eta_squared, 3)
  anova_table[, c("sumsq", "statistic", "p.value")] <- round(anova_table[, c("sumsq", "statistic", "p.value")], 3)

  # Adjusted (covariate-controlled) means for the group effect(s). An
  # explicit interaction term (from a user-supplied formula, e.g. "gf*caq")
  # is the fullest description of the design and is used alone. Without one,
  # 'groups' entered additively (the no-'formula' default for a multi-column
  # design) has no single term that summarizes every factor -- each factor's
  # own adjusted means is returned instead of silently keeping only one
  effects_list <- effects::allEffects(model)
  group_effect_terms <- setdiff(names(effects_list), "Edges")
  interaction_terms <- group_effect_terms[grepl(":", group_effect_terms, fixed = TRUE)]
  adjusted_means <- if(length(interaction_terms) > 0){
    as.data.frame(effects_list[[interaction_terms[length(interaction_terms)]]])
  }else if(length(group_effect_terms) == 1){
    as.data.frame(effects_list[[group_effect_terms]])
  }else{
    lapply(effects_list[group_effect_terms], as.data.frame)
  }

  # Pairwise post-hoc comparisons only make sense for a single factor with
  # more than two levels -- with two levels the ANOVA/ANCOVA F-test already
  # is the (only) pairwise comparison
  post_hoc <- NULL
  if(length(factor_names) == 1 && length(unique(frame[[factor_names]])) > 2){
    post_hoc <- as.data.frame(suppressWarnings(TukeyHSD(model))[[factor_names]])
  }

  return(list(table = anova_table, post_hoc = post_hoc, adjusted_means = adjusted_means))

}

#' @noRd
# Argument errors for `bootstrap_test_SemNeT()`
# Updated 06.09.2026
bootstrap_test_SemNeT_errors <- function(boot_objects, test, measures, formula, groups)
{

  # At least one `bootstrap_SemNeT` object is required
  if(length(boot_objects) < 1){
    stop("In `bootstrap_test_SemNeT()`: at least one `bootstrap_SemNeT` object must be provided via `...`.", call. = FALSE)
  }

  if(!all(vapply(boot_objects, inherits, logical(1), what = "bootstrap_SemNeT"))){
    stop(
      "In `bootstrap_test_SemNeT()`: every object passed via `...` must be a `bootstrap_SemNeT` object (from `bootstrap_SemNeT()`).",
      call. = FALSE
    )
  }

  group_names <- names(boot_objects[[1]]$groups)

  # Testing requires at least two groups to compare
  if(length(group_names) < 2){
    stop(
      "In `bootstrap_test_SemNeT()`: a single group cannot be tested against itself.\n",
      "Use `random_network_test()` to test a single group against random networks instead.",
      call. = FALSE
    )
  }

  # Every object being compared must describe the same groups
  same_groups <- vapply(boot_objects, function(x) identical(names(x$groups), group_names), logical(1))
  if(!all(same_groups)){
    stop(
      "In `bootstrap_test_SemNeT()`: every `bootstrap_SemNeT` object passed via `...` must contain the same groups, in the same order.",
      call. = FALSE
    )
  }

  # 'test' must be one (and only one) of the documented options
  test <- match.arg(test, c("t-test", "ANOVA", "ANCOVA"))

  # 'measures' must be one or more of the documented options
  object_error(measures, "vector", "bootstrap_test_SemNeT")
  typeof_error(measures, "character", "bootstrap_test_SemNeT")
  if(!all(measures %in% c("aspl", "cc", "q"))){
    stop("In `bootstrap_test_SemNeT()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'.", call. = FALSE)
  }

  # 'groups' organizes each group into one or more grouping/design variables
  # -- defaults to a single factor using each group's own name as its level
  if(is.null(groups)){

    groups_matrix <- matrix(group_names, ncol = 1, dimnames = list(group_names, "Group"))

  }else{

    object_error(groups, c("matrix", "data.frame"), "bootstrap_test_SemNeT")
    groups_matrix <- as.matrix(groups)

    if(nrow(groups_matrix) != length(group_names)){
      stop(
        paste0(
          "In `bootstrap_test_SemNeT()`: 'groups' has ", nrow(groups_matrix), " rows, but ",
          length(group_names), " groups were bootstrapped.\n",
          "'groups' must have exactly one row per group."
        ),
        call. = FALSE
      )
    }

    # Match rows to groups by name when the row names actually identify
    # them; otherwise fall back to the order groups were originally entered
    # into `bootstrap_SemNeT()` (the original's only supported behavior)
    if(!is.null(row.names(groups_matrix)) && setequal(row.names(groups_matrix), group_names)){
      groups_matrix <- groups_matrix[group_names, , drop = FALSE]
    }else{
      row.names(groups_matrix) <- group_names
    }

    if(is.null(colnames(groups_matrix))){
      colnames(groups_matrix) <- if(ncol(groups_matrix) == 1) "Group" else paste0("Group", seq_len(ncol(groups_matrix)))
    }

  }

  # A t-test is inherently pairwise -- a multi-factor design needs an
  # ANOVA/ANCOVA to test main effects and interactions properly
  if(test == "t-test" && ncol(groups_matrix) > 1){
    stop(
      "In `bootstrap_test_SemNeT()`: multi-factor `groups` are not compatible with `test = \"t-test\"`.\n",
      "Use `test = \"ANOVA\"` or `test = \"ANCOVA\"` instead.",
      call. = FALSE
    )
  }

  # A plain ANOVA on a 2-level single factor duplicates a t-test exactly --
  # ANCOVA is still allowed here, since its edge-count covariate makes it a
  # genuinely different (and useful) test even with just 2 groups
  if(test == "ANOVA" && ncol(groups_matrix) == 1 && length(unique(groups_matrix[, 1])) < 3){
    stop(
      "In `bootstrap_test_SemNeT()`: a single factor with only 2 levels is not compatible with `test = \"ANOVA\"`.\n",
      "Use `test = \"t-test\"` instead.",
      call. = FALSE
    )
  }

  # 'formula' must be a single character string, if supplied at all
  if(!is.null(formula)){
    length_error(formula, 1, "bootstrap_test_SemNeT")
    typeof_error(formula, "character", "bootstrap_test_SemNeT")
  }

  return(list(boot_objects = boot_objects, groups_matrix = groups_matrix))

}
