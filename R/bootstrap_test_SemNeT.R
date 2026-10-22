#' @title Statistical Tests for \code{\link{bootstrap_SemNeT}}
#'
#' @description Compares groups' bootstrapped network measures using Wald tests
#' and confidence intervals of the within-iteration group contrasts. The
#' bootstrap samples are never treated as independent observations
#'
#' @param ... One or more \code{\link{bootstrap_SemNeT}} objects
#' of the same groups.
#' More than one is useful for \code{type = "node"} results at different
#' \code{prop} values or for \code{type = "case"} results from different seeds.
#' \strong{Only} \code{type = "case"} results support inference about groups
#' of participants (see \strong{Details})
#'
#' @param measures Character.
#' One or more of \code{"aspl"}, \code{"cc"}, and \code{"q"}.
#' Defaults to all three
#'
#' @param groups Matrix or data frame.
#' One row per group (matched to the group names by row name if present,
#' otherwise by the order groups were entered), one column per design variable.
#' With one variable, every pair of its levels is compared; with several,
#' the main effect of each variable is compared (the unweighted average of the
#' groups at each level).
#' Defaults to \code{NULL} (one variable using the group names)
#'
#' @param formula Character (length = 1).
#' \strong{Only} for multiple variables in \code{groups}.
#' Outcome written as \code{"y"}, e.g., \code{"y ~ gf * caq"}, to add two-way
#' interaction contrasts (a difference of differences). Requires a fully
#' crossed design; higher-order interactions are not supported.
#' Defaults to \code{NULL} (main effects only)
#'
#' @param ci_level Numeric (length = 1).
#' Confidence level between 0 and 1 (exclusive).
#' Defaults to \code{.95}.
#' At least 1000 bootstrap iterations are recommended
#'
#' @param adjust Character (length = 1).
#' Multiplicity adjustment across the comparisons within each measure. Nothing
#' is ever adjusted across the measures or across the objects passed in
#' \code{...}; with several comparisons and \code{"none"}, each is an
#' unadjusted test.
#' Defaults to \code{"none"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"none"} --- No adjustment
#'
#' \item \code{"bonferroni"} --- Widens each interval to the
#' \code{1 - (1 - ci_level) / m} level and multiplies each \emph{p}-value by
#' \code{m}, the number of comparisons
#'
#' }
#'
#' @param ci_type Character (length = 1).
#' Type of test and interval (not used for node results).
#' Defaults to \code{"normal"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"normal"} --- Wald test and interval: the observed difference
#' plus and minus the normal quantile times the bootstrap standard error.
#' Assumes the bootstrap distribution is roughly normal and needs fewer
#' iterations than the percentile-based intervals. Requires the groups'
#' observed statistics (\code{NA} without them, with a message)
#'
#' \item \code{"bc"} --- Bias-corrected percentile interval (Efron & Tibshirani, 1993).
#' Requires the groups' observed statistics (falls back to \code{"percentile"}
#' without them, with a message)
#'
#' \item \code{"percentile"} --- Percentile interval (e.g., the 2.5th and 97.5th
#' percentiles of the within-iteration differences at \code{ci_level = .95})
#'
#' }
#'
#' @param test Character (length = 1).
#' \strong{Only} for \code{legacy = TRUE}: \code{"t-test"}, \code{"ANOVA"},
#' or \code{"ANCOVA"}.
#' Defaults to \code{NULL} (\code{"t-test"} when \code{legacy = TRUE}).
#' Supplying it with \code{legacy = FALSE} is an error
#'
#' @param legacy Boolean (length = 1).
#' Should the parametric tests on bootstrap replicates from versions before
#' 2.1.0 be reproduced?
#' \strong{These tests are statistically invalid}: they treat bootstrap samples
#' as independent observations and divide the bootstrap standard deviation
#' (already a standard error) by \code{sqrt(iter)}, inflating test statistics
#' by about \code{sqrt(iter)}. A warning is always issued.
#' Defaults to \code{FALSE}
#'
#' @details Always run \code{\link{permutation_SemNeT}} alongside this function
#' and report both: the bootstrap shows how precisely each group's networks and
#' their difference are estimated, whereas the permutation test asks whether
#' group membership matters.
#'
#' Groups are bootstrapped independently, so the difference between
#' two groups' measures within a bootstrap iteration is a draw from the
#' bootstrap distribution of the group difference.
#'
#' With \code{ci_type = "normal"} (default), the test statistic is the Wald
#' statistic \eqn{z = \hat\Delta / \mathrm{SD}^*}, where \eqn{\hat\Delta} is the
#' observed contrast and \eqn{\mathrm{SD}^*} is the standard deviation of the
#' within-iteration contrasts (the bootstrap standard error). The
#' \emph{p}-value is \eqn{2\{1-\Phi(|z|)\}} and matches the interval
#' \eqn{\hat\Delta \pm z_{1-\alpha/2}\,\mathrm{SD}^*}. The null hypothesis is
#' that the contrast \emph{at the study's sample sizes} is zero, not the
#' large-sample contrast, because network measures depend on sample size. The
#' test relies on a normal approximation and was conservative in permuted-data
#' checks. When group labels can be permuted, \code{\link{permutation_SemNeT}}
#' is the exact test and should be preferred.
#'
#' For \code{ci_type = "bc"} and \code{"percentile"}, a difference is reliable if
#' the interval excludes 0 (Tu et al., under review). No \emph{p}-value is
#' reported (\code{p_value} is \code{NA}): the smallest level at which such an
#' interval excludes 0 is not a null-distribution \emph{p}-value and was easily
#' mistaken for one. Use \code{ci_type = "normal"} for a Wald \emph{p}-value, or
#' \code{\link{permutation_SemNeT}} for a test. Case resampling changes
#' each resampled network's size, so the bootstrap distribution is not centred on
#' the full-data statistic (reported as \code{bias}).
#'
#' Node results (\code{type = "node"}) subsample words, not participants, and
#' the subsample sizes change the measures themselves, so they are summarized
#' descriptively instead of tested.
#'
#' @return Returns a list containing:
#'
#' \item{test}{\code{"bootstrap CI"} or the legacy \code{test}}
#'
#' \item{measures}{The measures that were tested}
#'
#' \item{groups}{The \code{groups} design matrix that was used}
#'
#' \item{results}{A named list with one list per object in \code{...}
#' (\code{"Case"} or \code{"Proportion (<prop>)"}), each with one list per
#' measure containing \code{table} and \code{differences} (an \code{iter} x
#' comparisons matrix of the within-iteration contrasts). The \code{table}'s
#' numbers are not rounded (the printed tables format them), so \code{z},
#' \code{p_value} and the interval agree exactly with \code{observed_diff} and
#' \code{se}; for \code{legacy = TRUE}, \code{table}, \code{post_hoc}, and
#' \code{adjusted_means}.
#'
#' \code{table} has one row per comparison: \code{term}, \code{contrast},
#' \code{mean_1}, \code{se_1}, \code{mean_2}, and \code{se_2} (the bootstrap mean
#' and standard error of each side; \code{NA} for interactions),
#' \code{observed_1} and \code{observed_2} (the full-sample value of each side;
#' \code{NA} for interactions), \code{mean_diff}, \code{observed_diff}, \code{bias}
#' (\code{mean_diff - observed_diff}), \code{se} (of the difference),
#' \code{bias_se} (\code{|bias| / se}), \code{z}
#' (Wald statistic; \code{ci_type = "normal"} only),
#' \code{ci_lower}, \code{ci_upper}, \code{p_value} (Wald test;
#' \code{ci_type = "normal"} only, otherwise \code{NA}), \code{n_boot}, and
#' \code{direction} (\code{"n.s."} when the interval includes 0).
#'
#' For node results, \code{table} has \code{term}, \code{contrast},
#' \code{observed_1}, \code{observed_2}, \code{observed_diff}, \code{mean_diff}, \code{sd_diff}, \code{q_lower} and
#' \code{q_upper} (central \code{ci_level} range of the subsample differences),
#' \code{prop_same_sign} (share with the observed difference's sign), and
#' \code{n_boot}: a robustness summary with no \emph{p}-value or interval}
#'
#' \item{meta}{A named list like \code{results} containing \code{type},
#' \code{method}, \code{iter}, \code{seed} (\code{NA} if none), \code{prop}
#' (node results only), and \code{networks} (each group's mean \code{nodes} and
#' \code{edges} across bootstrapped networks and its \code{observed_nodes} and
#' \code{observed_edges})}
#'
#' \item{ci_level}{The confidence level used}
#'
#' \item{adjust}{The multiplicity adjustment used}
#'
#' \item{ci_type}{The test and interval type used}
#'
#' \item{legacy}{Whether the legacy (invalid) tests were run}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' # Get openness data
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Bootstrap networks
#' boot_result <- bootstrap_SemNeT(
#'   low = low, high = high, iter = 1000, cores = 2,
#'   type = "case", method = "tmfg"
#' )
#'
#' # Wald tests of the group differences
#' bootstrap_test_SemNeT(boot_result)
#'
#' # Percentile intervals
#' bootstrap_test_SemNeT(boot_result, ci_type = "percentile")
#' }
#'
#' @references
#' \strong{Bootstrap} \cr
#' Efron, B., & Tibshirani, R. J. (1993).
#' \emph{An introduction to the bootstrap}.
#' Chapman & Hall.
#'
#' \strong{Inflated bootstrap statistics} \cr
#' Tu, C.-Y., Chodorow, M., Brooks, P. J., Ezrina, E., & Valian, V. (under review).
#' Inflated statistics lead to incorrect inference: How (not) to make
#' cognitive network group comparisons.
#'
#' @seealso \code{\link{permutation_SemNeT}} for a permutation test and
#' \code{\link{bootstrap_SemNeT}} for generating the bootstrap samples
#'
#' @export
# Bootstrap tests ----
# Updated 12.10.2026
bootstrap_test_SemNeT <- function(
  ..., measures = c("aspl", "cc", "q"), groups = NULL, formula = NULL,
  ci_level = .95, adjust = c("none", "bonferroni"), ci_type = c("normal", "bc", "percentile"),
  test = NULL, legacy = FALSE
)
{

  # Argument errors
  validated <- bootstrap_test_SemNeT_errors(
    list(...), measures, groups, formula, ci_level, adjust, ci_type,
    test, legacy
  )

  # Get bootstrap objects
  boot_objects <- validated$boot_objects

  # Get design of groups
  groups_matrix <- validated$groups_matrix

  # Get test
  test <- validated$test

  # Set adjustment
  adjust <- match.arg(adjust)

  # Set interval type
  ci_type <- match.arg(ci_type)

  # Set measures
  measures <- match.arg(measures, c("aspl", "cc", "q"), several.ok = TRUE)

  # Check for legacy tests
  if(legacy){

    # Send warning
    legacy_inference_warning(
      "bootstrap_test_SemNeT",
      "Use `bootstrap_test_SemNeT()` with `legacy = FALSE` (bootstrap CIs) or `permutation_SemNeT()` instead."
    )

  }

  # Check for tests needing observed statistics
  if(!legacy && ci_type %in% c("normal", "bc")){

    # Identify objects with observed statistics
    has_observed <- vapply(boot_objects, function(boot_object){
      boot_object$meta$type == "node" || all(vapply(boot_object$groups, function(group){
        !is.null(group$observed) && all(is.finite(group$observed$measures))
      }, logical(1)))
    }, logical(1))

    # Check for missing observed statistics
    if(!all(has_observed)){

      # Send message
      message(
        "In `bootstrap_test_SemNeT()`: some `bootstrap_SemNeT` objects have no observed (full-sample) statistics, ",
        if(ci_type == "bc"){
          "so bias correction is unavailable for them -- using percentile intervals instead."
        }else{
          "so the Wald test and normal intervals are unavailable for them -- their `z`, interval, and `p_value` are `NA`.\nRe-run `bootstrap_SemNeT()` or use `ci_type = \"percentile\"`."
        }
      )

    }

  }

  # Get label of each object
  condition_labels <- vapply(boot_objects, function(boot_object){

    # Check for node type
    if(boot_object$meta$type == "node"){

      # Label by proportion
      paste0("Proportion (", boot_object$meta$prop, ")")

    }else{

      # Label as case
      "Case"

    }

  }, character(1))

  # Make labels unique
  condition_labels <- make.unique(condition_labels, sep = " #")

  # Get contrasts of design
  contrasts <- if(!legacy) bootstrap_contrasts(groups_matrix, formula) else NULL

  # Loop over bootstrap objects
  results <- lapply(boot_objects, function(boot_object){

    # Loop over measures
    measure_results <- lapply(measures, function(measure){

      # Check for legacy tests
      if(legacy){

        # Perform legacy test
        legacy_test_one_measure(boot_object, measure, test, formula, groups_matrix)

      }else if(boot_object$meta$type == "node"){

        # Summarize robustness of nodes
        robustness_test_one_measure(boot_object, measure, contrasts, ci_level)

      }else{

        # Perform contrast tests
        contrast_test_one_measure(
          boot_object, measure, contrasts, ci_level, adjust, ci_type
        )

      }

    })

    # Name results by measure
    names(measure_results) <- measures

    # Return results of object
    return(measure_results)

  })

  # Name results by condition
  names(results) <- condition_labels

  # Get settings of each object
  meta <- lapply(boot_objects, bootstrap_meta)

  # Name settings by condition
  names(meta) <- condition_labels

  # Set up output
  output <- list(
    test = if(legacy) test else "bootstrap CI",
    measures = measures, groups = groups_matrix, results = results, meta = meta,
    ci_level = ci_level, adjust = adjust, ci_type = ci_type, legacy = legacy
  )

  # Set class
  class(output) <- "bootstrap_test_SemNeT"

  # Return output
  return(output)

}

#' @noRd
# Display of bootstrap tests ----
# Updated 22.10.2026
bootstrap_test_display <- function(x)
{

  # Check for legacy results
  if(isTRUE(x$legacy)){

    # Set legacy sections
    sections <- list()

    # Loop over conditions
    for(condition in names(x$results)){

      # Loop over measures
      for(measure in names(x$results[[condition]])){

        # Add legacy section
        sections[[length(sections) + 1]] <- list(
          settings = paste0(condition, " -- ", toupper(measure)),
          note = NULL, comparison = NULL,
          table = x$results[[condition]][[measure]]$table, footnote = NULL
        )

      }

    }

    # Return legacy display
    return(list(
      legacy = TRUE,
      header = paste(
        "*** LEGACY (INVALID) INFERENCE: parametric", x$test,
        "on bootstrap replicates. Do not use for inference. ***"
      ),
      sections = sections, notes = NULL, notices = NULL
    ))

  }

  # Check for only node results
  all_node <- all(vapply(x$results, function(condition) "prop_same_sign" %in% names(condition[[1]]$table), logical(1)))

  # Set header
  header <- if(all_node){

    # Set node header
    paste0("Bootstrap node-subsampling summary of within-iteration group contrasts (central ", format(100 * x$ci_level), "% range)")

  }else{

    # Set case header
    paste0(
      "Bootstrap ",
      switch(x$ci_type, "percentile" = "percentile ", "bc" = "bias-corrected percentile ", "Wald tests and normal "),
      format(100 * x$ci_level), "% CIs of within-iteration group contrasts",
      if(x$adjust != "none") paste0(" (", x$adjust, "-adjusted)") else ""
    )

  }

  # Initialize inference
  any_inference <- FALSE

  # Initialize sections
  sections <- list()

  # Loop over conditions
  for(condition in names(x$results)){

    # Get settings of condition
    settings <- bootstrap_meta_text(condition, x$meta[[condition]])

    # Get table of condition
    table <- bootstrap_print_table(x$results[[condition]])

    # Check for node results
    is_node <- "same_sign" %in% names(table)

    # Update inference
    any_inference <- any_inference || !is_node

    # Set note on node subsampling
    note <- if(is_node) "  (node subsampling: robustness across vocabulary, not inference about groups)" else NULL

    # Initialize comparison
    comparison <- NULL

    # Check for single comparison
    if(length(unique(table$contrast)) == 1){

      # Set comparison
      comparison <- paste0("  Comparison: ", table$contrast[1])

      # Get term of comparison
      term <- x$results[[condition]][[1]]$table$term[1]

      # Get groups of comparison
      compared <- strsplit(
        if(term == "Group") table$contrast[1] else substring(table$contrast[1], nchar(term) + 3),
        " - ", fixed = TRUE
      )[[1]]

      # Check for two named groups
      if(all(c("obs_1", "obs_2") %in% names(table)) && length(compared) == 2 && !anyDuplicated(compared)){

        # Name group columns
        names(table)[names(table) %in% c("obs_1", "obs_2")] <- compared

      }

      # Remove contrast column
      table$contrast <- NULL

    }

    # Check for unnamed group columns
    footnote <- if("obs_1" %in% names(table)){

      # Set note on group columns
      "  obs_1, obs_2: full-sample values of the first and second group of each contrast"

    }

    # Add section
    sections[[length(sections) + 1]] <- list(
      settings = settings, note = note, comparison = comparison, table = table, footnote = footnote
    )

  }

  # Initialize notes
  notes <- NULL

  # Check for inference
  if(any_inference){

    # Set note on p-value
    p_text <- if(x$ci_type == "normal"){

      # Set note on Wald test
      "p: Wald test, 2{1 - pnorm(|z|)} with z = diff / se, for the contrast at the observed sample sizes."

    }else{

      # Set note on intervals
      "No p-value: a difference is reliable when its interval excludes 0."

    }

    # Check for multiple comparisons
    many_comparisons <- any(vapply(x$results, function(condition){
      any(vapply(condition, function(measure) nrow(measure$table) > 1, logical(1)))
    }, logical(1)))

    # Set note on bias
    bias_text <- paste(
      "bias/se (|bias| / se): distance between the bootstrap mean and the observed difference, in standard errors.",
      "Case resampling shrinks the networks (about 63% of participants are unique per resample), so some bias is expected;",
      "above about 0.25 (Efron & Tibshirani, 1993), the bootstrap distribution is off-centre from the observed difference,",
      "and percentile and bias-corrected intervals are shifted from it (the Normal interval is centred on the observed difference)."
    )

    # Set note on adjustment
    adjust_text <- paste0(
      "Not adjusted across measures or objects",
      if(x$adjust == "none" && many_comparisons) " or across the comparisons within a measure (see `adjust`)" else "",
      "."
    )

    # Set notes
    notes <- c(
      paste(p_text, adjust_text, bias_text),
      "For a formal hypothesis test, use `permutation_SemNeT()`. Full tables: `$results`."
    )

  }

  # Return display
  return(list(
    legacy = FALSE, header = header, sections = sections, notes = notes,
    notices = comparison_notice("bootstrap_test_SemNeT")
  ))

}

#' @method print bootstrap_test_SemNeT
#' @export
#' @noRd
# S3 Print Method ----
# Updated 22.10.2026
print.bootstrap_test_SemNeT <- function(x, ...)
{

  # Get display
  display <- bootstrap_test_display(x)

  # Print header
  if(display$legacy) cat(display$header, "\n", sep = "") else writeLines(strwrap(display$header, width = 80, exdent = 2))

  # Loop over sections
  for(section in display$sections){

    # Add break space
    cat("\n")

    # Print settings, note, and comparison
    writeLines(c(section$settings, section$note, section$comparison))

    # Check for non-legacy
    if(!display$legacy) cat("\n")

    # Print table
    print(section$table, row.names = FALSE, right = TRUE)

    # Print note on group columns
    if(!is.null(section$footnote)) writeLines(section$footnote)

  }

  # Check for notes
  if(!is.null(display$notes)){

    # Add break space
    cat("\n")

    # Print notes
    writeLines(unlist(lapply(display$notes, strwrap, width = 80)))

  }

  # Add break space
  cat("\n")

  # Loop over comparison notices
  for(note in display$notices){

    # Print notice
    writeLines(strwrap(note, width = 80))

  }

  # Return object invisibly
  return(invisible(x))

}

#' @noRd
# Format numbers ----
# Updated 12.10.2026
format_number <- function(x, digits = 3)
{

  # Return formatted numbers
  return(vapply(x, function(value){
    if(is.na(value)) "NA" else trimws(formatC(value, digits = digits, format = "fg"))
  }, character(1), USE.NAMES = FALSE))

}

#' @noRd
# Format p-values ----
# Updated 12.10.2026
format_p_value <- function(x)
{

  # Return formatted p-values
  return(vapply(x, function(value){
    if(is.na(value)) "NA" else if(value < .001) "<.001" else sub("^0", "", sprintf("%.3f", value))
  }, character(1), USE.NAMES = FALSE))

}

#' @noRd
# Format intervals ----
# Updated 12.10.2026
format_interval <- function(lower, upper, digits = 3)
{

  # Set up intervals
  interval <- paste0("[", format_number(lower, digits), ", ", format_number(upper, digits), "]")

  # Set missing intervals
  interval[is.na(lower) | is.na(upper)] <- "NA"

  # Return intervals
  return(interval)

}

#' @noRd
# Table for printing ----
# Updated 12.10.2026
bootstrap_print_table <- function(condition_results)
{

  # Loop over measures
  rows <- lapply(names(condition_results), function(measure){

    # Get table of measure
    table <- condition_results[[measure]]$table

    # Get label of contrast
    contrast <- ifelse(table$term == "Group", table$contrast, paste0(table$term, ": ", table$contrast))

    # Add missing group values (results from earlier versions)
    for(column in c("observed_1", "observed_2")){
      if(is.null(table[[column]])) table[[column]] <- rep(NA_real_, nrow(table))
    }

    # Check for node results
    if("same_sign" %in% names(table) || "prop_same_sign" %in% names(table)){

      # Return node summary
      return(data.frame(
        measure = toupper(measure), contrast = contrast,
        obs_1 = format_number(table$observed_1), obs_2 = format_number(table$observed_2),
        diff = format_number(table$observed_diff), mean = format_number(table$mean_diff),
        sd = format_number(table$sd_diff), range = format_interval(table$q_lower, table$q_upper),
        same_sign = ifelse(is.na(table$prop_same_sign), "NA", sprintf("%.2f", table$prop_same_sign)),
        n = table$n_boot, stringsAsFactors = FALSE
      ))

    }

    # Set up case summary
    result <- data.frame(
      measure = toupper(measure), contrast = contrast,
      obs_1 = format_number(table$observed_1), obs_2 = format_number(table$observed_2),
      diff = format_number(table$observed_diff), se = format_number(table$se),
      CI = format_interval(table$ci_lower, table$ci_upper), `bias/se` = format_number(table$bias_se, 2), z = format_number(table$z), p = format_p_value(table$p_value),
      result = ifelse(is.na(table$direction), "NA", table$direction),
      stringsAsFactors = FALSE, check.names = FALSE
    )

    # Return case summary
    return(result)

  })

  # Combine measures
  table <- do.call(rbind, rows)

  # Check for no group values (interactions only)
  if(all(table$obs_1 == "NA") && all(table$obs_2 == "NA")){

    # Remove group values
    table$obs_1 <- NULL
    table$obs_2 <- NULL

  }

  # Check for no Wald statistics
  if("z" %in% names(table) && all(table$z == "NA")){

    # Remove Wald statistic
    table$z <- NULL

  }

  # Check for no p-values
  if("p" %in% names(table) && all(table$p == "NA")){

    # Remove p-value
    table$p <- NULL

  }

  # Return table
  return(table)

}

#' @noRd
# Bootstrap settings ----
# Updated 12.10.2026
bootstrap_meta <- function(boot_object)
{

  # Get mean or missing
  mean_or_na <- function(x) if(length(x) > 0) mean(x, na.rm = TRUE) else NA_real_

  # Get single finite value or missing
  value_or_na <- function(x) if(length(x) == 1 && is.finite(x)) unname(x) else NA_real_

  # Get network sizes of groups
  networks <- do.call(rbind, lapply(names(boot_object$groups), function(group_name){

    # Get group
    group <- boot_object$groups[[group_name]]

    # Return sizes of group
    data.frame(
      group = group_name,
      nodes = round(mean_or_na(group$nodes), 1), edges = round(mean_or_na(group$edges), 1),
      observed_nodes = value_or_na(group$observed$nodes), observed_edges = value_or_na(group$observed$edges),
      stringsAsFactors = FALSE
    )

  }))

  # Get settings
  meta <- boot_object$meta

  # Return settings and sizes
  return(list(
    type = meta$type, method = if(is.null(meta$method)) NA_character_ else meta$method,
    iter = if(is.null(meta$iter)) NA_integer_ else meta$iter,
    seed = if(is.null(meta$seed)) NA else meta$seed,
    prop = meta$prop, networks = networks
  ))

}

#' @noRd
# Text of bootstrap settings ----
# Updated 22.10.2026
bootstrap_meta_text <- function(condition, meta)
{

  # Check for no settings
  if(is.null(meta)){

    # Return nothing
    return(character(0))

  }

  # Get network sizes
  networks <- meta$networks

  # Return run settings and network sizes
  return(c(
    run_settings_text(
      paste0(
        condition, ": ", meta$iter, if(identical(meta$type, "node")) " node-subsampling" else " case-resampling", " iterations"
      ),
      meta$method, meta$seed
    ),
    group_sizes_text(networks$group, networks$nodes, networks$edges, networks$observed_nodes, networks$observed_edges)
  ))

}

#' @noRd
# Text of run settings ----
# Updated 22.10.2026
run_settings_text <- function(what, method, seed)
{

  # Return what was run, method, and seed
  return(paste0(
    what, ", ", method, " networks, ",
    if(is.null(seed) || is.na(seed) || seed == 0) "no seed set" else paste0("seed ", seed)
  ))

}

#' @noRd
# Text of group sizes ----
# Updated 22.10.2026
group_sizes_text <- function(groups, nodes, edges, full_nodes, full_edges)
{

  # Return size of each group
  return(vapply(seq_along(groups), function(i){
    paste0(
      "  ", groups[i], ": ", format_number(nodes[i], 4), " nodes, ", format_number(edges[i], 4), " edges",
      if(!is.na(full_nodes[i]) && !is.na(full_edges[i])){
        paste0(" (full sample: ", format_number(full_nodes[i], 4), ", ", format_number(full_edges[i], 4), ")")
      }else{
        ""
      }
    )
  }, character(1)))

}

#' @noRd
# Bootstrap contrasts ----
# Updated 10.10.2026
bootstrap_contrasts <- function(groups_matrix, formula)
{

  # Get group names
  group_names <- row.names(groups_matrix)

  # Get factor names
  factor_names <- colnames(groups_matrix)

  # Check for no formula
  if(is.null(formula)){

    # Test main effect of every factor
    term_labels <- factor_names

  }else{

    # Get model formula
    model_formula <- as.formula(gsub("\\by\\b", "Measure", formula))

    # Get terms of formula
    term_labels <- attr(terms(model_formula), "term.labels")

    # Get factors of terms
    term_factors <- unique(unlist(strsplit(term_labels, ":", fixed = TRUE)))

    # Check for unknown factors
    if(!all(term_factors %in% factor_names)){

      # Send error
      stop(
        paste0(
          "In `bootstrap_test_SemNeT()`: 'formula' uses ",
          paste0("'", setdiff(term_factors, factor_names), "'", collapse = ", "),
          ", which is not a column of 'groups'.\nColumns of 'groups': ", paste0(factor_names, collapse = ", "), "."
        ),
        call. = FALSE
      )

    }

    # Check for high-order interactions
    if(any(lengths(strsplit(term_labels, ":", fixed = TRUE)) > 2)){

      # Send error
      stop(
        "In `bootstrap_test_SemNeT()`: interactions of more than two factors are not supported.\n",
        "Use `permutation_SemNeT()` instead.",
        call. = FALSE
      )

    }

  }

  # Get weights averaging groups at levels
  level_weights <- function(factor, level){

    # Start with every group
    in_level <- rep(TRUE, length(group_names))

    # Loop over factors
    for(i in seq_along(factor)){

      # Keep groups at level
      in_level <- in_level & groups_matrix[, factor[i]] == level[i]

    }

    # Check for no groups at level
    if(!any(in_level)){

      # Send error
      stop(
        "In `bootstrap_test_SemNeT()`: the design in 'groups' is not fully crossed, so an interaction can't be estimated.",
        call. = FALSE
      )

    }

    # Return weights
    return(as.numeric(in_level) / sum(in_level))

  }

  # Initialize contrasts
  contrasts <- list()

  # Loop over terms
  for(term in term_labels){

    # Get factors of term
    term_factors <- strsplit(term, ":", fixed = TRUE)[[1]]

    # Check for main effect
    if(length(term_factors) == 1){

      # Get levels of factor
      levels <- unique(groups_matrix[, term_factors])

      # Check for fewer than two levels
      if(length(levels) < 2){

        # Skip term
        next

      }

      # Get pairs of levels
      pairs <- combn(levels, 2)

      # Loop over pairs
      for(pair in seq_len(ncol(pairs))){

        # Get weights of first level
        weights_1 <- level_weights(term_factors, pairs[1, pair])

        # Get weights of second level
        weights_2 <- level_weights(term_factors, pairs[2, pair])

        # Add contrast
        contrasts[[length(contrasts) + 1]] <- list(
          term = term, contrast = paste(pairs[1, pair], "-", pairs[2, pair]),
          levels = pairs[, pair],
          weights_1 = weights_1, weights_2 = weights_2, weights = weights_1 - weights_2
        )

      }

    }else{

      # Get levels of first factor
      levels_1 <- unique(groups_matrix[, term_factors[1]])

      # Get levels of second factor
      levels_2 <- unique(groups_matrix[, term_factors[2]])

      # Check for fewer than two levels
      if(length(levels_1) < 2 || length(levels_2) < 2){

        # Skip term
        next

      }

      # Get pairs of levels of first factor
      pairs_1 <- combn(levels_1, 2)

      # Get pairs of levels of second factor
      pairs_2 <- combn(levels_2, 2)

      # Loop over pairs of first factor
      for(i in seq_len(ncol(pairs_1))){

        # Loop over pairs of second factor
        for(j in seq_len(ncol(pairs_2))){

          # Get weights of cell
          cell <- function(level_1, level_2) level_weights(term_factors, c(level_1, level_2))

          # Get weights of difference of differences
          weights <- cell(pairs_1[1, i], pairs_2[1, j]) - cell(pairs_1[2, i], pairs_2[1, j]) -
            cell(pairs_1[1, i], pairs_2[2, j]) + cell(pairs_1[2, i], pairs_2[2, j])

          # Add contrast
          contrasts[[length(contrasts) + 1]] <- list(
            term = term,
            contrast = paste0(
              "(", pairs_1[1, i], " - ", pairs_1[2, i], ") x (", pairs_2[1, j], " - ", pairs_2[2, j], ")"
            ),
            weights_1 = NULL, weights_2 = NULL, weights = weights
          )

        }

      }

    }

  }

  # Check for no contrasts
  if(length(contrasts) == 0){

    # Send error
    stop("In `bootstrap_test_SemNeT()`: 'groups' has no factor with at least two levels to compare.", call. = FALSE)

  }

  # Return contrasts
  return(contrasts)

}

#' @noRd
# Contrast differences ----
# Updated 10.10.2026
contrast_differences <- function(boot_object, measure, contrasts)
{

  # Get group names
  group_names <- names(boot_object$groups)

  # Get bootstrap values of groups
  values <- vapply(group_names, function(group_name){
    boot_object$groups[[group_name]]$measures[measure, ]
  }, numeric(ncol(boot_object$groups[[1]]$measures)))

  # Ensure matrix
  values <- matrix(values, ncol = length(group_names), dimnames = list(NULL, group_names))

  # Get observed values of groups
  observed <- vapply(group_names, function(group_name){

    # Get observed value
    value <- boot_object$groups[[group_name]]$observed$measures[measure]

    # Return value or missing
    if(length(value) == 1) unname(value) else NA_real_

  }, numeric(1))

  # Get differences of contrasts
  differences <- vapply(contrasts, function(contrast) as.vector(values %*% contrast$weights), numeric(nrow(values)))

  # Ensure matrix
  differences <- matrix(differences, nrow = nrow(values))

  # Name contrasts
  colnames(differences) <- vapply(contrasts, function(contrast){
    if(contrast$term == "Group") contrast$contrast else paste0(contrast$term, ": ", contrast$contrast)
  }, character(1))

  # Get observed differences
  observed_differences <- vapply(contrasts, function(contrast) sum(contrast$weights * observed), numeric(1))

  # Get observed values of sides (missing for interactions)
  observed_sides <- t(vapply(contrasts, function(contrast){
    vapply(list(contrast$weights_1, contrast$weights_2), function(weights){
      if(is.null(weights)) NA_real_ else sum(weights * observed)
    }, numeric(1))
  }, numeric(2)))

  # Return values and differences
  return(list(
    values = values, observed = observed, differences = differences,
    observed_differences = observed_differences, observed_sides = matrix(observed_sides, ncol = 2)
  ))

}

#' @noRd
# Bias-correction constant ----
# Updated 10.10.2026
bc_z0 <- function(difference, observed)
{

  # Get number of differences
  n <- length(difference)

  # Get number of differences below observed
  below <- sum(difference < observed) + .5 * sum(difference == observed)

  # Return constant
  return(qnorm((below + .5) / (n + 1)))

}

#' @noRd
# Bias-corrected interval ----
# Updated 10.10.2026
bc_interval <- function(difference, level, z0)
{

  # Get alpha
  alpha <- 1 - level

  # Get adjusted probabilities
  probabilities <- pnorm(2 * z0 + qnorm(c(alpha / 2, 1 - alpha / 2)))

  # Return interval
  return(unname(quantile(difference, probs = probabilities)))

}

#' @noRd
# Normal interval ----
# Updated 12.10.2026
normal_interval <- function(difference, level, center)
{

  # Get alpha
  alpha <- 1 - level

  # Return interval
  return(center + qnorm(c(alpha / 2, 1 - alpha / 2)) * sd(difference))

}

#' @noRd
# Contrast test of one measure ----
# Updated 10.10.2026
contrast_test_one_measure <- function(boot_object, measure, contrasts, ci_level, adjust, ci_type)
{

  # Get contrast differences
  prepared <- contrast_differences(boot_object, measure, contrasts)

  # Get bootstrap values
  values <- prepared$values

  # Get bootstrap differences
  differences <- prepared$differences

  # Get number of comparisons for adjustment
  adjustment <- if(adjust == "bonferroni") length(contrasts) else 1

  # Get adjusted alpha
  alpha <- (1 - ci_level) / adjustment

  # Loop over contrasts
  rows <- lapply(seq_along(contrasts), function(i){

    # Get contrast
    contrast <- contrasts[[i]]

    # Get differences of contrast
    difference <- differences[, i]

    # Remove missing differences
    difference <- difference[!is.na(difference)]

    # Get number of bootstrap samples
    n_boot <- length(difference)

    # Get observed difference
    observed_difference <- prepared$observed_differences[i]

    # Get mean and standard error of one side
    side <- function(weights){

      # Check for no weights
      if(is.null(weights)){

        # Return missing
        return(c(NA_real_, NA_real_))

      }

      # Get values of side
      side_values <- as.vector(values %*% weights)

      # Return mean and standard error
      c(mean(side_values, na.rm = TRUE), sd(side_values, na.rm = TRUE))

    }

    # Get first side
    side_1 <- side(contrast$weights_1)

    # Get second side
    side_2 <- side(contrast$weights_2)

    # Set up row
    row <- data.frame(
      term = contrast$term, contrast = contrast$contrast,
      mean_1 = side_1[1], se_1 = side_1[2],
      mean_2 = side_2[1], se_2 = side_2[2],
      observed_1 = prepared$observed_sides[i, 1], observed_2 = prepared$observed_sides[i, 2],
      mean_diff = NA_real_, observed_diff = observed_difference, bias = NA_real_,
      se = NA_real_, bias_se = NA_real_, z = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
      p_value = NA_real_, n_boot = n_boot, direction = NA_character_, stringsAsFactors = FALSE
    )

    # Check for too few samples
    if(n_boot < 2){

      # Return row
      return(row)

    }

    # Check for differences that do not vary
    if(!isTRUE(sd(difference) > 0)){

      # Send warning
      warning(
        "In `bootstrap_test_SemNeT()`: the bootstrap differences for '", row$contrast[1],
        "' do not vary, so its test and interval are undefined (NA).",
        call. = FALSE
      )

      # Set mean difference
      row$mean_diff <- mean(difference)

      # Set bias
      row$bias <- mean(difference) - observed_difference

      # Set standard error
      row$se <- sd(difference)

      # Set direction as undefined
      row$direction <- "undefined"

      # Return row
      return(row)

    }

    # Check for normal intervals
    if(ci_type == "normal"){

      # Check for no observed difference
      if(!is.finite(observed_difference)){

        # Set mean difference
        row$mean_diff <- mean(difference)

        # Set standard error
        row$se <- sd(difference)

        # Return row
        return(row)

      }

      # Center on observed difference
      center <- observed_difference

      # Compute normal interval
      interval <- normal_interval(difference, 1 - alpha, center)

      # Get standard error of difference
      se_difference <- sd(difference)

      # Compute Wald p-value
      p_value <- if(se_difference > 0){

        # Compute p-value from z
        min(1, 2 * pnorm(-abs(center) / se_difference) * adjustment)

      }else{

        # Set p-value of no spread
        as.numeric(center == 0)

      }

      # Compute Wald statistic
      row$z <- if(se_difference > 0) center / se_difference else NA_real_

    }else{

      # Get bias-correction constant
      z0 <- if(ci_type == "bc" && is.finite(observed_difference)) bc_z0(difference, observed_difference) else 0

      # Compute percentile interval
      interval <- bc_interval(difference, 1 - alpha, z0)

      # Set no p-value
      p_value <- NA_real_

    }

    # Set mean difference
    row$mean_diff <- mean(difference)

    # Set bias
    row$bias <- mean(difference) - observed_difference

    # Set standard error
    row$se <- sd(difference)

    # Set bias relative to standard error
    row$bias_se <- abs(row$bias) / row$se

    # Set lower bound
    row$ci_lower <- interval[1]

    # Set upper bound
    row$ci_upper <- interval[2]

    # Set p-value
    row$p_value <- p_value

    # Set direction
    row$direction <- if(interval[1] <= 0 && interval[2] >= 0){

      # Interval includes zero
      "n.s."

    }else if(is.null(contrast$weights_1)){

      # Label sign of contrast
      if(interval[1] > 0) "contrast > 0" else "contrast < 0"

    }else if(interval[1] > 0){

      # First level greater
      paste(contrast$levels[1], ">", contrast$levels[2])

    }else{

      # First level smaller
      paste(contrast$levels[1], "<", contrast$levels[2])

    }

    # Return row
    return(row)

  })

  # Combine rows
  table <- do.call(rbind, rows)

  # Remove row names
  row.names(table) <- NULL

  # Return table and differences
  return(list(table = table, differences = differences))

}

#' @noRd
# Robustness test of one measure ----
# Updated 10.10.2026
robustness_test_one_measure <- function(boot_object, measure, contrasts, ci_level)
{

  # Get contrast differences
  prepared <- contrast_differences(boot_object, measure, contrasts)

  # Get bootstrap differences
  differences <- prepared$differences

  # Get alpha
  alpha <- 1 - ci_level

  # Loop over contrasts
  rows <- lapply(seq_along(contrasts), function(i){

    # Get contrast
    contrast <- contrasts[[i]]

    # Get differences of contrast
    difference <- differences[, i]

    # Remove missing differences
    difference <- difference[!is.na(difference)]

    # Get observed difference
    observed_difference <- prepared$observed_differences[i]

    # Get proportion of differences with sign of observed
    same_sign <- if(is.finite(observed_difference) && observed_difference != 0){

      # Compare signs
      mean(sign(difference) == sign(observed_difference))

    }else{

      # Set undefined proportion
      NA_real_

    }

    # Get central range of differences
    interval <- if(length(difference) > 0) unname(quantile(difference, probs = c(alpha / 2, 1 - alpha / 2))) else c(NA_real_, NA_real_)

    # Return row
    data.frame(
      term = contrast$term, contrast = contrast$contrast,
      observed_1 = prepared$observed_sides[i, 1], observed_2 = prepared$observed_sides[i, 2],
      observed_diff = observed_difference,
      mean_diff = mean(difference),
      sd_diff = if(length(difference) > 1) sd(difference) else NA_real_,
      q_lower = interval[1], q_upper = interval[2],
      prop_same_sign = same_sign, n_boot = length(difference),
      stringsAsFactors = FALSE
    )

  })

  # Combine rows
  table <- do.call(rbind, rows)

  # Remove row names
  row.names(table) <- NULL

  # Return table and differences
  return(list(table = table, differences = differences))

}

#' @noRd
# Legacy test of one measure ----
# Updated 10.10.2026
legacy_test_one_measure <- function(boot_object, measure, test, formula, groups_matrix)
{

  # Build data of measure
  frame <- build_measure_frame(boot_object, measure, groups_matrix)

  # Check for t-test
  if(test == "t-test"){

    # Return pairwise t-tests
    return(list(table = pairwise_t_test_table(frame), post_hoc = NULL, adjusted_means = NULL))

  }

  # Return ANOVA or ANCOVA
  return(anova_table(frame, test, formula, groups_matrix))

}

#' @noRd
# Build data of measure ----
# Updated 06.09.2026
build_measure_frame <- function(boot_object, measure, groups_matrix)
{

  # Get group names
  group_names <- names(boot_object$groups)

  # Loop over groups
  rows <- lapply(group_names, function(group_name){

    # Get bootstrap values
    values <- boot_object$groups[[group_name]]$measures[measure, ]

    # Get edge counts
    edges <- boot_object$groups[[group_name]]$edges

    # Return data of group
    data.frame(Measure = values, Edges = edges, Name = group_name, stringsAsFactors = FALSE)

  })

  # Combine groups
  frame <- do.call(rbind, rows)

  # Loop over groups
  for(group_name in group_names){

    # Get rows of group
    rows_in_group <- which(frame$Name == group_name)

    # Check for edge counts that do not vary
    if(length(unique(frame$Edges[rows_in_group])) == 1){

      # Set edge counts to zero
      frame$Edges[rows_in_group] <- 0

    }else{

      # Center and scale edge counts
      frame$Edges[rows_in_group] <- scale(frame$Edges[rows_in_group])

    }

  }

  # Get grouping variables of rows
  group_variables <- as.data.frame(groups_matrix[frame$Name, , drop = FALSE], stringsAsFactors = TRUE)

  # Return data with grouping variables
  return(cbind(frame[, c("Measure", "Edges")], group_variables))

}

#' @noRd
# Pairwise t-test table ----
# Updated 06.09.2026
pairwise_t_test_table <- function(frame)
{

  # Get name of factor
  factor_name <- colnames(frame)[3]

  # Get values of factor
  factor_values <- as.character(frame[[factor_name]])

  # Get levels of factor
  levels <- unique(factor_values)

  # Get pairs of levels
  pairs <- combn(levels, 2)

  # Loop over pairs
  rows <- lapply(seq_len(ncol(pairs)), function(pair){

    # Get first level
    one <- pairs[1, pair]

    # Get second level
    two <- pairs[2, pair]

    # Get values of first level
    values_1 <- frame$Measure[factor_values == one]

    # Get values of second level
    values_2 <- frame$Measure[factor_values == two]

    # Perform t-test
    test_result <- t.test(values_1, values_2, var.equal = TRUE)

    # Get mean of first level
    mean_1 <- mean(values_1, na.rm = TRUE)

    # Get mean of second level
    mean_2 <- mean(values_2, na.rm = TRUE)

    # Get direction of difference
    direction <- if(test_result$p.value > .05){

      # Not significant
      "n.s."

    }else if(mean_1 > mean_2){

      # First level greater
      paste(one, ">", two)

    }else{

      # First level smaller
      paste(one, "<", two)

    }

    # Return row
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

  # Combine rows
  result <- do.call(rbind, rows)

  # Remove row names
  row.names(result) <- NULL

  # Return table
  return(result)

}

#' @noRd
# ANOVA table ----
# Updated 06.09.2026
anova_table <- function(frame, test, formula, groups_matrix)
{

  # Get factor names
  factor_names <- colnames(groups_matrix)

  # Check for ANOVA
  if(test == "ANOVA"){

    # Remove edge covariate
    frame$Edges <- NULL

  }

  # Check for no formula
  if(is.null(formula)){

    # Enter factors additively
    model_formula <- paste("Measure ~", paste(factor_names, collapse = " + "))

  }else{

    # Replace outcome placeholder
    model_formula <- gsub("\\by\\b", "Measure", formula)

  }

  # Check for ANCOVA
  if(test == "ANCOVA"){

    # Split formula into sides
    sides <- strsplit(model_formula, "~")[[1]]

    # Add edge covariate first
    model_formula <- paste0(sides[1], "~ Edges +", sides[2])

  }

  # Fit model
  model <- aov(as.formula(model_formula), data = frame)

  # Get type of sums of squares
  anova_type <- if(length(factor_names) > 1) "III" else "II"

  # Perform ANOVA
  anova_result <- car::Anova(model, type = anova_type)

  # Get residual row
  residual_row <- which(row.names(anova_result) == "Residuals")

  # Get residual sum of squares
  residual_sum_sq <- anova_result$`Sum Sq`[residual_row]

  # Compute partial eta squared
  partial_eta_squared <- anova_result$`Sum Sq` / (anova_result$`Sum Sq` + residual_sum_sq)

  # Remove residual row
  partial_eta_squared[residual_row] <- NA

  # Check for ANCOVA
  if(test == "ANCOVA"){

    # Remove covariate row
    partial_eta_squared[row.names(anova_result) == "Edges"] <- NA

  }

  # Tidy ANOVA table
  anova_table <- as.data.frame(broom::tidy(anova_result), stringsAsFactors = FALSE)

  # Add partial eta squared
  anova_table$partial_eta_squared <- round(partial_eta_squared, 3)

  # Round statistics
  anova_table[, c("sumsq", "statistic", "p.value")] <- round(anova_table[, c("sumsq", "statistic", "p.value")], 3)

  # Get adjusted means
  effects_list <- effects::allEffects(model)

  # Get group terms
  group_effect_terms <- setdiff(names(effects_list), "Edges")

  # Get interaction terms
  interaction_terms <- group_effect_terms[grepl(":", group_effect_terms, fixed = TRUE)]

  # Select adjusted means
  adjusted_means <- if(length(interaction_terms) > 0){

    # Use highest interaction
    as.data.frame(effects_list[[interaction_terms[length(interaction_terms)]]])

  }else if(length(group_effect_terms) == 1){

    # Use single group term
    as.data.frame(effects_list[[group_effect_terms]])

  }else{

    # Use every group term
    lapply(effects_list[group_effect_terms], as.data.frame)

  }

  # Initialize post-hoc tests
  post_hoc <- NULL

  # Check for single factor with more than two levels
  if(length(factor_names) == 1 && length(unique(frame[[factor_names]])) > 2){

    # Perform Tukey tests
    post_hoc <- as.data.frame(suppressWarnings(TukeyHSD(model))[[factor_names]])

  }

  # Return tables
  return(list(table = anova_table, post_hoc = post_hoc, adjusted_means = adjusted_means))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
bootstrap_test_SemNeT_errors <- function(boot_objects, measures, groups, formula, ci_level, adjust, ci_type, test, legacy)
{

  # '...' errors
  if(length(boot_objects) < 1){

    # Send error
    stop("In `bootstrap_test_SemNeT()`: at least one `bootstrap_SemNeT` object must be provided via `...`.", call. = FALSE)

  }

  # Check for class
  if(!all(vapply(boot_objects, inherits, logical(1), what = "bootstrap_SemNeT"))){

    # Send error
    stop(
      "In `bootstrap_test_SemNeT()`: every object passed via `...` must be a `bootstrap_SemNeT` object (from `bootstrap_SemNeT()`).",
      call. = FALSE
    )

  }

  # Get group names
  group_names <- names(boot_objects[[1]]$groups)

  # Check for single group
  if(length(group_names) < 2){

    # Send error
    stop(
      "In `bootstrap_test_SemNeT()`: a single group cannot be tested against itself.\n",
      "Use `random_network_test()` to test a single group against random networks instead.",
      call. = FALSE
    )

  }

  # Check for same groups
  same_groups <- vapply(boot_objects, function(x) identical(names(x$groups), group_names), logical(1))

  # Check for different groups
  if(!all(same_groups)){

    # Send error
    stop(
      "In `bootstrap_test_SemNeT()`: every `bootstrap_SemNeT` object passed via `...` must contain the same groups, in the same order.",
      call. = FALSE
    )

  }

  # 'measures' errors
  typeof_error(measures, "character", "bootstrap_test_SemNeT")

  # Check for unknown measures
  if(!all(measures %in% c("aspl", "cc", "q"))){

    # Send error
    stop("In `bootstrap_test_SemNeT()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'.", call. = FALSE)

  }

  # 'groups' errors
  if(is.null(groups)){

    # Use group names as levels
    groups_matrix <- matrix(group_names, ncol = 1, dimnames = list(group_names, "Group"))

  }else{

    # Check for matrix or data frame
    object_error(groups, c("matrix", "data.frame"), "bootstrap_test_SemNeT")

    # Ensure matrix
    groups_matrix <- as.matrix(groups)

    # Check for one row per group
    if(nrow(groups_matrix) != length(group_names)){

      # Send error
      stop(
        paste0(
          "In `bootstrap_test_SemNeT()`: 'groups' has ", nrow(groups_matrix), " rows, but ",
          length(group_names), " groups were bootstrapped.\n",
          "'groups' must have exactly one row per group."
        ),
        call. = FALSE
      )

    }

    # Check for row names matching groups
    if(!is.null(row.names(groups_matrix)) && setequal(row.names(groups_matrix), group_names)){

      # Order rows by group
      groups_matrix <- groups_matrix[group_names, , drop = FALSE]

    }else{

      # Name rows by group
      row.names(groups_matrix) <- group_names

    }

    # Check for no column names
    if(is.null(colnames(groups_matrix))){

      # Name factors
      colnames(groups_matrix) <- if(ncol(groups_matrix) == 1) "Group" else paste0("Group", seq_len(ncol(groups_matrix)))

    }

  }

  # 'formula' errors
  if(!is.null(formula)){
    length_error(formula, 1, "bootstrap_test_SemNeT")
    typeof_error(formula, "character", "bootstrap_test_SemNeT")
  }

  # 'ci_level' errors
  length_error(ci_level, 1, "bootstrap_test_SemNeT")
  typeof_error(ci_level, "numeric", "bootstrap_test_SemNeT")

  # Check for limits of confidence level
  if(is.na(ci_level) || ci_level <= 0 || ci_level >= 1){

    # Send error
    stop("In `bootstrap_test_SemNeT()`: 'ci_level' must be strictly between 0 and 1.", call. = FALSE)

  }

  # 'adjust' errors
  length_error(adjust, c(1, 2), "bootstrap_test_SemNeT")
  typeof_error(adjust, "character", "bootstrap_test_SemNeT")

  # Check for known adjustment
  match.arg(adjust, c("none", "bonferroni"))

  # 'ci_type' errors
  length_error(ci_type, c(1, 3), "bootstrap_test_SemNeT")
  typeof_error(ci_type, "character", "bootstrap_test_SemNeT")

  # Check for known interval type
  match.arg(ci_type, c("normal", "bc", "percentile"))

  # 'legacy' errors
  length_error(legacy, 1, "bootstrap_test_SemNeT")
  typeof_error(legacy, "logical", "bootstrap_test_SemNeT")

  # 'test' errors
  if(!is.null(test) && !legacy){

    # Send error
    stop(
      "In `bootstrap_test_SemNeT()`: 'test' (t-test, ANOVA, ANCOVA) applied parametric tests to bootstrap replicates, which is statistically invalid.\n",
      "Omit 'test' for valid bootstrap confidence intervals, or set `legacy = TRUE` to reproduce the old (invalid) results.",
      call. = FALSE
    )

  }

  # Check for legacy tests
  if(legacy){

    # Check for test
    if(!is.null(test)){
      length_error(test, c(1, 3), "bootstrap_test_SemNeT")
      typeof_error(test, "character", "bootstrap_test_SemNeT")
    }

    # Match test
    test <- if(is.null(test)) "t-test" else match.arg(test, c("t-test", "ANOVA", "ANCOVA"))

    # Check for t-test with multiple factors
    if(test == "t-test" && ncol(groups_matrix) > 1){

      # Send error
      stop(
        "In `bootstrap_test_SemNeT()`: multi-factor `groups` are not compatible with `test = \"t-test\"`.\n",
        "Use `test = \"ANOVA\"` or `test = \"ANCOVA\"` instead.",
        call. = FALSE
      )

    }

    # Check for ANOVA with two groups
    if(test == "ANOVA" && ncol(groups_matrix) == 1 && length(unique(groups_matrix[, 1])) < 3){

      # Send error
      stop(
        "In `bootstrap_test_SemNeT()`: a single factor with only 2 levels is not compatible with `test = \"ANOVA\"`.\n",
        "Use `test = \"t-test\"` instead.",
        call. = FALSE
      )

    }

  }

  # Return validated input (matched 'test' and design in 'groups')
  return(list(boot_objects = boot_objects, groups_matrix = groups_matrix, test = test))

}
