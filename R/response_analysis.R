#' @title Response Analysis
#'
#' @description Computes the difference in the total and unique number of
#' responses between groups (follows Christensen et al., 2018). With more
#' than two groups, every pairwise comparison is returned, since both
#' underlying tests (a two-sample \emph{t}-test and McNemar's test) are
#' inherently pairwise
#'
#' @param ... Named matrices or data frames.
#' At least two response matrices (raw word responses or already-binary), one
#' per group, e.g. \code{response_analysis(low = low_data, high = high_data)}
#'
#' @return With exactly two groups, a list with:
#' \item{total}{A named vector comparing the total responses given by each
#' participant via a two-sample \emph{t}-test and Cohen's \emph{d}}
#' \item{unique}{A named vector comparing the number of unique responses
#' provided by each group via McNemar's test}
#' With more than two groups, a named list of such lists, one per pair of
#' groups (named \code{"<group1>_vs_<group2>"})
#'
#' @examples
#' # Obtain data
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' # Perform analysis
#' response_analysis(low = low, high = high)
#'
#' @references
#' Christensen, A. P., Kenett, Y. N., Cotter, K. N., Beaty, R. E., & Silvia, P. J. (2018).
#' Remotely close associations: Openness to experience and semantic memory structure.
#' \emph{European Journal of Personality}, \emph{32}, 480-492.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats t.test mcnemar.test cor
#' @importFrom utils combn
#'
#' @export
# Response analysis
# Updated 06.09.2026
response_analysis <- function(...)
{

  # Validate and coerce input before any computation begins
  groups <- response_analysis_errors(list(...))

  group_names <- names(groups)

  # Every pair of groups gets its own comparison -- for the common 2-group
  # case this is exactly one comparison, matching the original directly
  pairs <- combn(length(groups), 2)

  comparisons <- lapply(seq_len(ncol(pairs)), function(pair){
    first <- pairs[1, pair]
    second <- pairs[2, pair]
    response_analysis_pair(
      groups[[first]], groups[[second]],
      group_names[first], group_names[second]
    )
  })

  names(comparisons) <- vapply(seq_len(ncol(pairs)), function(pair){
    paste(group_names[pairs[1, pair]], group_names[pairs[2, pair]], sep = "_vs_")
  }, character(1))

  # With exactly two groups, return that single comparison directly rather
  # than nesting it one level deep in a length-1 list
  if(length(groups) == 2){
    return(comparisons[[1]])
  }

  return(comparisons)

}

#' @noRd
# One pairwise comparison between two response groups
# Updated 06.09.2026
response_analysis_pair <- function(group_1, group_2, name_1, name_2)
{

  total_1 <- rowSums(group_1)
  total_2 <- rowSums(group_2)

  totals_test <- t.test(total_1, total_2, var.equal = TRUE)

  total_result <- round(c(
    mean_1 = mean(total_1, na.rm = TRUE), sd_1 = sd(total_1, na.rm = TRUE),
    mean_2 = mean(total_2, na.rm = TRUE), sd_2 = sd(total_2, na.rm = TRUE),
    t_statistic = unname(totals_test$statistic), df = unname(totals_test$parameter),
    p_value = totals_test$p.value,
    ci_lower = totals_test$conf.int[1], ci_upper = totals_test$conf.int[2],
    cohens_d = cohens_d(total_1, total_2)
  ), 3)
  names(total_result) <- c(
    paste0("mean_", name_1), paste0("sd_", name_1),
    paste0("mean_", name_2), paste0("sd_", name_2),
    "t_statistic", "df", "p_value", "ci_lower", "ci_upper", "cohens_d"
  )

  # Every response actually given, by either group -- `method = "radix"`
  # benchmarked ~20x faster than the locale-aware default at realistic
  # vocabulary sizes; safe because this vocabulary only labels rows of an
  # internal indicator matrix, and every downstream statistic (column sums,
  # McNemar's test) is order-invariant
  responses_1 <- colnames(group_1)
  responses_2 <- colnames(group_2)
  vocabulary <- sort(unique(c(responses_1, responses_2)), method = "radix")

  # Which group(s) gave each response, as a 2-column indicator matrix
  given_by <- matrix(0, nrow = length(vocabulary), ncol = 2, dimnames = list(vocabulary, c(name_1, name_2)))
  given_by[match(responses_1, vocabulary), 1] <- 1
  given_by[match(responses_2, vocabulary), 2] <- 1

  unique_to_1 <- sum(given_by[, 1] == 1 & given_by[, 2] == 0)
  unique_to_2 <- sum(given_by[, 1] == 0 & given_by[, 2] == 1)

  contingency_table <- matrix(0, nrow = 2, ncol = 2)
  diag(contingency_table) <- colSums(given_by)
  contingency_table[1, 2] <- unique_to_1
  contingency_table[2, 1] <- unique_to_2

  uniqueness_test <- mcnemar.test(contingency_table)

  unique_result <- round(c(
    total_across_groups = length(vocabulary),
    total_1 = unname(colSums(given_by)[1]), total_2 = unname(colSums(given_by)[2]),
    unique_1 = unique_to_1, unique_2 = unique_to_2,
    mcnemars_x2 = unname(uniqueness_test$statistic), df = unname(uniqueness_test$parameter),
    p_value = uniqueness_test$p.value,
    phi = abs(cor(given_by)[1, 2])
  ), 3)
  names(unique_result) <- c(
    "total_across_groups", paste0("total_", name_1), paste0("total_", name_2),
    paste0("unique_", name_1), paste0("unique_", name_2),
    "mcnemars_x2", "df", "p_value", "phi"
  )

  return(list(total = total_result, unique = unique_result))

}

#' @noRd
# Argument errors for `response_analysis()`
# Updated 06.09.2026
response_analysis_errors <- function(groups)
{

  # At least two groups are required to compare anything
  if(length(groups) < 2){
    stop("In `response_analysis()`: at least two response matrices must be provided via `...`.", call. = FALSE)
  }

  # Every group must be named -- required in place of the original's
  # `substitute()`-based name-sniffing, and works correctly under `do.call()`
  if(is.null(names(groups)) || any(names(groups) == "")){
    stop(
      "In `response_analysis()`: every response matrix passed via `...` must be named, e.g. `response_analysis(low = low_data, high = high_data)`.",
      call. = FALSE
    )
  }

  # Every group's name must be unique -- with 3+ groups, two groups sharing a
  # name can produce two *different* pairwise comparisons that end up with
  # the exact same "<name1>_vs_<name2>" label, and `$`/`[[` access on the
  # returned list would then silently return only the first of them
  if(anyDuplicated(names(groups)) > 0){
    stop("In `response_analysis()`: every response matrix passed via `...` must have a unique name.", call. = FALSE)
  }

  # Every group must be a response matrix; raw word responses become binary
  groups <- lapply(groups, function(group){

    group <- response_matrix_error(group, "response_analysis")

    if(is.character(group) || is.factor(group)){
      group <- responses_to_binary(group)$binary
    }

    return(group)

  })

  return(groups)

}
