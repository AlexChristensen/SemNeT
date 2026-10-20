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
#' \item{total}{A named vector comparing the number of distinct responses given
#' by each participant (a response repeated by a participant counts once) via a
#' two-sample \emph{t}-test and Cohen's \emph{d} (absolute; the sign of the
#' difference is that of \code{t_statistic})}
#' \item{unique}{A named vector comparing the number of responses unique to
#' each group via McNemar's test (and \code{phi}, see Details)}
#' \item{vocabulary}{Descriptive statistics for the groups' vocabularies that
#' account for the groups' sizes, unlike \code{unique}:
#' \code{jaccard} (shared responses over all responses given by either group;
#' it still grows with group size, because larger groups reveal more rare
#' responses);
#' \code{n_participants_<group>}; \code{rarefied_n} (the sample size compared,
#' the smaller group's); \code{rarefied_<group>} (the expected number of
#' distinct responses in a random sample of \code{rarefied_n} participants from
#' the group, Hurlbert, 1971; the smaller group's is its observed number); and
#' \code{rarefied_diff} (the first group's minus the second's)}
#' With more than two groups, a named list of such lists, one per pair of
#' groups (named \code{"<group1>_vs_<group2>"})
#'
#' @details The number of responses unique to each group (\code{unique}) grows
#' with a group's number of participants, and the responses are not independent
#' observations, so the McNemar test on those counts should be read as a
#' description, not as a test of the groups. The \code{vocabulary} statistics
#' compare groups at the same number of participants instead.
#'
#' McNemar's test only uses the two off-diagonal cells (the numbers of responses
#' unique to each group), so here it is a continuity-corrected sign test on those
#' two counts; it is \code{NA} when neither group has a unique response.
#' \code{phi} is the correlation between the two groups' indicators of giving
#' each response across the responses given by either group. Because no response
#' is given by neither group, it is at most 0 by construction (and reported as an
#' absolute value), so it describes the overlap of the vocabularies and not an
#' association between the groups. The \emph{t}-test is \code{NA} when it is
#' undefined (e.g., every participant gave the same number of responses).
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
#' Hurlbert, S. H. (1971).
#' The nonconcept of species diversity: A critique and alternative parameters.
#' \emph{Ecology}, \emph{52}, 577-586.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Response analysis ----
# Updated 13.10.2026
response_analysis <- function(...)
{

  # Argument errors
  groups <- response_analysis_errors(list(...))

  # Get group names
  group_names <- names(groups)

  # Get pairs of groups
  pairs <- combn(length(groups), 2)

  # Loop over pairs
  comparisons <- lapply(seq_len(ncol(pairs)), function(pair){

    # Get first group
    first <- pairs[1, pair]

    # Get second group
    second <- pairs[2, pair]

    # Compare groups
    response_analysis_pair(
      groups[[first]], groups[[second]],
      group_names[first], group_names[second]
    )

  })

  # Name comparisons
  names(comparisons) <- vapply(seq_len(ncol(pairs)), function(pair){
    paste(group_names[pairs[1, pair]], group_names[pairs[2, pair]], sep = "_vs_")
  }, character(1))

  # Check for two groups
  if(length(groups) == 2){

    # Return single comparison
    return(comparisons[[1]])

  }

  # Return comparisons
  return(comparisons)

}

#' @noRd
# Response analysis of a pair ----
# Updated 06.09.2026
response_analysis_pair <- function(group_1, group_2, name_1, name_2)
{

  # Get total responses of first group
  total_1 <- rowSums(group_1)

  # Get total responses of second group
  total_2 <- rowSums(group_2)

  # Test difference in totals
  totals_test <- tryCatch(t.test(total_1, total_2, var.equal = TRUE), error = function(e) NULL)

  # Set up totals result
  total_result <- c(
    mean_1 = mean(total_1, na.rm = TRUE), sd_1 = sd(total_1, na.rm = TRUE),
    mean_2 = mean(total_2, na.rm = TRUE), sd_2 = sd(total_2, na.rm = TRUE),
    t_statistic = if(is.null(totals_test)) NA_real_ else unname(totals_test$statistic),
    df = if(is.null(totals_test)) NA_real_ else unname(totals_test$parameter),
    p_value = if(is.null(totals_test)) NA_real_ else totals_test$p.value,
    ci_lower = if(is.null(totals_test)) NA_real_ else totals_test$conf.int[1],
    ci_upper = if(is.null(totals_test)) NA_real_ else totals_test$conf.int[2],
    cohens_d = cohens_d(total_1, total_2)
  )

  # Name totals result
  names(total_result) <- c(
    paste0("mean_", name_1), paste0("sd_", name_1),
    paste0("mean_", name_2), paste0("sd_", name_2),
    "t_statistic", "df", "p_value", "ci_lower", "ci_upper", "cohens_d"
  )

  # Round totals result (p-values to significant digits, so they do not print as 0)
  total_result <- round_statistics(total_result)

  # Get responses given in first group
  responses_1 <- colnames(group_1)[colSums(group_1) > 0]

  # Get responses given in second group
  responses_2 <- colnames(group_2)[colSums(group_2) > 0]

  # Get vocabulary
  vocabulary <- sort(unique(c(responses_1, responses_2)), method = "radix")

  # Set up indicator of groups giving each response
  given_by <- matrix(0, nrow = length(vocabulary), ncol = 2, dimnames = list(vocabulary, c(name_1, name_2)))

  # Mark responses of first group
  given_by[match(responses_1, vocabulary), 1] <- 1

  # Mark responses of second group
  given_by[match(responses_2, vocabulary), 2] <- 1

  # Count responses unique to first group
  unique_to_1 <- sum(given_by[, 1] == 1 & given_by[, 2] == 0)

  # Count responses unique to second group
  unique_to_2 <- sum(given_by[, 1] == 0 & given_by[, 2] == 1)

  # Initialize contingency table
  contingency_table <- matrix(0, nrow = 2, ncol = 2)

  # Set totals of each group
  diag(contingency_table) <- colSums(given_by)

  # Set responses unique to first group
  contingency_table[1, 2] <- unique_to_1

  # Set responses unique to second group
  contingency_table[2, 1] <- unique_to_2

  # Test uniqueness
  uniqueness_test <- if(unique_to_1 + unique_to_2 == 0){

    # Set undefined test
    list(statistic = NA_real_, parameter = NA_real_, p.value = NA_real_)

  }else{

    # Perform McNemar's test
    mcnemar.test(contingency_table)

  }

  # Set up uniqueness result
  unique_result <- c(
    total_across_groups = length(vocabulary),
    total_1 = unname(colSums(given_by)[1]), total_2 = unname(colSums(given_by)[2]),
    unique_1 = unique_to_1, unique_2 = unique_to_2,
    mcnemars_x2 = unname(uniqueness_test$statistic), df = unname(uniqueness_test$parameter),
    p_value = uniqueness_test$p.value,
    phi = suppressWarnings(abs(cor(given_by)[1, 2]))
  )

  # Name uniqueness result
  names(unique_result) <- c(
    "total_across_groups", paste0("total_", name_1), paste0("total_", name_2),
    paste0("unique_", name_1), paste0("unique_", name_2),
    "mcnemars_x2", "df", "p_value", "phi"
  )

  # Round uniqueness result (p-values to significant digits, so they do not print as 0)
  unique_result <- round_statistics(unique_result)

  # Count responses shared by groups
  shared <- sum(given_by[, 1] == 1 & given_by[, 2] == 1)

  # Get rarefied sample size
  rarefied_n <- min(nrow(group_1), nrow(group_2))

  # Set up vocabulary result
  vocabulary_result <- round(c(
    jaccard = shared / length(vocabulary),
    n_1 = nrow(group_1), n_2 = nrow(group_2),
    rarefied_n = rarefied_n,
    rarefied_1 = rarefied_richness(colSums(group_1), nrow(group_1), rarefied_n),
    rarefied_2 = rarefied_richness(colSums(group_2), nrow(group_2), rarefied_n)
  ), 3)

  # Add rarefied difference
  vocabulary_result <- c(vocabulary_result, rarefied_diff = unname(vocabulary_result["rarefied_1"] - vocabulary_result["rarefied_2"]))

  # Name vocabulary result
  names(vocabulary_result) <- c(
    "jaccard", paste0("n_participants_", name_1), paste0("n_participants_", name_2), "rarefied_n",
    paste0("rarefied_", name_1), paste0("rarefied_", name_2), "rarefied_diff"
  )

  # Return results
  return(list(total = total_result, unique = unique_result, vocabulary = vocabulary_result))

}

#' @noRd
# Round statistics ----
# Updated 15.10.2026
round_statistics <- function(x, digits = 3)
{

  # Round to digits
  rounded <- round(x, digits)

  # Find p-values
  is_p_value <- names(x) == "p_value"

  # Keep p-values to significant digits
  rounded[is_p_value] <- signif(x[is_p_value], digits)

  # Return rounded statistics
  return(rounded)

}

#' @noRd
# Rarefied richness (Hurlbert) ----
# Updated 13.10.2026
rarefied_richness <- function(counts, N, m)
{

  # Keep responses that were given
  counts <- counts[counts > 0]

  # Check for full sample
  if(m >= N){

    # Return observed richness
    return(length(counts))

  }

  # Return expected richness
  return(sum(1 - exp(lchoose(N - counts, m) - lchoose(N, m))))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
response_analysis_errors <- function(groups)
{

  # '...' errors
  if(length(groups) < 2){

    # Send error
    stop("In `response_analysis()`: at least two response matrices must be provided via `...`.", call. = FALSE)

  }

  # Check for unnamed groups
  if(is.null(names(groups)) || any(names(groups) == "")){

    # Send error
    stop(
      "In `response_analysis()`: every response matrix passed via `...` must be named, e.g. `response_analysis(low = low_data, high = high_data)`.",
      call. = FALSE
    )

  }

  # Check for duplicate names
  if(anyDuplicated(names(groups)) > 0){

    # Send error
    stop("In `response_analysis()`: every response matrix passed via `...` must have a unique name.", call. = FALSE)

  }

  # Check each group
  groups <- lapply(groups, function(group){

    # Check for response matrix
    group <- response_matrix_error(group, "response_analysis")

    # Check for raw responses
    if(is.character(group) || is.factor(group)){

      # Convert to binary
      group <- responses2binary(group)$binary

    }

    # Return group
    return(group)

  })

  # Return 'groups' (in case of data frames or raw responses)
  return(groups)

}
