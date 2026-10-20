#' @title Permutation Test for Network Measures
#'
#' @description Tests whether global network measures differ between two samples
#' by comparing the observed difference with a null distribution from randomly
#' re-dealing the pooled participants into two samples
#'
#' @param sample_1 Matrix or data frame.
#' Response matrix to be compared with \code{sample_2}. The two samples are
#' pooled by rows, so their columns are lined up first: raw responses of
#' different widths are padded with missing responses, and numeric matrices are
#' matched by column (response) name
#'
#' @param sample_2 Matrix or data frame.
#' Response matrix to be compared with \code{sample_1}
#'
#' @param method Character (length = 1).
#' Network estimation method.
#' Defaults to \code{"tmfg"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"tmfg"} --- See \code{\link{tmfg}}
#'
#' \item \code{"cn"} --- See \code{\link{cn}}
#'
#' \item \code{"nrw"} --- See \code{\link{nrw}}
#'
#' \item \code{"pf"} --- See \code{\link{pf}}
#'
#' }
#'
#' @param method_args List.
#' Additional arguments passed to \code{method}'s function.
#' Defaults to \code{list()} (the method's defaults)
#'
#' @param sim Character (length = 1).
#' Similarity measure for \code{method = "tmfg"}.
#' Defaults to \code{"cosine"}.
#' See \code{\link{similarity}} for other options
#'
#' @param minimum_cases Numeric (length = 1).
#' \strong{Only} for \code{method = "tmfg"}.
#' Minimum number of participants who gave a response for it to be kept
#' (\code{\link{finalize}}) before the samples' vocabularies are equated
#' (\code{\link{equate}}).
#' Defaults to \code{2}
#'
#' @param measure Character.
#' One or more of \code{"aspl"}, \code{"cc"}, and \code{"q"}
#' (e.g., \code{c("aspl", "cc", "q")} for all three).
#' Every measure is computed from the same permuted networks, so testing
#' several costs little more than one.
#' Defaults to \code{"aspl"}
#'
#' @param alternative Character (length = 1).
#' Alternative hypothesis.
#' Defaults to \code{"two_tailed"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"two_tailed"} --- \code{sample_1} differs from \code{sample_2};
#' the \emph{p}-value is the proportion of permuted differences \eqn{T^*} with
#' \eqn{|T^*| \ge |T_{obs}|} (distance from 0)
#'
#' \item \code{"two_tailed_centered"} --- same hypothesis, but the distance is
#' measured from the permutation null's mean \eqn{\bar{T}^*}:
#' \eqn{|T^* - \bar{T}^*| \ge |T_{obs} - \bar{T}^*|}
#'
#' \item \code{"two_tailed_doubled"} --- same hypothesis, with the \emph{p}-value
#' twice the smaller one-sided \emph{p}-value (at most 1)
#'
#' \item \code{"less"} --- \code{sample_1} is less than \code{sample_2}
#'
#' \item \code{"greater"} --- \code{sample_1} is greater than \code{sample_2}
#'
#' }
#'
#' @param iter Numeric (length = 1).
#' Number of permutations (the first is the observed data).
#' Defaults to \code{1000}
#'
#' @param cores Numeric (length = 1).
#' Number of processing cores.
#' Defaults to \code{1} (sequential)
#'
#' @param previous \code{\link{permutation_SemNeT}} object.
#' Reuses the previous call's permuted networks to test other \code{measure}s
#' without re-estimating them (requires \code{keep_networks = TRUE} in that call).
#' Defaults to \code{NULL}
#'
#' @param keep_networks Boolean (length = 1).
#' Should every permuted network be kept?
#' Only needed to test other measures later with \code{previous}
#' and can be large.
#' Defaults to \code{FALSE}
#'
#' @param seed Numeric (length = 1).
#' Defaults to \code{NULL} or random results (\code{previous}'s \code{seed}
#' when it is used); \code{0} is random too.
#' Set a nonzero value for reproducible results.
#' Each permutation (and its \code{\link{q}} community detection) gets its own
#' seed, so results do not depend on \code{set.seed()} or \code{cores}.
#' The first permutation (the observed data) uses \code{seed} itself, so its
#' \code{\link{q}} matches \code{\link{bootstrap_SemNeT}}'s observed networks
#'
#' @param alpha Numeric (length = 1).
#' Significance level between 0 and 1 (exclusive) used to label the printed
#' \code{result}. Does not change the \emph{p}-value.
#' Defaults to \code{.05}
#'
#' @details Always run \code{\link{bootstrap_test_SemNeT}} (on
#' \code{\link{bootstrap_SemNeT}} results) alongside this function and report
#' both: the permutation test asks whether group membership matters, whereas the
#' bootstrap shows how precisely each group's networks and their difference are
#' estimated.
#'
#' The null hypothesis is that the participants' group labels are
#' exchangeable: both groups' participants come from the same population of
#' responses. A rejection therefore says the groups differ in their responses
#' (their vocabularies, how many responses they give, and the size of the
#' resulting networks as well as their structure); it does not by itself show a
#' difference in semantic structure. \code{diagnostics} shows how the networks'
#' sizes vary across permutations.
#'
#' The permutation null answers a different question than a bootstrap
#' interval, so their spreads need not agree: a permutation pools both groups'
#' participants and re-deals them, so the permuted networks' node and edge counts
#' (which ASPL and CC depend on) vary, whereas a case bootstrap
#' (\code{\link{bootstrap_SemNeT}}) resamples within each group. Use
#' \code{diagnostics} to see how the network sizes vary.
#'
#' The three two-sided definitions agree when the null is centred on 0 and
#' symmetric, which is the case when the two groups have (about) the same size.
#' With unequal group sizes the permutation null is generally not centred on 0,
#' because the measures (e.g., ASPL, CC) depend on a network's size. All three
#' are valid tests of the same null (the participants' group labels are
#' exchangeable) because the observed difference is one of the permutations.
#' \code{"two_tailed"} also counts the size-induced offset as evidence;
#' \code{"two_tailed_centered"} measures how far the observed difference is
#' from what the group sizes alone produce, assuming the null is roughly
#' symmetric about its mean; \code{"two_tailed_doubled"} makes no symmetry
#' assumption. The one-sided tests are unaffected by centring.
#'
#' When several \code{measure}s are tested, each is tested against its own null:
#' the \emph{p}-values are \emph{not} adjusted for the number of
#' measures.
#'
#' @return Returns a list containing:
#'
#' \item{result}{A data frame with one row per \code{measure} containing each
#' sample's observed value, the permutation \emph{p}-value (\code{p_value}), and
#' the alternative tested (\code{alternative})}
#'
#' \item{networks}{A list of \code{network_1} and \code{network_2}, the networks
#' of every permuted sample (the first entry is the observed data); \code{NULL}
#' unless \code{keep_networks = TRUE}}
#'
#' \item{method}{The \code{method} used}
#'
#' \item{seed}{The \code{seed} used (\code{NULL} if none)}
#'
#' \item{differences}{The measure difference for every permutation: a vector for
#' one \code{measure}, or a permutations x measures matrix for several}
#'
#' \item{diagnostics}{A data frame with a row per permutation containing each
#' sample's measure value (\code{value_1} and \code{value_2}, or
#' \code{<measure>_1} and \code{<measure>_2} for several measures) and each
#' network's \code{nodes_1}, \code{nodes_2}, \code{edges_1}, and \code{edges_2}}
#'
#' \item{alpha}{The \code{alpha} used}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' # Get openness data -- equating (see `equate()`) requires named responses
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Perform permutation test
#' perm_test <- permutation_SemNeT(
#'   low, high, method = "tmfg",
#'   iter = 100, # increase for more precise results
#'   cores = 2, keep_networks = TRUE, seed = 1234
#' )}
#'
#' @seealso \code{\link{bootstrap_test_SemNeT}} for bootstrap tests of group differences
#'
#' @export
# Permutation test ----
# Updated 20.10.2026
permutation_SemNeT <- function(
  sample_1 = NULL, sample_2 = NULL,
  method = c("tmfg", "cn", "nrw", "pf"), method_args = list(),
  sim = "cosine", minimum_cases = 2,
  measure = c("aspl", "cc", "q"),
  alternative = c("two_tailed", "two_tailed_centered", "two_tailed_doubled", "less", "greater"),
  iter = 1000, cores = 1, previous = NULL, keep_networks = FALSE,
  seed = NULL, alpha = .05
)
{

  # Get name of first sample
  sample_1_name <- deparse(substitute(sample_1))

  # Get name of second sample
  sample_2_name <- deparse(substitute(sample_2))

  # Argument errors
  validated <- permutation_SemNeT_errors(
    sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative, iter, cores, previous, keep_networks, seed,
    alpha
  )

  # Get first sample
  sample_1 <- validated$sample_1

  # Get second sample
  sample_2 <- validated$sample_2

  # Get method
  method <- validated$method

  # Get method arguments
  method_args <- validated$method_args

  # Get unique measures
  measure <- unique(measure)

  # Set alternative
  alternative <- match.arg(alternative)

  # Set up measure function
  measure_function <- function(A, seed = NULL){
    semantic_network_measures(A, measures = measure, weighted = FALSE, seed = seed)
  }

  # Check for previous seed
  if(!is.null(previous) && is.null(seed)){

    # Use seed of previous result
    seed <- previous$seed

  }

  # Check for random seed
  if(random_seed(seed) && (is.null(previous) || "q" %in% measure)){

    # Send message
    message("Argument 'seed' is `NULL` or 0. Results will not be reproducible. Set 'seed' for reproducible results")

  }

  # Check for previous result
  if(!is.null(previous)){

    # Get permuted networks of first sample
    networks_1 <- previous$networks$network_1

    # Get permuted networks of second sample
    networks_2 <- previous$networks$network_2

    # Get seeds
    seeds <- permutation_seeds(length(networks_1), seed)

    # Compute measures of first sample
    values_1 <- vapply(seq_along(networks_1), function(i) measure_function(networks_1[[i]], seeds[i]), numeric(length(measure)))

    # Compute measures of second sample
    values_2 <- vapply(seq_along(networks_2), function(i) measure_function(networks_2[[i]], seeds[i]), numeric(length(measure)))

    # Get counts of first sample
    counts_1 <- vapply(networks_1, network_counts, numeric(2))

    # Get counts of second sample
    counts_2 <- vapply(networks_2, network_counts, numeric(2))

  }else{

    # Get seeds
    seeds <- permutation_seeds(iter, seed)

    # Send message
    message("Estimating networks and measures...")

    # Estimate permutations
    estimated <- parallel_process(
      iter,
      function(i) permutation_iteration(
        i, sample_1, sample_2, seeds[i], method, method_args, sim, minimum_cases, measure_function, keep_networks
      ),
      cores = cores
    )

    # Get measures of first sample
    values_1 <- vapply(estimated, function(x) x$value_1, numeric(length(measure)))

    # Get measures of second sample
    values_2 <- vapply(estimated, function(x) x$value_2, numeric(length(measure)))

    # Get counts of first sample
    counts_1 <- vapply(estimated, function(x) x$counts_1, numeric(2))

    # Get counts of second sample
    counts_2 <- vapply(estimated, function(x) x$counts_2, numeric(2))

    # Get networks of first sample
    networks_1 <- if(keep_networks) lapply(estimated, function(x) x$network_1) else NULL

    # Get networks of second sample
    networks_2 <- if(keep_networks) lapply(estimated, function(x) x$network_2) else NULL

  }

  # Set measures of first sample (one row per measure)
  values_1 <- matrix(values_1, nrow = length(measure), dimnames = list(measure, NULL))

  # Set measures of second sample (one row per measure)
  values_2 <- matrix(values_2, nrow = length(measure), dimnames = list(measure, NULL))

  # Get differences between samples
  differences <- t(values_1 - values_2)

  # Get observed differences
  observed_difference <- differences[1, ]

  # Compute p-values
  p_value <- vapply(measure, function(name){
    permutation_p_value(differences[, name], observed_difference[name], alternative)
  }, numeric(1))

  # Check for previous result
  if(!is.null(previous)){

    # Use name of first sample of previous result
    sample_1_name <- names(previous$result)[1]

    # Use name of second sample of previous result
    sample_2_name <- names(previous$result)[2]

  }

  # Set direction of alternative
  direction <- switch(
    alternative,
    less = paste(sample_1_name, "<", sample_2_name),
    greater = paste(sample_1_name, ">", sample_2_name),
    paste(sample_1_name, "!=", sample_2_name)
  )

  # Set up result
  result <- data.frame(
    round(unname(values_1[, 1]), 3), round(unname(values_2[, 1]), 3), signif(unname(p_value), 3), direction,
    stringsAsFactors = FALSE
  )

  # Name columns
  names(result) <- c(sample_1_name, sample_2_name, "p_value", "alternative")

  # Name rows
  row.names(result) <- measure

  # Initialize measure columns
  value_columns <- list()

  # Loop over measures
  for(name in measure){

    # Set suffix of columns
    suffix <- if(length(measure) == 1) "value_" else paste0(name, "_")

    # Add measures of first sample
    value_columns[[paste0(suffix, "1")]] <- unname(values_1[name, ])

    # Add measures of second sample
    value_columns[[paste0(suffix, "2")]] <- unname(values_2[name, ])

  }

  # Set up diagnostics
  diagnostics <- data.frame(
    value_columns,
    nodes_1 = counts_1["nodes", ], nodes_2 = counts_2["nodes", ],
    edges_1 = counts_1["edges", ], edges_2 = counts_2["edges", ],
    row.names = NULL
  )

  # Set up output
  output <- list(
    result = result,
    networks = if(keep_networks) list(network_1 = networks_1, network_2 = networks_2) else NULL,
    method = method,
    seed = seed,
    differences = if(length(measure) == 1) unname(differences[, 1]) else differences,
    diagnostics = diagnostics,
    alpha = alpha
  )

  # Set class
  class(output) <- "permutation_SemNeT"

  # Return output
  return(output)

}

#' @method print permutation_SemNeT
#' @export
#' @noRd
# S3 Print Method ----
# Updated 12.10.2026
print.permutation_SemNeT <- function(x, ...)
{

  # Get measures
  measures <- row.names(x$result)

  # Get group names
  group_names <- names(x$result)[1:2]

  # Get differences of measures
  differences <- as.matrix(x$differences)

  # Name measures
  colnames(differences) <- measures

  # Print header
  writeLines(strwrap("Permutation test of group differences (shuffled group membership)", width = 80, exdent = 2))

  # Add break space
  cat("\n")

  # Print run settings
  print_run_settings(paste0(nrow(differences), " permutations (incl. observed)"), x$method, x$seed)

  # Check for diagnostics
  if(!is.null(x$diagnostics)){

    # Get diagnostics
    diagnostics <- x$diagnostics

    # Print network sizes
    print_group_sizes(
      group_names,
      c(mean(diagnostics$nodes_1), mean(diagnostics$nodes_2)),
      c(mean(diagnostics$edges_1), mean(diagnostics$edges_2)),
      c(diagnostics$nodes_1[1], diagnostics$nodes_2[1]),
      c(diagnostics$edges_1[1], diagnostics$edges_2[1])
    )

  }

  # Print comparison
  cat("  Comparison: ", group_names[1], " - ", group_names[2], " (H1: ", x$result$alternative[1], ")\n\n", sep = "")

  # Get quantiles of null distribution
  null_quantiles <- apply(differences, 2, quantile, probs = c(.025, .975), na.rm = TRUE, names = FALSE)

  # Set up table (columns follow the bootstrap tests' table)
  table <- data.frame(
    measure = toupper(measures),
    group_1 = format_number(x$result[[1]]), group_2 = format_number(x$result[[2]]),
    diff = format_number(differences[1, ]),
    null_range = format_interval(null_quantiles[1, ], null_quantiles[2, ]),
    null_sd = format_number(apply(differences, 2, sd, na.rm = TRUE)),
    p = format_p_value(x$result$p_value),
    stringsAsFactors = FALSE
  )

  # Name group columns
  names(table)[2:3] <- group_names

  # Set significance level
  alpha <- if(is.null(x$alpha)) .05 else x$alpha

  # Get observed differences and p-values
  observed <- differences[1, ]
  p_values <- x$result$p_value

  # Label result
  table$result <- ifelse(
    is.na(p_values) | is.na(observed), "NA",
    ifelse(
      p_values >= alpha, "n.s.",
      ifelse(observed > 0, paste(group_names[1], ">", group_names[2]), paste(group_names[1], "<", group_names[2]))
    )
  )

  # Print table
  print(table, row.names = FALSE, right = TRUE)

  # Set note on p-value
  notes <- paste(
    "p: share of permutations (observed included) with a difference at least as extreme as the observed one.",
    "result: n.s. when p >=", paste0(sub("^0", "", format(alpha)), ","),
    "otherwise the group with the larger value."
  )

  # Add note on null distribution
  notes <- paste(
    notes, "null_range: 2.5% and 97.5% quantiles of the permuted differences;",
    "null_sd: their standard deviation."
  )

  # Check for multiple measures
  if(length(measures) > 1){

    # Add note on adjustment
    notes <- paste(notes, "p-values are per measure, not adjusted for the number of measures tested.")

  }

  # Add break space
  cat("\n")

  # Print notes
  writeLines(strwrap(notes, width = 80))

  # Add break space
  cat("\n")

  # Loop over comparison notices
  for(note in comparison_notice("permutation_SemNeT")){

    # Print notice
    writeLines(strwrap(note, width = 80))

  }

  # Return object invisibly
  return(invisible(x))

}

#' @noRd
# Permutation p-value ----
# Updated 13.10.2026
permutation_p_value <- function(differences, observed, alternative)
{

  # Check for missing observed difference
  if(is.na(observed)){

    # Return missing p-value
    return(NA_real_)

  }

  # Check for undefined permuted differences
  if(anyNA(differences)){

    # Send warning
    warning(
      "In `permutation_SemNeT()`: ", sum(is.na(differences)), " of ", length(differences),
      " permutations had an undefined difference and are left out of the p-value.",
      call. = FALSE
    )

  }

  # Remove undefined differences
  differences <- differences[!is.na(differences)]

  # Set tolerance for ties
  tolerance <- 1e-8 * max(1, abs(observed))

  # Get center of null distribution
  center <- mean(differences)

  # Get proportion at least as great as observed
  greater <- mean(differences >= observed - tolerance)

  # Get proportion at most as great as observed
  less <- mean(differences <= observed + tolerance)

  # Return p-value
  return(switch(
    alternative,
    greater = greater,
    less = less,
    two_tailed = mean(abs(differences) >= abs(observed) - tolerance),
    two_tailed_centered = mean(abs(differences - center) >= abs(observed - center) - tolerance),
    two_tailed_doubled = min(1, 2 * min(greater, less))
  ))

}

#' @noRd
# Permutation seeds ----
# Updated 20.10.2026
permutation_seeds <- function(n, seed = NULL)
{

  # Get seeds
  seeds <- reproducible_seeds(n, seed)

  # Check for reproducible seed
  if(!random_seed(seed)){

    # Use seed for observed data
    seeds[1] <- seed

  }

  # Return seeds
  return(seeds)

}

#' @noRd
# Permutation iteration ----
# Updated 15.10.2026
permutation_iteration <- function(
    i, sample_1, sample_2, seed, method, method_args, sim, minimum_cases, measure_function, keep_networks
)
{

  # Check for first iteration
  if(i == 1){

    # Use original samples
    pair <- list(sample_1 = sample_1, sample_2 = sample_2)

  }else{

    # Pool samples
    pooled <- rbind(sample_1, sample_2)

    # Draw rows for first sample
    drawn <- shuffle(seq_len(nrow(pooled)), size = nrow(sample_1), seed = seed)

    # Split pooled rows into samples
    pair <- list(sample_1 = pooled[drawn, , drop = FALSE], sample_2 = pooled[-drawn, , drop = FALSE])

    # Remove pooled samples
    rm(pooled)

  }

  # Check for TMFG
  if(method == "tmfg"){

    # Get first sample
    one <- pair$sample_1

    # Get second sample
    two <- pair$sample_2

    # Check for raw responses in first sample
    if(is.character(one) || is.factor(one)){

      # Convert to binary
      one <- responses2binary(one)$binary

    }

    # Check for raw responses in second sample
    if(is.character(two) || is.factor(two)){

      # Convert to binary
      two <- responses2binary(two)$binary

    }

    # Equate samples
    equated <- equate(sample_1 = finalize(one, minimum_cases), sample_2 = finalize(two, minimum_cases))

    # Use equated samples
    pair <- list(sample_1 = equated$sample_1, sample_2 = equated$sample_2)

    # Check for similarity undefined for responses everyone gave
    if(sim %in% c("cor", "phi")){

      # Identify responses given by every participant of either sample
      constant <- colSums(pair$sample_1) == nrow(pair$sample_1) | colSums(pair$sample_2) == nrow(pair$sample_2)

      # Check for constant responses
      if(any(constant)){

        # Drop them from both samples (the samples stay equated)
        pair <- lapply(pair, function(sample) sample[, !constant, drop = FALSE])

      }

    }

  }

  # Estimate network
  estimate <- function(data){

    # Check for TMFG
    if(method == "tmfg"){

      # Compute similarity
      similarity_matrix <- similarity(data, method = sim)

      # Return TMFG network
      return(do.call(tmfg, c(list(data = similarity_matrix), method_args)))

    }

    # Get network function
    network_function <- switch(method, cn = cn, nrw = nrw, pf = pf)

    # Return network
    return(do.call(network_function, c(list(data = data), method_args)))

  }

  # Estimate network of first sample
  network_1 <- estimate(pair$sample_1)

  # Estimate network of second sample
  network_2 <- estimate(pair$sample_2)

  # Return networks, measures, and counts
  return(list(
    network_1 = if(keep_networks) network_1 else NULL,
    network_2 = if(keep_networks) network_2 else NULL,
    value_1 = measure_function(network_1, seed),
    value_2 = measure_function(network_2, seed),
    counts_1 = network_counts(network_1),
    counts_2 = network_counts(network_2)
  ))

}

#' @noRd
# Align samples ----
# Updated 13.10.2026
align_samples <- function(sample_1, sample_2)
{

  # Check for mixed types
  if(is.character(sample_1) != is.character(sample_2)){

    # Send error
    stop(
      "In `permutation_SemNeT()`: 'sample_1' and 'sample_2' must both be raw word responses or both be numeric response matrices.",
      call. = FALSE
    )

  }

  # Check for raw responses
  if(is.character(sample_1)){

    # Get widest sample
    width <- max(ncol(sample_1), ncol(sample_2))

    # Pad sample with missing responses
    pad <- function(sample){

      # Add missing columns
      sample <- cbind(sample, matrix(NA_character_, nrow = nrow(sample), ncol = width - ncol(sample)))

      # Remove column names
      colnames(sample) <- NULL

      # Return sample
      sample

    }

    # Return padded samples
    return(list(sample_1 = pad(sample_1), sample_2 = pad(sample_2)))

  }

  # Get responses of first sample
  names_1 <- colnames(sample_1)

  # Get responses of second sample
  names_2 <- colnames(sample_2)

  # Check for missing names
  if(is.null(names_1) || is.null(names_2)){

    # Check for mismatched unnamed columns
    if(ncol(sample_1) != ncol(sample_2) || !is.null(names_1) || !is.null(names_2)){

      # Send error
      stop(
        "In `permutation_SemNeT()`: numeric response matrices need column names (the responses) to be lined up, unless both have the same, unnamed columns.",
        call. = FALSE
      )

    }

    # Return samples
    return(list(sample_1 = sample_1, sample_2 = sample_2))

  }

  # Check for duplicate responses
  if(anyDuplicated(names_1) > 0 || anyDuplicated(names_2) > 0){

    # Send error
    stop("In `permutation_SemNeT()`: every response (column) name within a sample must be unique.", call. = FALSE)

  }

  # Get responses of both samples
  responses <- union(names_1, names_2)

  # Align sample to responses
  align <- function(sample){

    # Check for aligned sample
    if(identical(colnames(sample), responses)){

      # Return sample
      return(sample)

    }

    # Initialize aligned sample
    aligned <- matrix(0, nrow = nrow(sample), ncol = length(responses), dimnames = list(rownames(sample), responses))

    # Fill responses of sample
    aligned[, colnames(sample)] <- sample

    # Return aligned sample
    aligned

  }

  # Return aligned samples
  return(list(sample_1 = align(sample_1), sample_2 = align(sample_2)))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
permutation_SemNeT_errors <- function(
    sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative,
    iter, cores, previous, keep_networks, seed, alpha
)
{

  # 'previous' errors
  if(!is.null(previous)){

    # Check for class
    if(!inherits(previous, "permutation_SemNeT")){

      # Send error
      stop(
        "In `permutation_SemNeT()`: 'previous' is not a `permutation_SemNeT` object (from `permutation_SemNeT()`).",
        call. = FALSE
      )

    }

    # Check for stored networks
    if(is.null(previous$networks)){

      # Send error
      stop(
        "In `permutation_SemNeT()`: 'previous' has no stored networks to reuse -- call the original with `keep_networks = TRUE`.",
        call. = FALSE
      )

    }

    # Use method of previous result
    method <- previous$method

  }else{

    # 'method' errors
    length_error(method, c(1, 4), "permutation_SemNeT")
    typeof_error(method, "character", "permutation_SemNeT")

    # Match method
    method <- match.arg(method, c("tmfg", "cn", "nrw", "pf"))

    # 'sample_1' and 'sample_2' errors
    if(is.null(sample_1) || is.null(sample_2)){

      # Send error
      stop(
        "In `permutation_SemNeT()`: 'sample_1' and 'sample_2' must both be provided, unless 'previous' is.",
        call. = FALSE
      )

    }

    # Check for TMFG
    if(method == "tmfg"){

      # Check for response matrices
      sample_1 <- response_matrix_error(sample_1, "permutation_SemNeT")
      sample_2 <- response_matrix_error(sample_2, "permutation_SemNeT")

    }else{

      # Check for ordered response matrices
      sample_1 <- ordered_response_matrix_error(sample_1, "permutation_SemNeT")
      sample_2 <- ordered_response_matrix_error(sample_2, "permutation_SemNeT")

    }

  }

  # 'method_args' errors
  object_error(method_args, "list", "permutation_SemNeT")

  # Get network function
  network_function <- switch(method, tmfg = tmfg, cn = cn, nrw = nrw, pf = pf)

  # Get valid arguments
  valid_arguments <- setdiff(names(formals(network_function)), "data")

  # Check for invalid arguments
  if(!all(names(method_args) %in% valid_arguments)){

    # Send error
    stop(
      paste0(
        "In `permutation_SemNeT()`: 'method_args' contains an argument not used by `", method, "()`.\n",
        "Valid arguments: ", paste0(valid_arguments, collapse = ", "), "."
      ),
      call. = FALSE
    )

  }

  # 'sim' errors
  length_error(sim, 1, "permutation_SemNeT")
  typeof_error(sim, "character", "permutation_SemNeT")

  # 'minimum_cases' errors
  length_error(minimum_cases, 1, "permutation_SemNeT")
  typeof_error(minimum_cases, "numeric", "permutation_SemNeT")
  range_error(minimum_cases, c(1, Inf), "permutation_SemNeT")

  # 'measure' errors
  typeof_error(measure, "character", "permutation_SemNeT")

  # Check for unknown measures
  if(!all(measure %in% c("aspl", "cc", "q"))){

    # Send error
    stop("In `permutation_SemNeT()`: 'measure' must only contain 'aspl', 'cc', and/or 'q'.", call. = FALSE)

  }

  # 'alternative' errors
  length_error(alternative, c(1, 5), "permutation_SemNeT")
  typeof_error(alternative, "character", "permutation_SemNeT")

  # Match alternative
  alternative <- match.arg(alternative, c("two_tailed", "two_tailed_centered", "two_tailed_doubled", "less", "greater"))

  # 'iter' errors
  length_error(iter, 1, "permutation_SemNeT")
  typeof_error(iter, "numeric", "permutation_SemNeT")
  range_error(iter, c(1, Inf), "permutation_SemNeT")

  # 'cores' errors
  length_error(cores, 1, "permutation_SemNeT")
  typeof_error(cores, "numeric", "permutation_SemNeT")
  range_error(cores, c(1, Inf), "permutation_SemNeT")

  # 'keep_networks' errors
  length_error(keep_networks, 1, "permutation_SemNeT")
  typeof_error(keep_networks, "logical", "permutation_SemNeT")

  # 'seed' errors
  if(!is.null(seed)){
    length_error(seed, 1, "permutation_SemNeT")
    typeof_error(seed, "numeric", "permutation_SemNeT")
    range_error(seed, c(0, Inf), "permutation_SemNeT")
    seed_whole_error(seed, "permutation_SemNeT")
  }

  # 'alpha' errors
  length_error(alpha, 1, "permutation_SemNeT")
  typeof_error(alpha, "numeric", "permutation_SemNeT")

  # Check for limits of alpha
  if(is.na(alpha) || alpha <= 0 || alpha >= 1){

    # Send error
    stop("In `permutation_SemNeT()`: 'alpha' must be strictly between 0 and 1.", call. = FALSE)

  }

  # Check for samples
  if(is.null(previous)){

    # Align samples
    aligned <- align_samples(sample_1, sample_2)

    # Get first sample
    sample_1 <- aligned$sample_1

    # Get second sample
    sample_2 <- aligned$sample_2

  }

  # Return validated input (in case of data frames or matched 'method')
  return(list(sample_1 = sample_1, sample_2 = sample_2, method = method, method_args = method_args))

}
