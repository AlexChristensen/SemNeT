#' @title Bootstrapped Semantic Network Analysis
#'
#' @description Estimates each group's semantic network from \code{iter}
#' resampled datasets (participants resampled with replacement or a proportion
#' of nodes dropped) to build bootstrap distributions of \code{\link{aspl}},
#' \code{\link{cc}}, and \code{\link{q}}
#'
#' @param ... Named matrices or data frames.
#' One response matrix per group (at least one), e.g.,
#' \code{bootstrap_SemNeT(low = low_data, high = high_data)}
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
#' @param type Character (length = 1).
#' Type of bootstrap.
#' Defaults to \code{"case"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"case"} --- Resamples participants with replacement
#'
#' \item \code{"node"} --- Keeps every participant and randomly drops nodes
#' (see \code{prop}). Requires every group to share the same nodes. Each
#' iteration drops the \emph{same} nodes from every group, so a group
#' contrast within an iteration is not affected by which nodes happened to be
#' kept
#'
#' }
#'
#' @param prop Numeric (length = 1).
#' \strong{Only} for \code{type = "node"}.
#' Proportion of nodes to keep in each resampled network.
#' Defaults to \code{.50}
#'
#' @param sim Character (length = 1).
#' Similarity measure for \code{method = "tmfg"}.
#' Defaults to \code{"cosine"}.
#' See \code{\link{similarity}} for other options
#'
#' @param minimum_cases Numeric (length = 1).
#' \strong{Only} for \code{method = "tmfg"} and \code{type = "case"}.
#' Minimum number of participants who gave a response for it to be kept
#' (\code{\link{finalize}}) before the groups' vocabularies are equated
#' (\code{\link{equate}}). It is applied to each resample, in which a
#' participant can appear more than once, so a response only one participant
#' gave can survive in a resample (when that participant is drawn twice or more)
#' even though the full data would drop it. Resampled networks therefore tend to
#' contain somewhat different vocabularies than the observed network.
#' Defaults to \code{2}
#'
#' @param iter Numeric (length = 1).
#' Number of bootstrap samples per group.
#' Defaults to \code{1000}
#'
#' @param cores Numeric (length = 1).
#' Number of processing cores.
#' Defaults to \code{1} (sequential)
#'
#' @param keep_networks Boolean (length = 1).
#' Should every bootstrapped network be kept?
#' These can be large (\code{iter} dense networks per group)
#' and are not needed for testing.
#' Defaults to \code{FALSE}
#'
#' @param seed Numeric (length = 1).
#' Defaults to \code{NULL} or random results (\code{0} is random too).
#' Set a nonzero value for reproducible results.
#' Each bootstrap sample (and its \code{\link{q}} community detection) gets its
#' own seed, so results do not depend on \code{set.seed()}, \code{cores}, or
#' the order workers finish
#'
#' @return Returns a list containing:
#'
#' \item{groups}{A named list with one list per group containing
#' \code{networks} (the bootstrapped networks; \code{NULL} unless
#' \code{keep_networks = TRUE}), \code{measures} (a 3 x \code{iter} matrix of
#' \code{aspl}, \code{cc}, and \code{q}), \code{edges} and \code{nodes}
#' (each bootstrapped network's counts), \code{observed} (the \code{measures},
#' \code{edges}, and \code{nodes} of the network estimated from the full data;
#' \code{NA} if it could not be estimated), and \code{summary} (each measure's
#' bootstrap mean, standard error, and 95\% percentile range of the bootstrap
#' values; this describes the spread of the resampled statistics, which for a
#' case bootstrap is not centred on, and is not a confidence interval for, the
#' observed value -- see \code{observed} and \code{\link{bootstrap_test_SemNeT}})}
#'
#' \item{meta}{A list containing \code{prop} (\code{type = "node"} only),
#' \code{iter}, \code{type}, \code{method}, and \code{seed}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' # Simulate datasets
#' one <- simulate_fluency(20)
#' two <- simulate_fluency(20)
#'
#' \donttest{
#' # Bootstrap node-drop (partial) networks
#' boot_result <- bootstrap_SemNeT(
#'   one = one, two = two, prop = .50,
#'   iter = 100, # increase for more precise results
#'   cores = 2, method = "tmfg", type = "node", seed = 1234
#' )
#'
#' # Bootstrap case-resampled networks -- equating (see `equate()`) requires
#' # named responses, so this uses real word responses instead
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#' boot_result <- bootstrap_SemNeT(
#'   low = low, high = high,
#'   iter = 100, # increase for more precise results
#'   cores = 2, method = "tmfg", type = "case", minimum_cases = 2
#' )
#' }
#'
#' @seealso \code{\link{bootstrap_test_SemNeT}} for testing group differences and
#' \code{\link[=plot.bootstrap_SemNeT]{plot}} for plotting the distributions
#'
#' @export
# Bootstrap semantic networks ----
# Updated 14.10.2026
bootstrap_SemNeT <- function(
  ..., method = c("tmfg", "cn", "nrw", "pf"), method_args = list(),
  type = c("case", "node"), prop = .50, sim = "cosine",
  minimum_cases = 2, iter = 1000, cores = 1, keep_networks = FALSE,
  seed = NULL
)
{

  # Argument errors
  validated <- bootstrap_SemNeT_errors(
    list(...), method, method_args, type, prop, sim, minimum_cases, iter, cores, keep_networks, seed
  )

  # Get groups
  groups <- validated$groups

  # Get method arguments
  method_args <- validated$method_args

  # Set method
  method <- match.arg(method)

  # Set type
  type <- match.arg(type)

  # Get group names
  group_names <- names(groups)

  # Get seeds of iterations
  seeds <- reproducible_seeds(iter * length(groups), seed)

  # Split seeds by group
  seeds <- split(seeds, factor(rep(group_names, each = iter), levels = group_names))

  # Check for random seed
  if(random_seed(seed)){

    # Send message
    message("Argument 'seed' is `NULL` or 0. Results will not be reproducible. Set 'seed' for reproducible results")

  }

  # Send message
  message("Estimating networks and measures...")

  # Bootstrap iterations
  replicates <- parallel_process(
    iter,
    function(i) bootstrap_replicate(
      i, groups, seeds, type, prop, method, method_args, sim, minimum_cases, keep_networks, iter
    ),
    cores = cores
  )

  # Regroup iterations by group
  results <- lapply(group_names, function(group_name){
    lapply(replicates, `[[`, group_name)
  })

  # Name results by group
  names(results) <- group_names

  # Remove iterations
  rm(replicates)

  # Send message
  message("Estimating observed networks...")

  # Get observed results of full data
  observed <- observed_group_results(groups, method, method_args, type, sim, minimum_cases, seed)

  # Format results of each group
  groups_out <- lapply(results, function(group_result){

    # Get networks
    networks <- if(keep_networks) lapply(group_result, function(x) x$network) else NULL

    # Get measures
    measures <- vapply(group_result, function(x) x$measures, numeric(3))

    # Name measures
    row.names(measures) <- c("aspl", "cc", "q")

    # Get edges
    edges <- vapply(group_result, function(x) x$edges, numeric(1))

    # Get nodes
    nodes <- vapply(group_result, function(x) x$nodes, numeric(1))

    # Return group results
    list(
      networks = networks,
      measures = measures,
      edges = edges,
      nodes = nodes,
      observed = NULL,
      summary = summarize_bootstrap_measures(measures)
    )

  })

  # Loop over groups
  for(group_name in group_names){

    # Add observed results
    groups_out[[group_name]]$observed <- observed[[group_name]]

  }

  # Set up output
  output <- list(
    groups = groups_out,
    meta = list(prop = if(type == "node") prop else NULL, iter = iter, type = type, method = method, seed = seed)
  )

  # Set class
  class(output) <- "bootstrap_SemNeT"

  # Return output
  return(output)

}

#' @noRd
# Bootstrap replicate ----
# Updated 12.10.2026
bootstrap_replicate <- function(
    i, groups, seeds, type, prop, method, method_args, sim, minimum_cases, keep_networks, iter
)
{

  # Get group names
  group_names <- names(groups)

  # Check for node type
  if(type == "node"){

    # Sample same nodes in every group
    samples <- generate_node_samples(groups, seeds[[group_names[1]]][i], prop, needs_variation(method, sim), iter)

  }else{

    # Sample cases of each group
    samples <- lapply(group_names, function(group_name){
      generate_bootstrap_sample(groups[[group_name]], seeds[[group_name]][i], needs_variation(method, sim), iter)
    })

    # Name samples by group
    names(samples) <- group_names

  }

  # Check for case resampling with TMFG
  if(method == "tmfg" && type == "case"){

    # Remove rare responses
    samples <- lapply(samples, finalize, minimum_cases = minimum_cases)

    # Check for more than one group
    if(length(samples) > 1){

      # Equate groups
      samples <- equate_core(samples, "bootstrap_SemNeT")

    }

  }

  # Estimate network and measures of each group
  results <- lapply(group_names, function(group_name){
    bootstrap_iteration(
      samples[[group_name]], method, method_args, sim, keep_networks,
      seed = seeds[[group_name]][i]
    )
  })

  # Name results by group
  names(results) <- group_names

  # Return results
  return(results)

}

#' @noRd
# Generate case bootstrap sample ----
# Updated 13.10.2026
generate_bootstrap_sample <- function(data, seed, retry, iter)
{

  # Get cases
  case_sequence <- seq_len(nrow(data))

  # Start attempts
  attempt <- 0

  # Repeat until valid sample
  repeat{

    # Update attempts
    attempt <- attempt + 1

    # Get seed of attempt
    attempt_seed <- if(attempt == 1) seed else reproducible_seeds(attempt, seed)[attempt]

    # Sample cases with replacement
    candidate <- data[shuffle_replace(case_sequence, seed = attempt_seed), , drop = FALSE]

    # Check for valid sample
    if(!retry || !any(colSums(candidate) == nrow(candidate))){

      # Stop repeating
      break

    }

    # Check for too many attempts
    check_attempts(attempt, iter)

  }

  # Return sample
  return(candidate)

}

#' @noRd
# Generate node bootstrap samples ----
# Updated 13.10.2026
generate_node_samples <- function(groups, seed, prop, retry, iter)
{

  # Get nodes
  node_sequence <- seq_len(ncol(groups[[1]]))

  # Get number of nodes to keep
  keep_n_nodes <- floor(length(node_sequence) * prop)

  # Start attempts
  attempt <- 0

  # Repeat until valid sample
  repeat{

    # Update attempts
    attempt <- attempt + 1

    # Get seed of attempt
    attempt_seed <- if(attempt == 1) seed else reproducible_seeds(attempt, seed)[attempt]

    # Sample nodes without replacement
    keep <- shuffle(node_sequence, size = keep_n_nodes, seed = attempt_seed)

    # Keep nodes in every group
    candidates <- lapply(groups, function(group) group[, keep, drop = FALSE])

    # Check for valid sample
    if(!retry || !any(vapply(candidates, function(candidate) any(colSums(candidate) == nrow(candidate)), logical(1)))){

      # Stop repeating
      break

    }

    # Check for too many attempts
    check_attempts(attempt, iter)

  }

  # Return samples
  return(candidates)

}

#' @noRd
# Check for need of variation ----
# Updated 13.10.2026
needs_variation <- function(method, sim)
{

  # Return whether responses must vary
  method == "tmfg" && sim %in% c("cor", "phi")

}

#' @noRd
# Check attempts ----
# Updated 13.10.2026
check_attempts <- function(attempt, iter)
{

  # Check for too many attempts
  if(attempt > 100 * iter){

    # Send error
    stop(
      "In `bootstrap_SemNeT()`: couldn't generate a valid `tmfg` sample after many attempts.\n",
      "Check that `data` has enough participants and varying responses for the requested `prop`.",
      call. = FALSE
    )

  }

}

#' @noRd
# Bootstrap iteration ----
# Updated 11.10.2026
bootstrap_iteration <- function(data, method, method_args, sim, keep_networks, seed = NULL)
{

  # Check for TMFG
  if(method == "tmfg"){

    # Compute similarity
    similarity_matrix <- similarity(data, method = sim)

    # Estimate network
    network <- do.call(tmfg, c(list(data = similarity_matrix), method_args))

  }else{

    # Get network function
    network_function <- switch(method, cn = cn, nrw = nrw, pf = pf)

    # Estimate network
    network <- do.call(network_function, c(list(data = data), method_args))

  }

  # Get node and edge counts
  counts <- network_counts(network)

  # Return network, measures, and counts
  return(list(
    network = if(keep_networks) network else NULL,
    measures = semantic_network_measures(network, seed = seed),
    edges = unname(counts["edges"]),
    nodes = unname(counts["nodes"])
  ))

}

#' @noRd
# Observed results of groups ----
# Updated 11.10.2026
observed_group_results <- function(groups, method, method_args, type, sim, minimum_cases, seed = NULL)
{

  # Set results for failed estimation
  unavailable <- list(measures = c(aspl = NA_real_, cc = NA_real_, q = NA_real_), edges = NA_real_, nodes = NA_real_)

  # Estimate observed networks
  results <- tryCatch({

    # Get data
    data <- groups

    # Check for case resampling with TMFG
    if(method == "tmfg" && type == "case"){

      # Remove rare responses
      data <- lapply(data, finalize, minimum_cases = minimum_cases)

      # Check for more than one group
      if(length(data) > 1){

        # Equate groups
        data <- equate_core(data, "bootstrap_SemNeT")

      }

    }

    # Estimate network of each group
    lapply(data, function(group_data){

      # Estimate network and measures
      result <- bootstrap_iteration(group_data, method, method_args, sim, keep_networks = FALSE, seed = seed)

      # Return measures and counts
      list(measures = result$measures, edges = result$edges, nodes = result$nodes)

    })

  }, error = function(e){

    # Send message
    message("Observed (full-sample) networks could not be estimated (", conditionMessage(e), "); bias correction will be unavailable.")

    # Return nothing
    NULL

  })

  # Check for failed estimation
  if(is.null(results)){

    # Set unavailable results
    results <- lapply(groups, function(group) unavailable)

  }

  # Return results
  return(results)

}

#' @noRd
# Summarize bootstrap measures ----
# Updated 10.10.2026
summarize_bootstrap_measures <- function(measures)
{

  # Get means
  means <- rowMeans(measures, na.rm = TRUE)

  # Get standard errors
  standard_errors <- apply(measures, 1, sd, na.rm = TRUE)

  # Get percentile intervals
  intervals <- t(apply(measures, 1, quantile, probs = c(.025, .975), na.rm = TRUE, names = FALSE))

  # Return summary
  data.frame(
    mean = means,
    se = standard_errors,
    ci_lower = intervals[, 1],
    ci_upper = intervals[, 2],
    row.names = row.names(measures)
  )

}

#' @noRd
# Errors ----
# Updated 13.10.2026
bootstrap_SemNeT_errors <- function(groups, method, method_args, type, prop, sim, minimum_cases, iter, cores, keep_networks, seed)
{

  # '...' errors
  if(length(groups) < 1){

    # Send error
    stop("In `bootstrap_SemNeT()`: at least one response matrix must be provided via `...`.", call. = FALSE)

  }

  # Check for unnamed groups
  if(is.null(names(groups)) || any(names(groups) == "")){

    # Send error
    stop(
      "In `bootstrap_SemNeT()`: every response matrix passed via `...` must be named, e.g. `bootstrap_SemNeT(low = low_data, high = high_data)`.",
      call. = FALSE
    )

  }

  # Check for duplicate names
  if(anyDuplicated(names(groups)) > 0){

    # Send error
    stop(
      "In `bootstrap_SemNeT()`: every response matrix passed via `...` must have a unique name.",
      call. = FALSE
    )

  }

  # 'method' errors
  length_error(method, c(1, 4), "bootstrap_SemNeT")
  typeof_error(method, "character", "bootstrap_SemNeT")

  # Match method
  method <- match.arg(method, c("tmfg", "cn", "nrw", "pf"))

  # 'type' errors
  length_error(type, c(1, 2), "bootstrap_SemNeT")
  typeof_error(type, "character", "bootstrap_SemNeT")

  # Match type
  type <- match.arg(type, c("case", "node"))

  # Check each group
  groups <- lapply(groups, function(group){

    # Check for TMFG
    if(method == "tmfg"){

      # Check for response matrix
      group <- response_matrix_error(group, "bootstrap_SemNeT")

      # Check for raw responses
      if(is.character(group) || is.factor(group)){

        # Convert to binary
        group <- responses2binary(group)$binary

      }

    }else{

      # Check for ordered response matrix
      group <- ordered_response_matrix_error(group, "bootstrap_SemNeT")

    }

    # Return group
    return(group)

  })

  # 'method_args' errors
  object_error(method_args, "list", "bootstrap_SemNeT")

  # Get network function
  network_function <- switch(method, tmfg = tmfg, cn = cn, nrw = nrw, pf = pf)

  # Get valid arguments
  valid_arguments <- setdiff(names(formals(network_function)), "data")

  # Check for invalid arguments
  if(!all(names(method_args) %in% valid_arguments)){

    # Send error
    stop(
      paste0(
        "In `bootstrap_SemNeT()`: 'method_args' contains an argument not used by `", method, "()`.\n",
        "Valid arguments: ", paste0(valid_arguments, collapse = ", "), "."
      ),
      call. = FALSE
    )

  }

  # Check for node type
  if(type == "node"){

    # Get number of nodes of each group
    node_counts <- vapply(groups, ncol, numeric(1))

    # Check for different numbers of nodes
    if(length(unique(node_counts)) > 1){

      # Send error
      stop(
        "In `bootstrap_SemNeT()`: `type = \"node\"` requires every group to have the same number of nodes.",
        call. = FALSE
      )

    }

    # Get node names of each group
    node_names <- lapply(groups, colnames)

    # Check for node names
    if(!any(vapply(node_names, is.null, logical(1)))){

      # Check for different nodes
      if(!all(vapply(node_names, setequal, logical(1), y = node_names[[1]]))){

        # Send error
        stop(
          "In `bootstrap_SemNeT()`: `type = \"node\"` requires every group to have the same nodes (column names).",
          call. = FALSE
        )

      }

      # Align nodes of groups
      groups <- lapply(groups, function(group) group[, node_names[[1]], drop = FALSE])

    }

  }

  # 'prop' errors
  length_error(prop, 1, "bootstrap_SemNeT")
  typeof_error(prop, "numeric", "bootstrap_SemNeT")
  range_error(prop, c(0, 1), "bootstrap_SemNeT")

  # 'sim' errors
  length_error(sim, 1, "bootstrap_SemNeT")
  typeof_error(sim, "character", "bootstrap_SemNeT")

  # Check for need of variation
  if(needs_variation(method, sim)){

    # Identify groups with responses given by everyone
    constant <- vapply(groups, function(group) any(colSums(group) == nrow(group)), logical(1))

    # Check for constant responses
    if(any(constant)){

      # Send error
      stop(
        "In `bootstrap_SemNeT()`: in group(s) ", paste0("'", names(groups)[constant], "'", collapse = ", "),
        " a response was given by every participant, so a '", sim, "' similarity is undefined.\n",
        "Remove such responses first (see `finalize()`) or use another `sim` (e.g., \"cosine\").",
        call. = FALSE
      )

    }

  }

  # 'minimum_cases' errors
  length_error(minimum_cases, 1, "bootstrap_SemNeT")
  typeof_error(minimum_cases, "numeric", "bootstrap_SemNeT")
  range_error(minimum_cases, c(0, Inf), "bootstrap_SemNeT")

  # 'iter' errors
  length_error(iter, 1, "bootstrap_SemNeT")
  typeof_error(iter, "numeric", "bootstrap_SemNeT")
  range_error(iter, c(1, Inf), "bootstrap_SemNeT")

  # 'cores' errors
  length_error(cores, 1, "bootstrap_SemNeT")
  typeof_error(cores, "numeric", "bootstrap_SemNeT")
  range_error(cores, c(1, Inf), "bootstrap_SemNeT")

  # 'keep_networks' errors
  length_error(keep_networks, 1, "bootstrap_SemNeT")
  typeof_error(keep_networks, "logical", "bootstrap_SemNeT")

  # 'seed' errors
  if(!is.null(seed)){
    length_error(seed, 1, "bootstrap_SemNeT")
    typeof_error(seed, "numeric", "bootstrap_SemNeT")
    range_error(seed, c(0, Inf), "bootstrap_SemNeT")
  }

  # Return validated input (in case of data frames or raw responses)
  return(list(groups = groups, method_args = method_args))

}
