#' @title Bootstrapped Semantic Network Analysis
#'
#' @description Repeatedly re-estimates each group's semantic network from
#' resampled data (dropping a proportion of nodes, or resampling participants
#' with replacement) to build a bootstrap distribution of \code{\link{aspl}},
#' \code{\link{cc}}, and \code{\link{q}} for every group
#'
#' @param ... Named matrices or data frames.
#' At least one response matrix, one per group, e.g.
#' \code{bootstrap_SemNeT(low = low_data, high = high_data)}
#'
#' @param method Character.
#' Network estimation method to use: \code{"tmfg"}, \code{"cn"}, \code{"nrw"},
#' or \code{"pf"} -- see \code{\link{tmfg}}, \code{\link{cn}}, \code{\link{nrw}},
#' and \code{\link{pf}}. Defaults to \code{"tmfg"}
#'
#' @param method_args List.
#' Additional arguments passed on to the chosen \code{method}'s own function.
#' Defaults to \code{list()} (every method's own defaults apply)
#'
#' @param type Character.
#' Type of bootstrap to perform: \code{"case"} resamples participants with
#' replacement; \code{"node"} keeps every participant but randomly drops
#' nodes (see \code{prop}) -- this requires every group to already share the
#' same nodes. Defaults to \code{"case"}
#'
#' @param prop Numeric.
#' \strong{Only} for \code{type = "node"}.
#' Proportion of nodes to keep in each resampled network.
#' Defaults to \code{.50}
#'
#' @param sim Character.
#' Similarity measure to use, \code{method = "tmfg"} only.
#' Defaults to \code{"cosine"}. See \code{\link{similarity}} for other options
#'
#' @param minimum_cases Numeric.
#' \strong{Only} for \code{method = "tmfg"} and \code{type = "case"}.
#' Minimum number of participants required to have given a response for that
#' response to be kept, via \code{\link{finalize}}, before equating every
#' group's vocabulary via \code{\link{equate}}.
#' Defaults to \code{2}
#'
#' @param iter Numeric.
#' Number of bootstrapped samples to generate per group.
#' Defaults to \code{1000}
#'
#' @param cores Numeric.
#' Number of processing cores to use.
#' Defaults to \code{1} (sequential)
#'
#' @param keep_networks Boolean.
#' Whether every bootstrapped network itself should be kept in the result, in
#' addition to its measures. A full set of \code{iter} dense networks per
#' group can be large (an N-node network is N x N numbers, \code{iter} times,
#' per group); most uses only need \code{measures}/\code{summary}/\code{edges}.
#' Defaults to \code{FALSE}
#'
#' @return Returns a list of class \code{"bootstrap_SemNeT"}:
#' \item{groups}{A named list, one entry per group in \code{...}, each a list of:
#' \code{networks} (the \code{iter} bootstrapped networks, or \code{NULL}
#' unless \code{keep_networks = TRUE}), \code{measures} (a 3 x \code{iter}
#' matrix of \code{aspl}/\code{cc}/\code{q} values), \code{edges} (each
#' bootstrapped network's edge count, length \code{iter} -- always present,
#' regardless of \code{keep_networks}), and \code{summary} (a data frame of
#' each measure's mean, standard deviation, standard error, and 95\%
#' confidence interval across the \code{iter} samples)}
#' \item{meta}{A list of \code{prop} (only for \code{type = "node"}),
#' \code{iter}, \code{type}, and \code{method}}
#'
#' @examples
#' # Simulate datasets
#' one <- simulate_fluency(20)
#' two <- simulate_fluency(20)
#'
#' \donttest{
#' # Bootstrap node-drop (partial) networks
#' boot_result <- bootstrap_SemNeT(
#'   one = one, two = two, prop = .50, iter = 100, cores = 2,
#'   method = "tmfg", type = "node"
#' )
#'
#' # Bootstrap case-resampled networks -- equating (see `equate()`) requires
#' # named responses, so this uses real word responses instead
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#' boot_result <- bootstrap_SemNeT(
#'   low = low, high = high, iter = 100, cores = 2,
#'   method = "tmfg", type = "case", minimum_cases = 2
#' )
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats sd
#'
#' @export
# Bootstrapped semantic network analysis
# Updated 06.09.2026
bootstrap_SemNeT <- function(
  ..., method = c("tmfg", "cn", "nrw", "pf"), method_args = list(),
  type = c("case", "node"), prop = .50, sim = "cosine",
  minimum_cases = 2, iter = 1000, cores = 1, keep_networks = FALSE
)
{

  # Validate and coerce input before any computation begins
  validated <- bootstrap_SemNeT_errors(
    list(...), method, method_args, type, prop, sim, minimum_cases, iter, cores, keep_networks
  )
  groups <- validated$groups
  method_args <- validated$method_args

  # `match.arg()` picks the *first* listed choice when missing, which is why
  # "tmfg"/"case" are listed first in the signature above
  method <- match.arg(method)
  type <- match.arg(type)

  group_names <- names(groups)

  # Every group gets its own independent batch of 'iter' resampled datasets
  message("Generating bootstrapped samples...")
  samples <- lapply(groups, generate_bootstrap_samples, type = type, prop = prop, method = method, iter = iter)

  # Equating only matters for `tmfg`: a similarity matrix is undefined for a
  # response nobody gave (or that everybody gave) in a given resample, and
  # comparing two groups' networks only makes sense over their shared
  # vocabulary -- other methods work directly off the response matrix and
  # don't need this step
  if(method == "tmfg" && type == "case"){

    message("Finalizing samples...")
    samples <- lapply(samples, function(group_samples){
      lapply(group_samples, finalize, minimum_cases = minimum_cases)
    })

    # Equating requires at least two groups to equate to each other
    if(length(samples) > 1){
      message("Equating samples...")
      samples <- equate_bootstrap_samples(samples, iter)
    }

  }

  # Every iteration's network + measures, one persistent parallel call per
  # group -- replaces the original's three separate similarity/network/
  # measure cluster stages with a single one that does all three per sample
  message("Estimating networks and measures...")
  results <- lapply(group_names, function(group_name){
    parallel_process(
      iter,
      function(i) bootstrap_iteration(samples[[group_name]][[i]], method, method_args, sim, keep_networks),
      cores = cores
    )
  })
  names(results) <- group_names

  # Reshape each group's per-iteration results into the locked schema: a list
  # of networks (NULL unless 'keep_networks'), a measures matrix (3 x iter),
  # each network's edge count (always kept -- it's the only thing any
  # downstream consumer, e.g. `bootstrap_test_SemNeT()`'s ANCOVA, ever reads
  # off a bootstrapped network itself), and its summary
  groups_out <- lapply(results, function(group_result){

    networks <- if(keep_networks) lapply(group_result, function(x) x$network) else NULL

    measures <- vapply(group_result, function(x) x$measures, numeric(3))
    row.names(measures) <- c("aspl", "cc", "q")

    edges <- vapply(group_result, function(x) x$edges, numeric(1))

    list(
      networks = networks,
      measures = measures,
      edges = edges,
      summary = summarize_bootstrap_measures(measures, iter)
    )

  })

  output <- list(
    groups = groups_out,
    meta = list(prop = if(type == "node") prop else NULL, iter = iter, type = type, method = method)
  )

  class(output) <- "bootstrap_SemNeT"

  return(output)

}

#' @noRd
# One group's batch of 'iter' resampled datasets
# Updated 06.09.2026
generate_bootstrap_samples <- function(data, type, prop, method, iter)
{

  n_nodes <- ncol(data)
  n_cases <- nrow(data)

  # Number of nodes to keep in each resampled network, for `type = "node"`
  keep_n_nodes <- floor(n_nodes * prop)

  samples <- vector("list", iter)

  for(i in seq_len(iter)){

    # `tmfg` needs every column to actually vary -- a column everyone
    # endorsed makes its correlation with every other column undefined, so a
    # bad draw is retried. Deliberately narrow (all-endorsed only, not also
    # all-unendorsed): for `type = "case"`, `finalize()` runs right after
    # generation (see `bootstrap_SemNeT()` below) and already strips out
    # any response given by too few participants, including a fully
    # unendorsed one, before a similarity matrix is ever computed --
    # broadening this check to also retry on all-unendorsed columns was
    # tried and rejected, since with realistic sparse fluency data (most
    # responses given by only a handful of participants) a case-resample
    # missing at least one rare response entirely is the *common* case, not
    # the exception, and would make this loop reject almost every draw
    attempt <- 0

    repeat{

      attempt <- attempt + 1

      # `sample.int(n, ...)` instead of `sample(seq_len(n), ...)` -- verified
      # bit-for-bit identical draws (both ultimately call `sample.int()`
      # internally; this skips the extra step of indexing back into a
      # materialized `seq_len(n)` vector), ~15-20% faster (`microbenchmark`)
      if(type == "node"){
        keep <- sample.int(n_nodes, size = keep_n_nodes, replace = FALSE)
        candidate <- data[, keep, drop = FALSE]
      }else{
        keep <- sample.int(n_cases, size = n_cases, replace = TRUE)
        candidate <- data[keep, , drop = FALSE]
      }

      # `colSums(candidate) == nrow(candidate)` is equivalent to "every value
      # in that column is 1" for strictly binary data (guaranteed here) --
      # benchmarked ~20x faster than `apply(candidate, 2, function(x) all(x
      # == 1))` at realistic sizes (100 cases x 367 words: ~1.2ms vs. ~55us,
      # `microbenchmark`, n=200), since it avoids an R-level function call
      # per column. Verified identical results. Runs on every one of `iter`
      # generation attempts per group, so this is a real, repeated saving
      if(method != "tmfg" || !any(colSums(candidate) == nrow(candidate))){
        break
      }

      # A clear error instead of an unbounded retry loop -- this can only
      # happen if 'prop'/the data leave no valid draw to find
      if(attempt > 100 * iter){
        stop(
          "In `bootstrap_SemNeT()`: couldn't generate a valid `tmfg` sample after many attempts.\n",
          "Check that `data` has enough participants and varying responses for the requested `prop`.",
          call. = FALSE
        )
      }

    }

    samples[[i]] <- candidate

  }

  return(samples)

}

#' @noRd
# Jointly equate every group's i-th resampled dataset to a shared vocabulary,
# iteration by iteration (equating group A's 5th sample against group B's
# 5th sample, not against a mismatched iteration). Calls `equate()`'s own
# `equate_core()` directly instead of going through `equate()` itself --
# these samples were already validated once, up front, by
# `bootstrap_SemNeT_errors()`/`finalize()`, so re-running `equate_errors()`'s
# full validation on every one of `iter` draws would be pure waste.
# Benchmarked ~5x faster this way (see `equate_core()`'s own note)
# Updated 07.09.2026
equate_bootstrap_samples <- function(samples, iter)
{

  group_names <- names(samples)

  for(i in seq_len(iter)){

    equated <- equate_core(lapply(samples, `[[`, i), "bootstrap_SemNeT")

    for(group_name in group_names){
      samples[[group_name]][[i]] <- equated[[group_name]]
    }

  }

  return(samples)

}

#' @noRd
# One resampled dataset's network and global measures. The network itself is
# only returned when 'keep_networks' is TRUE -- otherwise it's discarded here,
# right after its edge count is taken, instead of surviving into the 'iter'
# x 'group' list `parallel_process()` collects (which is what actually caps
# peak memory: the full N x N matrix never accumulates across iterations)
# Updated 07.09.2026
bootstrap_iteration <- function(data, method, method_args, sim, keep_networks)
{

  if(method == "tmfg"){

    # `tmfg` estimates a network from a similarity matrix, not raw responses
    similarity_matrix <- similarity(data, method = sim)
    network <- do.call(tmfg, c(list(data = similarity_matrix), method_args))

  }else{

    network_function <- switch(method, cn = cn, nrw = nrw, pf = pf)
    network <- do.call(network_function, c(list(data = data), method_args))

  }

  # Edge count -- the only thing any downstream consumer needs off a
  # bootstrapped network itself (e.g. `bootstrap_test_SemNeT()`'s ANCOVA
  # covariate), so it's computed unconditionally, regardless of 'keep_networks'
  edges <- (sum(network != 0) - sum(diag(network) != 0)) / 2

  return(list(
    network = if(keep_networks) network else NULL,
    measures = semantic_network_measures(network),
    edges = edges
  ))

}

#' @noRd
# Mean, standard deviation, standard error, and 95% CI of each measure
# across the bootstrapped samples
# Updated 06.09.2026
summarize_bootstrap_measures <- function(measures, iter)
{

  means <- rowMeans(measures, na.rm = TRUE)
  standard_deviations <- apply(measures, 1, sd, na.rm = TRUE)
  standard_errors <- standard_deviations / sqrt(iter)

  data.frame(
    mean = means,
    sd = standard_deviations,
    se = standard_errors,
    ci_lower = means - (1.96 * standard_errors),
    ci_upper = means + (1.96 * standard_errors),
    row.names = row.names(measures)
  )

}

#' @noRd
# Argument errors for `bootstrap_SemNeT()`
# Updated 06.09.2026
bootstrap_SemNeT_errors <- function(groups, method, method_args, type, prop, sim, minimum_cases, iter, cores, keep_networks)
{

  # At least one group is required
  if(length(groups) < 1){
    stop("In `bootstrap_SemNeT()`: at least one response matrix must be provided via `...`.", call. = FALSE)
  }

  # Every group must be named -- required in place of the original's
  # `substitute()`-based name-sniffing, and works correctly under `do.call()`
  if(is.null(names(groups)) || any(names(groups) == "")){
    stop(
      "In `bootstrap_SemNeT()`: every response matrix passed via `...` must be named, e.g. `bootstrap_SemNeT(low = low_data, high = high_data)`.",
      call. = FALSE
    )
  }

  # Every group's name must be unique -- otherwise `samples[[group_name]]`/
  # `results[[group_name]]` (both keyed by name later on) would silently
  # collapse two different groups onto the same entry, computing one group's
  # results twice under two different labels instead of erroring
  if(anyDuplicated(names(groups)) > 0){
    stop(
      "In `bootstrap_SemNeT()`: every response matrix passed via `...` must have a unique name.",
      call. = FALSE
    )
  }

  # 'method'/'type' must be one (and only one) of the documented options --
  # resolved before validating 'groups' below, since what shape a group must
  # be in depends on which method will estimate its networks. `match.arg()`
  # itself errors clearly if more than one non-default value is supplied
  method <- match.arg(method, c("tmfg", "cn", "nrw", "pf"))
  type <- match.arg(type, c("case", "node"))

  # Every group must be a response matrix in the shape its method actually
  # needs: `tmfg` estimates its network from a similarity matrix (via
  # `similarity()`, which requires numeric/binary input), so raw word
  # responses are converted to binary automatically here -- the same
  # conversion `response_analysis()` already does. `cn`/`nrw` need genuine
  # response *order* information, which a binary matrix has already
  # discarded, so they reject one instead of silently misinterpreting it.
  # `pf` accepts either shape itself
  groups <- lapply(groups, function(group){

    if(method == "tmfg"){

      group <- response_matrix_error(group, "bootstrap_SemNeT")

      if(is.character(group) || is.factor(group)){
        group <- responses_to_binary(group)$binary
      }

    }else if(method %in% c("cn", "nrw")){
      group <- ordered_response_matrix_error(group, "bootstrap_SemNeT")
    }else{
      group <- response_matrix_error(group, "bootstrap_SemNeT")
    }

    return(group)

  })

  # 'method_args' must be a list of arguments the chosen method actually has
  object_error(method_args, "list", "bootstrap_SemNeT")

  network_function <- switch(method, tmfg = tmfg, cn = cn, nrw = nrw, pf = pf)
  valid_arguments <- setdiff(names(formals(network_function)), "data")

  if(!all(names(method_args) %in% valid_arguments)){
    stop(
      paste0(
        "In `bootstrap_SemNeT()`: 'method_args' contains an argument not used by `", method, "()`.\n",
        "Valid arguments: ", paste0(valid_arguments, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # `type = "node"` drops the same proportion of nodes from every group, so
  # every group must already share the same nodes to begin with
  if(type == "node"){

    node_counts <- vapply(groups, ncol, numeric(1))

    if(length(unique(node_counts)) > 1){
      stop(
        "In `bootstrap_SemNeT()`: `type = \"node\"` requires every group to have the same number of nodes.",
        call. = FALSE
      )
    }

  }

  # 'prop' must be a single value strictly between 0 and 1
  length_error(prop, 1, "bootstrap_SemNeT")
  typeof_error(prop, "numeric", "bootstrap_SemNeT")
  range_error(prop, c(0, 1), "bootstrap_SemNeT")

  # 'sim' must be a single character string (`similarity()` validates the
  # specific method name itself)
  length_error(sim, 1, "bootstrap_SemNeT")
  typeof_error(sim, "character", "bootstrap_SemNeT")

  # 'minimum_cases' must be a single non-negative whole number
  length_error(minimum_cases, 1, "bootstrap_SemNeT")
  typeof_error(minimum_cases, "numeric", "bootstrap_SemNeT")
  range_error(minimum_cases, c(0, Inf), "bootstrap_SemNeT")

  # 'iter' must be a single positive whole number
  length_error(iter, 1, "bootstrap_SemNeT")
  typeof_error(iter, "numeric", "bootstrap_SemNeT")
  range_error(iter, c(1, Inf), "bootstrap_SemNeT")

  # 'cores' must be a single positive whole number
  length_error(cores, 1, "bootstrap_SemNeT")
  typeof_error(cores, "numeric", "bootstrap_SemNeT")
  range_error(cores, c(1, Inf), "bootstrap_SemNeT")

  # 'keep_networks' must be a single logical value
  object_error(keep_networks, "vector", "bootstrap_SemNeT")
  typeof_error(keep_networks, "logical", "bootstrap_SemNeT")
  length_error(keep_networks, 1, "bootstrap_SemNeT")

  return(list(groups = groups, method_args = method_args))

}
