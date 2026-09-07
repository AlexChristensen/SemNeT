#' @title Permutation Test for Network Measures
#'
#' @description Computes a permutation test to determine whether there's a
#' difference in a global network measure between two samples
#'
#' @param sample_1 Matrix or data frame.
#' Response matrix to be compared with \code{sample_2}
#'
#' @param sample_2 Matrix or data frame.
#' Response matrix to be compared with \code{sample_1}
#'
#' @param method Character.
#' Network estimation method to use: \code{"tmfg"}, \code{"cn"}, \code{"nrw"},
#' or \code{"pf"} -- see \code{\link{tmfg}}, \code{\link{cn}}, \code{\link{nrw}},
#' and \code{\link{pf}}
#'
#' @param method_args List.
#' Additional arguments passed on to the chosen \code{method}'s own function.
#' Defaults to \code{list()} (every method's own defaults apply)
#'
#' @param sim Character.
#' Similarity measure to use, \code{method = "tmfg"} only.
#' Defaults to \code{"cosine"}. See \code{\link{similarity}} for other options
#'
#' @param minimum_cases Numeric.
#' \strong{Only} for \code{method = "tmfg"}.
#' Minimum number of participants required to have given a response for that
#' response to be kept, via \code{\link{finalize}}, before equating the two
#' samples' vocabularies via \code{\link{equate}}.
#' Defaults to \code{2}
#'
#' @param measure Character.
#' Global network measure to compare: one of \code{"aspl"}, \code{"cc"}, or \code{"q"}
#'
#' @param alternative Character.
#' Alternative hypothesis: \code{"two_tailed"}, \code{"less"}, or \code{"greater"}.
#' Defaults to \code{"two_tailed"}
#'
#' @param iter Numeric.
#' Number of permuted samples to generate.
#' Defaults to \code{1000}
#'
#' @param cores Numeric.
#' Number of processing cores to use.
#' Defaults to \code{1} (sequential)
#'
#' @param previous A \code{permutation_SemNeT} object from a previous call.
#' Reuses its already-permuted networks to test a different \code{measure}
#' without regenerating and re-estimating every permuted sample from scratch.
#' Requires that original call to have used \code{keep_networks = TRUE}.
#' Defaults to \code{NULL}
#'
#' @param keep_networks Boolean.
#' Whether every permuted sample's estimated networks should be kept in the
#' result, in addition to the measure values already computed from them. Only
#' needed to test a second \code{measure} later via \code{previous} -- a full
#' set of \code{iter} dense networks per sample can be large, and no other use
#' needs them. Defaults to \code{FALSE}
#'
#' @return Returns a list of class \code{"permutation_SemNeT"}:
#' \item{result}{A data frame with \code{sample_1}'s and \code{sample_2}'s
#' observed \code{measure}, the permutation \emph{p}-value, and the direction
#' of the (in)equality tested}
#' \item{networks}{A list of \code{network_1}/\code{network_2}, the networks
#' estimated for every permuted sample (its first entry is each original,
#' unpermuted sample), or \code{NULL} unless \code{keep_networks = TRUE} --
#' reusable via \code{previous} only when kept}
#' \item{method}{The \code{method} that was used}
#' \item{differences}{The observed measure difference for every permutation}
#'
#' @examples
#' # Get openness data -- equating (see `equate()`) requires named responses
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # aspl
#' perm_aspl <- permutation_SemNeT(
#'   low, high, method = "tmfg", measure = "aspl", iter = 100, cores = 2,
#'   keep_networks = TRUE
#' )
#'
#' # cc, reusing the same permuted networks
#' perm_cc <- permutation_SemNeT(previous = perm_aspl, measure = "cc", cores = 2)
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Permutation test for a global network measure
# Updated 06.09.2026
permutation_SemNeT <- function(
  sample_1 = NULL, sample_2 = NULL,
  method = c("tmfg", "cn", "nrw", "pf"), method_args = list(),
  sim = "cosine", minimum_cases = 2,
  measure = c("aspl", "cc", "q"),
  alternative = c("two_tailed", "less", "greater"),
  iter = 1000, cores = 1, previous = NULL, keep_networks = FALSE
)
{

  # Captured before any reassignment below -- once 'sample_1'/'sample_2' are
  # reassigned to their validated/coerced values, they're no longer unforced
  # promises, so `substitute()` would return their (potentially huge) value
  # instead of the caller's original argument expression
  sample_1_name <- deparse(substitute(sample_1))
  sample_2_name <- deparse(substitute(sample_2))

  # Validate and coerce input before any computation begins
  validated <- permutation_SemNeT_errors(
    sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative, iter, cores, previous, keep_networks
  )
  sample_1 <- validated$sample_1
  sample_2 <- validated$sample_2
  method <- validated$method
  method_args <- validated$method_args

  measure <- match.arg(measure)
  alternative <- match.arg(alternative)
  measure_function <- switch(measure, aspl = aspl, cc = cc, q = q)

  # Reuse a previous call's already-permuted networks to test a different
  # measure, instead of regenerating and re-estimating every sample again --
  # `permutation_SemNeT_errors()` already confirmed `previous$networks` isn't
  # NULL (i.e. that call used `keep_networks = TRUE`)
  if(!is.null(previous)){

    values_1 <- vapply(previous$networks$network_1, measure_function, numeric(1))
    values_2 <- vapply(previous$networks$network_2, measure_function, numeric(1))
    networks_1 <- previous$networks$network_1
    networks_2 <- previous$networks$network_2

  }else{

    message("Generating permuted samples...")
    permuted <- generate_permuted_samples(sample_1, sample_2, iter)

    if(method == "tmfg"){

      message("Finalizing and equating samples...")

      # Raw word responses are converted to binary *after* permuting, not
      # before -- each permuted draw can contain a different mix of
      # participants, so its binary vocabulary has to be (re)built from
      # that specific draw, not inherited from the original two samples
      permuted <- lapply(permuted, function(pair){

        one <- pair$sample_1
        two <- pair$sample_2

        if(is.character(one) || is.factor(one)){
          one <- responses_to_binary(one)$binary
        }
        if(is.character(two) || is.factor(two)){
          two <- responses_to_binary(two)$binary
        }

        equated <- equate(sample_1 = finalize(one, minimum_cases), sample_2 = finalize(two, minimum_cases))

        list(sample_1 = equated$sample_1, sample_2 = equated$sample_2)

      })

    }

    # Each permuted pair's measure values are computed immediately after its
    # networks are estimated, inside `permutation_iteration()` -- the networks
    # themselves are only kept (per sample, per iteration) when 'keep_networks'
    # is TRUE, so a full `iter`-sized network list never has to accumulate in
    # memory just to compute a single measure's values
    message("Estimating networks and measures...")
    estimated <- parallel_process(
      iter,
      function(i) permutation_iteration(permuted[[i]], method, method_args, sim, measure_function, keep_networks),
      cores = cores
    )

    values_1 <- vapply(estimated, function(x) x$value_1, numeric(1))
    values_2 <- vapply(estimated, function(x) x$value_2, numeric(1))
    networks_1 <- if(keep_networks) lapply(estimated, function(x) x$network_1) else NULL
    networks_2 <- if(keep_networks) lapply(estimated, function(x) x$network_2) else NULL

  }

  # The first entry is always each original, unpermuted sample -- the
  # observed statistic being tested against the permuted null distribution
  observed_1 <- values_1[1]
  observed_2 <- values_2[1]
  observed_difference <- observed_1 - observed_2

  differences <- values_1 - values_2

  p_value <- switch(
    alternative,
    greater = mean(differences >= observed_difference, na.rm = TRUE),
    less = mean(differences <= observed_difference, na.rm = TRUE),
    two_tailed = mean(abs(differences) >= abs(observed_difference), na.rm = TRUE)
  )

  # A reused `previous` result already has its own names recorded
  if(!is.null(previous)){
    sample_1_name <- names(previous$result)[1]
    sample_2_name <- names(previous$result)[2]
  }

  direction <- switch(
    alternative,
    less = paste(sample_1_name, "<", sample_2_name),
    greater = paste(sample_1_name, ">", sample_2_name),
    two_tailed = paste(sample_1_name, "!=", sample_2_name)
  )

  result <- data.frame(
    round(observed_1, 3), round(observed_2, 3), round(p_value, 3), direction, stringsAsFactors = FALSE
  )
  names(result) <- c(sample_1_name, sample_2_name, "p_value", "alternative")
  row.names(result) <- measure

  output <- list(
    result = result,
    networks = if(keep_networks || !is.null(previous)) list(network_1 = networks_1, network_2 = networks_2) else NULL,
    method = method,
    differences = differences
  )

  class(output) <- "permutation_SemNeT"

  return(output)

}

#' @noRd
# 'iter' permuted (sample_1, sample_2) pairs, drawn by reshuffling which
# rows of the pooled data belong to each sample -- the first pair is always
# the two original, unpermuted samples
# Updated 06.09.2026
generate_permuted_samples <- function(sample_1, sample_2, iter)
{

  pooled <- rbind(sample_1, sample_2)
  n_1 <- nrow(sample_1)
  n_pooled <- nrow(pooled)

  permuted <- vector("list", iter)
  permuted[[1]] <- list(sample_1 = sample_1, sample_2 = sample_2)

  for(i in seq_len(iter)[-1]){
    # `sample.int()` instead of `sample(seq_len(n_pooled), ...)` -- verified
    # bit-for-bit identical draws, ~15-20% faster (`microbenchmark`)
    drawn <- sample.int(n_pooled, n_1)
    permuted[[i]] <- list(sample_1 = pooled[drawn, , drop = FALSE], sample_2 = pooled[-drawn, , drop = FALSE])
  }

  return(permuted)

}

#' @noRd
# One permuted pair's two estimated networks and measure values -- one code
# path for both samples (instead of the original's separately-written,
# copy-pasted per-sample blocks, which is exactly where its `enrich <-`/
# `thresold`/wrong-parameter-name bugs lived). Each network's measure value is
# computed immediately, here, rather than in a second pass over a fully
# materialized `iter`-sized network list -- the network itself is only kept
# (per sample) when 'keep_networks' is TRUE
# Updated 07.09.2026
permutation_iteration <- function(pair, method, method_args, sim, measure_function, keep_networks)
{

  estimate <- function(data){

    if(method == "tmfg"){
      similarity_matrix <- similarity(data, method = sim)
      return(do.call(tmfg, c(list(data = similarity_matrix), method_args)))
    }

    network_function <- switch(method, cn = cn, nrw = nrw, pf = pf)
    return(do.call(network_function, c(list(data = data), method_args)))

  }

  network_1 <- estimate(pair$sample_1)
  network_2 <- estimate(pair$sample_2)

  return(list(
    network_1 = if(keep_networks) network_1 else NULL,
    network_2 = if(keep_networks) network_2 else NULL,
    value_1 = measure_function(network_1),
    value_2 = measure_function(network_2)
  ))

}

#' @noRd
# Argument errors for `permutation_SemNeT()`
# Updated 06.09.2026
permutation_SemNeT_errors <- function(sample_1, sample_2, method, method_args, sim, minimum_cases, measure, alternative, iter, cores, previous, keep_networks)
{

  if(!is.null(previous)){

    if(!inherits(previous, "permutation_SemNeT")){
      stop(
        "In `permutation_SemNeT()`: 'previous' is not a `permutation_SemNeT` object (from `permutation_SemNeT()`).",
        call. = FALSE
      )
    }

    if(is.null(previous$networks)){
      stop(
        "In `permutation_SemNeT()`: 'previous' has no stored networks to reuse -- call the original with `keep_networks = TRUE`.",
        call. = FALSE
      )
    }

    method <- previous$method

  }else{

    if(is.null(sample_1) || is.null(sample_2)){
      stop(
        "In `permutation_SemNeT()`: 'sample_1' and 'sample_2' must both be provided, unless 'previous' is.",
        call. = FALSE
      )
    }

    # 'method' must be one (and only one) of the documented options
    method <- match.arg(method, c("tmfg", "cn", "nrw", "pf"))

    # Every sample must be a response matrix in the shape its method needs --
    # same rule `bootstrap_SemNeT()` uses. `tmfg`'s conversion to binary
    # happens later, *after* permuting (see `permutation_SemNeT()` below),
    # since raw responses are pooled and reshuffled first
    if(method == "tmfg"){

      sample_1 <- response_matrix_error(sample_1, "permutation_SemNeT")
      sample_2 <- response_matrix_error(sample_2, "permutation_SemNeT")

    }else if(method %in% c("cn", "nrw")){
      sample_1 <- ordered_response_matrix_error(sample_1, "permutation_SemNeT")
      sample_2 <- ordered_response_matrix_error(sample_2, "permutation_SemNeT")
    }else{
      sample_1 <- response_matrix_error(sample_1, "permutation_SemNeT")
      sample_2 <- response_matrix_error(sample_2, "permutation_SemNeT")
    }

  }

  # 'method_args' must be a list of arguments the chosen method actually has
  object_error(method_args, "list", "permutation_SemNeT")

  network_function <- switch(method, tmfg = tmfg, cn = cn, nrw = nrw, pf = pf)
  valid_arguments <- setdiff(names(formals(network_function)), "data")

  if(!all(names(method_args) %in% valid_arguments)){
    stop(
      paste0(
        "In `permutation_SemNeT()`: 'method_args' contains an argument not used by `", method, "()`.\n",
        "Valid arguments: ", paste0(valid_arguments, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # 'sim' must be a single character string
  length_error(sim, 1, "permutation_SemNeT")
  typeof_error(sim, "character", "permutation_SemNeT")

  # 'minimum_cases' must be a single non-negative whole number
  length_error(minimum_cases, 1, "permutation_SemNeT")
  typeof_error(minimum_cases, "numeric", "permutation_SemNeT")
  range_error(minimum_cases, c(0, Inf), "permutation_SemNeT")

  # 'measure' must be one (and only one) of the documented options
  measure <- match.arg(measure, c("aspl", "cc", "q"))

  # 'alternative' must be one (and only one) of the documented options
  alternative <- match.arg(alternative, c("two_tailed", "less", "greater"))

  # 'iter' must be a single positive whole number
  length_error(iter, 1, "permutation_SemNeT")
  typeof_error(iter, "numeric", "permutation_SemNeT")
  range_error(iter, c(1, Inf), "permutation_SemNeT")

  # 'cores' must be a single positive whole number
  length_error(cores, 1, "permutation_SemNeT")
  typeof_error(cores, "numeric", "permutation_SemNeT")
  range_error(cores, c(1, Inf), "permutation_SemNeT")

  # 'keep_networks' must be a single logical value
  object_error(keep_networks, "vector", "permutation_SemNeT")
  typeof_error(keep_networks, "logical", "permutation_SemNeT")
  length_error(keep_networks, 1, "permutation_SemNeT")

  return(list(sample_1 = sample_1, sample_2 = sample_2, method = method, method_args = method_args))

}
