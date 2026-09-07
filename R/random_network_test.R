#' @title Test Against Random Networks
#'
#' @description Performs significance tests for global measures of semantic
#' networks against the global measures of equivalent size (and density)
#' random networks
#'
#' @param ... Named matrices or data frames.
#' Semantic networks to be compared against random networks, e.g.
#' \code{random_network_test(low = low_net, high = high_net)}
#'
#' @param iter Numeric.
#' Number of random networks to generate per input network.
#' Defaults to \code{1000}
#'
#' @param cores Numeric.
#' Number of computer processing cores to use.
#' Defaults to \code{1}
#'
#' @return Returns a named list (one entry per input network) of data frames
#' with the \emph{p}-value, and the random-network distribution's mean and
#' standard deviation, for each of \code{aspl}, \code{cc}, and \code{q}
#'
#' @examples
#' # Get openness data
#' one <- open.clean[which(open.group == "Low"), ]
#' two <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Compute networks
#' net_one <- cn(one)
#' net_two <- cn(two)
#'
#' # Perform random networks test
#' random_network_test(low = net_one, high = net_two, iter = 100, cores = 2)
#' }
#'
#' @references
#' Viger, F., & Latapy, M. (2016).
#' Efficient and simple generation of random simple connected graphs with prescribed degree sequence.
#' \emph{Journal of Complex Networks}, \emph{4}, 15-37.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats pnorm sd
#'
#' @export
# Test observed network measures against a random-network null distribution
# Updated 06.09.2026
random_network_test <- function(..., iter = 1000, cores = 1)
{

  # Validate and coerce input before any computation begins -- named
  # arguments (`low = low_net`) replace the original's `substitute()`-based
  # name-sniffing entirely, and work correctly under `do.call()` too
  networks <- random_network_test_errors(list(...), iter, cores)

  message("Generating random networks and measures...")

  # One batch of 'iter' random networks (same size and density as the
  # observed network) per input network -- each one is reduced to its 3
  # global measures immediately, right here, rather than in a later second
  # pass over a fully materialized `iter`-sized network list. Nothing this
  # function returns ever needs the random networks themselves (only their
  # measures, via `random_network_significance()` below), so there's no
  # reason for all `iter` of them to exist in memory simultaneously
  random_measures <- lapply(networks, function(observed){

    # `cn()`/`nrw()`/`pf()` (this function's intended inputs) never put
    # anything on the diagonal, but a network estimated a different way
    # (e.g. `tmfg()`'s `diag = 1` convention) would otherwise inflate the
    # edge count below and bias every random network to be denser than
    # the observed one
    diag(observed) <- 0

    batch <- parallel_process(
      iter,
      function(i){
        random_network <- generate_random_network(nodes = ncol(observed), edges = floor(sum(observed) / 2))
        semantic_network_measures(random_network)
      },
      cores = cores,
      progress = TRUE
    )

    # `batch` is a list of `iter` named 3-value measure vectors -- `sapply()`
    # simplifies it into the same 3 x `iter` matrix shape the old two-pass
    # version produced
    simplify2array(batch)
  })

  # Compare each observed network's own measures against its batch's null distribution
  results <- Map(
    function(observed, batch_measures) random_network_significance(observed, batch_measures),
    networks,
    random_measures
  )

  return(results)

}

#' @noRd
# One network's observed measures vs. its random-network null distribution,
# as a two-tailed z-test
# Updated 06.09.2026
random_network_significance <- function(observed, batch_measures)
{

  observed_measures <- semantic_network_measures(observed)

  random_mean <- rowMeans(batch_measures)
  random_sd <- apply(batch_measures, 1, sd)

  z <- (observed_measures - random_mean) / random_sd
  p_value <- 2 * pnorm(-abs(z))

  data.frame(
    p_value = round(p_value, 4),
    random_mean = round(random_mean, 4),
    random_sd = round(random_sd, 4),
    row.names = names(observed_measures)
  )

}

#' @noRd
# A random network with the same number of nodes and edges as an observed one
# (an Erdos-Renyi-style random graph, used as the null-model comparison)
# Updated 06.09.2026
generate_random_network <- function(nodes, edges)
{

  full <- matrix(1, nrow = nodes, ncol = nodes)
  diag(full) <- 0

  # Every possible edge position (upper triangle only, since undirected)
  possible_edges <- which(upper.tri(full) == TRUE)

  # Randomly choose exactly 'edges' of them to actually connect. `seq_len()`
  # (not `1:edges`) matters here: an all-zero observed network legitimately
  # has `edges == 0`, and `1:0` is `c(1, 0)`, not empty -- since a 0 index is
  # silently dropped rather than erroring, that would select 1 edge instead
  # of the intended 0
  chosen_edges <- possible_edges[sample(length(possible_edges))][seq_len(edges)]

  random <- matrix(0, nrow = nodes, ncol = nodes)
  random[chosen_edges] <- 1
  random <- random + t(random)

  return(random)

}

#' @noRd
# Argument errors for `random_network_test()`
# Updated 06.09.2026
random_network_test_errors <- function(networks, iter, cores)
{

  # At least one network is required
  if(length(networks) < 1){
    stop("In `random_network_test()`: at least one network must be provided via `...`.", call. = FALSE)
  }

  # Every network must be named -- required in place of the original's
  # `substitute()`-based name-sniffing
  if(is.null(names(networks)) || any(names(networks) == "")){
    stop(
      "In `random_network_test()`: every network passed via `...` must be named, e.g. `random_network_test(low = low_net, high = high_net)`.",
      call. = FALSE
    )
  }

  # Every network's name must be unique -- the returned list is named via
  # `Map()`, which would otherwise carry a duplicate name through, and
  # `$`/`[[` access would then silently return only the first network's result
  if(anyDuplicated(names(networks)) > 0){
    stop("In `random_network_test()`: every network passed via `...` must have a unique name.", call. = FALSE)
  }

  # Every network must be a valid square, numeric adjacency matrix --
  # `lapply()` over a named list preserves those names on the result
  networks <- lapply(networks, function(net) adjacency_matrix_error(net, "random_network_test"))

  # 'iter'/'cores' must each be a single positive whole number
  for(argument in list(iter = iter, cores = cores)){
    length_error(argument, 1, "random_network_test")
    typeof_error(argument, "numeric", "random_network_test")
    range_error(argument, c(1, Inf), "random_network_test")
  }

  return(networks)

}
