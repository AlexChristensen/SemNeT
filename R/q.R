#' @title Modularity
#'
#' @description Computes a global modularity measure (Q) using the Louvain
#' community detection algorithm
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data
#'
#' @param resolution Numeric.
#' Adjusts modularity to prefer smaller (\code{resolution > 1}) or larger
#' (\code{0 < resolution < 1}) communities.
#' Defaults to \code{1} (standard modularity)
#'
#' @param seed Numeric.
#' A seed for the Louvain algorithm's internal random number generator, for
#' reproducible community detection.
#' Defaults to \code{NULL} (not reproducible). Note this is independent of
#' R's own \code{set.seed()} -- see Details
#'
#' @param signed Boolean.
#' Whether modularity should be computed on the signed network (Gomez et
#' al., 2009), discounting negative edges instead of folding them in as
#' positive.
#' Defaults to \code{FALSE} (the original, \code{abs()}-based behavior)
#'
#' @return Returns Q, a measure of how well the network partitions into
#' distinct, densely-connected communities
#'
#' @details Community detection runs via a compiled Louvain implementation
#' (Blondel et al., 2008) whose internal randomness depends only on
#' \code{seed}, not on R's global random number generator -- unlike
#' \code{igraph::cluster_louvain()}, calling \code{set.seed()} beforehand has
#' no effect on \code{q()}'s reproducibility; pass \code{seed} directly instead.
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 1000, replace = TRUE), nrow = 100, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' modularity <- q(A, seed = 1)
#'
#' @references
#' Blondel, V. D., Guillaume, J. L., Lambiotte, R., & Lefebvre, E. (2008).
#' Fast unfolding of communities in large networks.
#' \emph{Journal of Statistical Mechanics: Theory and Experiment}, \emph{2008}, P10008.
#'
#' Gomez, S., Jensen, P., & Arenas, A. (2009).
#' Analysis of community structure in networks of correlated data.
#' \emph{Physical Review E}, \emph{80}, 016114.
#'
#' Rubinov, M., & Sporns, O. (2010).
#' Complex network measures of brain connectivity: Uses and interpretations.
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @useDynLib SemNeT, .registration = TRUE
#'
#' @export
# Louvain community detection modularity
# Updated 06.09.2026
q <- function(A, resolution = 1, seed = NULL, signed = FALSE)
{

  # Validate and coerce input before any computation begins
  A <- q_errors(A, resolution, seed, signed)

  # A node's own self-loop (e.g. TMFG's `diag(A) <- 1` convention) is never
  # a real edge, and both `modularity_values()` and `signed_modularity()`
  # treat every nonzero cell -- diagonal included -- as one. The original
  # avoided this via `convert2igraph()`'s `diag = FALSE`; stripped here for
  # the same reason before either computation ever sees the network
  diag(A) <- 0

  # Community membership is always found on the unsigned (absolute-value)
  # network -- connection *strength*, not sign, is what determines which
  # nodes cluster together, matching the original's `abs(A)` behavior
  communities <- louvain(abs(A), resolution = resolution, seed = seed)

  # The unsigned modularity is already computed as a byproduct of finding
  # the communities; the signed variant (Gomez et al., 2009) instead scores
  # that same partition while discounting negative edges rather than
  # folding them in as positive
  if(signed){
    return(signed_modularity(A, communities$membership, resolution))
  }

  return(communities$modularity)

}

#' @noRd
# Argument errors for `q()`
# Updated 06.09.2026
q_errors <- function(A, resolution, seed, signed)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame) --
  # the original had no validation at all here
  A <- adjacency_matrix_error(A, "q")

  # 'resolution' must be a single positive number
  length_error(resolution, 1, "q")
  typeof_error(resolution, "numeric", "q")
  range_error(resolution, c(0, Inf), "q")

  # 'seed' must be `NULL` or a single number
  if(!is.null(seed)){
    length_error(seed, 1, "q")
    typeof_error(seed, "numeric", "q")
  }

  # 'signed' must be a single logical value
  object_error(signed, "vector", "q")
  typeof_error(signed, "logical", "q")
  length_error(signed, 1, "q")

  return(A)

}

#' @noRd
# Compiled Louvain community detection (Blondel et al., 2008)
# Vendored from {EGAnet} (same author, see src/louvain.c) -- replaces
# `igraph::cluster_louvain()`. Its randomness depends only on `seed`, not
# R's global RNG, so results are reproducible regardless of R's RNG state,
# call order, or parallel worker. Used only by `q()`, so kept local here
# rather than in helpers.R
# Updated 06.09.2026
louvain <- function(network, resolution = 1, seed = NULL)
{

  # A `seed` of exactly 0 tells the C code "not reproducible, use clock
  # time" -- `NULL` (the R-level default) maps onto that same convention
  seed_value <- swiftelse(is.null(seed), 0, seed)

  # `as.matrix()` is a generic with real `UseMethod()` dispatch overhead
  # (benchmarked ~10x slower than a direct `is.matrix()` check) -- worth
  # skipping here since this is an internal, `@noRd` function with exactly
  # one caller (`q()`), which always hands it an already-validated matrix
  if(!is.matrix(network)){
    network <- as.matrix(network)
  }

  result <- .Call(
    "r_louvain",
    network, as.double(resolution), as.double(seed_value),
    0L, # order = "higher": full multilevel aggregation to convergence
    PACKAGE = "SemNeT"
  )

  # Only the finest/highest-modularity level is needed here -- the last row
  # is always the level actually reached, regardless of how many aggregation
  # levels ran
  final_level <- dim(result$memberships)[1]

  return(list(
    membership = result$memberships[final_level, ],
    modularity = result$modularity[final_level]
  ))

}

#' @noRd
# Signed modularity scoring (Gomez et al., 2009)
# Vendored from {EGAnet} (same author, see src/signed_modularity.c) --
# scores a given community partition while discounting negative edges,
# rather than folding them in as positive via `abs()`. Used only by `q()`,
# so kept local here rather than in helpers.R
# Updated 06.09.2026
signed_modularity <- function(network, membership, resolution = 1)
{

  # Same rationale as `louvain()` above: skip `as.matrix()`'s dispatch
  # overhead when the (internal-only) caller already has a plain matrix
  if(!is.matrix(network)){
    network <- as.matrix(network)
  }

  .Call(
    "r_signed_modularity",
    network, as.integer(membership), as.double(resolution),
    PACKAGE = "SemNeT"
  )

}
