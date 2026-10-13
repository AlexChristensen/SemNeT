#' @title Modularity
#'
#' @description Computes a global modularity measure (Q) using the Louvain
#' community detection algorithm
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data. Must be symmetric
#'
#' @param resolution Numeric.
#' Adjusts modularity to prefer smaller (\code{resolution > 1}) or larger
#' (\code{0 < resolution < 1}) communities.
#' Defaults to \code{1} (standard modularity)
#'
#' @param seed Numeric.
#' A seed for the Louvain algorithm's internal random number generator, for
#' reproducible community detection.
#' Defaults to \code{NULL} (not reproducible); \code{0} also gives random
#' results. Note this is independent of R's own \code{set.seed()} -- see Details
#'
#' @param signed Boolean.
#' Whether modularity should be computed on the signed network (Gomez et
#' al., 2009), discounting negative edges instead of folding them in as
#' positive.
#' Defaults to \code{FALSE} (the original, \code{abs()}-based behavior)
#'
#' @return Returns Q, a measure of how well the network partitions into
#' distinct, densely-connected communities. A network without edges has no
#' defined modularity (\code{NaN})
#'
#' @details Community detection runs via a compiled Louvain implementation
#' (Blondel et al., 2008) whose internal randomness depends only on
#' \code{seed}, not on R's global random number generator -- unlike
#' \code{igraph::cluster_louvain()}, calling \code{set.seed()} beforehand has
#' no effect on \code{q()}'s reproducibility; pass \code{seed} directly instead.
#' A given nonzero \code{seed} always gives the same partition; \code{seed = 0}
#' (like \code{NULL}) seeds from the clock.
#'
#' With \code{signed = TRUE}, the partition (found on the absolute-value
#' network) is scored as
#' \eqn{Q = \frac{W^+}{W^+ + W^-} Q^+ - \frac{W^-}{W^+ + W^-} Q^-}
#' (Gomez et al., 2009), where \eqn{W^+} and \eqn{W^-} are the total positive
#' and absolute negative weights and \eqn{Q^+} and \eqn{Q^-} are the modularity
#' of the positive and of the absolute negative edges.
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
# Louvain modularity ----
# Updated 13.10.2026
q <- function(A, resolution = 1, seed = NULL, signed = FALSE)
{

  # Argument errors
  A <- q_errors(A, resolution, seed, signed)

  # Remove self-loops
  diag(A) <- 0

  # Check for no edges
  if(all(A == 0)){

    # Return undefined modularity
    return(NaN)

  }

  # Find communities on absolute weights
  communities <- louvain(abs(A), resolution = resolution, seed = seed)

  # Check for signed modularity
  if(signed){

    # Return signed modularity of communities
    return(signed_modularity(A, communities$membership, resolution))

  }

  # Return modularity
  return(communities$modularity)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
q_errors <- function(A, resolution, seed, signed)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "q", require_symmetric = TRUE)

  # 'resolution' errors
  length_error(resolution, 1, "q")
  typeof_error(resolution, "numeric", "q")
  range_error(resolution, c(0, Inf), "q")

  # Check for zero resolution
  if(resolution <= 0){

    # Send error
    stop("In `q()`: 'resolution' must be greater than 0.", call. = FALSE)

  }

  # 'seed' errors
  if(!is.null(seed)){
    length_error(seed, 1, "q")
    typeof_error(seed, "numeric", "q")
    range_error(seed, c(0, Inf), "q")
  }

  # 'signed' errors
  length_error(signed, 1, "q")
  typeof_error(signed, "logical", "q")

  # Return 'A' (in case of data frame)
  return(A)

}

#' @noRd
# Louvain community detection (Blondel et al., 2008) ----
# Updated 06.09.2026
louvain <- function(network, resolution = 1, seed = NULL)
{

  # Get seed for compiled code
  seed_value <- compiled_seed(seed)

  # Ensure matrix
  if(!is.matrix(network)){
    network <- as.matrix(network)
  }

  # Run compiled Louvain
  result <- .Call(
    "r_louvain",
    network, as.double(resolution), as.double(seed_value),
    0L, # order = "higher": full multilevel aggregation to convergence
    PACKAGE = "SemNeT"
  )

  # Get final level
  final_level <- dim(result$memberships)[1]

  # Return membership and modularity of final level
  return(list(
    membership = result$memberships[final_level, ],
    modularity = result$modularity[final_level]
  ))

}

#' @noRd
# Signed modularity (Gomez et al., 2009) ----
# Updated 06.09.2026
signed_modularity <- function(network, membership, resolution = 1)
{

  # Ensure matrix
  if(!is.matrix(network)){
    network <- as.matrix(network)
  }

  # Return signed modularity from compiled code
  .Call(
    "r_signed_modularity",
    network, as.integer(membership), as.double(resolution),
    PACKAGE = "SemNeT"
  )

}
