#' @title Average Shortest Path Length
#'
#' @description Computes the global average shortest path length of a network
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data
#'
#' @param weighted Boolean.
#' Is the network weighted?
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures
#'
#' @return Returns the network's ASPL. The per-node average shortest path
#' length and eccentricity are attached as the \code{"node"} and
#' \code{"eccentricity"} attributes, rather than being computed and discarded
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 1000, replace = TRUE), nrow = 100, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' # Unweighted
#' network_aspl <- aspl(A)
#'
#' @references
#' Rubinov, M., & Sporns, O. (2010).
#' Complex network measures of brain connectivity: Uses and interpretations.
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Average shortest path length
# Updated 06.09.2026
aspl <- function(A, weighted = FALSE)
{

  # Validate and coerce input before any computation begins
  A <- aspl_errors(A, weighted)

  # Shortest-path distance between every pair of nodes; unreachable pairs
  # are `Inf` (see `distance()` in helpers.R)
  D <- distance(A, weighted = weighted)

  # A node's distance to itself doesn't belong in its own average
  diag(D) <- NA

  # Unreachable pairs are excluded from the average entirely -- the original
  # instead zeroed them out and left them in the denominator, which silently
  # treated "unreachable" the same as "adjacent" and understated ASPL for
  # any network with even one disconnected pair (see `distance()`'s note)
  D[is.infinite(D)] <- NA

  # Per-node average distance to every node it can actually reach. A fully
  # isolated node (no finite distances at all) becomes `NaN` here, which
  # `mean(..., na.rm = TRUE)` below then correctly excludes network-wide
  per_node_aspl <- colMeans(D, na.rm = TRUE)
  names(per_node_aspl) <- colnames(A)

  # Global ASPL: the mean of every (reachable) node's own average
  result <- mean(per_node_aspl, na.rm = TRUE)

  # Eccentricity: each node's single longest shortest path to another node
  per_node_eccentricity <- apply(D, 1, function(row){
    swiftelse(all(is.na(row)), NA_real_, max(row, na.rm = TRUE))
  })
  names(per_node_eccentricity) <- colnames(A)

  # Keep the per-node detail available without changing ASPL's numeric contract
  attr(result, "node") <- per_node_aspl
  attr(result, "eccentricity") <- per_node_eccentricity

  return(result)

}

#' @noRd
# Argument errors for `aspl()`
# Updated 06.09.2026
aspl_errors <- function(A, weighted)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame). Validated
  # up front -- the original mutated 'A' via `distance()` before ever
  # checking it was square
  A <- adjacency_matrix_error(A, "aspl")

  # 'weighted' must be a single logical value
  object_error(weighted, "vector", "aspl")
  typeof_error(weighted, "logical", "aspl")
  length_error(weighted, 1, "aspl")

  return(A)

}
