#' @title Average Shortest Path Length
#'
#' @description Computes the global average shortest path length of a network
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data
#'
#' @param weighted Boolean.
#' Is the network weighted? A weighted edge's length is the inverse of its
#' weight (weights are strengths), so the value depends on the scale of the
#' weights and negative weights are not allowed.
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures
#'
#' @param type Character (length = 1).
#' How the shortest-path distances are averaged.
#' Defaults to \code{"arithmetic"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"arithmetic"} --- The mean of every node's mean distance to the
#' nodes it can reach. Unreachable pairs are left out of the average
#'
#' \item \code{"harmonic"} --- The harmonic mean of the distances between all
#' pairs of nodes, the inverse of the global efficiency
#' (Latora & Marchiori, 2001): \eqn{1 / \mathrm{mean}(1 / d_{ij})} over all
#' pairs of distinct nodes. Unreachable pairs have an infinite distance, whose
#' inverse is 0, so they are counted instead of left out
#'
#' }
#'
#' @details For a connected network, \code{"arithmetic"} is the usual average
#' shortest path length (e.g., \code{igraph::mean_distance()}), and
#' \code{"harmonic"} is smaller than it. They differ most for a disconnected
#' network: \code{"arithmetic"} ignores unreachable pairs, which makes a
#' fragmented network look better connected than it is and weights each node by
#' the size of its component, whereas \code{"harmonic"} counts them. A network
#' with no connected pair has an \code{Inf} harmonic ASPL.
#'
#' @return Returns the network's ASPL. The per-node average distance
#' (\code{type}-matched; \code{NaN} for an isolated node when
#' \code{"arithmetic"}, \code{Inf} when \code{"harmonic"}) and the
#' eccentricity (the longest finite shortest path) are attached as the
#' \code{"node"} and \code{"eccentricity"} attributes, rather than being
#' computed and discarded
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
#' Latora, V., & Marchiori, M. (2001).
#' Efficient behavior of small-world networks.
#' \emph{Physical Review Letters}, \emph{87}, 198701.
#'
#' Rubinov, M., & Sporns, O. (2010).
#' Complex network measures of brain connectivity: Uses and interpretations.
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Average shortest path length ----
# Updated 13.10.2026
aspl <- function(A, weighted = FALSE, type = c("arithmetic", "harmonic"))
{

  # Argument errors
  A <- aspl_errors(A, weighted, type)

  # Set type
  type <- match.arg(type)

  # Get shortest path distances
  D <- distance(A, weighted = weighted)

  # Remove self-distances
  diag(D) <- NA

  # Copy distances
  reachable <- D

  # Remove unreachable pairs
  reachable[is.infinite(reachable)] <- NA

  # Check for arithmetic
  if(type == "arithmetic"){

    # Get mean distance of each node
    per_node_aspl <- rowMeans(reachable, na.rm = TRUE)

    # Get mean of node means
    result <- mean(per_node_aspl, na.rm = TRUE)

  }else{

    # Get inverse distances
    inverse <- 1 / D

    # Get harmonic mean distance of each node
    per_node_aspl <- 1 / rowMeans(inverse, na.rm = TRUE)

    # Get harmonic mean distance of network
    result <- 1 / mean(inverse, na.rm = TRUE)

  }

  # Name nodes
  names(per_node_aspl) <- colnames(A)

  # Get eccentricity of each node
  per_node_eccentricity <- apply(reachable, 1, function(row){
    swiftelse(all(is.na(row)), NA_real_, max(row, na.rm = TRUE))
  })

  # Name nodes
  names(per_node_eccentricity) <- colnames(A)

  # Attach node ASPL
  attr(result, "node") <- per_node_aspl

  # Attach node eccentricity
  attr(result, "eccentricity") <- per_node_eccentricity

  # Return result
  return(result)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
aspl_errors <- function(A, weighted, type)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "aspl")

  # 'weighted' errors
  length_error(weighted, 1, "aspl")
  typeof_error(weighted, "logical", "aspl")

  # 'type' errors
  length_error(type, c(1, 2), "aspl")
  typeof_error(type, "character", "aspl")

  # Check for negative weights
  if(weighted && any(A < 0, na.rm = TRUE)){

    # Send error
    stop(
      "In `aspl()`: weighted path lengths use the inverse of each weight, which is undefined for negative weights. ",
      "Use `weighted = FALSE` or take the absolute value of the network first.",
      call. = FALSE
    )

  }

  # Return 'A' (in case of data frame)
  return(A)

}
