#' @title Clustering Coefficient
#'
#' @description Computes the clustering coefficient (CC) of a network, either as
#' the average of the nodes' local clustering coefficients (the default) or as
#' the global clustering coefficient (transitivity)
#'
#' @param A Matrix or data frame.
#' A symmetric adjacency matrix of network data
#'
#' @param weighted Boolean.
#' Is the network weighted? The weighted coefficient generalizes the
#' triangle count to the geometric-mean triangle intensity (Onnela et al., 2005;
#' Rubinov & Sporns, 2010), which is defined for weights scaled by the network's
#' largest absolute weight, so the weights are divided by it first: CC does not
#' depend on the scale of the weights. With negative weights (a signed
#' network) a triangle's intensity can be negative, so CC can be negative; this
#' signed extension (the signed cube root of the weights) is not part of the
#' cited definitions.
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures of CC
#'
#' @param na.rm Boolean.
#' A node with fewer than two neighbors has no possible triangle, so its local
#' clustering coefficient is undefined. With \code{type = "average"}, should
#' those nodes be left out of the average (\code{TRUE}), or counted as having a
#' clustering coefficient of 0 (\code{FALSE})?
#' Counting them as 0 lowers CC in sparse networks (e.g., \code{\link{cn}},
#' \code{\link{nrw}}) in proportion to how many such nodes there are.
#' With \code{type = "global"}, such nodes add nothing to the number of
#' triangles or of connected triples, so \code{na.rm} does \emph{not} change the
#' result; it only decides whether their values in the \code{"node"}
#' attribute are \code{NA} (\code{TRUE}) or \code{0} (\code{FALSE}).
#' Defaults to \code{FALSE}
#'
#' @param type Character (length = 1).
#' How CC is computed:
#' \itemize{
#' \item \code{"average"} --- the average of the nodes' local clustering
#' coefficients (Watts & Strogatz, 1998), the density of each node's
#' neighborhood
#' \item \code{"global"} --- the global clustering coefficient (transitivity),
#' the sum of the nodes' (weighted) triangle counts divided by the number of
#' connected triples (3 x triangles / connected triples; Newman, 2003)
#' }
#' Defaults to \code{"average"}
#'
#' @details The local clustering coefficient of a node is the (weighted)
#' triangle count of the node divided by \eqn{k_i (k_i - 1)}, where \eqn{k_i} is
#' the node's number of neighbors. The average coefficient weights every node
#' equally, so nodes with few neighbors count as much as hubs. The global
#' coefficient sums triangles and triples over the nodes before dividing, so it
#' is dominated by high-degree nodes; the two can differ substantially in
#' networks with hubs. For an unweighted network, \code{type = "global"} equals
#' \code{igraph::transitivity(type = "global")}, which is \code{NaN} (as it is
#' here) when no node has two neighbors, because no triangle is then possible.
#'
#' @return Returns the network's CC. The per-node (local) clustering coefficient
#' is attached as the \code{"node"} attribute (\code{NA} for a node with fewer
#' than two neighbors when \code{na.rm = TRUE}, and \code{0} otherwise)
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 1000, replace = TRUE), nrow = 100, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' # Average of the local clustering coefficients
#' network_cc <- cc(A)
#'
#' # Global clustering coefficient (transitivity)
#' global_cc <- cc(A, type = "global")
#'
#' @references
#' Newman, M. E. J. (2003).
#' The structure and function of complex networks.
#' \emph{SIAM Review}, \emph{45}, 167-256.
#'
#' Onnela, J.-P., Saramaki, J., Kertesz, J., & Kaski, K. (2005).
#' Intensity and coherence of motifs in weighted complex networks.
#' \emph{Physical Review E}, \emph{71}, 065103.
#'
#' Rubinov, M., & Sporns, O. (2010).
#' Complex network measures of brain connectivity: Uses and interpretations.
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#'
#' Watts, D. J., & Strogatz, S. H. (1998).
#' Collective dynamics of 'small-world' networks.
#' \emph{Nature}, \emph{393}, 440-442.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Clustering coefficient ----
# Updated 15.10.2026
cc <- function(A, weighted = FALSE, na.rm = FALSE, type = c("average", "global"))
{

  # Argument errors
  A <- cc_errors(A, weighted, na.rm, type)

  # Set type
  type <- match.arg(type)

  # Remove self-loops
  diag(A) <- 0

  # Get number of neighbors of each node
  neighbor_count <- rowSums(A != 0)

  # Check for unweighted
  if(!weighted){

    # Binarize network (its cube root is itself)
    cube_root_weights <- ifelse(A != 0, 1, 0)

  }else{

    # Get largest absolute weight
    largest_weight <- max(abs(A))

    # Scale weights by largest weight
    scaled_weights <- if(largest_weight > 0) A / largest_weight else A

    # Take signed cube root of weights
    cube_root_weights <- sign(scaled_weights) * abs(scaled_weights)^(1 / 3)

  }

  # Get (weighted) triangle count of each node: the sum of triangle intensities
  # over both directions in which each triangle is traversed
  triangle_count <- diag(cube_root_weights %*% cube_root_weights %*% cube_root_weights)

  # Get number of connected triples centered on each node (ordered pairs of neighbors)
  triple_count <- neighbor_count * (neighbor_count - 1)

  # Compute local clustering coefficient of each node (0 without two neighbors)
  per_node_cc <- ifelse(triple_count > 0, triangle_count / pmax(triple_count, 1), 0)

  # Check for removing undefined nodes
  if(na.rm){

    # Set nodes with fewer than two neighbors to missing
    per_node_cc[neighbor_count < 2] <- NA

  }

  # Name nodes
  names(per_node_cc) <- colnames(A)

  # Check for average of local coefficients
  if(type == "average"){

    # Compute average local clustering coefficient
    result <- mean(per_node_cc, na.rm = na.rm)

  }else{

    # Compute global clustering coefficient
    result <- if(sum(triple_count) > 0) sum(triangle_count) / sum(triple_count) else NaN

  }

  # Attach node clustering coefficients
  attr(result, "node") <- per_node_cc

  # Return result
  return(result)

}

#' @noRd
# Errors ----
# Updated 15.10.2026
cc_errors <- function(A, weighted, na.rm, type)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "cc", require_symmetric = TRUE)

  # 'weighted' errors
  length_error(weighted, 1, "cc")
  typeof_error(weighted, "logical", "cc")

  # 'na.rm' errors
  length_error(na.rm, 1, "cc")
  typeof_error(na.rm, "logical", "cc")

  # 'type' errors
  length_error(type, c(1, 2), "cc")
  typeof_error(type, "character", "cc")

  # Return 'A' (in case of data frame)
  return(A)

}
