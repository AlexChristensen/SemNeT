#' @title Clustering Coefficient
#'
#' @description Computes the global clustering coefficient (CC) of a network
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data
#'
#' @param weighted Boolean.
#' Is the network weighted? The weighted coefficient (Rubinov & Sporns, 2010)
#' is defined for weights scaled by the network's largest absolute weight, so
#' the weights are divided by it first: CC does not depend on the scale of the
#' weights, and for a signed network a node's coefficient can be negative.
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures of CC
#'
#' @param na.rm Boolean.
#' A node with fewer than two neighbors has no possible triangle, so its
#' clustering coefficient is undefined. Should those nodes be left out of the
#' average (\code{TRUE}), or counted as having a clustering coefficient of 0
#' (\code{FALSE})?
#' Counting them as 0 lowers CC in sparse networks (e.g., \code{\link{cn}},
#' \code{\link{nrw}}) in proportion to how many such nodes there are.
#' Defaults to \code{FALSE}
#'
#' @return Returns the network's CC. The per-node clustering coefficient is
#' attached as the \code{"node"} attribute (\code{NA} for a node with fewer than
#' two neighbors when \code{na.rm = TRUE}), rather than being computed and discarded
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 1000, replace = TRUE), nrow = 100, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' # Unweighted
#' network_cc <- cc(A)
#'
#' @references
#' Rubinov, M., & Sporns, O. (2010).
#' Complex network measures of brain connectivity: Uses and interpretations.
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Clustering coefficient ----
# Updated 13.10.2026
cc <- function(A, weighted = FALSE, na.rm = FALSE)
{

  # Argument errors
  A <- cc_errors(A, weighted, na.rm)

  # Remove self-loops
  diag(A) <- 0

  # Get number of nodes
  n <- ncol(A)

  # Get number of neighbors of each node
  neighbor_count <- rowSums(A != 0)

  # Check for unweighted
  if(!weighted){

    # Binarize network
    binary <- ifelse(A != 0, 1, 0)

    # Initialize node clustering coefficients
    per_node_cc <- numeric(n)

    # Loop over nodes
    for(node in seq_len(n)){

      # Get neighbors of node
      neighbors <- which(binary[node, ] != 0)

      # Get degree of node
      degree <- length(neighbors)

      # Check for at least two neighbors
      if(degree >= 2){

        # Get network among neighbors
        neighborhood <- binary[neighbors, neighbors]

        # Compute density of neighborhood
        per_node_cc[node] <- sum(neighborhood) / (degree^2 - degree)

      }

    }

  }else{

    # Get degree of each node
    degree <- colSums(A != 0)

    # Get largest absolute weight
    largest_weight <- max(abs(A))

    # Scale weights by largest weight
    scaled_weights <- if(largest_weight > 0) A / largest_weight else A

    # Take signed cube root of weights
    cube_root_weights <- sign(scaled_weights) * abs(scaled_weights)^(1 / 3)

    # Get triangles of each node
    triangle_count <- diag(cube_root_weights %*% cube_root_weights %*% cube_root_weights)

    # Set degree to infinite for nodes without triangles
    degree[triangle_count == 0] <- Inf

    # Compute clustering coefficient of each node
    per_node_cc <- triangle_count / (degree * (degree - 1))

  }

  # Check for removing undefined nodes
  if(na.rm){

    # Set nodes with fewer than two neighbors to missing
    per_node_cc[neighbor_count < 2] <- NA

  }

  # Name nodes
  names(per_node_cc) <- colnames(A)

  # Compute mean clustering coefficient
  result <- mean(per_node_cc, na.rm = na.rm)

  # Attach node clustering coefficients
  attr(result, "node") <- per_node_cc

  # Return result
  return(result)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
cc_errors <- function(A, weighted, na.rm)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "cc")

  # 'weighted' errors
  length_error(weighted, 1, "cc")
  typeof_error(weighted, "logical", "cc")

  # 'na.rm' errors
  length_error(na.rm, 1, "cc")
  typeof_error(na.rm, "logical", "cc")

  # Return 'A' (in case of data frame)
  return(A)

}
