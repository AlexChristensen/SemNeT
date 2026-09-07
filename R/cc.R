#' @title Clustering Coefficient
#'
#' @description Computes the global clustering coefficient (CC) of a network
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of network data
#'
#' @param weighted Boolean.
#' Is the network weighted?
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures of CC
#'
#' @return Returns the network's CC. The per-node clustering coefficient is
#' attached as the \code{"node"} attribute, rather than being computed and discarded
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
# Clustering coefficient
# Updated 06.09.2026
cc <- function(A, weighted = FALSE)
{

  # Validate the input *before* mutating it -- the original zeroed the
  # diagonal first and only checked afterward whether 'A' was even square
  A <- cc_errors(A, weighted)

  # A node's own self-loop never counts toward its neighborhood
  diag(A) <- 0

  n <- ncol(A)

  if(!weighted){

    # Binary neighborhoods: a node's clustering coefficient is the density
    # of the subgraph formed by its own neighbors
    binary <- ifelse(A != 0, 1, 0)
    per_node_cc <- numeric(n)

    for(node in seq_len(n)){

      neighbors <- which(binary[node, ] != 0)
      degree <- length(neighbors)

      # Clustering is undefined below 2 neighbors (no possible triangle);
      # leave it at its initialized 0, matching the original's convention
      if(degree >= 2){
        neighborhood <- binary[neighbors, neighbors]
        per_node_cc[node] <- sum(neighborhood) / (degree^2 - degree)
      }

    }

    per_node_cc <- round(per_node_cc, 3)

  }else{

    # Weighted clustering via geometric-mean triangle counting (Rubinov & Sporns, 2010)
    degree <- colSums(A != 0)

    # A plain `A^(1/3)` returns `NaN` for every negative edge weight -- R's
    # `^` operator computes fractional powers via `exp(log(x) * p)`, and
    # `log()` of a negative number is undefined. Signed networks (e.g. a
    # TMFG built on raw, signed correlations) need the real-valued cube
    # root, which keeps the original sign: cbrt(-8) = -2, not `NaN`
    cube_root_weights <- sign(A) * abs(A)^(1 / 3)
    triangle_count <- diag(cube_root_weights %*% cube_root_weights %*% cube_root_weights)

    # A node with no triangles gets `Inf` in the denominator below, forcing
    # its clustering coefficient to (correctly) evaluate to 0
    degree[triangle_count == 0] <- Inf

    per_node_cc <- round(triangle_count / (degree * (degree - 1)), 3)

  }

  names(per_node_cc) <- colnames(A)
  result <- mean(per_node_cc)

  # Keep the per-node detail available without changing CC's numeric contract
  attr(result, "node") <- per_node_cc

  return(result)

}

#' @noRd
# Argument errors for `cc()`
# Updated 06.09.2026
cc_errors <- function(A, weighted)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame)
  A <- adjacency_matrix_error(A, "cc")

  # 'weighted' must be a single logical value
  object_error(weighted, "vector", "cc")
  typeof_error(weighted, "logical", "cc")
  length_error(weighted, 1, "cc")

  return(A)

}
