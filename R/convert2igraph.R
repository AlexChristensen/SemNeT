#' @title Convert a Network to igraph's Format
#'
#' @description Converts an adjacency matrix into \code{igraph}'s network format.
#' Nodes without any edge (a nonzero entry in their row or column) are
#' removed, so the returned network can have fewer nodes than \code{A}
#'
#' @param A Matrix or data frame.
#' A symmetric adjacency (network) matrix
#'
#' @return Returns a network in \code{igraph}'s format
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 500, replace = TRUE), nrow = 50, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' igraph_network <- convert2igraph(A)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Convert matrix to igraph ----
# Updated 15.10.2026
convert2igraph <- function(A)
{

  # Argument errors
  A <- convert2igraph_errors(A)

  # Remove self-loops
  diag(A) <- 0

  # Identify nodes without edges (nonzero entries, not signed sums)
  unconnected <- colSums(A != 0) == 0

  # Check for unconnected nodes
  if(any(unconnected)){

    # Remove unconnected nodes
    A <- A[!unconnected, !unconnected, drop = FALSE]

  }

  # Return {igraph} network
  return(
    suppressWarnings(
      igraph::graph_from_adjacency_matrix(
        A, weighted = TRUE, mode = "undirected", diag = FALSE
      )
    )
  )

}

#' @noRd
# Errors ----
# Updated 13.10.2026
convert2igraph_errors <- function(A)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "convert2igraph", require_symmetric = TRUE)

  # Return 'A' (in case of data frame)
  return(A)

}
