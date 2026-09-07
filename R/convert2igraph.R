#' @title Convert a Network to igraph's Format
#'
#' @description Converts an adjacency matrix into \code{igraph}'s network format
#'
#' @param A Matrix or data frame.
#' An adjacency (network) matrix
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
# Convert a matrix to igraph's format
# Updated 06.09.2026
convert2igraph <- function(A)
{

  # Validate and coerce input before any computation begins
  A <- convert2igraph_errors(A)

  # Nodes with zero strength (no edges at all) have no place in a network --
  # zero the diagonal first so a node's own self-loop weight doesn't mask it
  # having no real connections to anything else. The diagonal is never
  # restored afterward: `graph_from_adjacency_matrix()` below is told
  # `diag = FALSE`, so whatever value sits on the diagonal is ignored either way
  diag(A) <- 0

  unconnected <- colSums(A) == 0
  if(any(unconnected)){
    A <- A[!unconnected, !unconnected]
  }

  # Build the {igraph} network directly from the adjacency matrix. The
  # original round-tripped through `qgraph::qgraph()` purely to get an
  # {igraph} object back out of it -- an entire dependency (`qgraph`) used
  # for nothing but a conversion {igraph} already does natively.
  # `suppressWarnings()` matches `EGAnet::convert2igraph()`'s own defensive
  # wrapping of this exact call, and the original `CN.R` already needed to
  # suppress warnings from this same conversion when detecting components
  return(
    suppressWarnings(
      igraph::graph_from_adjacency_matrix(
        A, weighted = TRUE, mode = "undirected", diag = FALSE
      )
    )
  )

}

#' @noRd
# Argument errors for `convert2igraph()`
# Updated 06.09.2026
convert2igraph_errors <- function(A)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame)
  A <- adjacency_matrix_error(A, "convert2igraph")

  return(A)

}
