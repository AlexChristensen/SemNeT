#' @title Convert an Adjacency Matrix to Cytoscape Format
#'
#' @description Converts an adjacency matrix to Cytoscape's sparse edge-list format
#'
#' @param A Matrix or data frame.
#' A cleaned, finalized network matrix ready to be visualized
#'
#' @return A sparse matrix formatted for Cytoscape, with columns
#' \code{nodeTO}, \code{nodeFROM}, and \code{nodeWEIGHT}
#'
#' @examples
#' # Simulate toy binarized datasets
#' one <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#' two <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#'
#' # Compute similarity matrices
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#'
#' # Compute networks
#' net1 <- suppressWarnings(tmfg(cos1))
#' net2 <- suppressWarnings(tmfg(cos2))
#'
#' # Convert to Cytoscape format
#' cyto1 <- convert2cytoscape(net1)
#' cyto2 <- convert2cytoscape(net2)
#'
#' # Write to .csv
#' write.csv(cyto1, file.path(tempdir(), "cyto1.csv"), row.names = FALSE)
#' write.csv(cyto2, file.path(tempdir(), "cyto2.csv"), row.names = FALSE)
#'
#' @references
#' Shannon, P., Markiel, A., Ozier, O., Baliga, N. S., Wang, J. T., Ramage, D., ... & Ideker, T. (2003).
#' Cytoscape: A software environment for integrated models of biomolecular interaction networks.
#' \emph{Genome Research}, \emph{13}, 2498-2504.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Convert an adjacency matrix to Cytoscape's sparse format
# Updated 06.09.2026
convert2cytoscape <- function(A)
{

  # Validate and coerce input before any computation begins
  A <- convert2cytoscape_errors(A)

  node_names <- colnames(as.data.frame(A))
  n_nodes <- ncol(A)

  # A node's self-loop is never a real edge to list
  diag(A) <- 0

  # Every (row, column) pair of the matrix, in the same column-major order
  # `as.vector()` flattens the matrix itself in -- built once as plain
  # integer/numeric vectors so the zero-weight filter below stays a safe
  # numeric comparison, rather than the original's repeated per-node loop
  # that reassigned individual matrix cells to node names (silently
  # coercing the whole sparse matrix from numeric to character bit by bit)
  to_index <- sort(rep(seq_len(n_nodes), n_nodes))
  from_index <- rep(seq_len(n_nodes), n_nodes)
  weight <- as.vector(as.matrix(A))

  # A weight of exactly 0 means "no edge" -- drop it before ever touching names
  present <- weight != 0

  sparse <- cbind(
    nodeTO = node_names[to_index[present]],
    nodeFROM = node_names[from_index[present]],
    nodeWEIGHT = weight[present]
  )

  return(noquote(sparse))

}

#' @noRd
# Argument errors for `convert2cytoscape()`
# Updated 06.09.2026
convert2cytoscape_errors <- function(A)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame)
  A <- adjacency_matrix_error(A, "convert2cytoscape")

  return(A)

}
