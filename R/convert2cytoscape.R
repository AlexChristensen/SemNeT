#' @title Convert an Adjacency Matrix to Cytoscape Format
#'
#' @description Converts an adjacency matrix to Cytoscape's sparse edge-list format
#'
#' @param A Matrix or data frame.
#' A cleaned, finalized network matrix ready to be visualized
#'
#' @return A sparse matrix formatted for Cytoscape, with columns
#' \code{nodeTO}, \code{nodeFROM}, and \code{nodeWEIGHT}. An undirected edge
#' is listed twice, once in each direction (as Cytoscape expects for a directed
#' edge list), and the weights are stored as text together with the node names
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
# Convert matrix to Cytoscape ----
# Updated 06.09.2026
convert2cytoscape <- function(A)
{

  # Argument errors
  A <- convert2cytoscape_errors(A)

  # Get node names
  node_names <- colnames(as.data.frame(A))

  # Get number of nodes
  n_nodes <- ncol(A)

  # Remove self-loops
  diag(A) <- 0

  # Get target node index of every cell
  to_index <- sort(rep(seq_len(n_nodes), n_nodes))

  # Get source node index of every cell
  from_index <- rep(seq_len(n_nodes), n_nodes)

  # Get weight of every cell
  weight <- as.vector(as.matrix(A))

  # Identify edges
  present <- weight != 0

  # Set up sparse edge list
  sparse <- cbind(
    nodeTO = node_names[to_index[present]],
    nodeFROM = node_names[from_index[present]],
    nodeWEIGHT = weight[present]
  )

  # Return edge list without quotes
  return(noquote(sparse))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
convert2cytoscape_errors <- function(A)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "convert2cytoscape")

  # Return 'A' (in case of data frame)
  return(A)

}
