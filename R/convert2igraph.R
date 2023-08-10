#' Convert Network(s) to igraph's Format
#' @description Converts single or multiple networks into \code{\link{igraph}}'s format for network analysis
#' 
#' @param A Adjacency matrix (network matrix) or brain connectivity array
#' (from \code{convertConnBrainMat})
#' 
#' @param neural Defunct.
#' 
#' @return Returns a network matrix in \code{\link{igraph}}'s format
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(similarity(sim.fluency(50), method = "cor"))
#' 
#' igraphNetwork <- convert2igraph(A)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Convert matrices to igraph format----
# Updated 10.08.2023
convert2igraph <- function (A, neural = FALSE)
{
    # Check for strength of zero
    original_diagonal <- diag(A)[1]
    diag(A) <- 0
    unconnected <- colSums(A) == 0
    if(any(unconnected)){
      A <- A[!unconnected, !unconnected]
    }
    
    # Set diagonal back to original
    diag(A) <- original_diagonal
    
    net <- igraph::as.igraph(qgraph::qgraph(A,DoNotPlot=TRUE))
    # igraph::vertex_attr(net, "name") <- igraph::V(net)$label
    return(net)
}
#----