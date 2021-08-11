#' Convert Network(s) to igraph's Format
#' @description Converts single or multiple networks into \code{\link{igraph}}'s format for network analysis
#' 
#' @param A Adjacency matrix (network matrix) or brain connectivity array
#' (from \code{\link[NetworkToolbox]{convertConnBrainMat}})
#' 
#' @return Returns a network matrix in \code{\link{igraph}}'s format
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(similarity(sim.fluency(50), method = "cor"))
#' 
#' igraphNetwork <- convert2igraph(A)
#' 
#' \dontrun{ 
#' neuralarray <- convertConnBrainMat()
#' 
#' igraphNeuralList <- convert2igraph(neuralarray, neural = TRUE)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Convert matrices to igraph format----
# Updated 09.08.2021
convert2igraph <- function (A)
{
    net <- igraph::as.igraph(qgraph::qgraph(A,DoNotPlot=TRUE))
    igraph::vertex_attr(net, "name") <- V(net)$label
    return(net)
}
#----