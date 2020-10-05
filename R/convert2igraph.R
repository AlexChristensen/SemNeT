#' Convert Network(s) to igraph's Format
#' @description Converts single or multiple networks into \code{\link{igraph}}'s format for network analysis
#' 
#' @param A Adjacency matrix (network matrix) or brain connectivity array
#' (from \code{\link[NetworkToolbox]{convertConnBrainMat}})
#' 
#' @param neural Is input a brain connectivity array (i.e., m x m x n)?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to convert each brain connectivity matrix
#' 
#' @return Returns a network matrix in \code{\link{igraph}}'s format or
#' returns a list of brain connectivity matrices each of which have been
#' convert to \code{\link{igraph}}'s format
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
# Updated 25.09.2020
convert2igraph <- function (A, neural = FALSE)
{
    if(!neural)
    {
        net <- igraph::as.igraph(qgraph::qgraph(A,DoNotPlot=TRUE))
        return(net)
    }else if(neural)
    {
        netlist <- list()
        
        n<-length(A)/nrow(A)/ncol(A)
        
        pb <- txtProgressBar(max=n, style = 3)
        
        for(i in 1:n)
        {
            netlist[[i]] <- igraph::as.igraph(qgraph::qgraph(A[,,i],DoNotPlot=TRUE))
            setTxtProgressBar(pb, i)
        }
        close(pb)
        
        return(netlist)
    }
}
#----