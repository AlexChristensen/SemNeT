#' Semantic Network Measures
#' @description Computes the average shortest path length (ASPL),
#' clustering coefficient(CC), and modularity (Q) of the network
#' 
#' @param A Matrix or data frame.
#' An adjacency matrix of a network
#' 
#' @param weighted Boolean.
#' Should weighted measures be computed?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted measures
#' 
#' @return Returns a values for ASPL, CC, and Q
#' 
#' @examples
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos <- similarity(one, method = "cosine")
#' 
#' # Compute networks using NetworkToolbox
#' net <- NetworkToolbox::TMFG(cos)$A
#' 
#' # Compute global network measures
#' globmeas <- semnetmeas(net)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Semantic Network Measures----
semnetmeas <- function (A, weighted = FALSE)
{
    # Average shortest path length
    aspl <- NetworkToolbox::pathlengths(A, weighted = weighted)$ASPL
    # Clustering coefficient
    cc <- NetworkToolbox::clustcoeff(A, weighted = weighted)$CC
    # Modularity
    ## Using igraph because it doesn't get stuck in while loop
    q <- max(igraph::cluster_louvain(NetworkToolbox::convert2igraph(A))$modularity)
    
    # Vector of measures
    sn.meas <- c(aspl,cc,q)
    
    # Name measures
    names(sn.meas)<-c("ASPL","CC","Q")
    
    return(sn.meas)
}
#----