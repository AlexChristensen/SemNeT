#' Semantic Network Measures
#' @description Computes the average shortest path length (ASPL),
#' clustering coefficient(CC), and modularity (Q) of the network
#' 
#' @param A Matrix or data frame.
#' An adjacency matrix of a network
#' 
#' @param meas Character.
#' Global network measures to compute.
#' By default, computes ASPL, CC, and Q.
#' Individual measures can be selected
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
semnetmeas <- function (A, meas = c("ASPL", "CC", "Q"), weighted = FALSE)
{
    # Full measures
    full <- c("ASPL", "CC", "Q")
    
    # Weighted
    if(!weighted)
    {A <- NetworkToolbox::binarize(A)}
    
    # Average shortest path length
    if("ASPL" %in% meas)
    {ASPL <- NetworkToolbox::pathlengths(A, weighted = weighted)$ASPL}
    
    # Clustering coefficient
    if("CC" %in% meas)
    {CC <- NetworkToolbox::clustcoeff(A, weighted = weighted)$CC}
    
    # Modularity
    ## Using igraph because it doesn't get stuck in while loop
    if("Q" %in% meas)
    {Q <- max(igraph::cluster_louvain(NetworkToolbox::convert2igraph(abs(A)))$modularity)}
    
    # Vector of measures
    sn.meas <- unlist(lapply(full[match(meas, full)],get,envir=environment()))
    
    # Name measures
    names(sn.meas) <- meas
    
    return(sn.meas)
}
#----