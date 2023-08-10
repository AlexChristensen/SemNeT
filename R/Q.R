#' Modularity
#' 
#' @description Computes a global modularity measure (Q) using the Louvain community detection algorithm
#' 
#' @param A An adjacency matrix of network data
#' 
#' @return Returns Q or a measure of how well the communities in the network are compartmentalized
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(similarity(sim.fluency(100), method = "cor"))
#' 
#' modularity <- Q(A)
#' 
#' @references
#' Blondel, V. D., Guillaume, J. L., Lambiotte, R., & Lefebvre, E. (2008).
#' Fast unfolding of communities in large networks. 
#' \emph{Journal of Statistical Mechanics: Theory and Experiment}, \emph{2008}, P10008.
#'  
#' Rubinov, M., & Sporns, O. (2010). 
#' Complex network measures of brain connectivity: Uses and interpretations. 
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Louvain Community Detection (SemNeT)----
# Updated 02.09.2020
Q <- function (A)
{
  Q <- max(igraph::cluster_louvain(convert2igraph(abs(A)))$modularity)
  
  return(Q)
}
#----