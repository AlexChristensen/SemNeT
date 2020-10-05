#' Community Network Estimation
#' @description Estimates a semantic network using the Community Network
#' method described in Goni et al. (2011)
#' 
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where
#' rows are participants and columns are verbal fluency
#' responses
#' 
#' @param window Numeric.
#' Size of window to look for co-occurences in.
#' Defaults to \code{2}
#' 
#' @param alpha Numeric.
#' Significance value.
#' Defaults to \code{.05}
#' 
#' @param enrich Boolean.
#' Should the network be enriched by connecting
#' all nodes in their respective modules?
#' Defaults to \code{FALSE}
#' 
#' @return Returns a undirected semantic network
#' 
#' @examples
#' # Get data
#' data <- open.clean
#' 
#' # Organize group data
#' ## Get group data
#' group <- open.group
#' 
#' ## Low and high openness to experience groups
#' low <- data[which(group == "Low"),]
#' high <- data[which(group == "High"),]
#' 
#' \dontrun{
#' # Compute networks
#' low.net <- CN(low)
#' high.net <- CN(high)
#' }
#' 
#' @references 
#' Goni, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizabal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011).
#' The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory.
#' \emph{Cognitive Processing}, \emph{12}, 183-196.
#' \href{https://doi.org/10.1007/s10339-010-0372-x}{https://doi.org/10.1007/s10339-010-0372-x}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Community Network----
# Updated 13.09.2020
CN <- function (data, window = 2, alpha = .05, enrich = FALSE)
{
  # Compute statistical co-occurrence
  adj <- stat.cooccur(data, window = window, alpha = alpha)
  
  # Get component structure
  comp <- suppressWarnings(igraph::components(convert2igraph(adj)))
  
  # Retain responses in the largest connected component
  adj <- adj[which(comp$membership == which.max(comp$csize)),
             which(comp$membership == which.max(comp$csize))]
  
  # Check for network enrichment
  if(enrich)
  {adj <- enrich.network(adj, gtom(adj))}
  
  return(adj)
}
#----