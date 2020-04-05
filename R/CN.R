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
#' @return Returns a undirected semantic network
#' 
#' @examples
#' # Get data
#' data <- SemNetCleaner::bad.response(as.matrix(SemNetCleaner::open.animals[,-c(1:3)]), 99)
#' 
#' # Organize group data
#' ## Get group data
#' group <- SemNetCleaner::open.animals$Group
#' 
#' ## Low and high openness to experience groups
#' low <- data[which(group == 1),]
#' high <- data[which(group == 2),]
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
# Commmunity Network----
# Updated 03.27.2020
CN <- function (data, window = 2, alpha = .05)
{
  # Compute statistical co-occurrence
  adj <- stat.cooccur(data, window = window, alpha = alpha)
  
  # Get component structure
  comp <- suppressWarnings(igraph::components(NetworkToolbox::convert2igraph(adj)))
  
  # Retain responses in the largest connected component
  adj <- adj[which(comp$membership == which.max(comp$csize)),
             which(comp$membership == which.max(comp$csize))]
  
  return(adj)
}
#----