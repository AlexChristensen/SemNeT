#' Pathfinder Network 
#' 
#' @description Estimates a pathfinder network using the MST-Pathfinder
#' Network method from Quirin et al. (2008; see also Schvaneveldt, 1990)
#' 
#' @param data Matrix or data frame.
#' A binary response matrix
#' 
#' @param progBar Boolean.
#' Should a progress bar be displayed?
#' Defaults to \code{TRUE}
#' 
#' @return An adjacency matrix
#' 
#' @examples
#' # Obtain data
#' data <- open.binary
#' 
#' \dontrun{
#' # Estimate network
#' pf.net <- PF(data)
#' }
#' 
#' @references 
#' Quirin, A., Cordon, O., Guerrero-Bote, V. P., Vargas-Quesada, B., & Moya-Aneon, F. (2008)
#' A quick MST-based algorithm to obtain Pathfinder networks (Inf, n-1).
#' \emph{Journal of the American Society for Information Science and Technology}, \emph{59}, 1912-1924.
#' 
#' Schvaneveldt, R. W. (1990).
#' \emph{Pathfinder associative networks: Studies in knowledge organization}.
#' Norwood, NJ: Ablex Publishing.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
# Pathfinder Network Estimation
# Updated 29.03.2020
PF <- function (data, progBar = TRUE)
{
  # Check if the matrix is responses
  if(all(apply(data, 2, is.character)))
  {data <- resp2bin(data)}
  
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace "" with NA
  mat <- ifelse(mat == "", NA, mat)
  
  # Co-occurrence
  mat <- t(mat) %*% mat
  
  # Number of nodes
  n <- ncol(mat)
  
  # Distance
  D <- as.matrix(dist(mat, method = "minkowski"))
  D[upper.tri(D)] <- 0
  
  # Initialize sparse edge matrix
  nodeTO <- sort(c(rep(1:n,n)))
  nodeFROM <- c(rep(1:n,n))
  nodeWEIGHT <- as.vector(D)
  
  # Sparse matrix
  W <- cbind(nodeTO, nodeFROM, nodeWEIGHT)
  
  # Sorted sparse matrix
  S <- W[order(W[,3], W[,1], decreasing = TRUE),]
  
  # Remove zeros
  S <- S[-which(S[,3] == 0),]
  
  # Number of rows in sparse matrix
  n_sparse <- nrow(S)
  
  # Initialize clusters
  clusters <- as.matrix(cbind(c(1:n),c(1:n)))
  
  # Initialize network
  pfn <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  
  # Pathfinder sparse
  pfs <- matrix(0, nrow = 1, ncol = 3)[-1,]
  
  if(progBar)
  {pb <- txtProgressBar(min = 0, max = n_sparse, style = 3)}
  
  while(nrow(S) != 0)
  {
    # Pathfinder matrix
    pf <- matrix(0, nrow = 1, ncol = 3)[-1,]
    
    # Target edges
    target <- which(S[,3] == S[1,3])
    
    # Bind with pfs
    pf <- rbind(pf, S[target,])
    
    # Remove edge(s)
    S <- S[-target,]
    
    # Only select edges that are not in same cluster
    for(i in 1:nrow(pf))
    {
      if(clusters[pf[i,1],2] != clusters[pf[i,2],2])
      {
        # Insert into network
        pfn[pf[i,1], pf[i,2]] <- pf[i,3]
        pfn[pf[i,2], pf[i,1]] <- pf[i,3]
        
        # Insert into sparse network
        pfs <- rbind(pfs, pf[i,])
        
        # Update clusters
        if(clusters[pf[i,2],2] != clusters[pf[i,1],2])
        {clusters[which(clusters[,2] == clusters[pf[i,2],2]),2] <- clusters[pf[i,1],2]}
        
      }
    }
    
    # Make sure S remains a matrix for progress bar
    if(is.vector(S))
    {S <- t(as.matrix(S))}
    
    if(progBar)
    {setTxtProgressBar(pb, n_sparse - nrow(S))}
    
  }
  
  if(progBar)
  {close(pb)}
  
  # Name dimensions
  row.names(pfn) <- colnames(data)
  colnames(pfn) <- colnames(data)
  
  return(pfn)
}
#----