#' Naive Random Walk Network Estimation
#' 
#' @description Estimates a semantic network using the Naive Random Walk
#' method described in Lerner, Ogrocki, and Thomas (2009)
#' 
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where
#' rows are participants and columns are verbal fluency
#' responses
#' 
#' @param threshold Numeric.
#' Minimum number of total co-occurrences to be included
#' in the network
#' 
#' @return Returns a undirected semantic network
#' 
#' @examples
#' # Get data
#' data <- open.binary
#' 
#' # Organize group data
#' ## Get group data
#' group <- open.group
#' 
#' ## Low and high openness to experience groups
#' low <- data[which(group == "Low"),]
#' high <- data[which(group == "High"),]
#' 
#' # Compute networks
#' low.net <- NRW(low)
#' high.net <- NRW(high)
#' 
#' @references 
#' Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009).
#' Network graph analysis of category fluency testing.
#' \emph{Cognitive and Behavioral Neurology}, \emph{22}, 45-52.
#' \href{https://doi.org/10.1097/WNN.0b013e318192ccaf}{https://doi.org/10.1097/WNN.0b013e318192ccaf}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Naive Random Walk Network----
# Updated 03.27.2020
NRW <- function(data, threshold = 3)
{
  # Check if the matrix is numeric
  if(all(apply(data, 2, is.numeric)))
  {data <- bin2resp(data, to.data.frame = TRUE)}
  
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace bad responses with NA
  mat <- bad.response(mat)
  
  # Number of cases
  cases <- nrow(mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Initalize binary matrix
  bin.mat <- matrix(0, nrow = length(uniq.resp), ncol = length(uniq.resp))
  colnames(bin.mat) <- uniq.resp
  row.names(bin.mat) <- uniq.resp
  
  # Add NAs to beginning and end of response matrix
  resp.mat <- cbind(rep(NA, nrow(mat)), mat, rep(NA, nrow(mat)))
  
  # Loop through response matrix
  for(i in 1:nrow(resp.mat))
  {
    # Target responses
    target <- resp.mat[i,!is.na(resp.mat[i,])]
    
    # Full responses
    full <- resp.mat[i,]
    
    # Loop through target responses
    for(j in 1:length(target))
    {
      # Responses on either side of target response
      sides <- na.omit(full[c((which(full == target[j])-1),
                              (which(full == target[j])+1))])
      
      # Input into binary response matrix
      bin.mat[target[j], sides] <- 1
      bin.mat[sides, target[j]] <- 1
    }
  }
  
  # Remove responses less than threshold
  bin.mat <- bin.mat[-which(rowSums(bin.mat) < threshold),
                     -which(colSums(bin.mat) < threshold)]
  
  return(bin.mat)
}
#----