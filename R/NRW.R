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
#' @param type Character.
#' Type of \code{threshold} to apply.
#' \itemize{
#' 
#' \item{\code{"num"}}
#' {Minimum number of co-occurrences}
#' 
#' \item{\code{"prop"}}
#' {Minimum proportion of co-occurrences}
#' 
#' }
#' Defaults to \code{"num"}
#' 
#' @param threshold Numeric.
#' Value of the minimum number or proportion of co-occurrences.
#' Defaults to \code{0} for both \code{"num"} and \code{"prop"}
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
#' # Compute networks
#' low.net <- NRW(low)
#' high.net <- NRW(high)
#' 
#' @references 
#' Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009).
#' Network graph analysis of category fluency testing.
#' \emph{Cognitive and Behavioral Neurology}, \emph{22}, 45-52.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Naive Random Walk Network----
# Updated 05.12.2020
NRW <- function(data, type = c("num", "prop"), threshold = 0)
{
  # Check for type
  if(missing(type)){
    type <- "num"
  }else{type <- match.arg(type)}

  # Check for appropriate threshold values
  if(type == "num"){
    if(threshold < 0){
      stop("'threshold' must be greater than or equal to 0")
    }
  }else if(type == "prop"){
    if(threshold > 1 || threshold < 0){
      stop("'threshold' must be greater than or equal to 0 and less than 1")
    }
  }
  
  # Check if the matrix is numeric
  if(any(apply(data, 2, is.numeric)))
  {
    if(max(range(data)) >= 1)
    {stop("NRW(): Only a response matrix or ordered numeric matrix can be used as input for 'data'")
    }else{data <- bin2resp(data)}
  }
  
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace bad responses with NA
  mat <- bad.response(mat)
  
  # Number of cases
  cases <- nrow(mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Initialize binary matrix
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
      if(length(sides) != 0){
        
        bin.mat[target[j], sides] <- bin.mat[target[j], sides] + 1
        bin.mat[sides, target[j]] <- bin.mat[sides, target[j]] + 1
        
      }
    }
  }
  
  # Compute proportion (if type = "prop")
  if(type == "prop"){
    bin.mat <- bin.mat / nrow(data)
  }
  
  # Change values less than threshold to zero
  bin.mat[bin.mat < threshold] <- 0
  
  # Change values greater than zero to one
  if(threshold != 0){
    bin.mat[bin.mat >= threshold] <- 1
    
    # Remove nodes with no connections
    bin.mat <- bin.mat[-which(colSums(bin.mat) == 0),
                       -which(colSums(bin.mat) == 0)]
  }else{
    bin.mat[bin.mat != 0] <- 1
  }
  
  return(bin.mat)
}
#----