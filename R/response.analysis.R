#' Response Analysis 
#' 
#' @description Computes the difference in the total and unique
#' number of responses between two groups (follows Christensen et al., 2018)
#' 
#' @param ... Matrix or data frame.
#' Responses matrices for two different groups
#' 
#' @return A list containing objects:
#' 
#' \item{total}{A vector with the total responses given by each participant. A \emph{t}-test
#' is used to compare, on average, whether one group provides more response
#' than the other}
#' 
#' \item{unique}{A vector with the number of unique responses provided by both groups
#' (\code{Total Across Groups}), the number of unique responses provided by
#' each group (\code{Total}), and the number of unique resposnes provided by each
#' group that were \emph{not} provided by the other group (\code{Unique}). A
#' McNemar's test is used to compare whether the number of unique responses
#' are different between groups}
#' 
#' @examples
#' # Obtain data
#' low <- open.clean[which(open.group == "Low"),]
#' high <- open.clean[which(open.group == "High"),]
#' 
#' # Perform analysis
#' response.analysis(low, high)
#' 
#' @references 
#' Christensen, A. P., Kenett, Y. N., Cotter, K. N., Beaty, R. E., & Silvia, P. J. (2018).
#' Remotely close associations: Openness to experience and semantic memory structure.
#' \emph{European Journal of Personality}, \emph{32}, 480-492.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats mcnemar.test
#' 
#' @export
# Response analysis
# Updated 02.12.2020
response.analysis <- function(...)
{
  #Get names of data objects
  name <- as.character(substitute(list(...)))
  name <- name[-which(name=="list")]
  
  #Create list of input
  datalist <- list(...)
  
  #Check for binary or character responses
  for(i in 1:length(datalist)){
    
    if(all(apply(datalist[[i]], 2, is.character))){
      datalist[[i]] <- resp2bin(datalist[[i]])$binary
    }
    
  }
  
  #Initialize results list
  res <- list()
  
  #Get total number of responses
  totals <- lapply(datalist, rowSums)
  
  #Test
  totals.res <- t.test(totals[[1]], totals[[2]], var.equal = TRUE)
  
  #Prepare results vector
  res.totals <- numeric(10)
  names(res.totals) <- c(
    paste("M", name[1]), paste("SD", name[1]),
    paste("M", name[2]), paste("SD", name[2]),
    "t-statistic", "df", "p-value",
    paste("95%", c("Lower", "Upper")),
    "Cohen's d"
  )
  
  d <- function(one, two){
    num <- mean(one, na.rm = TRUE) - mean(two, na.rm = TRUE)
    denom <- sqrt(
      (sd(one, na.rm = TRUE)^2 + sd(two, na.rm = TRUE)^2) / 2
    )
    
    return(abs(num / denom))
  }
  
  res.totals[1] <- mean(totals[[1]], na.rm = TRUE)
  res.totals[2] <- sd(totals[[1]], na.rm = TRUE)
  res.totals[3] <- mean(totals[[2]], na.rm = TRUE)
  res.totals[4] <- sd(totals[[2]], na.rm = TRUE)
  res.totals[5] <- totals.res$statistic
  res.totals[6] <- totals.res$parameter
  res.totals[7] <- totals.res$p.value
  res.totals[8:9] <- totals.res$conf.int
  res.totals[10] <- d(totals[[1]], totals[[2]])
  
  res$total <- round(res.totals, 3)
  
  #Get responses
  resplist <- lapply(datalist, function(x){colnames(x)})
  
  #Get all unique responses
  named.uniq <- sort(unique(unlist(resplist)))
  uniq.resp <- length(named.uniq)
  
  #Separate unique responses by group
  uniq.mat <- matrix(0, nrow = uniq.resp, ncol = 2)
  row.names(uniq.mat) <- named.uniq
  colnames(uniq.mat) <- name
  
  #Input 1s for responses given
  for(i in 1:length(resplist)){
    uniq.mat[match(resplist[[i]], named.uniq),i] <- 1
  }
  
  #Prepare results vector
  res.uniq <- numeric(9)
  names(res.uniq) <- c("Total Across Groups",
                       paste("Total", name),
                       paste("Unique", name),
                       "McNemar's X^2", "df", "p-value",
                       "Phi (effect size)")
  
  #Get unique totals
  uniq.totals <- colSums(uniq.mat)
  uniq.one <- length(which(uniq.mat[,1] == 1 & uniq.mat[,2] == 0))
  uniq.two <- length(which(uniq.mat[,1] == 0 & uniq.mat[,2] == 1))
  
  #Create contingency table
  cont.tab <- matrix(0, nrow = 2, ncol = 2)
  diag(cont.tab) <- uniq.totals
  cont.tab[1,2] <- uniq.one
  cont.tab[2,1] <- uniq.two
  
  #Test
  uniq.res <- mcnemar.test(cont.tab)
  
  res.uniq[1] <- uniq.resp
  res.uniq[2:3] <- uniq.totals
  res.uniq[4] <- uniq.one
  res.uniq[5] <- uniq.two
  res.uniq[6] <- uniq.res$statistic
  res.uniq[7] <- uniq.res$parameter
  res.uniq[8] <- uniq.res$p.value
  res.uniq[9] <- abs(cor(uniq.mat)[1,2])
  
  res$unique <- round(res.uniq, 3)
  
  return(res)
  
}
