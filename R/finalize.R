#' Finalize Response Matrix
#' @description Finalizes the response matrix by keeping
#' responses that are given by a certain number of people
#' 
#' @param rmat Binary matrix.
#' A \link[SemNetCleaner]{textcleaner} filtered response matrix
#' 
#' @param minCase Numeric.
#' Minimum number of cases to produce a response
#' 
#' @return A binary response matrix with responses
#' given by at least \code{minCase} people
#' 
#' @examples
#' # Obtain binary data
#' bin <- open.binary
#' 
#' # Finalize mat1
#' mat1 <- finalize(bin)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Finalize Function----
#Updated 31.03.2020
finalize <- function (rmat, minCase = 2)
{
    fmat <- rmat[,which(colSums(rmat)>=minCase)]
    
    return(fmat)
}
#----