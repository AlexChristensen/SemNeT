#' Finalize Response Matrix
#' @description Finalizes the response matrix by keeping responses that are given by two or more people
#' @param rmat A semnetcleaner filtered response matrix
#' @param minCase Minumum number of cases to produce a response
#' @return A matrix with responses given by two or more people
#' @examples \donttest{
#' convmat <- autoConverge(rmat)
#' }
#' 
#' finalRmat <- finalize(convmat)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Finalize Function----
finalize <- function (rmat, minCase = 2)
{fmat <- rmat[which(colSums(rmat)>=minCase)]
return(fmat)}
#----