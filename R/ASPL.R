#' Average Shortest Path Length
#' 
#' @description Computes the global average shortest path length of the network
#' 
#' @param A An adjacency matrix of network data
#' 
#' @param weighted Is the network weighted?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted measures
#' 
#' @return Returns the ASPL of the network
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(similarity(sim.fluency(100), method = "cor"))
#' 
#' # Unweighted
#' aspl <- ASPL(A)
#' 
#' @references 
#' Rubinov, M., & Sporns, O. (2010). 
#' Complex network measures of brain connectivity: Uses and interpretations. 
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# ASPL (SemNeT)----
# Updated 02.09.2020
ASPL <- function (A, weighted = FALSE)
{
    if(nrow(A)!=ncol(A))
    {stop("Input not an adjacency matrix")}
    if(!weighted)
    {D<-distance(A,weighted=FALSE)}else if(weighted){D<-distance(A,weighted=TRUE)}
    n<-nrow(D)
    for(i in 1:ncol(D))
        for(j in 1:nrow(D))
            if(is.infinite(D[j,i]))
            {D[j,i]<-0}
    if(any(colSums(D)==0))
    {
        newD <- D
        newD[,(which(colSums(D)==0))] <- rep(NA,length(which(colSums(D)==0)))
    }else{newD <- D}
    
    aspli<-colSums(newD*(newD!=0))/(ncol(newD)-1)
    aspl<-mean(aspli,na.rm=TRUE)
    
    Emat<-(D*(D!=0))
    
    ecc<-matrix(nrow=nrow(Emat),ncol=1)
    
    for(i in 1:nrow(Emat))
    {ecc[i,]<-max(Emat[i,])}
    
    d<-max(ecc)
    
    ecc <- as.vector(ecc)
    names(ecc) <- colnames(A)
    
    aspli <- as.vector(aspli)
    names(aspli) <- colnames(A)
    
    return(aspl)
}
#----