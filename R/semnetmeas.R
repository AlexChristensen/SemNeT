#' Semantic Network Measures
#' @description Computes the average shortest path length (ASPL),
#' clustering coefficient(CC),
#' modularity (Q),
#' small-worldness (S; defaults to "rand"),
#' and mean network strength (MNS)
#' @param A An adjacency matrix of network A
#' @param iter Number of iterations for the small-worldness measure
#' @param weighted Should weighted measures be computed?
#' Defaults to FALSE.
#' Set to TRUE for weighted measures
#' @param swm Method for computing small-worldness.
#' Defaults to "rand".
#' See \link[NetworkToolbox]{smallworldness} for other options
#' @return Returns a values for ASPL, CC, Q, and S
#' @examples
#' fin <- finalize(convmat)
#' 
#' cosL <- cosine(fin)
#' 
#' A <- NetworkToolbox::TMFG(cosL)$A
#' 
#' globmeas <- semnetmeas(A)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Semantic Network Measures----
semnetmeas <- function (A, iter,
                        weighted = FALSE,
                        swm = "rand")
{
    if(missing(iter))
    {iter<-100
    }else{iter<-iter}
    
    aspl<-NetworkToolbox::pathlengths(A, weighted = weighted)$ASPL
    if(swm=="rand")
    {cc<-NetworkToolbox::clustcoeff(A, weighted = weighted)$CC
    }else if(swm=="HG")
    {cc<-NetworkToolbox::transitivity(A)}
    q<-NetworkToolbox::louvain(A)$Q
    s<-NetworkToolbox::smallworldness(A,iter=iter,method=swm,progBar = FALSE)
    swm<-s$swm
    mns <- NetworkToolbox::conn(A)$mean
    raspl<-s$rASPL
    rCC<-s$lrCCt
    
    semnetmeas<-cbind(aspl,cc,q,swm,mns,raspl,rCC)
    
    semnetmeas<-as.data.frame(semnetmeas)
    
    colnames(semnetmeas)<-c("ASPL","CC","Q","S","MNS","randASPL","randCC")
    
    semnetmeas<-as.matrix(semnetmeas)
    
    return(semnetmeas)
}
#----