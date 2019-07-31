#' Convert Adjacency Matrix to Cytoscape Format
#' 
#' @description Converts an adjacency matrix to Cytoscape's sparse matrix format
#' 
#' @param A Matrix or data frame.
#' A cleaned, finalized response matrix ready to be visualized
#' 
#' @return A sparse matrix formatted for Cytoscape
#' 
#' @examples 
#' # Finalize rmatA
#' finalCmat <- SemNetCleaner::finalize(SemNetCleaner::convmat)
#' # Finalize rmatB
#' finalRmat <- SemNetCleaner::finalize(SemNetCleaner::rmat)
#'
#' # Equate rmatA and rmatB
#' eq1 <- SemNetCleaner::equate(finalCmat,finalRmat)
#' 
#' # Obtain respective equated response matrices
#' eqCmat <- eq1$rmatA
#' eqRmat <- eq1$rmatB
#' 
#' # Compute similarity matrix
#' cosC <- similarity(eqCmat, method = "cosine")
#' cosR <- similarity(eqRmat, method = "cosine")
#' 
#' # Compute networks using NetworkToolbox
#' Cnet <- NetworkToolbox::TMFG(cosC)$A
#' Rnet <- NetworkToolbox::TMFG(cosR)$A
#' 
#' # Cnovert to Cytoscape format
#' ctyoC <- convert2cytoscape(Cnet)
#' ctyoR <- convert2cytoscape(Rnet)
#' 
#' \dontrun{
#' 
#' # Export to .csv
#' write.csv(cytoC, "netC.csv", row.names = FALSE)
#' write.csv(cytoR, "netR.csv", row.names = FALSE)
#' 
#' }
#' 
#' @references 
#' 
#' Shannon, P., Markiel, A., Ozier, O., Baliga, N. S., Wang, J. T., Ramage, D., ... & Ideker, T. (2003).
#' Cytoscape: A software environment for integrated models of biomolecular interaction networks.
#' \emph{Genome Research}, \emph{13}, 2498-2504.
#' doi:\href{http://www.genome.org/cgi/doi/10.1101/gr.1239303}{10.1101/gr.1239303}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats na.omit
#' 
#' @export
#' 
#Convert2Cytoscape----
convert2cytoscape <- function (A)
{
    #number of nodes
    n <- ncol(A)
    
    #diagonal to zero
    diag(A) <- 0
    
    nodeTO<-sort(c(rep(1:n,n)))
    nodeFROM<-c(rep(1:n,n))
    nodeWEIGHT<-as.vector(A)
    
    #sparse matrix
    S <- cbind(nodeTO,nodeFROM,nodeWEIGHT)
    
    for(i in 1:ncol(A))
    {
        len1 <- length(which(S[,1]==i))
        len2 <- length(which(S[,2]==i))
        
        S[which(S[,1]==i),1] <- rep(colnames(A)[i],len1)
        S[which(S[,2]==i),2] <- rep(colnames(A)[i],len2)
    }
    
    #remove weights of 0
    S[,3] <- ifelse(S[,3]==0,NA,S[,3])
    
    
    cyto <- noquote(na.omit(S))
    
    return(cyto)
}
#----