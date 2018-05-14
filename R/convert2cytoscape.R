#' Convert Adjacency Matrix to Cytoscape Format
#' @description Converts an adjacency matrix to Cytoscape's sparse matrix format
#' @param A A cleaned, finalized response matrix ready to be visualized
#' @return A sparse matrix formatted for Cytoscape
#' @examples 
#' #finalize rmatA
#' finalCmat <- finalize(convmat)
#' #finalize rmatB
#' finalRmat <- finalize(rmat)
#'
#' #equate rmatA and rmatB
#' eq1 <- equate(finalCmat,finalRmat)
#' 
#' #obtain respective equated response matrices
#' eqCmat <- eq1$rmatA
#' eqRmat <- eq1$rmatB
#' 
#' #compute similarity matrix
#' cosC <- cosine(eqCmat)
#' cosR <- cosine(eqRmat)
#' 
#' #compute networks using NetworkToolbox
#' Cnet <- NetworkToolbox::TMFG(cosC)$A
#' Rnet <- NetworkToolbox::TMFG(cosR)$A
#' 
#' #covert to Cytoscape format
#' ctyoC <- convert2cytoscape(Cnet)
#' ctyoR <- convert2cytoscape(Rnet)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
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