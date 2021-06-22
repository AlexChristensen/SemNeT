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
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' two <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#' 
#' # Compute networks
#' net1 <- TMFG(cos1)
#' net2 <- TMFG(cos2)
#' 
#' # Convert to Cytoscape format
#' cyto1 <- convert2cytoscape(net1)
#' cyto2 <- convert2cytoscape(net2)
#' 
#' # Write to .csv
#' write.csv(cyto1, file.path(tempdir(), "cyto1.csv"), row.names = FALSE)
#' write.csv(cyto2, file.path(tempdir(), "cyto2.csv"), row.names = FALSE)
#' 
#' @references 
#' 
#' Shannon, P., Markiel, A., Ozier, O., Baliga, N. S., Wang, J. T., Ramage, D., ... & Ideker, T. (2003).
#' Cytoscape: A software environment for integrated models of biomolecular interaction networks.
#' \emph{Genome Research}, \emph{13}, 2498-2504.
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
    #column names
    name <- colnames(as.data.frame(A))
    
    #number of nodes
    n <- ncol(A)
    
    #diagonal to zero
    diag(A) <- 0
    
    nodeTO<-sort(c(rep(1:n,n)))
    nodeFROM<-c(rep(1:n,n))
    nodeWEIGHT<-as.vector(as.matrix(A))
    
    #sparse matrix
    S <- cbind(nodeTO,nodeFROM,nodeWEIGHT)
    
    for(i in 1:ncol(A))
    {
        len1 <- length(which(S[,1]==i))
        len2 <- length(which(S[,2]==i))
        
        S[which(S[,1]==i),1] <- rep(name[i],len1)
        S[which(S[,2]==i),2] <- rep(name[i],len2)
    }
    
    #remove weights of 0
    S[,3] <- ifelse(S[,3]==0,NA,S[,3])
    
    cyto <- na.omit(S)
    
    attr(cyto, "na.action") <- NULL
    attr(cyto, "class") <- NULL
    
    return(noquote(cyto))
}
#----