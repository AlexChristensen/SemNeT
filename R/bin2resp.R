#' Binary Responses to Character Responses
#' @description Converts the binary response matrix into characters for each participant
#' @param rmat A binarized response matrix of verbal fluency or linguistic data
#' @return A list containing objects for each participant and their responses
#' @examples 
#' charmat <- bin2resp(convmat)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Binary to Response----
bin2resp <- function (rmat)
{
    #grab response names
    name <- colnames(rmat)
    
    #number of responses
    n <- ncol(rmat)
    
    #initialize matrix
    mat <- matrix(NA,nrow=nrow(rmat),ncol=ncol(rmat))
    
    #loop for each name
    for(i in 1:n)
    {mat[,i] <- ifelse(rmat[,i]==1,name[i],NA)}
    
    #number of participants
    p <- nrow(rmat)
    
    #initialize participant list
    part <- list()
    
    #loop for each participant
    for(j in 1:p)
    {
        resps <- na.omit(mat[j,])
        attributes(resps)$na.action <- NULL
        part[[row.names(rmat)[j]]] <- resps
    }
    
    return(part)
}
#----