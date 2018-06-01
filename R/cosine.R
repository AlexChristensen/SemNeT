#' Cosine Similarity
#' @description Computes cosine similarity
#' @param data A binarized dataset of verbal fluency or linguistic data
#' @param addConstant A constant to be added to the cosine similarity matrix
#' @return A cosine similarity matrix
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Cosine similarity----
cosine <- function (data, addConstant = 0)
{
    n <- ncol(data)
    
    mat <- matrix(0, nrow=n, ncol=n)
    
    for(i in 1:n)
        for(j in 1:n)
    {mat[i,j] <- sum(data[,i]*data[,j])/(sqrt(sum(data[,i]^2))*sqrt(sum(data[,j]^2)))}
    
    colnames(mat) <- colnames(data)
    row.names(mat) <- colnames(data)
    
    if(addConstant>0)
    {mat <- mat + addConstant}
    
    diag(mat) <- 1
    
    return(mat)
}
#----