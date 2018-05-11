#' Cosine Similarity
#' @description Computes cosine similarity
#' @param data A binarized dataset of verbal fluency or linguistic data
#' @return A cosine similarity matrix
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Cosine similarity----
cosine <- function (data)
{
    n <- ncol(data)
    
    mat <- matrix(0, nrow=n, ncol=n)
    
    for(i in 1:n)
        for(j in 1:n)
    {mat[i,j] <- sum(data[,i]*data[,j])/(sqrt(sum(data[,i]^2))*sqrt(sum(data[,j]^2)))}
    
    return(mat)
}
#----