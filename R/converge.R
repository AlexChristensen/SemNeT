#' Converge Responses
#' @description Merge a column of binarized response data with another
#' @param rmat A semnetcleaner filtered response matrix
#' @param word The column name that will incoporate the \strong{replace} column's binarized responses (must be characters)
#' @param replace The column name that should be merged with the \strong{word} column (must be characters)
#' @return The response matrix with the \strong{word} column merged and the \strong{replace} column removed
#' @examples
#' #converge "kitten" into response of "cat"
#' rmat <- converge(rmat,"cat","kitten")
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Converge Function----
converge <- function (rmat, word, replace)
{
    if(any(colnames(rmat)==replace))
    {
        if(any(colnames(rmat)==word))
        {
            for(i in 1:nrow(rmat))
                if(rmat[i,which(colnames(rmat)==replace)]==1)
                {rmat[i,which(colnames(rmat)==word)] <- 1}
            
            #coverge word to be replaced with correct word
            rmat[which(colnames(rmat)==word)]
            #remove column with spelling difference
            rmat<-rmat[-which(colnames(rmat)==replace)]
            
            return(rmat)
        }else{stop("word not found")} #produce error if word does not exist
    }else{stop("word to replace not found")} #produce error if word to replace does not exist
}
#----