#' Automated De-string Responses
#' @description Automated de-string responses after performing semnetcleaner
#' @param rmat A semnetcleaner filtered response matrix
#' @param char Minimum number of characters in a string to be checked for \link[SemNetCleaner]{destr}.
#' Defaults to 10
#' @return A question asking whether the response should be de-string-ed.
#' If yes, \link[SemNetCleaner]{destr} will be applied.
#' If no, the next response will be checked
#' @examples
#' #create example stringed responses
#' stringed <- cbind(rowSums(cbind(rmat[,c(1,2)])),convmat)
#' #change name to stringed name
#' colnames(stringed)[1] <- "alligator.ant"
#' 
#' \donttest{
#' #automated de-string
#' convmat <- destr(stringed, 10)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#Automated De-string Function----
autoDestr <- function (rmat, char = 10)
{
    #potential de-string candidates
    pot <- which(nchar(colnames(rmat))>char)
    
    for(i in length(pot):1)
    {
        print(colnames(rmat[pot[i]]))
        ans <- menu(c("Yes","No"),title="De-string response?")
        
        if(ans==1)
        {rmat <- destr(rmat, pot[i])}
    }
    
    return(rmat)
}
#----