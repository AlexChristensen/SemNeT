#' De-string Responses
#' @description De-string responses after performing semnetcleaner
#' @param rmat A semnetcleaner filtered response matrix
#' @param rm.str The column number of the stringed response
#' @return The response matrix with the string column merged into appropriate response columns and the string response removed
#' @examples
#' #create example stringed responses
#' stringed <- cbind(rowSums(cbind(rmat[,c(1,2)])),convmat)
#' #change name to stringed name
#' colnames(stringed)[1] <- "alligator.ant"
#' 
#' #de-string
#' convmat <- destr(stringed, 1)
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
#De-string Function----
destr <- function (rmat, rm.str)
{
    
    replace<-unlist(strsplit(colnames(rmat[rm.str]),"[.]"))
    
    if(any(is.na(match(replace,colnames(rmat)))))
    {warning(paste(replace[which(is.na(match(replace,colnames(rmat))))],"is not a current response for subject",
                   which(rmat[colnames(rmat[rm.str])]!=0),"additional manipulation is required",sep=" ",collapse=", "))
        replace<-replace[-which(is.na(match(replace,colnames(rmat))))]}
    
    rmat[match(replace,colnames(rmat))][which(rmat[rm.str]!=0),]<-1
    rmat<-rmat[,-rm.str]
    
    return(rmat)
}
#----