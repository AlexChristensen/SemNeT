#' De-string Responses
#' @description De-string responses after performing semnetcleaner
#' @param rmat A semnetcleaner filtered response matrix
#' @param column The column number or name of the stringed response
#' @param sep Separating string character (e.g., " ", ".", ",").
#' Must be input as a character
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
destr <- function (rmat, column, sep)
{
    replace <- unlist(strsplit(colnames(rmat[column]),paste("[",sep,"]",sep="")))
    
    included <- replace
    
    na.included <- NULL
    
    if(any(is.na(match(replace,colnames(rmat)))))
    {
        warning(paste(replace[which(is.na(match(replace,colnames(rmat))))],"is not a current response",sep=" ",collapse=", "))
        
        included <- replace[-which(is.na(match(replace,colnames(rmat))))]
        na.included <- replace[which(is.na(match(replace,colnames(rmat))))]
    }
    
    rmat[match(included,colnames(rmat))][which(rmat[column]!=0),] <- 1
    
    if(!is.null(na.included))
    {
        n <- length(na.included)
        
        for(i in 1:n)
        {
            new <- vector(mode="numeric",length = nrow(rmat))
            rmat <- cbind(rmat,new)
            colnames(rmat)[which(colnames(rmat)=="new")] <- na.included[i]
            rmat[match(na.included[i],colnames(rmat))][which(rmat[column]!=0),] <- 1
            warning(paste("A new response column has been created for: ", na.included[i]))
        }
    }
    
    if(is.character(column))
    {rm.col <- which(colnames(rmat)==column)
    }else if(is.numeric(column)){rm.col <- column}
    
    rmat <- rmat[,-rm.col]
    
    rmat <- rmat[,order(colnames(rmat))]
    
    return(rmat)
}
#----