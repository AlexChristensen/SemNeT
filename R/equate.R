#' Equate Group Responses
#' @description An automated cleaning function for matching groups' responses
#' @param rmatA A semnetcleaner filtered response matrix for group 1
#' @param rmatB A semnetcleaner filtered response matrix for group 2
#' @return A list of responses matched for group 1 (rmatA) and group 2 (rmatB)
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
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @export
# Equate----
equate<-function(rmatA,rmatB)
{
    if(length(colnames(rmatA))>=length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    
    if(length(colnames(rmatA))>=length(colnames(rmatB)))
    {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
    }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
    {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
    }else if(all(match(colnames(rmatA),colnames(rmatB))))
    {print("Responses match")}
    return(list(rmatA=rmatA,rmatB=rmatB))
}
#----