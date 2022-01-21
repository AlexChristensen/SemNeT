#' Equate Groups
#' @description A function to "equate" multiple response matrices to one another.
#' \emph{N} number of groups are matched based on their responses so
#' that every group has the same responses in their data
#' 
#' @param ... Matrices, data frames or a list of matrices and data frames.
#' Binary response matrices to be equated
#' 
#' @return This function returns a list containing the
#' equated binary response matrices in the order they were input.
#' The response matrices are labeled as the object name they were
#' entered with
#' 
#' @examples
#' # Obtain binary data
#' bin <- open.binary
#' 
#' # Finalize mat1
#' mat1 <- finalize(bin[c(1:5),])
#' 
#' # Finalize mat2
#' mat2 <- finalize(bin[c(6:10),])
#'
#' # Equate mat1 and mat1
#' eq <- equate(mat1, mat2)
#' 
#' # Obtain respective equated response matrices
#' eq.mat1 <- eq$mat1 # list objects are named with the names
#' eq.mat2 <- eq$mat2 # they were entered with
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Equate matrices function
# Updated 21.01.2022
equate <- function(..., input_list = NULL)
{
    # Equate function
    equat <- function (rmatA, rmatB)
    {
        while(length(colnames(rmatA))!=length(colnames(rmatB)))
        {
            if(length(colnames(rmatA))>=length(colnames(rmatB)))
            {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
            }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
            {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
            }else if(all(match(colnames(rmatA),colnames(rmatB))))
            {print("Responses match")}
        }
        
        return(list(rmatA=rmatA,rmatB=rmatB))
    }
    
    if(is.null(input_list)){
        
        name <- as.character(substitute(list(...)))
        name <- name[-which(name=="list")]
        
        datalist <- list(...)
        
    }else{
        
        name <- names(input_list)
        
        if(is.null(name)){
            name <- 1:length(input_list)
        }
        
        datalist <- input_list
        
    }
    
    len <- length(datalist)
    
    if(len > 2){
        
        first <- datalist[[1]]
        eq <- equat(first,datalist[[2]])$rmatA
        
        for(i in 2:(len-1)){
            eq <- equat(eq,datalist[[(i+1)]])$rmatA
        }
        
        finlist <- list()
        
        for(j in 1:len){
            finlist[[name[j]]] <- equat(eq,datalist[[j]])$rmatB
        }

        
    }else if(len == 2){
        
        finlist <- equat(datalist[[1]], datalist[[2]])
        names(finlist) <- name
    }
    
    return(finlist)
}
#----