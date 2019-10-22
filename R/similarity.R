#' Measures of Similarity
#' @description Computes several measures of similarity
#' (see Choi, Cha, & Tappert, 2010 for additional measures)
#' 
#' @param data Matrix or data frame.
#' A binarized dataset of verbal fluency or linguistic data
#' 
#' @param method Character.
#' Type of similarity measure to compute.
#' 
#' Below are the definitions for each bin:
#' 
#' \tabular{ccccc}{
#' \tab 1 \tab 0 \tab \tab \cr
#' 1 \tab a \tab b \tab a+b \tab (R1)\cr
#' 0 \tab c \tab d  \tab c+d \tab (R2)\cr
#' \tab a+c \tab b+d \tab a+b+c+d \tab (N) \cr
#' \tab (C1) \tab (C2) \tab (N) \tab \cr
#' }
#'     
#' Options include:
#' 
#' \itemize{
#' 
#' \item{\code{"angular"} =}
#' {\eqn{1 - (2 * acos(cosine similarity) / \pi)}}
#' 
#' \item{\code{"cosine"} =}
#' {\eqn{a / \sqrt{(a + b)(a + c)}}}
#' 
#' \item{\code{"faith"} =}
#' {\eqn{a + 0.5d / a + b + c + d}}
#' 
#' \item{\code{"jaccard"} =}
#' {\eqn{a / a + b + c}}
#' 
#' \item{\code{"phi"} and \code{"cor"} =}
#' {\eqn{ad - bc / \sqrt(R1 x R2 x C1 x C2)}}
#' 
#' \item{\code{"rr"} =}
#' {\eqn{a / a + b + c + d}}
#' 
#' }
#' 
#' @return A symmetric similarity matrix
#' 
#' @examples
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos <- similarity(one, method = "cosine")
#' 
#' @references 
#' Choi, S. S., Cha, S. H., & Tappert, C. C. (2010).
#' A survey of binary similarity and distance measures.
#' \emph{Journal of Systemics, Cybernetics and Informatics}, \emph{8}, 43-48.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Similiarity measures----
similarity <- function (data, method = c("angular", "cor", "cosine",
                                         "euclid", "faith", "jaccard",
                                         "phi", "rr"))
{
    if(missing(method))
    {method <- "cosine"
    }else{method <- match.arg(method)}
    
    # Number of variables
    n <- ncol(data)
    
    # Initiliaze matrix
    mat <- matrix(0, nrow = n, ncol = n)
    
    # Matrix methods
    mat.methods <- c("angular", "faith", "jaccard", "rr")
    
    if(method %in% mat.methods)
    {
        # Matrix indices
        mat.ind <- which(lower.tri(mat),arr.ind = TRUE)
        # Lower matrix indices
        lower.ind <- which(lower.tri(mat))
        
        # Loop for matrix
        for(i in 1:nrow(mat.ind))
        {
            # Locations
            a <- sum(ifelse(rowSums(data[,mat.ind[i,]])==2,1,0))
            b <- sum(ifelse(data[,mat.ind[i,1]]==1&data[,mat.ind[i,2]]==0,1,0))
            c <- sum(ifelse(data[,mat.ind[i,1]]==0&data[,mat.ind[i,2]]==1,1,0))
            d <- sum(ifelse(rowSums(data[,mat.ind[i,]])==0,1,0))
            
            # Method
            if(method == "angular")
            {mat[lower.ind[i]] <- 1-(2*acos(a / sqrt((a + b) * (a + c)))/pi)
            }else if(method == "faith")
            {mat[lower.ind[i]] <- (a + .5*d) / (a + b + c + d)
            }else if(method == "jaccard")
            {mat[lower.ind[i]] <- a / (a + b + c)
            }else if(method == "phi")
            {mat[lower.ind[i]] <- (a*d - b*c) / sqrt((a+b)*(c+d)*(a+c)*(b+d))
            }else if(method == "rr")
            {mat[lower.ind[i]] <- a / (a + b + c + d)}
        }
        
        # Symmetricize
        mat <- mat + t(mat)
        diag(mat) <- 1
        
        colnames(mat) <- colnames(data)
        row.names(mat) <- colnames(data)
        
    }else if(method == "euclid")
    {mat <- dist(t(data), diag = TRUE, upper = TRUE)
    }else if(method == "cosine")
    {mat <- lsa::cosine(as.matrix(data))
    }else if(method == "phi" || method == "cor")
    {mat <- cor(data)}
    
    return(mat)
}
#----