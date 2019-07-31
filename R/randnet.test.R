#' Test Against Random Networks
#' @description Performs significance tests for global measures
#' of semantic networks against the global measures of equivalent
#' size (and density) random networks
#' 
#' @param ... Matrices or data frames.
#' Semantic networks to be compared against random networks
#' 
#' @param iter Numeric.
#' Number of iterations in bootstrap.
#' Defaults to \code{1000}
#' 
#' @param cores Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maxmimum amount of cores on your computer
#' 
#' @return Returns a matrix containing p-values
#' for the network measures of the input networks against
#' the distribution of equivalent random networks. The last
#' two columns contain the mean (\code{"M.rand"}) and
#' standard deviation (\code{"SD.rand"}) of the network measures
#' for the random network distribution
#' 
#' @examples 
#' # Finalize rmatA
#' finalCmat <- SemNetCleaner::finalize(SemNetCleaner::convmat)
#' 
#' # Finalize rmatB
#' finalRmat <- SemNetCleaner::finalize(SemNetCleaner::rmat)
#' 
#' # Finalize rmatC
#' finalYmat <- SemNetCleaner::finalize(SemNetCleaner::rmat)
#'
#' # Equate rmatA and rmatB
#' eq <- SemNetCleaner::equate.multi(finalCmat,finalRmat,finalYmat)
#' 
#' # Obtain respective equated response matrices
#' eqCmat <- eq$finalCmat
#' eqRmat <- eq$finalRmat
#' eqYmat <- eq$finalYmat
#' 
#' \dontrun{
#' 
#' #Compute networks
#' Cnet <- NetworkToolbox::TMFG(similarity(eqCmat))$A
#' Rnet <- NetworkToolbox::TMFG(similarity(eqRmat))$A
#' Ynet <- NetworkToolbox::TMFG(similarity(eqYmat))$A
#' 
#' #Perform random networks test
#' randnet.test(Cnet, Rnet, Ynet, iter = 1000)
#' 
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
randnet.test <- function (..., iter, cores)
{
    #Missing arguments
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    
    if(missing(iter))
    {iter <- 1000
    }else{iter <- iter}
    
    #Get names of networks
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    #Create list of input
    datalist <- list(...)
    
    #Number of nodes
    nodes <- ncol(datalist[[1]])
    
    #Number of edges
    edges <- sum(colSums(NetworkToolbox::binarize(datalist[[1]])))/2
    
    #Initialize random networks list
    rand.list <- list()
    
    #Initialize count
    count <- 0
    
    #Message for begin random networks
    message("Generating random networks...", appendLF = FALSE)
    
    repeat{
        
        #Increase count
        count <- count + 1
        
        #Generate random network
        rand.list[[count]] <- NetworkToolbox::randnet(nodes, edges)
        
        #Break out of repeat when
        #count reaches iter
        if(count == iter)
        {break}
    }
    
    #Message for end of random networks
    message("done", appendLF = TRUE)
    
    #Message for begin of network measures
    message("Computing network measures...", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export variables
    parallel::clusterExport(cl, "rand.list", envir = environment())
    
    #Compute network measures
    net.meas <- pbapply::pbsapply(X = rand.list, FUN = semnetmeas, cl = cl)
    
    #Stop parallel processing
    parallel::stopCluster(cl)
    
    #Compute significance tests
    sig.mat <- matrix(0, nrow = 3, ncol = length(name)+2)
    row.names(sig.mat) <- c("ASPL","CC","Q")
    colnames(sig.mat) <- c(name, "M.rand", "SD.rand")
    
    #Insert random means and sds
    sig.mat[,"M.rand"] <- round(rowMeans(net.meas),4)
    sig.mat[,"SD.rand"] <- round(apply(net.meas,1,sd),4)
    
    for(i in 1:length(name))
    {
        #Compute semantic network measures for network
        meas <- semnetmeas(datalist[[i]])
        
        ##ASPL
        sig.mat["ASPL",i] <- round(1 - pnorm(meas["ASPL"], mean = sig.mat["ASPL","M.rand"], sd = sig.mat["ASPL","SD.rand"]),4)
        ##CC
        sig.mat["CC",i] <- round(1 - pnorm(meas["CC"], mean = sig.mat["CC","M.rand"], sd = sig.mat["CC","SD.rand"]),4)
        ##Q
        sig.mat["Q",i] <- round(1 - pnorm(meas["Q"], mean = sig.mat["Q","M.rand"], sd = sig.mat["Q","SD.rand"]),4)
    }
    
    return(sig.mat)
}
#----