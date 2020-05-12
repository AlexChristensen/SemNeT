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
#' Set to any number between 1 and maximum amount of cores on your computer
#' 
#' @return Returns a matrix containing p-values
#' for the network measures of the input networks against
#' the distribution of equivalent random networks. The last
#' two columns contain the mean (\code{"M.rand"}) and
#' standard deviation (\code{"SD.rand"}) of the network measures
#' for the random network distribution
#' 
#' @examples 
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos <- similarity(one, method = "cosine")
#' 
#' # Compute networks using NetworkToolbox
#' net <- NetworkToolbox::TMFG(cos)$A
#' \donttest{
#' # Perform random networks test
#' randnet.test(net, iter = 1000, cores = 2)
#' }
#' \dontshow{randnet.test(net, iter = 1, cores = 2)}
#' 
#' @references 
#' Viger, F., & Latapy, M. (2016).
#' Efficient and simple generation of random simple connected graphs with prescribed degree sequence.
#' \emph{Journal of Complex Networks}, \emph{4}, 15-37.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats dnorm
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
    listdata <- list(...)
    
    #Initialize data list
    data.list <- vector("list", length = length(name))
    
    for(i in 1:length(name))
        for(j in 1:iter)
            {data.list[[i]][[j]] <- listdata[[i]]}
    
    #Initialize random networks list
    rand.list <- vector("list", length = length(name))
    names(rand.list) <- name
    
    #Message for begin random networks
    message("Generating random networks...", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export variables
    parallel::clusterExport(cl, c("data.list", "rand.list"), envir = environment())
    
    #Compute random networks
    for(i in 1:length(data.list))
    {rand.list[[i]] <- pbapply::pblapply(X = data.list[[i]], FUN = function(X){randnet(A = X)}, cl = cl)}
    
    #Message for begin of network measures
    message("Computing network measures...\n", appendLF = FALSE)
    
    #Initialize network measures list
    net.meas <- vector("list", length = length(name))
    names(net.meas) <- name
    
    #Export variables
    parallel::clusterExport(cl, c("net.meas"), envir = environment())
    
    for(i in 1:length(data.list))
    {
        #Compute network measures
        net.meas[[i]] <- pbapply::pbsapply(X = rand.list[[i]], FUN = semnetmeas, cl = cl)
    }
    
    #Stop parallel processing
    parallel::stopCluster(cl)
    
    #Initialize result list
    res <- vector("list", length = length(name))
    names(res) <- name
    
    #Compute significance tests
    for(i in 1:length(data.list))
    {
        sig.mat <- matrix(0, nrow = 3, ncol = 3)
        row.names(sig.mat) <- c("ASPL","CC","Q")
        colnames(sig.mat) <- c(paste(name[i], "(p-value)"), "M.rand", "SD.rand")
        
        #Insert random means and sds
        sig.mat[,"M.rand"] <- round(rowMeans(net.meas[[i]]),4)
        sig.mat[,"SD.rand"] <- round(apply(net.meas[[i]],1,sd),4)
        
        #Compute semantic network measures for network
        meas <- semnetmeas(listdata[[i]])
        
        ##ASPL
        z.aspl <- (meas["ASPL"] - sig.mat["ASPL","M.rand"]) / sig.mat["ASPL","SD.rand"]
        sig.mat["ASPL",paste(name[i], "(p-value)")] <- round(dnorm(z.aspl, mean = sig.mat["ASPL","M.rand"], sd = sig.mat["ASPL","SD.rand"]),4)
        ##CC
        z.cc <- (meas["CC"] - sig.mat["CC","M.rand"]) / sig.mat["CC","SD.rand"]
        sig.mat["CC",paste(name[i], "(p-value)")] <- round(dnorm(z.cc, mean = sig.mat["CC","M.rand"], sd = sig.mat["CC","SD.rand"]),4)
        ##Q
        z.q <- (meas["Q"] - sig.mat["Q","M.rand"]) / sig.mat["Q","SD.rand"]
        sig.mat["Q",paste(name[i], "(p-value)")] <- round(dnorm(z.q, mean = sig.mat["Q","M.rand"], sd = sig.mat["Q","SD.rand"]),4)
    
        #Insert results
        res[[i]] <- sig.mat
    }
    
    return(res)
}
#----