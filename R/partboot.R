#' Partial Bootstrapped Semantic Network Analysis
#' 
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' 
#' @param data Matrix or data frame.
#' Cleaned response matrix
#' 
#' @param paired Matrix or data frame.
#' Input for a second network to be paired with \code{data}.
#' Defaults to \code{NULL}.
#' Input a matrix or data frame containing another sample
#' 
#' @param percent Numeric.
#' Percent of nodes to remain in the network.
#' Defaults to \code{.50}
#' 
#' @param sim Character.
#' Similarity measure to use.
#' Defaults to \code{"cosine"}.
#' See \code{\link[SemNeT]{similarity}} for other options
#' 
#' @param weighted Boolean.
#' Should weighted ASPL and CC be used?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted ASPL and CC
#' 
#' @param iter Numeric.
#' Number of iterations in bootstrap.
#' Defaults to \code{1000}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maxmimum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @return Returns a list containing:
#' 
#' \item{dataMeas}{A matrix for the network input in the \code{data}
#' arugment, where columns are the semantic network measures
#' from \code{\link[SemNeT]{semnetmeas}} and rows are their values from each
#' bootstrapped sample (results in a matrix with the dimensions \code{iter} by 3)}
#' 
#' \item{dataSumm}{Summary statistics across the bootrapped samples for the
#' network input in the \code{data} argument}
#' 
#' \item{percent}{Outputs the percent used from the \code{percent} argument}
#' 
#' \item{iter}{Outputs the number of bootstrapped samples
#' used from the \code{iter} argument}
#' 
#' If a \code{paired} network is input, then also returns:
#' 
#' \item{pairedMeas}{A matrix for the network input in the \code{paired}
#' arugment, where columns are the semantic network measures
#' from \code{\link[SemNeT]{semnetmeas}} and rows are their values from each
#' bootstrapped sample (results in a matrix with the dimensions \code{iter} by 3)}
#' 
#' \item{pairedSumm}{Summary statistics across the bootrapped samples for the
#' network input in the \code{paired} argument}
#' 
#' @examples
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' one.result <- partboot(data = one, percent = .50, iter = 1000,
#' sim = "cosine", cores = 2)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats sd cor dist pnorm runif
#' 
#' @export
#Partial Bootstrapped Semantic Network Analysis----
partboot <- function (data, paired = NULL, percent, sim, weighted = FALSE,
                      iter = 1000, cores)
{
    ####Missing arguments####
    if(missing(paired))
    {paired <- NULL}
    
    if(missing(sim))
    {sim <- "cosine"
    }else{sim <- sim}
    
    cormat <- similarity(data, method = sim)
    
    if(!is.null(paired))
    {cormatP <- similarity(paired, method = sim)}
    
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    ####Missing arguments####
    
    #Number of nodes in full data
    full <- ncol(data)
    
    #Initialize count
    count <- 0
    
    #Initialize data list(s)
    datalist <- list()
    
    if(!is.null(paired))
    {datalist.p <- list()}
    
    #######################
    #### GENERATE DATA ####
    #######################
    
    repeat{
        
        #Increase count
        count <- count + 1
        
        #Randomly sample nodes
        rand <- sample(full, (full*percent), replace=FALSE)
        
        #Input into data list
        datalist[[count]] <- data[,rand]
        
        #Input into paired list
        if(!is.null(paired))
        {datalist.p[[count]] <- paired[,rand]}
        
        if(count == iter)
        {break}
    }

    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    #Let user know simliairity is being computed
    message("Computing similarity measures...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
        
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("datalist"), envir = environment())
        
    #Compute similarity
    dataSim <- pbapply::pblapply(X = datalist,
                                      cl = cl,
                                      FUN = similarity,
                                      method = sim)
    #Stop Cluster
    parallel::stopCluster(cl)
    
    if(!is.null(paired))
    {
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = c("datalist.p"), envir = environment())
        
        pairedSim <- pbapply::pblapply(X = datalist.p,
                                        cl = cl,
                                        FUN = similarity,
                                        method = sim)
        #Stop Cluster
        parallel::stopCluster(cl)
    }
    
    ############################
    #### CONSTRUCT NETWORKS ####
    ############################
    
    #Let user know networks are being computed
    message("Constructing networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("dataSim"), envir = environment())
    
    #Compute similarity
    dataNet <- pbapply::pblapply(X = dataSim,
                                  cl = cl,
                                  FUN = NetworkToolbox::TMFG)
    #Stop Cluster
    parallel::stopCluster(cl)
    
    if(!is.null(paired))
    {
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = c("pairedSim"), envir = environment())
        
        #Compute similarity
        pairedNet <- pbapply::pblapply(X = pairedSim,
                                     cl = cl,
                                     FUN = NetworkToolbox::TMFG)
        #Stop Cluster
        parallel::stopCluster(cl)
    }
    
    #Grab networks
    if(is.null(paired))
    {
        #Initialize semantic network list
        dataSemnet <- list()
        
        #Grab network only
        for(i in 1:iter)
        {dataSemnet[[i]] <- dataNet[[i]]$A}
    }else{
        
        #Initialize semantic network list
        dataSemnet <- list()
        pairedSemnet <- list()
        
        #Grab network only
        for(i in 1:iter)
        {
            dataSemnet[[i]] <- dataNet[[i]]$A
            pairedSemnet[[i]] <- pairedNet[[i]]$A
        }
    }
    
    ##############################
    #### COMPUTING STATISTICS ####
    ##############################
    
    #Let user know networks are being computed
    message("Computing statistics...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("dataSemnet"), envir = environment())
    
    #Compute similarity
    dataMeas <- pbapply::pbsapply(X = dataSemnet,
                                 cl = cl,
                                 FUN = semnetmeas)
    #Stop Cluster
    parallel::stopCluster(cl)
    
    if(!is.null(paired))
    {
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = c("pairedSemnet"), envir = environment())
        
        #Compute similarity
        pairedMeas <- pbapply::pbsapply(X = pairedSemnet,
                                       cl = cl,
                                       FUN = semnetmeas)
        #Stop Cluster
        parallel::stopCluster(cl)
    }
    
    #Compute summary statistics
    summ.table <- function (data, n)
    {
        stats <- list()
        stats$mean <- rowMeans(data)
        stats$stdev <- apply(data,1,sd)
        stats$se <- stats$stdev/sqrt(n)
        stats$lower <- stats$mean - (1.96 * stats$se)
        stats$upper <- stats$mean + (1.96 * stats$se)
        
        return(stats)
    }
    
    #Return results
    bootlist <- list()
    
    bootlist$dataMeas <- dataMeas
    bootlist$dataSumm <- summ.table(dataMeas, iter)
    
    if(!is.null(paired))
    {
        bootlist$pairedMeas <- pairedMeas
        bootlist$pairedSumm <- summ.table(pairedMeas, iter)
    }
    
    bootlist$percent <- percent
    bootlist$iter <- iter
    
    class(bootlist) <- "partboot"
    
    return(bootlist)
}
#----