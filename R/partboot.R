#' Partial Bootstrapped Semantic Network Analysis
#' 
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' 
#' @param ... Matrices or data frames.
#' Binary response matrices (e.g., \code{binary} output from
#' \code{\link[SemNetCleaner]{textcleaner}})
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
#' Defaults to \emph{n} / 2 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @return Returns a list containing:
#' 
#' \item{dataMeas}{A matrix for the network input in the \code{data}
#' argument, where columns are the semantic network measures
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
partboot <- function (..., percent = .50, sim, weighted = FALSE,
                      iter = 1000, cores)
{
    ####Missing arguments####
    if(missing(sim))
    {sim <- "cosine"
    }else{sim <- sim}
    
    if(missing(cores))
    {cores <- parallel::detectCores() / 2
    }else{cores <- cores}
    ####Missing arguments####
    
    #Get names of networks
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    #Create list of input
    datalist <- list(...)
    
    #Check that all data have same number of nodes
    if(length(unique(unlist(lapply(datalist,ncol))))!=1)
    {stop("All datasets must have the same number of columns")}
    
    #Number of nodes in full data
    full <- unique(unlist(lapply(datalist,ncol)))
    
    #######################
    #### GENERATE DATA ####
    #######################
    
    for(i in 1:length(name))
    {
        #Initialize count
        count <- 0
        
        #Initialize new data list
        new <- list()
        
        repeat{
            
            #Increase count
            count <- count + 1
            
            #Randomly sample nodes
            rand <- sample(full, (full*percent), replace=FALSE)
            
            #Input into data list
            new[[count]] <- get(name[i], envir = environment())[,rand]
            
            if(count == iter)
            {break}
        }
        
        #Insert data list
        assign(paste("dl.",name[i],sep=""),new)
    }

    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    #Let user know simliairity is being computed
    message("Computing similarity measures...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
        
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = paste("dl.",name,sep=""), envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute similarity
        newSim <- pbapply::pblapply(X = get(paste("dl.",name[i],sep=""), envir = environment()),
                                    cl = cl,
                                    FUN = similarity,
                                    method = sim)
        
        #Insert similarity list
        assign(paste("sim.",name[i],sep=""),newSim)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    ############################
    #### CONSTRUCT NETWORKS ####
    ############################
    
    #Let user know networks are being computed
    message("Estimating networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = paste("sim.",name,sep=""), envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute networks
        newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                    cl = cl,
                                    FUN = NetworkToolbox::TMFG)
        
        #Insert network list
        assign(paste("net.",name[i],sep=""),newNet)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    #Grab networks
    for(i in 1:length(name))
    {
        #Initialize semantic network list
        Semnet <- list()
        
        #Loop through networks
        for(j in 1:iter)
        {Semnet[[j]] <- get(paste("net.",name[i],sep=""), envir = environment())[[j]]$A}
        
        #Insert semantic network list
        assign(paste("Semnet.",name[i],sep=""),Semnet)
    }
    
    ##############################
    #### COMPUTING STATISTICS ####
    ##############################
    
    #Let user know networks are being computed
    message("Computing statistics...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = paste("Semnet.",name[i],sep=""), envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute network measures
        netMeas <- pbapply::pbsapply(X = get(paste("Semnet.",name[i],sep=""), envir = environment()),
                                     cl = cl,
                                     FUN = semnetmeas)
        
        #Insert network measures list
        assign(paste("meas.",name[i],sep=""),netMeas)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
        
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
    
    #Initialize bootlist
    bootlist <- list()
    
    #Insert results
    for(i in 1:length(name))
    {
        bootlist[[paste(name[i],"Meas",sep="")]] <- get(paste("meas.",name[i],sep=""), envir = environment())
        bootlist[[paste(name[i],"Summ",sep="")]] <- summ.table(get(paste("meas.",name[i],sep=""), envir = environment()), iter)
    }
    
    #Insert percent remaining and iterations
    bootlist$percent <- percent
    bootlist$iter <- iter
    
    class(bootlist) <- "partboot"
    
    return(bootlist)
}
#----