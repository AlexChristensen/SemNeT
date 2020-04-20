#' Bootstrapped Semantic Network Analysis
#' 
#' @description Bootstrap techniques to generate
#' semantic networks and compute global network characteristics
#' 
#' @param ... Matrices or data frames.
#' Cleaned response matrices (e.g., \code{responses$clean} from
#' \code{\link[SemNetCleaner]{textcleaner}}) or  binary response matrices
#' (e.g., \code{binary} output from \code{\link[SemNetCleaner]{textcleaner}})
#' 
#' @param method Character.
#' Network estimation method to use.
#' Current options include:
#' 
#' \itemize{
#' \item{\code{\link[NetworkToolbox]{TMFG}}}
#' {Triangulated Maximally Filtered Graph}
#' 
#' \item{\code{\link[SemNeT]{CN}}}
#' {Community Network}
#' 
#' \item{\code{\link[SemNeT]{NRW}}}
#' {Naive Random Walk}
#' 
#' \item{\code{\link[SemNeT]{PF}}}
#' {Pathfinder}
#' 
#' }
#' 
#' @param type Character.
#' Type of bootstrap to perform
#' 
#' \itemize{
#' \item{\code{node}}
#' {Generates partial networks based on dropping a certain
#' proportion of nodes (see argument \code{prop})}
#' 
#' \item{\code{case}}
#' {Samples with replacement the same number of participants
#' as in the original dataset}
#' }
#' 
#' @param prop Numeric.
#' \strong{Only} for \code{type = "node"}.
#' Proportion of nodes to remain in the network.
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
#' \item{prop}{Outputs the proportion used from the \code{prop} argument}
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
#' # Run bootstrap node-drop (partial) networks
#' one.result <- bootSemNeT(one, prop = .50, iter = 1000,
#' sim = "cosine", cores = 2, method = "TMFG", type = "node")
#' 
#' }
#' # Bootstrap case-wise networks
#' ## Get openness data
#' low <- open.binary[which(open.group == "Low"),]
#' high <- open.binary[which(open.group == "High"),]
#' \donttest{
#' ## Run
#' open <- bootSemNeT(low, high, iter = 1000, cores = 2, method = "NRW", type = "case")
#' 
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats sd cor dist pnorm runif
#' 
#' @export
# Bootstrapped Semantic Network Analysis----
# Updated 15.04.2020
bootSemNeT <- function (..., method = c("CN", "NRW", "PF", "TMFG"),
                        type = c("case", "node"),
                        prop = .50, sim, weighted = FALSE,
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
    
    #Number of nodes in full data
    full <- unique(unlist(lapply(datalist,ncol)))
    
    #######################
    #### GENERATE DATA ####
    #######################
    
    #Let user know data is being generated
    message("Generating data...", appendLF = FALSE)
    
    for(i in 1:length(name))
    {
        #Initialize count
        count <- 0
        
        #Initialize new data list
        new <- list()
        
        repeat{
            
            #Increase count
            count <- count + 1
            
            if(type == "node")
            {
                #Check that all data have same number of nodes
                if(length(unique(unlist(lapply(datalist,ncol))))!=1)
                {stop("bootSemNeT(): All datasets must have the same number of columns")}
                
                #Error on method
                if(method != "TMFG")
                {stop(paste("bootSemNeT(): Node-wise bootstrap is not supported with the", method, "method", sep = " "))}
                
                #Randomly sample nodes
                rand <- sample(full, (full*prop), replace=FALSE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[,rand]
            }else if(type == "case")
            {
                #Error on method = "TMFG"
                if(method == "TMFG")
                {stop("bootSemNeT(): Case-wise bootstrap is not supported with the TMFG method")}
                
                #Randomly sample nodes
                rand <- sample(nrow(get(name[i], envir = environment())),
                               nrow(get(name[i], envir = environment())),
                               replace=TRUE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[rand,]
            }
            
            if(count == iter)
            {break}
        }
        
        #Insert data list
        assign(paste("dl.",name[i],sep=""),new)
    }
    
    #Let user know data generation is finished
    message("done\n")

    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    if(method == "TMFG")
    {
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
    }else{
        for(i in 1:length(name))
        {
            #Insert similarity list
            assign(paste("sim.",name[i],sep=""),
                   get(paste("dl.",name[i],sep=""), envir = environment()))
        }
    }
    
    ############################
    #### CONSTRUCT NETWORKS ####
    ############################
    
    #Assign network function
    NET.FUNC <- switch(method,
                       TMFG = NetworkToolbox::TMFG,
                       CN = CN,
                       NRW = NRW,
                       PF = PF)
    
    #Let user know networks are being computed
    message("Estimating networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c(paste("sim.",name,sep=""), "NET.FUNC"),
                            envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute networks
        newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                    cl = cl,
                                    FUN = NET.FUNC)
        
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
        if(method == "TMFG")
        {
            for(j in 1:iter)
            {Semnet[[j]] <- get(paste("net.",name[i],sep=""), envir = environment())[[j]]$A}
        }else{
            for(j in 1:iter)
            {Semnet[[j]] <- get(paste("net.",name[i],sep=""), envir = environment())[[j]]}
        }

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
                                     FUN = semnetmeas
                                     )
        
        #Insert network measures list
        assign(paste("meas.",name[i],sep=""),netMeas)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
        
    #Compute summary statistics
    summ.table <- function (data, n)
    {
        stats <- list()
        stats$mean <- rowMeans(data, na.rm = TRUE)
        stats$stdev <- apply(data, 1, sd, na.rm = TRUE)
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
    
    #Insert proportion remaining and iterations
    if(type == "node")
    {bootlist$prop <- prop}
    
    bootlist$iter <- iter
    
    bootlist$type <- type
    
    class(bootlist) <- "bootSemNeT"
    
    return(bootlist)
}
#----