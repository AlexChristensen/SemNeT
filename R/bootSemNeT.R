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
#' @param methodArgs List.
#' A list of additional arguments for the network estimation function.
#' See links in argument \code{method} for additional arguments (see also Examples)
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
#' argument, where columns are the semantic network measures
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
#' one.result <- bootSemNeT(one, prop = .50, iter = 100,
#' sim = "cosine", cores = 2, method = "TMFG", type = "node")
#' 
#' # Run bootstrap case-drop networks
#' ## Includes additional equating argument: minCase
#' one.result <- bootSemNeT(one, iter = 100, sim = "cosine",
#' cores = 2, method = "TMFG", type = "case", methodArgs = list(minCase = 2))
#' 
#' }
#' # Bootstrap case-wise networks
#' ## Get openness data
#' low <- open.clean[which(open.group == "Low"),]
#' high <- open.clean[which(open.group == "High"),]
#' \donttest{
#' ## Run
#' ### Inlcudes additional NRW argument: threshold
#' open <- bootSemNeT(low, high, iter = 100, cores = 2, method = "NRW", type = "case",
#' methodArgs = list(type = "num", threshold = 3))
#' 
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats sd cor dist pnorm runif
#' 
#' @export
# Bootstrapped Semantic Network Analysis----
# Updated 02.12.2020
bootSemNeT <- function (..., method = c("CN", "NRW", "PF", "TMFG"),
                        methodArgs = list(),
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
    
    #Assign network function
    NET.FUNC <- switch(method,
                       "TMFG" = TMFG,
                       "CN" = CN,
                       "NRW" = NRW,
                       "PF" = PF)
    
    #Check for missing arguments in function
    form.args <- methods::formalArgs(NET.FUNC)[-which(methods::formalArgs(NET.FUNC) == "data")]
    
    #Get arguments
    if(any(!form.args %in% names(methodArgs))){
        
        #Find which arguments are needed
        need.args <- form.args[which(!form.args %in% names(methodArgs))]
        
        #Get default arguments for methods
        if(method == "CN"){
            
            if("window" %in% need.args){methodArgs$window <- 2}
            if("alpha" %in% need.args){methodArgs$alpha <- .05}
            if("enrich" %in% need.args){methodArgs$enrich <- FALSE}
            
        }else if(method == "NRW"){
            
            if("type" %in% need.args){methodArgs$type <- "num"}
            if("threshold" %in% need.args){
                
                if(methodArgs$type == "num"){
                    
                    methodArgs$threshold <- 3
                    
                }else if(methodArgs$type == "prop"){
                    
                    methodArgs$threshold <- .05
                    
                }
            
            }
            
        }else if(method == "TMFG"){
            
            if("depend" %in% need.args){methodArgs$depend <- FALSE}
            
        }
    }
    
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
                
                #Randomly sample nodes
                rand <- sample(1:full, (full*prop), replace=FALSE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[,rand]
                
            }else if(type == "case")
            {
                #Randomly sample nodes
                rand <- sample(1:nrow(get(name[i], envir = environment())),
                               nrow(get(name[i], envir = environment())),
                               replace=TRUE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[rand,]
            }
            
            if(count == iter)
            {break}
        }
        
        #Insert data list
        assign(paste("dl.",name[i],sep=""),new, envir = environment())
    }
    
    #Let user know data generation is finished
    message("done\n")
    
    ##################
    #### EQUATING ####
    ##################
    
    #Check for appropriate conditions
    if(method == "TMFG" && type == "case")
    {
        #Let user know the samples are being equated
        message("Equating samples...", appendLF = FALSE)
        
        # If missing minCase
        if("minCase" %in% names(methodArgs))
        {minCase <- methodArgs$minCase
        }else{minCase <- 2}
        
        # Finalize each sample
        for(i in 1:length(name))
        {
            assign(
                paste("dl.",name[i],sep=""),
                
                lapply(get(paste("dl.",name[i],sep=""), envir = environment()),
                       FUN = finalize,
                       minCase = minCase),
                
                envir = environment()
            )
        }
        
        if(length(name) > 1)
        {
            #Get data to equate into a single list
            eq.dat <- mget(paste("dl.",name,sep=""), envir = environment())
            
            #Initialize equating list
            eq <- vector("list", length = iter)
            
            for(i in 1:iter)
            {eq[[i]] <- equateShiny(lapply(eq.dat, function(x){x[[i]]}))}
            
            for(i in 1:length(name))
            {
                assign(
                    paste("dl.",name[i],sep=""),
                    unlist(lapply(eq, function(x){x[paste("dl.",name[i],sep="")]}), recursive = FALSE),
                    envir = environment()
                )
            }
        }
        
        #Let user know data generation is finished
        message("done\n")
    }

    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    if(method == "TMFG")
    {
        #Let user know simliarity is being computed
        message("Computing similarity measures...\n", appendLF = FALSE)
        
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = c(),#paste("dl.",name,sep=""),
                                envir = environment())
        
        for(i in 1:length(name))
        {
            #Compute similarity
            newSim <- pbapply::pblapply(X = get(paste("dl.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = similarity,
                                        method = sim)
            
            #Insert similarity list
            assign(paste("sim.",name[i],sep=""),newSim, envir = environment())
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
    
    #Let user know networks are being computed
    message("Estimating networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = #c(paste("sim.",name,sep=""),
                                c("NET.FUNC"),
                            envir = environment())
    
    if(method == "TMFG")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        depend = methodArgs$depend)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "CN")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        window = methodArgs$window,
                                        alpha = methodArgs$alpha)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "NRW")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        type = methodArgs$type,
                                        threshold = methodArgs$threshold)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "PF")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    ##############################
    #### COMPUTING STATISTICS ####
    ##############################
    
    #Let user know network measures are being computed
    message("Computing network measures...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c(),#paste("Semnet.",name[i],sep=""),
                                envir = environment())
    
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
        bootlist[[paste(name[i],"Net",sep="")]] <- get(paste("Semnet.",name[i],sep=""), envir = environment())
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