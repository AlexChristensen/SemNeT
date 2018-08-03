#' Partial Bootstrapped Semantic Network Analysis
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' @param data Cleaned response matrix
#' @param paired Should bootstrapped nodes be paired?
#' Defaults to NULL.
#' Input a matrix, data frame or list containing another sample
#' @param weighted Should weighted ASPL and CC be used?
#' Defaults to FALSE.
#' Set to TRUE for weighted ASPL and CC
#' @param n Number of nodes for bootstrap.
#' Defaults to round(ncol(data)/2,0) (i.g., 50\% of nodes)
#' @param iter Number of iterations in bootstrap.
#' Defaults to 1000
#' @param corr Association method to use.
#' Defaults to "cosine"
#' @param cores Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maxmimum amount of cores on your computer
#' @param seeds Seeds used in previous run.
#' Defaults to NULL.
#' Input a vector from previous run to replicate analyses
#' @return Returns a list that includes the original semantic network measures (origmeas; ASPL, CC, Q, S),
#' the bootstrapped semantic network measures (bootmeas),
#' and Seeds that can be used to replicate analysis
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
#' 
#' \donttest{
#' results <- partboot(eqCmat, eqRmat, iter = 10, corr = "cosine", cores = 4)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom stats cor runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach %dopar%
#' @export
#Partial Bootstrapped Semantic Network Analysis----
partboot <- function (data, paired = NULL, n, weighted = FALSE,
                      iter = 1000, corr = c("cor","cosine"),
                      cores, seeds = NULL)
{
    if(missing(n))
    {n <- round((ncol(data)/2),0)
    }else{n <- round(n,0)}
    
    if(missing(corr))
    {corr <- "cosine"
    }else{corr <- match.arg(corr)}
    
    if(corr=="cor")
    {
        cormat <- cor(data)
        if(!is.null(paired))
        {cormatP <- cor(paired)}
    }else{
        cormat <- cosine(as.matrix(data))
        if(!is.null(paired))
        {cormatP <- cosine(as.matrix(paired))}
    }
    
    if(is.null(seeds))
    {Seeds <- vector(mode="numeric",length=iter)
    }else{
        seeds<-as.vector(seeds)
        Seeds <- seeds
        iter <- length(seeds)
    }
    
    full <- ncol(data)
    
    sampslist<-list() #initialize sample list
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    sampslist<-foreach::foreach(i=1:iter,
                                .packages = c("NetworkToolbox","SemNeT")
    )%dopar%
    {
        samps <- list()
        
        f<-round(runif(i,min=1,max=1000000),0)
        if(is.null(seeds))
        {
            Seed <- sample(f,1)
            set.seed(Seed)
        }else{Seed <- seeds[i]
        set.seed(seeds[i])
        }
        
        rand <- sample(1:full,n,replace=FALSE)
        
        mat <- data[,rand]
        
        if(corr=="cor")
        {cmat <- cor(mat)
        }else{cmat <- cosine(mat,.01)}
        
        net <- NetworkToolbox::TMFG(cmat)$A
        
        if(!is.null(paired))
        {
            matP <- paired[,rand]
            
            if(corr=="cor")
            {cmatP <- cor(matP)
            }else{cmatP <- cosine(matP,.01)}
            
            netP <- NetworkToolbox::TMFG(cmatP)$A
        }
        
        samps$data<-c(suppressWarnings(semnetmeas(net,iter=10,weighted=weighted)),Seed,rand)
        if(!is.null(paired))
        {samps$paired<-c(suppressWarnings(semnetmeas(netP,iter=10,weighted=weighted)),Seed,rand)}
        
        return(samps)
    }
    parallel::stopCluster(cl)
    
    tru<-suppressWarnings(semnetmeas(NetworkToolbox::TMFG(cormat)$A,weighted=weighted))
    
    if(!is.null(paired))
    {truP<-suppressWarnings(semnetmeas(NetworkToolbox::TMFG(cormatP)$A,weighted=weighted))}
    
    metrics <- matrix(0,nrow=iter,ncol=7)
    if(!is.null(paired))
    {metricsP <- matrix(0,nrow=iter,ncol=7)}
    
    Seeds <- vector(mode="numeric",length=iter)
    removed <- list()
    
    for(i in 1:length(sampslist))
    {
        metrics[i,] <- sampslist[[i]]$data[1:7]
        
        if(!is.null(paired))
        {
            metrics[i,] <- sampslist[[i]]$data[1:7]
            metricsP[i,] <- sampslist[[i]]$paired[1:7]
        }
        
        Seeds[i] <- sampslist[[i]]$data[8]
        removed$nodes[[i]] <- sampslist[[i]]$data[9:(n+8)]
    }
    
    metrics<-as.data.frame(metrics)
    colnames(metrics)<-c("ASPL","CC","Q","S","randASPL","randCC","MNS")
    
    if(!is.null(paired))
    {
        metricsP<-as.data.frame(metricsP)
        colnames(metricsP)<-c("ASPL","CC","Q","S","randASPL","randCC","MNS")
    }
    
    stat.table <- data.frame(0, nrow = 5, ncol = 5)
    
    if(!is.null(paired))
    {stat.tableP <- data.frame(0, nrow = 5, ncol = 5)}
    
    stattable <- function (data, n)
    {
        stats <- list()
        stats$mean <- mean(data)
        stats$stdev <- sd(data)
        stats$se <- stats$stdev/sqrt(n)
        stats$lower <- stats$mean - (1.96 * stats$se)
        stats$upper <- stats$mean + (1.96 * stats$se)
        
        return(stats)
    }
    
    for(i in 1:5)
    {
        stat <- stattable(metrics[,i], iter)
        
        stat.table[i,1] <- stat$mean
        stat.table[i,2] <- stat$stdev
        stat.table[i,3] <- stat$se
        stat.table[i,4] <- stat$lower
        stat.table[i,5] <- stat$upper
        
        if(!is.null(paired))
        {
            stat <- stattable(metricsP[,i], iter)
            
            stat.tableP[i,1] <- stat$mean
            stat.tableP[i,2] <- stat$stdev
            stat.tableP[i,3] <- stat$se
            stat.tableP[i,4] <- stat$lower
            stat.tableP[i,5] <- stat$upper
        }
    }
    
    colnames(stat.table) <- c("mean","sd","se","lower","upper")
    row.names(stat.table) <- c("ASPL","CC","Q","S","MNS")
    if(!is.null(paired))
    {
        colnames(stat.tableP) <- c("mean","sd","se","lower","upper")
        row.names(stat.tableP) <- c("ASPL","CC","Q","S","MNS")
    }
    
    bootlist <- list()
    
    if(is.null(paired))
    {
        bootlist$origmeas <- tru
        bootlist$bootmeas <- metrics
        bootlist$statData <- stat.table
        bootlist$nodesRemoved <- removed
        bootlist$Seeds <- Seeds
        
    }else(!is.null(paired))
    {
        bootlist$origDataMeas <- tru
        bootlist$bootDataMeas <- metrics
        bootlist$statData <- stat.table
        bootlist$nodesRemoved <- removed
        bootlist$origPairedMeas <- truP
        bootlist$bootPairedMeas <- metricsP
        bootlist$statPaired <- stat.tableP
        bootlist$nodesRemoved <- removed$nodes
        bootlist$Seeds <- Seeds
    }
    
    class(bootlist) <- "partboot"
    
    return(bootlist)
}
#----