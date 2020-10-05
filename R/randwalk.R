#' Random Walk Simulation
#' 
#' @description Simulates random walks over two networks to examine the characteristics
#' of spontaneous spreading activation (see Kenett & Austerweil, 2016)
#' 
#' @param A Matrix or data frame.
#' Adjacency matrix of a semantic network
#' 
#' @param B Matrix or data frame.
#' A comparison adjacency matrix of a semantic network
#' 
#' @param reps Numeric.
#' Number of repetitions of increments in 10 steps.
#' Defaults to \code{20}
#' 
#' @param steps Numeric.
#' Number of random steps to begin with.
#' Defaults to \code{10}
#' 
#' @param iter Numeric.
#' Number of iterations for each random walk.
#' Defaults to \code{10000}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' 
#' @return A result matrix containing the means and standard deviations for
#' several measures as well as \emph{p}-values for a Mann-Whitney U test
#' 
#' @examples
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' two <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#' 
#' # Compute networks
#' net1 <- TMFG(cos1)
#' net2 <- TMFG(cos2)
#' \donttest{
#' # Run random walk analysis
#' rw.results <- randwalk(net1, net2, iter = 100, cores = 2)
#' }
#' \dontshow{rw.results <- randwalk(net1, net2, iter = 10, cores = 2)}
#' 
#' @references
#' Kenett, Y. N., & Austerweil, J. L. (2016).
#' Examining search processes in low and high creative individuals with random walks.
#' In \emph{Paper presented at the proceedings of the 38th annual meeting of the cognitive science society}. Austin, TX.
#' Retrieved from: \href{http://alab.psych.wisc.edu/papers/files/Kenett16CreativityRW.pdf}{http://alab.psych.wisc.edu/papers/files/Kenett16CreativityRW.pdf}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> and Yoed Kenett <yoedkenett@gmail.com>
#' 
#' @importFrom stats wilcox.test
#' 
#' @export
# Random Walks
# Updated 14.06.2020
randwalk <- function (A, B, reps = 20, steps = 10,
                      iter = 10000, cores)
{
    #Missing arguments
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    
    #Grab names of matrices
    nameA <- as.character(substitute(A))
    nameB <- as.character(substitute(B))
    
    #Get names from list
    if("$" %in% unlist(strsplit(nameA, split = "")))
    {
        nameA <- unlist(strsplit(nameA, split = "\\$"))
        nameA <- nameA[length(nameA)]
    }
    
    if("$" %in% unlist(strsplit(nameB, split = "")))
    {
        nameB <- unlist(strsplit(nameB, split = "\\$"))
        nameB <- nameB[length(nameB)]
    }
    
    #number of nodes
    nA <- ncol(A)
    nB <- ncol(B)
    
    #starting step
    start <- steps
    
    #binarize matrices
    A <- binarize(A)
    B <- binarize(B)
    diag(A) <- 0
    diag(B) <- 0
    
    #transition matrices
    TA <- matrix(0,nrow=nA,ncol=nA)
    TB <- matrix(0,nrow=nB,ncol=nB)
    
    degA <- colSums(A)
    degB <- colSums(B)
    
    for(i in 1:nA)
        for(j in 1:nA)
        {TA[i,j] <- A[i,j]/degA[i]}
    
    for(i in 1:nB)
        for(j in 1:nB)
        {TB[i,j] <- B[i,j]/degB[i]}
    
    #distance matrices
    DA <- distance(A)
    DB <- distance(B)
    
    # Random walk function
    rw <- function(steps, nA, nB, TA, TB, DA, DB)
    {
        #initialize matrices
        sim <- numeric(length = 2)
        sim5 <- sim
        uniq <- sim
        results <- vector("numeric",17)
        vnA <- numeric(length = steps)
        vnB <- vnA
        visit <- matrix(NA, nrow = 2, ncol = steps)
        
        permA <- sample(nA, 1)
        permB <- sample(nB, 1)
        
        startingNodeA <- permA
        startingNodeB <- permB
        
        vnA[1] <- permA
        vnB[1] <- permB
        
        for(k in 2:steps)
        {
            cdfA <- cumsum(TA[startingNodeA,])
            cdfB <- cumsum(TB[startingNodeB,])
            
            x <- runif(1)
            
            nA <- which(cdfA > x)[1]
            nB <- which(cdfB > x)[1]
            
            vnA[k] <- nA
            vnB[k] <- nB
            
            startingNodeA <- nA
            startingNodeB <- nB
        }
        
        uA <- unique(vnA)
        uB <- unique(vnB)
        
        a <- uA[length(uA)]
        b <- uB[length(uB)]
        
        simA <- exp(-DA[vnA[1],a])
        simB <- exp(-DB[vnB[1],b])
        
        s5ind <- 5
        su <- min(c(length(uA),length(uB)))
        if(s5ind > su)
        {s5ind <- su}
        
        g5ind <- 5
        if(g5ind > s5ind)
        {g5ind <- s5ind}
        
        simA5 <- exp(-DA[vnA[1],uA[s5ind]])
        simB5 <- exp(-DB[vnB[1],uB[s5ind]])
        
        visit[1,] <- vnA
        visit[2,] <- vnB
        
        sim[1] <- simA
        sim5[1] <- simA5
        uniq[1] <- length(uA)
        sim[2] <- simB
        sim5[2] <- simB5
        uniq[2] <- length(uB)
        
        # Initialize result list
        res <- list()
        res$sim <- sim
        res$sim5 <- sim5
        res$uniq <- uniq
        res$visit <- visit
        
        return(res)
    }
    
    # Initialize steps list
    steps <- seq(start, reps*10 , 10)
    
    step.list <- list()
    
    for(i in 1:length(steps))
    {step.list[[i]] <- as.list(rep(steps[i], iter))}
    
    # Initialize parallelization results
    pb.res <- vector("list", length = length(steps))
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("rw", "steps", "step.list", "nA", "nB", "reps",
                                                 "TA", "TB", "DA", "DB", "pb.res"), envir = environment())
    
    #Let user know analysis is starting
    message("Computing random walks...")
    
    for(i in 1:length(steps))
    {
        message(paste("Repetition ", i, " of ", reps, " (", steps[i], " steps)", sep = ""))
        
        
        pb.res[[i]] <- pbapply::pblapply(step.list[[i]], function(x){rw(x, nA, nB, TA, TB, DA, DB)})
    }
    
    parallel::stopCluster(cl)
    
    # Initialize result matrix
    results <- matrix(NA, nrow = reps, ncol = 17)
    
    colnames(results) <- c("Steps",
                           paste("M.uniq",nameA,sep="."), paste("SD.uniq",nameA,sep="."),
                           paste("M.sim",nameA,sep="."), paste("SD.sim",nameA,sep="."),
                           paste("M.sim5",nameA,sep="."), paste("SD.sim5",nameA,sep="."),
                           paste("M.uniq",nameB,sep="."), paste("SD.uniq",nameB,sep="."),
                           paste("M.sim",nameB,sep="."), paste("SD.sim",nameB,sep="."),
                           paste("M.sim5",nameB,sep="."), paste("SD.sim5",nameB,sep="."),
                           "pu", "ps", "ps5", "g5ind")
    
    # Organized into matrices
    for(i in 1:reps)
    {
        sim <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$sim
            }),
            rbind
        ))
        
        sim5 <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$sim5
            }),
            rbind
        ))
        
        uniq <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$uniq
            }),
            rbind
        ))
        
        #p-values
        pu <- suppressWarnings(wilcox.test(uniq[,1],uniq[,2])$p.value)
        ps <- suppressWarnings(wilcox.test(sim[,1],sim[,2])$p.value)
        ps5 <- suppressWarnings(wilcox.test(sim5[,1],sim5[,2])$p.value)
        
        #results
        results[i,1] <- steps[i]
        results[i,2] <- mean(uniq[,1])
        results[i,3] <- sd(uniq[,1])
        results[i,4] <- mean(sim[,1])
        results[i,5] <- sd(sim[,1])
        results[i,6] <- mean(sim5[,1])
        results[i,7] <- sd(sim5[,1])
        results[i,8] <- mean(uniq[,2])
        results[i,9] <- sd(uniq[,2])
        results[i,10] <- mean(sim[,2])
        results[i,11] <- sd(sim[,2])
        results[i,12] <- mean(sim5[,2])
        results[i,13] <- sd(sim5[,2])
        results[i,14] <- pu
        results[i,15] <- ps
        results[i,16] <- ps5
        results[i,17] <- 5
    }
    
    # Get directions
    ## pu
    dir.pu <- ifelse(results[,"pu"] < .05,
                     ifelse(results[,2] > results[,8],
                            paste(nameA, ">", nameB),
                            paste(nameB, ">", nameA)),
                     "n.s.")
    
    ## ps
    dir.ps <- ifelse(results[,"ps"] < .05,
                     ifelse(results[,4] > results[,10],
                            paste(nameA, ">", nameB),
                            paste(nameB, ">", nameA)),
                     "n.s.")
    
    ## ps5
    dir.ps5 <- ifelse(results[,"ps5"] < .05,
                      ifelse(results[,6] > results[,12],
                             paste(nameA, ">", nameB),
                             paste(nameB, ">", nameA)),
                      "n.s.")
    
    # Change p-values
    short.results <- round(results[,c("Steps","pu","ps","ps5")], 3)
    short.results <- ifelse(short.results < .001, "< .001", short.results)
    
    # Combine p-values and effect directions
    short.results <- cbind(short.results[,1:2], dir.pu,
                           short.results[,3], dir.ps,
                           short.results[,4], dir.ps5)
    
    # Rename columns
    colnames(short.results) <- c("Steps",
                                 "Unique Nodes (p-value)", "Direction",
                                 "Similarity (p-value)", "Direction",
                                 "5-step Similarlity (p-value)", "Direction")
    # Convert to data frame
    short.results <- as.data.frame(short.results)
    
    # Result list
    res <- list()
    res$long <- results
    res$short <- short.results
    
    return(res)
}
#----
