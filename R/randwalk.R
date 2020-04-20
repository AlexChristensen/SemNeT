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
#' @param short.res Boolean.
#' Should shorten results (p-values only) be produced?
#' Defaults to \code{TRUE}
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
#' # Compute networks using NetworkToolbox
#' net1 <- NetworkToolbox::TMFG(cos1)$A
#' net2 <- NetworkToolbox::TMFG(cos2)$A
#' \donttest{
#' # Run random walk analysis
#' rw.results <- randwalk(net1, net2, iter = 1000, cores = 2)
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
#Random Walks
randwalk <- function (A, B, reps = 20, steps = 10,
                      iter = 10000, short.res = TRUE, cores)
{
    #Missing arguments
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    
    #Grab names of matrices
    nameA <- as.character(substitute(A))
    nameB <- as.character(substitute(B))
    
    #number of nodes
    nA <- ncol(A)
    nB <- ncol(B)
    
    #starting steps
    start <- steps
    
    #binarize matrices
    A <- NetworkToolbox::binarize(A)
    B <- NetworkToolbox::binarize(B)
    
    #transition matrices
    TA <- matrix(0,nrow=nA,ncol=nA)
    TB <- matrix(0,nrow=nB,ncol=nB)
    
    degA <- NetworkToolbox::degree(A)
    degB <- NetworkToolbox::degree(B)
    
    for(i in 1:nA)
        for(j in 1:nA)
        {TA[i,j] <- A[i,j]/degA[i]}
    
    for(i in 1:nB)
        for(j in 1:nB)
        {TB[i,j] <- B[i,j]/degB[i]}
    
    #distance matrices
    DA <- NetworkToolbox::distance(A)
    DB <- NetworkToolbox::distance(B)
    
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
    steps <- seq(steps, steps*reps, 10)
    
    step.list <- list()
    
    for(i in 1:reps)
    {step.list[[i]] <- as.list(rep(steps[i], iter))}
    
    # Initialize parallelization results
    pb.res <- vector("list", length = reps)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("rw", "steps", "step.list", "nA", "nB", "reps",
                                                 "TA", "TB", "DA", "DB", "pb.res"), envir = environment())
    
    #Let user know analysis is starting
    message("Computing random walks...")
    
    for(i in 1:reps)
    {
        message(paste("Repetition ", i, " of ", reps, " (", steps[i], " steps)", sep = ""))
        
        
        pb.res[[i]] <- pbapply::pblapply(step.list[[i]], function(x){rw(x, nA, nB, TA, TB, DA, DB)})
    }
    
    parallel::stopCluster(cl)
    
    # Initialize result matrix
    results <- matrix(NA, nrow = reps, ncol = 17)
    
    colnames(results) <- c("Steps",paste("M.uniq",nameA,sep="."),
                           paste("SD.uniq",nameA,sep="."),
                           paste("M.sim",nameA,sep="."),
                           paste("SD.sim",nameA,sep="."),
                           paste("M.sim5",nameA,sep="."),
                           paste("SD.sim5",nameA,sep="."),
                           paste("M.uniq",nameB,sep="."),
                           paste("SD.uniq",nameB,sep="."),
                           paste("M.sim",nameB,sep="."),
                           paste("SD.sim",nameB,sep="."),
                           paste("M.sim5",nameB,sep="."),
                           paste("SD.sim5",nameB,sep="."),
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
    
    if(short.res)
    {results <- results[,c("Steps","pu","ps","ps5")]}
    
    
    return(results)
}
#----
