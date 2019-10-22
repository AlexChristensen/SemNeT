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
#' Number of iterations for each random walk
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
#' @importFrom foreach %dopar%
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
    nameB <- as.character(substitute(A))
    
    #number of nodes
    n <- ncol(A)
    
    #starting steps
    start <- steps
    
    #binarize matrices
    A <- NetworkToolbox::binarize(A)
    B <- NetworkToolbox::binarize(B)
    
    #transition matrices
    TA <- matrix(0,nrow=n,ncol=n)
    TB <- matrix(0,nrow=n,ncol=n)
    
    degA <- NetworkToolbox::degree(A)
    degB <- NetworkToolbox::degree(B)
    
    for(i in 1:n)
        for(j in 1:n)
        {
            TA[i,j] <- A[i,j]/degA[i]
            TB[i,j] <- B[i,j]/degB[i]
        }
    
    #distance matrices
    DA <- NetworkToolbox::distance(A)
    DB <- NetworkToolbox::distance(B)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    #Let user know analysis is starting
    message("Computing random walks...", appendLF = FALSE)
    
    results <- foreach::foreach(i=1:reps,
                               .combine = rbind,
                               .packages = c("NetworkToolbox","SemNeT")
                               ) %dopar%
        {
            
            #initialize matrices
            sim <- matrix(0,nrow=iter,ncol=2)
            sim5 <- matrix(0,nrow=iter,ncol=2)
            uniq <- matrix(0,nrow=iter,ncol=2)
            results <- vector("numeric",17)
            vnA <- vector("numeric",length=steps)
            vnB <- vector("numeric",length=steps)
            visitA <- matrix(0,nrow=iter,ncol=steps)
            visitB <- matrix(0,nrow=iter,ncol=steps)
            
            g5ind <- 5
            
            #random walk
            for(j in 1:iter)
            {
                perm <- sample(n,1)
                startingNodeA <- perm
                startingNodeB <- perm
                
                vnA[1] <- perm
                vnB[1] <- perm
                
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
                
                if(g5ind > s5ind)
                {g5ind <- s5ind}
                
                simA5 <- exp(-DA[vnA[1],uA[s5ind]])
                simB5 <- exp(-DB[vnB[1],uB[s5ind]])
                
                visitA[j,] <- vnA
                visitB[j,] <- vnB
                
                sim[j,1] <- simA
                sim5[j,1] <- simA5
                uniq[j,1] <- length(uA)
                sim[j,2] <- simB
                sim5[j,2] <- simB5
                uniq[j,2] <- length(uB)
            }
            
            #p-values
            pu <- suppressWarnings(wilcox.test(uniq[,1],uniq[,2])$p.value)
            ps <- suppressWarnings(wilcox.test(sim[,1],sim[,2])$p.value)
            ps5 <- suppressWarnings(wilcox.test(sim5[,1],sim5[,2])$p.value)
            
            #results
            results[1] <- steps
            results[2] <- mean(uniq[,1])
            results[3] <- sd(uniq[,1])
            results[4] <- mean(sim[,1])
            results[5] <- sd(sim[,1])
            results[6] <- mean(sim5[,1])
            results[7] <- sd(sim5[,1])
            results[8] <- mean(uniq[,2])
            results[9] <- sd(uniq[,2])
            results[10] <- mean(sim[,2])
            results[11] <- sd(sim[,2])
            results[12] <- mean(sim5[,2])
            results[13] <- sd(sim5[,2])
            results[14] <- pu
            results[15] <- ps
            results[16] <- ps5
            results[17] <- g5ind
            
            steps <- steps + 10
            rm(perm)
            
            return(results)
        }
    
    parallel::stopCluster(cl)
    
    #Let user know analysis is starting
    message("done", appendLF = TRUE)
    
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
    
    if(short.res)
    {results <- results[,c("Steps","pu","ps","ps5")]}
    
    
    return(results)
}
#----