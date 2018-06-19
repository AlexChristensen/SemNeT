#' Random Walk Simulation
#' @description Simulates random walks over two networks to examine the characteristics
#' of spontaneous spreading activation
#' @param A Adjacency matrix of a semantic network
#' @param B A comparison adjacency matrix of a semantic network
#' @param reps Number of repetitions of increments in 10 steps.
#' Defaults to 20
#' @param steps Number of random steps to begin with.
#' Defaults to 10
#' @param iter Number of iterations for each random walk
#' @return A result matrix containing the means and standard deviations for
#' several measures as well as p-values for a Mann-Whitney U test
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
#' rw.results <- randwalk(eqCmat,eqRmat)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom stats wilcox.test
#' @export
#Random Walks
randwalk <- function (A, B, reps = 20, 
                      steps = 10, iter = 10000)
{
    #number of nodes
    n <- ncol(A)
    #initialize matrices
    sim <- matrix(0,nrow=iter,ncol=2)
    sim5 <- matrix(0,nrow=iter,ncol=2)
    uniq <- matrix(0,nrow=iter,ncol=2)
    results <- matrix(0,nrow=20,ncol=17)
    
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
    
    #progress bar
    pb <- txtProgressBar(max=reps, style = 3)
    
    #randomw walk
    for(i in 1:reps)
    {
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
        results[i,1] <- steps
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
        results[i,17] <- g5ind
        
        steps <- steps + 10
        rm(perm)
        
        setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    return(results)
}
#----