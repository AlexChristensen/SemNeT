#' Permutation Test for Network Measures
#' 
#' @description Computes a permutation test to determine whether
#' there are difference in centrality and global network measures
#' 
#' @param sample1 Matrix or data frame.
#' Sample to be compared with \code{sample2}
#' 
#' @param sample2 Matrix or data frame.
#' Sample to be compared with \code{sample1}
#' 
#' @param iter Numeric.
#' Number of iterations to perform.
#' Defaults to \code{1000}
#' 
#' @param method Character.
#' Network estimation method to use.
#' Current options include:
#' 
#' \itemize{
#' \item{\code{\link[SemNeT]{TMFG}}}
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
#' @param sim Character.
#' Similarity measure to use (\code{\link[SemNeT]{TMFG}} only).
#' Defaults to \code{"cosine"}.
#' See \code{\link[SemNeT]{similarity}} for other options
#' 
#' @param minCase Numeric.
#' Minimum number of cases to produce a response (\code{\link[SemNeT]{TMFG}} only).
#' Defaults to \code{2}
#' 
#' @param measure Character.
#' Network measure to be compared in the permutation test
#' 
#' @param weighted Boolean.
#' Should weighted network measures be used?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted network measures
#' 
#' @param alternative Character.
#' Alternative hypothesis test to perform.
#' Defaults to \code{"two.tailed"}
#' 
#' @param groups Character.
#' Names of samples.
#' Defaults to \code{NULL}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} / 2 number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @param prev.perm \code{network.permutation} class object.
#' An object of previously performed permutation test. The
#' networks generated in the previous permutation will be
#' used to compute other network measures. This saves time
#' when computing multiple permutation tests
#' 
#' @param ... Additional arguments for the network estimation methods
#' (see the \code{method} argument for links to each method)
#' 
#' @return Returns a list containing two objects:
#' 
#' \item{result}{The results of the permutation test. For centrality measures,
#' this is a matrix where the rows represent each node and the columns are
#' the observed values of the centrality measure for \code{sample1}, \code{sample2},
#' and the \emph{p}-value from the permutation test. For global network measures,
#' this is a vector with the observed values of the global network measure for
#' \code{sample1}, \code{sample2}, and the \emph{p}-value from the permutation test.}
#' 
#' \item{networks}{A list containing two lists: \code{network1} and \code{network2}.
#' The network lists correspond to the networks generated in the permutation test
#' for \code{sample1} and \code{sample2}, respectively. This output is used primarily
#' for the computation of other network measures using the same datasets
#' (see \code{prev.perm} explanation)}
#'
#' @examples
#' # Generate samples
#' random1 <- sim.fluency(100)
#' random2 <- sim.fluency(100)
#' 
#' \donttest{# ASPL
#' perm.TMFG.ASPL <- permSemNeT(random1, random2, iter = 100, method = "TMFG",
#' measure = "ASPL", cores = 2)
#' 
#' # CC
#' perm.TMFG.CC <- permSemNeT(prev.perm = perm.TMFG.ASPL, measure = "CC", cores = 2)
#' 
#' # Q
#' perm.TMFG.Q <- permSemNeT(prev.perm = perm.TMFG.ASPL, measure = "Q", cores = 2)}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats lm
#' 
#' @noRd
# Semantic Network Permutation Test----
# 15.09.2020
permSemNeT <- function(sample1 = NULL, sample2 = NULL, iter,
                       method = c("CN", "NRW", "PF", "TMFG"), sim = NULL, minCase = NULL,
                       measure = c("ASPL", "CC", "Q"), weighted = FALSE,
                       alternative = c("less", "greater", "two.tailed"),
                       groups = NULL,
                       cores, prev.perm = NULL, ...)
{
  #### Argument check ####
  
  if(missing(iter))
  {iter <- 1000
  }else{iter <- iter}
  
  if(is.null(prev.perm))
  {
    if(missing(method))
    {stop("Argument 'method' must be defined")
    }else{method <- match.arg(method)}
    
    if(is.null(sample1) || is.null(sample2))
    {
      stop("Arguments 'sample1' and 'sample2' must be input or
           argument 'prev.perm' must have a previous result")
    }
  }else if(class(prev.perm) != "permSemNeT")
  {
    stop("Object input into argument 'prev.perm' is not 
         a 'permSemNeT' class object")
  }else{method <- prev.perm$method}
  
  if(missing(alternative))
  {alternative <- "two.tailed"
  }else{alternative <- match.arg(alternative)}
  
  if(missing(cores))
  {cores <- floor(parallel::detectCores() / 2)}
  
  args <- list(...)
  
  #### Argument check ####
  
  #### Arguments for methods ####
  
  if(method == "CN")
  {
    if(!"window" %in% names(args))
    {args$window <- 2}
    
    if(!"alpha" %in% names(args))
    {args$alpha <- .05}
    
    if(!"enrich" %in% names(args))
    {args$enrich <- FALSE}
  }
  
  if(method == "NRW")
  {
    if(!"threshold" %in% names(args))
    {args$threshold <- 3}
  }
  
  #### Arguments for methods ####
  
  #### Statistic function ####
  
  get_statistic <- function(meas1, meas2) {meas1 - meas2}
  
  #### Statistic function ####
  
  # Check for previous permutation test
  if(!is.null(prev.perm))
  {
    # Obtain networks
    net.list1 <- prev.perm$networks$network1
    net.list2 <- prev.perm$networks$network2
    
    # Obtain names of samples
    sample1.name <- names(prev.perm$result)[1]
    sample2.name <- names(prev.perm$result)[2]
    
  }else{
    
    # Obtain names of samples
    if(is.null(groups))
    {
      sample1.name <- paste(deparse(substitute(sample1)))
      sample2.name <- paste(deparse(substitute(sample2)))
    }else{
      sample1.name <- groups[1]
      sample2.name <- groups[2]
    }
    
    # Combine samples
    comb.sample <- rbind(sample1, sample2)
    
    # Initialize data permutation list
    data.list1 <- vector("list", length = iter)
    data.list2 <- data.list1
    
    data.list1[[1]] <- sample1
    data.list2[[1]] <- sample2

    # Message to user
    message("Generating permutated samples...", appendLF = FALSE)
    
    # Generate permutated samples
    for(i in 2:iter)
    {
      # Randomly draw sample 1
      k <- sample(1:nrow(comb.sample), nrow(sample1))
      
      # New sample 1
      data.list1[[i]] <- comb.sample[k,]
      
      # New sample 2
      data.list2[[i]] <- comb.sample[-k,]
    }
    
    # Message to user
    message("done")
    
    # Check for TMFG
    if(method == "TMFG")
    {
      # Message to user
      message("Equating responses...", appendLF = FALSE)
      
      ind <- as.list(1:iter)
      
      sample.list <- lapply(ind, FUN = function(x, samp1, samp2, minCase){
        tmfg_setup(samp1[[x]], samp2[[x]], minCase = minCase)
      }, samp1 = data.list1, samp2 = data.list2, minCase = minCase)
      
      # Put data back into their respective lists
      eq.list1 <- lapply(sample.list, function(x){x$`samp1[[x]]`})
      eq.list2 <- lapply(sample.list, function(x){x$`samp2[[x]]`})
      
      # Message to user
      message("done")
      
      # Message to user
      message("Computing similarity matrices...\n", appendLF = FALSE)
      
      # Parallel processing
      cl <- parallel::makeCluster(cores)
      
      # Export data lists
      parallel::clusterExport(cl = cl, varlist = c("sim"), envir = environment())
      
      # Compute association metric
      data.list1 <- pbapply::pblapply(eq.list1, cl = cl, FUN = similarity, method = sim)
      data.list2 <- pbapply::pblapply(eq.list2, cl = cl, FUN = similarity, method = sim)
      
      # Stop cluster
      parallel::stopCluster(cl)
      
    }
    
    #### Estimate networks ####
    message("Estimating networks...\n", appendLF = FALSE)
    
    # Parallel processing
    cl <- parallel::makeCluster(cores)
    
    # Export data lists
    parallel::clusterExport(cl = cl, varlist = c("args"), envir = environment())
    
    # Compute networks
    if(method == "TMFG")
    {
      net.list1 <- pbapply::pblapply(data.list1, cl = cl, FUN = TMFG)
      
      net.list2 <- pbapply::pblapply(data.list2, cl = cl, FUN = TMFG)
      
    }else if(method == "CN")
    {
      net.list1 <- pbapply::pblapply(data.list1, cl = cl,
                                     FUN = CN, window = args$window,
                                     alpha = args$alpha,
                                     enrich = args$enrich)
      
      net.list2 <- pbapply::pblapply(data.list2, cl = cl,
                                     FUN = CN, window = args$window,
                                     alpha = args$alpha,
                                     enrich <- args$enrich)
    }else if(method == "NRW")
    {
      net.list1 <- pbapply::pblapply(data.list1, cl = cl,
                                     FUN = NRW, threshold = args$thresold)
      
      net.list2 <- pbapply::pblapply(data.list2, cl = cl,
                                     FUN = NRW, window = args$threshold)
    }else if(method == "PF")
    {
      net.list1 <- pbapply::pblapply(data.list1, cl = cl, FUN = PF)
      
      net.list2 <- pbapply::pblapply(data.list2, cl = cl, FUN = PF)
    }
    
    # Stop cluster
    parallel::stopCluster(cl)
    
  }
  
  #### Compute measures ####
  message("Computing network measures...\n", appendLF = FALSE)
  
  # Parallel processing
  cl <- parallel::makeCluster(cores)
  
  # Export network lists
  parallel::clusterExport(cl = cl, varlist = c(), envir = environment())
  
  #Compute measures
  if(measure == "ASPL")
  {
    stat.list1 <- pbapply::pbsapply(net.list1, cl = cl, FUN = ASPL)
    
    stat.list2 <- pbapply::pbsapply(net.list2, cl = cl, FUN = ASPL)
  }else if(measure == "CC")
  {
    stat.list1 <- pbapply::pbsapply(net.list1, cl = cl, FUN = CC)
    
    stat.list2 <- pbapply::pbsapply(net.list2, cl = cl, FUN = CC) 
  }else if(measure == "Q")
  {
    stat.list1 <- pbapply::pbsapply(net.list1, cl = cl, FUN = Q)
    
    stat.list2 <- pbapply::pbsapply(net.list2, cl = cl, FUN = Q)
  }
  
  #Stop cluster
  parallel::stopCluster(cl)
  
  #### Compute measures ####
  
  # Obtain number of nodes and edges
  nodes <- cbind(unlist(lapply(net.list1, ncol)), unlist(lapply(net.list2, ncol)))
  edges <- cbind(unlist(lapply(net.list1, function(x){
    X <- binarize(x)
    return(sum(X[lower.tri(X)]))
  })),
  unlist(lapply(net.list2, function(x){
    X <- binarize(x)
    return(sum(X[lower.tri(X)]))
  })))
  
  group1.vals <- cbind(stat.list1, nodes[,1], edges[,1])
  colnames(group1.vals) <- c("Measure", "Nodes", "Edges")
  group1.vals <- as.data.frame(group1.vals)
  reg1 <- lm(Measure ~ Nodes + Edges, data = group1.vals)
  
  group2.vals <- cbind(stat.list2, nodes[,2], edges[,2])
  colnames(group2.vals) <- c("Measure", "Nodes", "Edges")
  group2.vals <- as.data.frame(group2.vals)
  reg2 <- lm(Measure ~ Nodes + Edges, data = group2.vals)
  
  diff <- reg1$residuals - reg2$residuals
  diff0 <- reg1$residuals[1] - reg2$residuals[1]
  
  # Compute statistic
  diff0 <- stat.list1[1] - stat.list2[1]
  diff <- stat.list1 - stat.list2
  
  # Alternative test
  if(alternative == "greater")
  {p <- mean(diff >= diff0, na.rm = TRUE)
  }else if(alternative == "less")
  {p <- mean(diff <= diff0, na.rm = TRUE)
  }else{p <- mean(abs(diff) >= abs(diff0), na.rm = TRUE)}
  
  alt <- switch(alternative,
                "less" = paste(sample1.name, "<", sample2.name, sep = " "),
                "greater" = paste(sample1.name, ">", sample2.name, sep = " "),
                "two.tailed" = paste(sample1.name, "!=", sample2.name, sep = " "),
                )
  
  res <- c(stat.list1[1], stat.list2[1], p,  alt)
  
  names(res) <- c(sample1.name, sample2.name, "p-value", "Alternative")
  
  res <- as.data.frame(t(as.matrix(res)))
  row.names(res) <- measure
  
  res[[1]] <- round(as.numeric(res[[1]]), 2)
  res[[2]] <- round(as.numeric(res[[2]]), 2)
  res$`p-value` <- round(as.numeric(res$`p-value`), 3)
  
  # Result list
  res.list <- list()
  res.list$result <- res
  res.list$networks <- list(network1 = net.list1, network2 = net.list2)
  res.list$method <- method
  res.list$diff <- diff
  class(res.list) <- "permSemNeT"
  
  return(res.list)
}
#----