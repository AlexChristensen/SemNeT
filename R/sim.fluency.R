#' Simulates a verbal fluency binary response matrix
#' 
#' @description Simulates verbal fluency data based on the number of
#' nodes in the desired network. The summed total of
#' each response is simulated from a poisson distribution 
#' (see \code{\link{rpois}}), using frequencies from the
#' \code{\link[SemNeT]{animals.freq}} data. Using these
#' sums, participants responses are simulated with a probability of giving
#' a response as the total of the summed response over the number of participants.
#' 
#' @param nodes Numeric.
#' Number of nodes to simulate in data.
#' Defaults to \code{100}
#' 
#' @param cases Numeric.
#' Number of participants to simulate in data.
#' Defaults to \code{500}
#' 
#' @param random Boolean.
#' Should the frequencies be randomly sampled from?
#' Defaults to \code{FALSE}.
#' 
#' @return A binary matrix with \code{p} (participants) by \code{n} (nodes)
#' 
#' @examples
#' # Simulate data for 50 nodes and 200 participants
#' sim.fluency(nodes = 50, cases = 200)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats rpois
#' 
#' @export
#'
#Simulate Verbal Fluency Data----
# Updated 02.12.2020
sim.fluency <- function(nodes, cases, random = FALSE)
{
    # Defaults
    if(missing(nodes))
    {nodes <- 100}
    
    if(missing(cases))
    {cases <- 500}
    
    # Compute response frequency distribution
    ## Based on animals verbal fluency
    if(random)
    {samp.freq <- sample(SemNeT::animals.freq, size = nodes, replace = TRUE)
    }else{samp.freq <- SemNeT::animals.freq}
    
    sum.resp <- rpois(nodes, samp.freq) + 1
    
    # Compute proportions
    props <- sum.resp / (max(sum.resp) * 1.05)
    
    # Initialize response matrix
    resp.mat <- matrix(0, nrow = cases, ncol = nodes)
    
    for(i in 1:length(props))
    {
        # Endorse probability
        end.prob <- props[i]
        # Not endorse probability
        no.end.prob <- 1 - end.prob
        
        #Generate responses
        resp.mat[,i] <- sample(c(0,1), cases, replace = TRUE, prob = c(no.end.prob,end.prob))
    }
    
    # Make sure there are no zeros in response matrix
    if(any(colSums(resp.mat)==0))
    {
        # Target columns
        target <- which(colSums(resp.mat)==0)
        
        # Add a response
        for(i in 1:length(target))
        {resp.mat[sample(cases, 1),target[i]] <- 1}
    }
    
    return(resp.mat)
}
#----