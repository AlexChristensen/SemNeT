#' Simulates a verbal fluency binary response matrix
#' 
#' @description Simulates verbal fluency data based on the number of
#' nodes in the desired network. Participants are 5 percent greater than
#' the maximum simulated fluency response given. The summed total of
#' each response is simulated from a gamma distribution with a
#' shape of .25 and rate of 1 (see \code{\link{pgamma}}). Using these
#' sums, participants responses are simulated with a probability of giving
#' a response as the total of the summed response over the number of participants.
#' 
#' @param nodes Numeric.
#' Number of nodes to simulate in data
#' 
#' @return A binary matrix with \code{p} (participants) by \code{n} (nodes)
#' 
#' @details Simulates data
#' 
#' @examples
#' # Simulate data for 50 nodes
#' sim.fluency(nodes = 50)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats rgamma
#' 
#' @export
#' 
#Simulate Verbal Fluency Data----
sim.fluency <- function(nodes)
{
    # Compute response frequency distribution
    sum.resp <- ceiling(rgamma(nodes, shape = .25, rate = 1)*100)
    
    parts <- max(sum.resp) + ceiling(nodes * .05)
    
    # Initialize response matrix
    resp.mat <- matrix(0, nrow = parts, ncol = nodes)
    
    for(i in 1:length(sum.resp))
    {
        # Endorse probability
        end.prob <- sum.resp[i]/parts
        # Not endorse probability
        no.end.prob <- 1 - end.prob
        
        #Generate responses
        resp.mat[,i] <- sample(c(0,1), parts, replace = TRUE, prob = c(no.end.prob,end.prob))
    }
    
    # Make sure there are no zeros
    if(any(colSums(resp.mat)==0))
    {
        # Target columns
        target <- which(colSums(resp.mat)==0)
        
        # Add a response
        for(i in 1:length(target))
        {resp.mat[sample(parts, 1),target] <- 1}
    }
    
    return(resp.mat)
}
#----