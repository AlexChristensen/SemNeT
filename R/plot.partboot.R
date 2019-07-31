#' Plot for partboot
#' 
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' 
#' @param ... Object(s) from \code{\link[SemNeT]{partboot}}
#' 
#' @param groups Character.
#' Labels for groups in the order they were entered
#' in \code{\link[SemNeT]{partboot}} (e.g.,
#' \code{data} = first,
#' \code{paired} = second)
#'
#' @param measures Character.
#' Measures to be plotted
#' 
#' @return Returns plots for the specified measures
#' 
#' @examples
#' # Finalize rmatA
#' finalCmat <- SemNetCleaner::finalize(SemNetCleaner::convmat)
#' # Finalize rmatB
#' finalRmat <- SemNetCleaner::finalize(SemNetCleaner::rmat)
#'
#' # Equate rmatA and rmatB
#' eq1 <- SemNetCleaner::equate(finalCmat,finalRmat)
#' 
#' # Obtain respective equated response matrices
#' eqCmat <- eq1$rmatA
#' eqRmat <- eq1$rmatB
#' 
#' \dontrun{
#' 
#' # Run partial bootstrap networks
#' results <- partboot(data = eqCmat, paired = eqRmat,
#' percent = .50, iter = 1000, sim = "cosine", cores = 4)
#' 
#' # Plot
#' plot(results, groups = c("eqCmat","eqRmat"))
#' 
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Plot: Partial Bootstrapped Semantic Network Analysis----
plot.partboot <- function (..., groups = NULL, measures = c("ASPL","CC","Q"))
{
    #Obtain ... in a list
    input <- list(...)
    
    #Number of input
    len <- length(input)
    
    #Identify if input is paired
    paired <- vector("logical", length = len)
    for(i in 1:len)
    {
        if(length(grep("paired",names(input[[i]])))!=0)
        {paired[i] <- TRUE
        }else{paired[i] <- FALSE}
    }
    
    #Error if single sample results are mixed with
    if(all(paired))
    {paired <- 2 # used in 'org.plot'
    }else if(all(!paired))
    {paired <- 1 # used in 'org.plot'
    }else{stop("Single samples are mixed with paired samples")}
    
    #Missing arguments
    if(missing(measures))
    {measures <- c("ASPL","CC","Q")
    }else{measures <- match.arg(measures,several.ok=TRUE)}
    
    #Plots
    
    plot <- list()
    
    if("ASPL" %in% measures)
    {plot$aspl <- org.plot(input = input, paired = paired,
                           len = len, groups = groups, netmeas = "ASPL")}
    
    if("CC" %in% measures)
    {plot$cc <- org.plot(input = input, paired = paired,
                         len = len, groups = groups, netmeas = "CC")}
    
    if("Q" %in% measures)
    {plot$q <- org.plot(input = input, paired = paired,
                        len = len, groups = groups, netmeas = "Q")}
    
    return(plot)
}
#----