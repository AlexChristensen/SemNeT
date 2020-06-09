#' Plot for \link[SemNeT]{bootSemNeT}
#' 
#' @description Plots output from \link[SemNeT]{bootSemNeT}
#' 
#' @param ... Object(s) from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param groups Character.
#' Labels for groups in the order they were entered
#' in \code{\link[SemNeT]{bootSemNeT}}
#'
#' @param measures Character.
#' Measures to be plotted
#' 
#' @return Returns plots for the specified measures
#' 
#' @examples
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' one.result <- bootSemNeT(one, prop = .50, iter = 1000,
#' sim = "cosine", cores = 2, type = "node", method = "TMFG")
#' }
#' # Plot
#' plot(one.result, groups = c("One"))
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Plot: Partial Bootstrapped Semantic Network Analysis----
plot.bootSemNeT <- function (..., groups = NULL, measures = c("ASPL","CC","Q"))
{
    #Obtain ... in a list
    input <- list(...)
    
    #Check for 'partboot' object
    if(all(unlist(lapply(input, class)) != "bootSemNeT"))
    {stop("Object input into 'bootSemNeT.obj' is not a 'bootSemNeT' object")}
    
    #Number of input
    len <- length(input)
    
    #Get names of networks
    name <- unique(gsub("Summ","",gsub("Meas","",names(input[[1]]))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Missing arguments
    if(missing(measures))
    {measures <- c("ASPL","CC","Q")
    }else{measures <- match.arg(measures,several.ok=TRUE)}
    
    #Plots
    
    plot <- list()
    
    if("ASPL" %in% measures)
    {plot$aspl <- org.plot(input = input, len = len, name = name,
                           groups = groups, netmeas = "ASPL")}
    
    if("CC" %in% measures)
    {plot$cc <- org.plot(input = input, len = len, name = name,
                         groups = groups, netmeas = "CC")}
    
    if("Q" %in% measures)
    {plot$q <- org.plot(input = input, len = len, name = name,
                        groups = groups, netmeas = "Q")}
    
    return(plot)
}
#----