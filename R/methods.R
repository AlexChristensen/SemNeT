# Methods:

# Plot bootSemNeT
plot.bootSemNeT <- function (..., groups = NULL, measures = c("ASPL","CC","Q"))
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

# Plot Shiny compare
plot.compareShiny <- function (res)
{
    for(i in 1:length(res$datalist))
    {
        #Network specific arguments
        ##Networks
        if(res$config == "mds")
        {res$qgraph.args$qgraph_net <- res$layouts[[i]]
        }else{res$qgraph.args$input <- res$layouts[[i]]}
        ##Network title and labels
        res$qgraph.args$title <- res$title[[i]]
        res$qgraph.args$labels <- res$labs[[i]]
        
        #Generate plot
        ifelse(res$config == "mds",
               do.call(networktools::MDSnet, args = res$qgraph.args),
               do.call(qgraph::qgraph, args = res$qgraph.args))
    }
}
