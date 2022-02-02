# Methods:

# Plot bootSemNeT
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
    name <- unique(gsub("Net", "", gsub("Summ","",gsub("Meas","",names(input[[1]])))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Remove resampling and covariates
    name <- na.omit(gsub("resampling",NA,gsub("covariates",NA,name)))
    attr(name, "na.action") <- NULL
    
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
plot.compareShiny <- function (x, ...)
{
    for(i in 1:length(x$datalist))
    {
        #Network specific arguments
        ##Networks
        #if(x$config == "mds")
        #{x$qgraph.args$qgraph_net <- x$layouts[[i]]
        #}else{
        x$qgraph.args$input <- x$layouts[[i]]#}
        ##Network title and labels
        x$qgraph.args$title <- x$title[[i]]
        x$qgraph.args$labels <- x$labs[[i]]
        
        #Generate plot
        #ifelse(x$config == "mds",
               #do.call(networktools::MDSnet, args = x$qgraph.args),
               do.call(qgraph::qgraph, args = x$qgraph.args)#)
    }
}

# Plot Shiny animate
plot.animateShiny <- function (x, ...)
{
    animation::ani.replay(x, ...)
}
