#' Plots Networks for Comparison
#' 
#' @description Uses \code{\link[qgraph]{qgraph}} and \code{\link[networktools]{MDSnet}}
#' to plot networks. Accepts any number of networks and will organize the plots
#' in the number of side-by-side columns using the heuristic of taking the square root of the number of 
#' input and rounding down to the nearest integer (i.e., \code{floor(sqrt(length(input)))}).
#' 
#' \strong{Examples}
#' \itemize{
#' \item{3 networks:}
#' {1 x 3}
#' \item{6 networks:}
#' {2 x 3}
#' \item{9 networks:}
#' {3 x 3}
#' }
#' 
#' @param ... Matrices or data frames of network adjacency matrices
#' 
#' @param title List.
#' Characters denoting titles of plots
#' 
#' @param config Character.
#' Defaults to \code{\link[networktools]{MDSnet}}.
#' See \code{\link[qgraph]{qgraph}} for more options
#' 
#' @param placement Character.
#' How should nodes be placed when comparing groups?
#' Defaults to \code{"default"}
#' 
#' \itemize{
#' \item{\code{"match"}}
#' {places nodes in the same position for all networks}
#' 
#' \item{\code{"default"}}
#' {places nodes in the default \code{config} positions} 
#' }
#' 
#' @param weighted Boolean.
#' Should networks be plotted with weights?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to plot networks with weights
#' corresponding to association strength. Often, unweighted
#' networks are more aesthetically representational of the
#' networks
#' 
#' @param qgraph.args List.
#' An argument list to be passed onto \code{\link[qgraph]{qgraph}}.
#' See \code{\link[qgraph]{qgraph}} for possible arguments
#' 
#' @return Plots networks using \code{\link[qgraph]{qgraph}}
#' or \code{\link[networktools]{MDSnet}}
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
#' 
#' # Compare networks
#' compare.nets(net1, net2, title = list("One", "Two"), config = "spring")
#' 
#' # Change edge colors
#' compare.nets(net1, net2, title = list("One", "Two"),
#' config = "spring", qgraph.args = list(edge.color = "blue"))
#' 
#' @references 
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012).
#' qgraph: Network visualizations of relationships in psychometric data.
#' \emph{Journal of Statistical Software}, \emph{48}, 1-18.
#' Retrieved from: http://www.jstatsoft.org/v48/i04/
#' 
#' Jones, P. J. (2019).
#' networktools: Tools for Identifying Important Nodes in Networks.
#' R package version 1.2.1.
#' \href{https://CRAN.R-project.org/package=networktools}{https://CRAN.R-project.org/package=networktools}
#' 
#' Jones, P. J., Mair, P., & McNally, R. (2018).
#' Visualizing psychological networks: A tutorial in R.
#' \emph{Frontiers in Psychology}, \emph{9}, 1742.
#' \href{https://doi.org/10.3389/fpsyg.2018.01742}{10.3389/fpsyg.2018.01742}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom graphics layout
#' 
#' @export
#Compare Graphs----
compare.nets <- function (..., title, config,
                          placement = c("match", "default"),
                          weighted = FALSE,
                          qgraph.args = list())
{
    #Get names of networks
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    # MISSING ARGUMENTS
    if(missing(title))
    {
        #Initialize title list
        title <- list()
        
        for(i in 1:length(name))
        {title[[i]] <- name[i]}
    }else if(!is.list(title))
    {stop("Argument 'title' only takes list objects")}
    
    if(missing(config))
    {config <- "MDS"
    }else{config <- config}
    
    if(missing(placement))
    {placement <- "default"
    }else{placement <- match.arg(placement)}
    # MISSING ARGUMENTS
    
    #Create list of input
    datalist <- list(...)
    
    #Initialize layout and labels list
    layouts <- list()
    labs <- list()
    
    for(i in 1:length(datalist))
    {
        #Change weights
        if(!weighted)
        {datalist[[i]] <- NetworkToolbox::binarize(datalist[[i]])}
        
        #Diagonals to zero
        diag(datalist[[i]]) <- 0
        
        #Create graph layouts
        if(config == "MDS")
        {layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE)
        }else{layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE,layout=config)}
        
        #Get labels
        labs[[i]] <- as.factor(colnames(datalist[[i]]))
    }
    
    #Manipulate R plot window
    if(length(datalist) == 2)
    {layout(t(1:2))
    }else if(length(datalist) > 2)
    {
        #Find square root
        len <- floor(sqrt(length(datalist)))
        
        #Remainder
        remain <- length(datalist)%%len
        
        #Change layout accordingly
        layout(t(matrix(1:(length(datalist)+remain),ncol=len)))
    }
    
    #Change layout arguments to FALSE
    for(i in 1:length(layouts))
    {layouts[[i]]$Arguments$DoNotPlot <- FALSE}
    
    #Create average layout
    if(placement == "match")
    {Layout <- qgraph::averageLayout(layouts)
    }else if(placement == "default")
    {Layout <- config}
    
    #Default 'qgraph' arguments for 'compare.nets()'
    if(is.list(qgraph.args))
    {
        #Name of arguments
        arg.name <- names(qgraph.args)
        
        #Check for 'compare.nets' defaults
        ##Vertex size
        if(!"vsize" %in% arg.name)
        {qgraph.args$vsize <- 4}
        ##Label proportion
        if(!"label.prop" %in% arg.name)
        {qgraph.args$label.prop <- 1}
        ##Aspect
        if(!"aspect" %in% arg.name)
        {qgraph.args$aspect <- FALSE}
        ##Repulsion
        if(config == "spring")
        {
            if(!"repulsion" %in% arg.name)
            {qgraph.args$repulsion <- 1.15}
        }
        ##Edge color
        if(!"edge.color" %in% arg.name)
        {
            if(!weighted)
            {qgraph.args$edge.color <- "black"}
        }
        
    }else{stop("qgraph.args must be input as a list")}
    
    #Add general defaults arguments
    qgraph.args$layout <- Layout
    
    for(i in 1:length(datalist))
    {
        #Network specific arguments
        ##Networks
        if(config == "MDS")
        {qgraph.args$qgraph_net <- layouts[[i]]
        }else{qgraph.args$input <- layouts[[i]]}
        ##Network title and labels
        qgraph.args$title <- title[[i]]
        qgraph.args$labels <- labs[[i]]
        
        #Generate plot
        ifelse(config == "MDS",
               do.call(networktools::MDSnet, args = qgraph.args),
               do.call(qgraph::qgraph, args = qgraph.args))
    }
}
#----