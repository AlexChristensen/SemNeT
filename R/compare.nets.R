#' Plots Two Networks for Comparison
#' 
#' @description Uses \code{\link[qgraph]{qgraph}} and \code{\link[networktools]{MDSnet}}
#' to plot networks in a layout that provides meaningful comparisons of distances
#' (see Jones, Mair, & McNally, 2018)
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
#' compare.nets(net1, net2, title = list("One", "Two"))
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
compare.nets <- function (..., title, config)
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
    # MISSING ARGUMENTS
    
    #Create list of input
    datalist <- list(...)
    
    #Initialize layout and labels list
    layouts <- list()
    labs <- list()
    
    for(i in 1:length(datalist))
    {
        #Diagonals to zero
        diag(datalist[[i]]) <- 0
        
        #Create graph layouts
        if(config == "MDS")
        {layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE)
        }else{layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE,layout=config)}
        
        #Get labels
        labs[[i]] <- as.factor(colnames(datalist[[i]]))
    }
    
    #Create average layout
    Layout <- qgraph::averageLayout(layouts)
    
    #Manipulate R plot window
    if(length(datalist) == 2)
    {layout(t(1:2))
    }else if(length(datalist) > 2)
    {
        #Find square root
        len <- floor(sqrt(length(datalist)))
        
        #Change layout accordingly
        layout(t(matrix(1:length(datalist),nrow=len)))
    }
    
    #Change layout arguments to FALSE
    for(i in 1:length(layouts))
    {layouts[[i]]$Arguments$DoNotPlot <- FALSE}
    
    if(config == "MDS")
    {
        for(i in 1:length(datalist))
        {
            networktools::MDSnet(layouts[[i]], layout = Layout, title = title[[i]],
                                 esize = 20, vsize = 4, label.prop = 1,
                                 labels = labs[[i]])
        }
        
    }else{
        
        for(i in 1:length(datalist))
        {
            qgraph::qgraph(layouts[[i]], layout = Layout, title = title[[i]],
                           esize = 20, vsize = 4, label.prop = 1,
                           labels = labs[[i]])
        }
    }
}
#----