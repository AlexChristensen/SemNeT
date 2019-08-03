#' Plots Two Networks for Comparison
#' 
#' @description Uses \code{\link[qgraph]{qgraph}} and \code{\link[networktools]{MDSnet}}
#' to plot networks in a layout that provides meaningful comparisons of distances
#' (see Jones, Mair, & McNally, 2018)
#' 
#' @param A Character.
#' Matrix or data frame of network adjacency matrix
#' 
#' @param B Character.
#' Matrix or data frame of network adjacency matrix
#' 
#' @param titleA Character.
#' Title for network \code{A}
#' 
#' @param titleB Character.
#' Title for network \code{B}
#' 
#' @return Plots two networks with multidimensional scaling layout
#' (see \code{\link[networktools]{MDSnet}} for more details)
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
#' compare.nets(net1, net2, "One", "Two")
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
compare.nets <- function (A, B, titleA, titleB)
{
    if(missing(titleA))
    {titleA <- as.character(substitute(A))
    }else{titleA <- titleA}
    
    if(missing(titleB))
    {titleB <- as.character(substitute(B))
    }else{titleB <- titleB}
    
    diag(A)<-0
    diag(B)<-0
    
    Alayout <- qgraph::qgraph(A,DoNotPlot=TRUE)
    Blayout <- qgraph::qgraph(B,DoNotPlot=TRUE)
    
    Alabels = as.factor(colnames(A))
    Blabels = as.factor(colnames(B))

    Layout <- qgraph::averageLayout(Alayout,Blayout)
    layout(t(1:2))
    
    Alayout$Arguments$DoNotPlot <- FALSE
    Blayout$Arguments$DoNotPlot <- FALSE
    
    networktools::MDSnet(Alayout, layout = Layout, title = titleA,
                   esize = 20, vsize = 4, label.prop = 1,
                   labels = Alabels)
    networktools::MDSnet(Blayout, layout = Layout, title = titleB,
                   esize = 20, vsize = 4, label.prop = 1,
                   labels = Blabels)
}
#----