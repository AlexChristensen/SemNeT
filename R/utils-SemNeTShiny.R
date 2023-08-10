#' Read in Common Data File Extensions (from \code{\link{SemNetCleaner}})
#' 
#' @description A single function to read in common data file extensions.
#' Note that this function is specialized for reading in text data in the
#' format necessary for functions in SemNetCleaner
#' 
#' File extensions supported:
#' \itemize{
#' \item{.Rdata} \item{.rds} \item{.csv} \item{.xlsx}
#' \item{.xls} \item{.sav} \item{.txt} \item{.mat}
#' }
#'
#' @param file Character.
#' A path to the file to load.
#' Defaults to interactive file selection using \code{\link{file.choose}}
#' 
#' @param header Boolean.
#' A logical value indicating whether the file contains the
#' names of the variables as its first line.
#' If missing, the value is determined from the file format:
#' header is set to \code{TRUE} if and only if the first row
#' contains one fewer field than the number of columns
#' 
#' @param sep Character.
#' The field separator character.
#' Values on each line of the file are separated by this character.
#' If sep = "" (the default for \code{\link{read.table}}) the separator
#' is a 'white space', that is one or more spaces, tabs, newlines or
#' carriage returns
#' 
#' @param ... Additional arguments.
#' Allows for additional arguments to be passed onto
#' the respective read functions. See documentation in the list below:
#' 
#' \itemize{
#' \item{.Rdata}
#' {\code{\link{load}}}
#' \item{.rds}
#' {\code{\link{readRDS}}}
#' \item{.csv}
#' {\code{\link[utils]{read.table}}}
#' \item{.xlsx}
#' {\code{\link[readxl]{read_excel}}}
#' \item{.xls}
#' {\code{\link[readxl]{read_excel}}}
#' \item{.sav}
#' {\code{\link[foreign]{read.spss}}}
#' \item{.txt}
#' {\code{\link[utils]{read.table}}}
#' \item{.mat}
#' {\code{\link[R.matlab]{readMat}}}
#' }
#' 
#' @return A data frame containing a representation of the data in the file.
#' If file extension is ".Rdata", then data will be read to the global environment
#'
#' @examples 
#' # Use this example for your data
#' if(interactive())
#' {read.data()}
#' 
#' # Example for CRAN tests
#' ## Create test data
#' test1 <- c(1:5, "6,7", "8,9,10")
#' 
#' ## Path to temporary file
#' tf <- tempfile()
#' 
#' ## Create test file
#' writeLines(test1, tf)
#' 
#' ## Read in data
#' read.data(tf)
#' 
#' # See documentation of respective R functions for specific examples
#' 
#' @references 
#' # R Core Team
#' 
#' R Core Team (2019). R: A language and environment for
#' statistical computing. R Foundation for Statistical Computing,
#' Vienna, Austria. URL https://www.R-project.org/.
#' 
#' # readxl
#' 
#' Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel
#' Files. R package version 1.3.1.
#' https://CRAN.R-project.org/package=readxl
#' 
#' # R.matlab
#' 
#' Henrik Bengtsson (2018). R.matlab: Read and Write MAT Files
#' and Call MATLAB from Within R. R package version 3.6.2.
#' https://CRAN.R-project.org/package=R.matlab
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils read.table read.csv
#' @importFrom tools file_ext
#' 
#' @noRd
# Read data----
# Updated 15.04.2020
read.data <- function (file = file.choose(), header = TRUE, sep = ",", ...)
{
    # Grab extension
    ext <- tolower(file_ext(file))
    
    # Report error
    if(!ext %in% c("rdata", "rds", "csv", "xlsx",
                   "xls", "sav", "txt", "mat", ""))
    {stop("File extension not supported")}
    
    # Determine data load
    if(ext != "")
    {
        switch(ext,
               rdata = load(file, envir = .GlobalEnv),
               rds = readRDS(file),
               csv = read.csv(file, header = header, sep = sep, as.is = TRUE, ...),
               xlsx = as.data.frame(readxl::read_xlsx(file, col_names = header, ...)),
               xls = as.data.frame(readxl::read_xls(file, col_names = header, ...)),
               sav = foreign::read.spss(file, to.data.frame = TRUE, stringAsFactors = FALSE, ...),
               txt = read.table(file, header = header, sep = sep, ...),
               mat = as.data.frame(R.matlab::readMat(file, ...))
        )
    }else{read.table(file, header = header, sep = sep, ...)}
}

#' Equate Groups for Shiny
#' 
#' @description A function to "equate" multiple response matrices to one another.
#' \emph{N} number of groups are matched based on their responses so
#' that every group has the same responses in their data
#' 
#' @param dat List.
#' Binary response matrices to be equated
#' 
#' @return This function returns a list containing the
#' equated binary response matrices in the order they were input.
#' The response matrices are labeled as the object name they were
#' entered with
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#'
# Eqaute for Shiny----
# Updated 20.03.2020
equateShiny <- function(dat)
{
    # Equate function
    equat <- function (rmatA, rmatB)
    {
        while(length(colnames(rmatA))!=length(colnames(rmatB)))
        {
            if(length(colnames(rmatA))>=length(colnames(rmatB)))
            {rmatA<-rmatA[,(!is.na(match(colnames(rmatA),colnames(rmatB))))]
            }else if(length(colnames(rmatB))>=length(colnames(rmatA)))
            {rmatB<-rmatB[,(!is.na(match(colnames(rmatB),colnames(rmatA))))]
            }else if(all(match(colnames(rmatA),colnames(rmatB))))
            {print("Responses match")}
        }
        
        return(list(rmatA=rmatA,rmatB=rmatB))
    }
    
    name <- names(dat)
    
    datalist <- dat
    
    len <- length(datalist)
    
    if(len>2)
    {
        first <- datalist[[1]]
        eq <- equat(first,datalist[[2]])$rmatA
        
        for(i in 2:(len-1))
        {eq <- equat(eq,datalist[[(i+1)]])$rmatA}
        
        finlist <- list()
        
        for(j in 1:len)
        {finlist[[name[j]]] <- equat(eq,datalist[[j]])$rmatB}
        
    }else if(len==2)
    {
        finlist <- equat(datalist[[1]],datalist[[2]])
        names(finlist) <- name
    }else{stop("Must be at least two datasets as input")}
    
    return(finlist)
}

#' Plots Networks for Comparison in Shiny
#' 
#' @description Uses \code{\link[qgraph]{qgraph}}
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
#' @param dat List.
#' Matrices or data frames of network adjacency matrices
#' 
#' @param title List.
#' Characters denoting titles of plots
#' 
#' @param config Character.
#' Defaults to \code{"spring"}.
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
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Compare Graphs----
# Updated 20.03.2020
compare_netShiny <- function (dat, title, config,
                              placement = c("match", "default"),
                              weighted = FALSE,
                              qgraph.args = list())
{
    #Get names of networks
    name <- names(dat)
    
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
    {config <- tolower("spring")
    }else{config <- tolower(config)}
    
    if(missing(placement))
    {placement <- "default"
    }else{placement <- match.arg(placement)}
    # MISSING ARGUMENTS
    
    #Create list of input
    datalist <- dat
    
    #Initialize layout and labels list
    layouts <- list()
    labs <- list()
    
    for(i in 1:length(datalist))
    {
        #Change weights
        if(!weighted)
        {datalist[[i]] <- binarize(datalist[[i]])}
        
        #Diagonals to zero
        diag(datalist[[i]]) <- 0
        
        #Create graph layouts
        #if(config == "mds")
        #{layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE)
        #}else{
        layouts[[i]] <- qgraph::qgraph(datalist[[i]],DoNotPlot=TRUE,layout=config)#}
        
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
    
    #Return
    res <- list()
    res$datalist <- datalist
    res$qgraph.args <- qgraph.args
    res$config <- config
    res$title <- title
    res$labs <- labs
    res$layouts <- layouts
    
    class(res) <- "compareShiny"
    
    return(res)
}
#----


#' Plots Networks for Comparison from Shiny
#' 
#' @description Uses \code{\link[qgraph]{qgraph}}
#' to plot networks. Accepts any number of networks and will organize the plots
#' in the number of side-by-side columns using the heuristic of taking the square root of the number of 
#' input and rounding down to the nearest integer (i.e., \code{floor(sqrt(length(input)))}).
#' Performs the same operations as \code{\link[SemNeT]{compare_nets}}
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
#' @param x Shiny result \code{resultShiny$comparePlot}
#' 
#' @param ... Additional arguments
#' 
#' @return Plots networks using \code{\link[qgraph]{qgraph}}
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
#' # Compute networks
#' net1 <- TMFG(cos1)
#' net2 <- TMFG(cos2)
#' 
#' # Compare networks
#' compare_nets(net1, net2, title = list("One", "Two"), config = "spring")
#' 
#' # Change edge colors
#' compare_nets(net1, net2, title = list("One", "Two"),
#' config = "spring", qgraph.args = list(edge.color = "blue"))
#' 
#' @references 
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012).
#' qgraph: Network visualizations of relationships in psychometric data.
#' \emph{Journal of Statistical Software}, \emph{48}, 1-18.
#' 
#' Jones, P. J. (2019).
#' networktools: Tools for Identifying Important Nodes in Networks.
#' R package version 1.2.1.
#' 
#' Jones, P. J., Mair, P., & McNally, R. (2018).
#' Visualizing psychological networks: A tutorial in R.
#' \emph{Frontiers in Psychology}, \emph{9}, 1742.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Compare Graphs----
# Updated 05.04.2020
plot.compareShiny <- function (x, ...)
{
    for(i in 1:length(x$datalist))
    {
        #Network specific arguments
        ##Networks
        #if(x$config == "mds")
        #{x$qgraph.args$qgraph_net <- x$layouts[[i]]
        #}else{
        x$qgraph.args$input <- x$layouts[[i]]
        #}
        ##Network title and labels
        x$qgraph.args$title <- x$title[[i]]
        x$qgraph.args$labels <- x$labs[[i]]
        
        #Generate plot
        #ifelse(x$config == "mds",
               #do.call(networktools::MDSnet, args = x$qgraph.args),
               do.call(qgraph::qgraph, args = x$qgraph.args)
               #)
    }
}
#----

#' Test Against Random Networks
#' @description Performs significance tests for global measures
#' of semantic networks against the global measures of equivalent
#' size (and density) random networks
#' 
#' @param dat List.
#' Matrices or data frames.
#' Semantic networks to be compared against random networks
#' 
#' @param iter Numeric.
#' Number of iterations in bootstrap.
#' Defaults to \code{1000}
#' 
#' @param cores Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' 
#' @return Returns a matrix containing p-values
#' for the network measures of the input networks against
#' the distribution of equivalent random networks. The last
#' two columns contain the mean (\code{"M.rand"}) and
#' standard deviation (\code{"SD.rand"}) of the network measures
#' for the random network distribution
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Random Network Analyses Shiny----
randnet.testShiny <- function (dat, iter, cores)
{
    #Missing arguments
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    
    if(missing(iter))
    {iter <- 1000
    }else{iter <- iter}
    
    #Get names of networks
    name <- names(dat)
    
    #Initialize data list
    data.list <- vector("list", length = length(name))
    
    for(i in 1:length(name))
        for(j in 1:iter)
        {data.list[[i]][[j]] <- dat[[i]]}
    
    #Initialize random networks list
    rand.list <- vector("list", length = length(name))
    names(rand.list) <- name
    
    # Message that this is for random network
    message(styletext(styletext("\nRandom Network Analyses\n", defaults = "underline"), defaults = "bold"))
    
    #Message for begin random networks
    message("Generating random networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export variables
    parallel::clusterExport(cl, c("data.list", "rand.list"), envir = environment())
    
    #Compute random networks
    for(i in 1:length(data.list))
    {rand.list[[i]] <- pbapply::pblapply(X = data.list[[i]], FUN = function(X){randnet(A = X)}, cl = cl)}
    
    #Message for begin of network measures
    message("Computing network measures...\n", appendLF = FALSE)
    
    #Initialize network measures list
    net.meas <- vector("list", length = length(name))
    names(net.meas) <- name
    
    #Export variables
    parallel::clusterExport(cl, c("net.meas"), envir = environment())
    
    for(i in 1:length(data.list))
    {
        #Compute network measures
        net.meas[[i]] <- pbapply::pbsapply(X = rand.list[[i]], FUN = semnetmeas, cl = cl)
    }
    
    #Stop parallel processing
    parallel::stopCluster(cl)
    
    #Initialize result list
    res <- vector("list", length = length(name))
    names(res) <- name
    
    #Compute significance tests
    for(i in 1:length(data.list))
    {
        sig.mat <- matrix(0, nrow = 3, ncol = 3)
        row.names(sig.mat) <- c("ASPL","CC","Q")
        colnames(sig.mat) <- c(paste(name[i], "(p-value)"), "M.rand", "SD.rand")
        
        #Insert random means and sds
        sig.mat[,"M.rand"] <- round(rowMeans(net.meas[[i]]),4)
        sig.mat[,"SD.rand"] <- round(apply(net.meas[[i]],1,sd),4)
        
        #Compute semantic network measures for network
        meas <- semnetmeas(dat[[i]])
        
        ##ASPL
        z.aspl <- (meas["ASPL"] - sig.mat["ASPL","M.rand"]) / sig.mat["ASPL","SD.rand"]
        sig.mat["ASPL",paste(name[i], "(p-value)")] <- round(2 * pnorm(-abs(z.aspl)), 4)
        ##CC
        z.cc <- (meas["CC"] - sig.mat["CC","M.rand"]) / sig.mat["CC","SD.rand"]
        sig.mat["CC",paste(name[i], "(p-value)")] <- round(2 * pnorm(-abs(z.cc)), 4)
        ##Q
        z.q <- (meas["Q"] - sig.mat["Q","M.rand"]) / sig.mat["Q","SD.rand"]
        sig.mat["Q",paste(name[i], "(p-value)")] <- round(2 * pnorm(-abs(z.q)), 4)
        
        #Insert results
        res[[i]] <- sig.mat
    }
    
    return(res)
}
#----

#' Bootstrapped Semantic Network Analysis for Shiny
#' 
#' @description Bootstrap techniques to generate
#' semantic networks and compute global network characteristics
#' 
#' @param dat Matrices or data frames.
#' Cleaned response matrices (e.g., \code{responses$clean} from
#' \code{\link[SemNetCleaner]{textcleaner}}) or  binary response matrices
#' (e.g., \code{binary} output from \code{\link[SemNetCleaner]{textcleaner}})
#' 
#' @param method Character.
#' Network estimation method to use.
#' Current options include:
#' 
#' \itemize{
#' \item{\code{\link[NetworkToolbox]{TMFG}}}
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
#' @param type Character.
#' Type of bootstrap to perform
#' 
#' \itemize{
#' \item{\code{node}}
#' {Generates partial networks based on dropping a certain
#' proportion of nodes (see argument \code{prop})}
#' 
#' \item{\code{case}}
#' {Samples with replacement the same number of participants
#' as in the original dataset}
#' }
#' 
#' @param prop Numeric.
#' \strong{Only} for \code{type = "node"}.
#' Proportion of nodes to remain in the network.
#' Defaults to \code{.50}
#' 
#' @param sim Character.
#' Similarity measure to use.
#' Defaults to \code{"cosine"}.
#' See \code{\link[SemNeT]{similarity}} for other options
#' 
#' @param weighted Boolean.
#' Should weighted ASPL and CC be used?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted ASPL and CC
#' 
#' @param iter Numeric.
#' Number of iterations in bootstrap.
#' Defaults to \code{1000}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} / 2 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' (see \code{parellel::detectCores()})
#' 
#' @return Returns a list containing:
#' 
#' \item{dataMeas}{A matrix for the network input in the \code{data}
#' argument, where columns are the semantic network measures
#' from \code{\link[SemNeT]{semnetmeas}} and rows are their values from each
#' bootstrapped sample (results in a matrix with the dimensions \code{iter} by 3)}
#' 
#' \item{dataSumm}{Summary statistics across the bootrapped samples for the
#' network input in the \code{data} argument}
#' 
#' \item{prop}{Outputs the proportion used from the \code{prop} argument}
#' 
#' \item{iter}{Outputs the number of bootstrapped samples
#' used from the \code{iter} argument}
#' 
#' If a \code{paired} network is input, then also returns:
#' 
#' \item{pairedMeas}{A matrix for the network input in the \code{paired}
#' arugment, where columns are the semantic network measures
#' from \code{\link[SemNeT]{semnetmeas}} and rows are their values from each
#' bootstrapped sample (results in a matrix with the dimensions \code{iter} by 3)}
#' 
#' \item{pairedSumm}{Summary statistics across the bootrapped samples for the
#' network input in the \code{paired} argument}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#Bootstrapped Semantic Network Analysis----
#Updated 16.08.2021
bootSemNeTShiny <- function (dat, method = c("CN", "NRW", "PF", "TMFG"),
                             methodArgs = NULL,
                             type = c("case", "node"),
                             prop = .50, sim, weighted = FALSE,
                             iter = 1000, cores)
{
    ####Missing arguments####
    if(missing(sim))
    {sim <- "cosine"
    }else{sim <- sim}
    
    if(missing(cores))
    {cores <- parallel::detectCores() / 2
    }else{cores <- cores}
    ####Missing arguments####
    
    #Get names of networks
    name <- names(dat)
    
    #Assign names of dat
    if(method == "TMFG")
    {
        for(i in 1:length(name))
        {assign(name[i],
                eq[[i]],
                envir = environment())}
    }
    
    #Create list of input
    datalist <- dat
    
    #Assign network function
    NET.FUNC <- switch(method,
                       "TMFG" = TMFG,
                       "CN" = CN,
                       "NRW" = NRW,
                       "PF" = PF)
    
    #Check for missing arguments in function
    form.args <- methods::formalArgs(NET.FUNC)[-which(methods::formalArgs(NET.FUNC) == "data")]
    
    #Get arguments
    if(any(!form.args %in% names(methodArgs))){
        
        #Find which arguments are needed
        need.args <- form.args[which(!form.args %in% names(methodArgs))]
        
        #Get default arguments for methods
        if(method == "CN"){
            
            if("window" %in% need.args){methodArgs$window <- 2}
            if("alpha" %in% need.args){methodArgs$alpha <- .05}
            if("enrich" %in% need.args){methodArgs$enrich <- FALSE}
            
        }else if(method == "NRW"){
            
            if("type" %in% need.args){methodArgs$type <- "num"}
            if("threshold" %in% need.args){methodArgs$threshold <- 0}
            
        }else if(method == "TMFG"){
            
            if("depend" %in% need.args){methodArgs$depend <- FALSE}
            
        }
    }
    
    #Number of nodes in full data
    full <- unique(unlist(lapply(datalist,ncol)))
    
    # Message that this is for bootstrap
    message(styletext(styletext("\nBootstrap Analyses\n", defaults = "underline"), defaults = "bold"))
    
    #######################
    #### GENERATE DATA ####
    #######################
    
    #Let user know data is being generated
    message("Generating data...", appendLF = FALSE)
    
    for(i in 1:length(name))
    {
        #Initialize count
        count <- 0
        
        #Initialize new data list
        new <- list()
        
        repeat{
            
            #Increase count
            count <- count + 1
            
            if(type == "node")
            {
                #Check that all data have same number of nodes
                if(length(unique(unlist(lapply(datalist,ncol))))!=1)
                {stop("bootSemNeT(): All datasets must have the same number of columns")}
                
                #Randomly sample nodes
                rand <- sample(1:full, (full*prop), replace=FALSE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[,rand]
                
            }else if(type == "case")
            {
                #Randomly sample nodes
                rand <- sample(1:nrow(get(name[i], envir = environment())),
                               nrow(get(name[i], envir = environment())),
                               replace=TRUE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[rand,]
            }
            
            # Check for TMFG
            if(method == "TMFG"){
                
                # Check for binary matrix
                if(all(apply(new[[count]], 2, is.numeric))){
                    
                    # Check for no variance
                    variance <- apply(new[[count]], 2, function(x){all(x == 1)})
                    
                    # Reduce count if no variance in response
                    if(any(variance)){count <- count - 1}
                    
                }
                
            }
            
            if(count == iter)
            {break}
        }
        
        #Insert data list
        assign(paste("dl.",name[i],sep=""),new, envir = environment())
    }
    
    #Let user know data generation is finished
    message("done\n")
    
    ##################
    #### EQUATING ####
    ##################
    
    #Check for appropriate conditions
    if(method == "TMFG" && type == "case")
    {
        #Let user know the samples are being equated
        message("Equating samples...", appendLF = FALSE)
        
        # If missing minCase
        if("minCase" %in% names(methodArgs))
        {minCase <- methodArgs$minCase
        }else{minCase <- 2}
        
        # Finalize each sample
        for(i in 1:length(name))
        {
            assign(
                paste("dl.",name[i],sep=""),
                
                lapply(get(paste("dl.",name[i],sep=""), envir = environment()),
                       FUN = finalize,
                       minCase = minCase),
                
                envir = environment()
            )
        }
        
        if(length(name) > 1)
        {
            #Get data to equate into a single list
            eq.dat <- mget(paste("dl.",name,sep=""), envir = environment())
            
            #Initialize equating list
            eq <- vector("list", length = iter)
            
            for(i in 1:iter)
            {eq[[i]] <- equateShiny(lapply(eq.dat, function(x){x[[i]]}))}
            
            for(i in 1:length(name))
            {
                assign(
                    paste("dl.",name[i],sep=""),
                    unlist(lapply(eq, function(x){x[paste("dl.",name[i],sep="")]}), recursive = FALSE),
                    envir = environment()
                )
            }
        }
        
        #Let user know data generation is finished
        message("done\n")
    }
    
    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    if(method == "TMFG")
    {
        #Let user know simliarity is being computed
        message("Computing similarity measures...\n", appendLF = FALSE)
        
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = c(),#paste("dl.",name,sep=""),
                                envir = environment())
        
        for(i in 1:length(name))
        {
            #Compute similarity
            newSim <- pbapply::pblapply(X = get(paste("dl.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = similarity,
                                        method = sim)
            
            #Insert similarity list
            assign(paste("sim.",name[i],sep=""),newSim, envir = environment())
        }
        
        #Stop Cluster
        parallel::stopCluster(cl)
    }else{
        for(i in 1:length(name))
        {
            #Insert similarity list
            assign(paste("sim.",name[i],sep=""),
                   get(paste("dl.",name[i],sep=""), envir = environment()))
        }
    }
    
    ############################
    #### CONSTRUCT NETWORKS ####
    ############################
    
    #Let user know networks are being computed
    message("Estimating networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = #c(paste("sim.",name,sep=""),
                                c("NET.FUNC"),
                            envir = environment())
    
    if(method == "TMFG")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        depend = methodArgs$depend)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "CN")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        window = methodArgs$window,
                                        alpha = methodArgs$alpha)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "NRW")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC,
                                        threshold = methodArgs$threshold)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }else if(method == "PF")
    {
        for(i in 1:length(name))
        {
            #Compute networks
            newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = NET.FUNC)
            
            #Insert network list
            assign(paste("Semnet.",name[i],sep=""), newNet)
        }
        
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    ##############################
    #### COMPUTING STATISTICS ####
    ##############################
    
    #Let user know networks are being computed
    message("Computing network measures...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c(),#paste("Semnet.",name[i],sep=""),
                            envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute network measures
        netMeas <- pbapply::pbsapply(X = get(paste("Semnet.",name[i],sep=""), envir = environment()),
                                     cl = cl,
                                     FUN = semnetmeas
        )
        
        #Insert network measures list
        assign(paste("meas.",name[i],sep=""),netMeas)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    #Compute summary statistics
    summ.table <- function (data, n)
    {
        stats <- list()
        stats$mean <- rowMeans(data, na.rm = TRUE)
        stats$stdev <- apply(data, 1, sd, na.rm = TRUE)
        stats$se <- stats$stdev/sqrt(n)
        stats$lower <- stats$mean - (1.96 * stats$se)
        stats$upper <- stats$mean + (1.96 * stats$se)
        
        return(stats)
    }
    
    #Initialize bootlist
    bootlist <- list()
    
    #Insert results
    for(i in 1:length(name))
    {
        bootlist[[paste(name[i],"Net",sep="")]] <- get(paste("Semnet.",name[i],sep=""), envir = environment())
        bootlist[[paste(name[i],"Meas",sep="")]] <- get(paste("meas.",name[i],sep=""), envir = environment())
        bootlist[[paste(name[i],"Summ",sep="")]] <- summ.table(get(paste("meas.",name[i],sep=""), envir = environment()), iter)
    }
    
    #Insert proportion remaining and iterations
    if(type == "node")
    {bootlist$prop <- prop}
    
    bootlist$iter <- iter
    
    bootlist$type <- type
    
    class(bootlist) <- "bootSemNeT"
    
    return(bootlist)
}
#----

#' Statistical tests for \code{\link[SemNeT]{bootSemNeT}} in Shiny
#' 
#' @description Computes statistical tests for partial bootstrapped
#' networks from \code{\link[SemNeT]{bootSemNeT}}. Automatically
#' computes \emph{t}-tests (\code{\link{t.test}}) or ANOVA
#' (\code{\link{aov}}) including Tukey's HSD for pairwise comparisons
#' (\code{\link{TukeyHSD}})
#' 
#' @param formula Character.
#' A formula for specifying an ANOVA structure. The formula should
#' have the predictor variable as "y" and include the names the variables
#' are grouped by (e.g., \code{formula = "y ~ group_var1 * group_var2"}).
#' See Two-way ANOVA example in examples
#' 
#' @param groups Data frame.
#' A data frame specifying the groups to be input into the formula.
#' The column names should be the variable names of interest. The
#' groups should be in the same order as the groups input into
#' \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param input Object(s) from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @return Returns a list containing the objects:
#' 
#' \item{ASPL}{Test statistics for each proportion of nodes remaining for ASPL}
#' 
#' \item{CC}{Test statistics for each proportion of nodes remaining for CC}
#' 
#' \item{Q}{Test statistics for each proportion of nodes remaining for Q}
#' 
#' If two groups:
#' 
#' A matrix in each object has the following columns:
#' 
#' \item{t-statistic}{Statistic from the \code{\link{t.test}}}
#' 
#' \item{df}{Degrees of freedom}
#' 
#' \item{p-value}{\emph{p}-value with values equal to \code{0} being \emph{p} < .001}
#' 
#' \item{d}{Cohen's \emph{d}}
#' 
#' \item{CI95.lower}{Lower bound of the 95 percent confidence interval}
#' 
#' \item{CI95.upper}{Upper bound of the 95 percent confidence interval}
#' 
#' \item{Direction}{Direction of the effect. The argument \code{groups} will
#' specify specifically which group is higher or lower on the measure. If no
#' groups are input, then \code{"d"} and \code{"p"} are used to represent
#' \code{data} and \code{paired} samples from \code{\link[SemNeT]{bootSemNeT}}, respectively}
#' 
#' Row names refer to the proportion of nodes remaining in bootstrapped networks
#' 
#' If three or more groups:
#' 
#' A list containing two objects:
#' 
#' \item{ANOVA}{A matrix containing the \emph{F}-statistic, group degrees of freedom,
#' residual degrees of freedom, \emph{p}-value, and partial eta squared {\code{p.eta.sq}}}
#' 
#' \item{HSD}{A matrix containing the differences between each group (\code{diff}),
#' lower (\code{lwr}) and upper (\code{upr}) bounds of the 95\% confidence interval,
#' and the adjusted \emph{p}-value (\code{p.adj})}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#Test: Bootstrapped Network Statistics----
# Updated 26.04.2021
test.bootSemNeTShiny <- function (input, test = c("ANCOVA", "ANOVA", "t-test"),
                                  measures = c("ASPL", "CC", "Q"),
                                  formula = NULL, groups = NULL)
{
    #Missing arguments
    if(missing(measures))
    {measures <- c("ASPL", "CC", "Q")
    }else{measures <- match.arg(measures)}
    
    #Names of groups
    name <- unique(gsub("Net", "", gsub("Summ","",gsub("Meas","",names(input[[1]])))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Check for groups
    if(is.null(groups))
    {groups <- name}
    
    if(!is.matrix(groups))
    {groups <- as.matrix(groups)}
    
    #Check for wrong test
    if(ncol(groups) > 1){
        if(test == "t-test"){
            stop("Number of groups not compatiable with t-tests.\n\nPlease use: 'test = \"ANOVA\"' OR 'test = \"ANCOVA\"'")
        }
    }else if(nrow(groups) < 3){
        if(test == "ANOVA"){
            stop("Groups not compatiable with ANOVAs.\n\nPlease use: 'test = \"t-test\"'")
        }
    }
    
    #Length of groups
    len <- length(name)
    
    #Type
    type <- input[[1]]$type
    
    #Proportions
    if(type == "node")
    {props <- paste("Proportion (", unlist(lapply(input, function(x){x$prop})), "0)", sep = "")
    }else{props <- "Case"}
    
    #Initialize result list
    res <- list()
    
    #Initialize temporary results list
    temp.res <- list()
    
    for(i in 1:length(input))
    {temp.res[[props[i]]] <- suppressPackageStartupMessages(boot.one.test(input[[i]],
                                                                          test = test,
                                                                          measures = measures,
                                                                          formula = formula,
                                                                          groups = groups))}
    
    
    #Check for ANOVA
    if(test == "ANOVA"){
        temp.res <- lapply(temp.res, function(x){
            lapply(x, function(x){
                names(x) <- c("ANOVA", "Means", "HSD")
                return(x)
            })
        })
    }
    
    # Insert full results
    res$fullResults <- temp.res
    
    #Type of test
    if(test == "ANCOVA"){##ANCOVA
        
        #Create tables of results
        ##Get ANCOVA values
        
        if(ncol(groups) == 1)
        {
            acov.vals <- lapply(temp.res, function(x, extra){
                lapply(x, function(x, extra){
                    x$ANCOVA[which(x$ANCOVA$Term == "Group"),]
                })
            })
        }
        
        ##Get Residual degress of freedom
        res.df <- lapply(temp.res, function(x){
            lapply(x, function(x){
                x$ANCOVA[which(x$ANCOVA$Term == "Residuals"),"df"]
            })
        })
        
        ##Get adjusted mean values
        adj.vals <- unlist(lapply(temp.res, function(x){
            lapply(x, function(x){
                means <- as.vector(x$adjustedMeans$fit)
                names(means) <- x$adjustedMeans$variables$Group$levels
                means
            })
        }), recursive = FALSE)
        
        adj.vals <- t(simplify2array(adj.vals))
        
        if(length(row.names(adj.vals)) > length(measures))
        {
            row.names(adj.vals) <- paste(rep(gsub("\\)", "", gsub("Proportion \\(", "", props)), each = length(measures)), measures)
            colnames(adj.vals) <- paste("Group", 1:nrow(groups))
        }else{row.names(adj.vals) <- measures}
        
        #Insert adjusted means
        res$adjustedMeans <- adj.vals
        
        if(ncol(groups) == 1)
        {
            ##Loop through to get tables
            if(length(acov.vals) == 1)
            {
                #Get measures
                meas.val <- unlist(acov.vals, recursive = FALSE)
                #Table measures
                tab.acov <- t(simplify2array(meas.val, higher = FALSE))[,-c(1:2)]
                #Adjusted means
                tab.acov <- cbind(round(adj.vals, 3), tab.acov)
                #Add residual degrees of freedom
                tab.acov <- as.data.frame(cbind(tab.acov[,c(1:(length(names)+2))], unlist(res.df), tab.acov[,(length(names)+3):ncol(tab.acov)]), stringsAsFactors = FALSE)
                #Recheck names
                name <- colnames(tab.acov)[1:length(name)]
                
                # Provided direction if two groups
                if(length(name) == 2)
                {
                    #Add direction
                    Direction <- apply(tab.acov, 1, function(x, name){
                        p.num <- as.numeric(gsub("< ", "", x["p-value"]))
                        
                        if(p.num <= .05)
                        {
                            if(as.numeric(x[1]) > as.numeric(x[2]))
                            {paste(name[1], ">", name[2], sep = " ")
                            }else{paste(name[1], "<", name[2], sep = " ")}
                        }else{"n.s."}
                    }, name = name)
                    
                    tab.acov <- cbind(tab.acov, Direction)
                }
                
                #Change column name
                colnames(tab.acov)[length(name)+2] <- "Residual df"
                colnames(tab.acov)[1:length(name)] <- paste("Adj. M.", name)
                #Change row names
                row.names(tab.acov) <- measures
                
                # Insert table results
                res$ANCOVA <- tab.acov
                
            }else{
                
                for(j in 1:length(measures))
                {
                    #Get measures
                    meas.val <- lapply(acov.vals, function(x){x[[measures[j]]]})
                    #Get residual degrees of freedom
                    res.val <- lapply(res.df, function(x){x[[measures[j]]]})
                    #Table measures
                    tab.acov <- t(simplify2array(meas.val, higher = FALSE))[,-c(1:2)]
                    #Adjusted means
                    tab.acov <- cbind(round(adj.vals[grep(measures[[j]], row.names(adj.vals)),], 3), tab.acov)
                    #Add residual degrees of freedom
                    tab.acov <- as.data.frame(cbind(tab.acov[,c(1:(length(names)+2))], unlist(res.val), tab.acov[,(length(names)+3):ncol(tab.acov)]), stringsAsFactors = FALSE)
                    #Recheck names
                    name <- colnames(tab.acov)[1:length(name)]
                    
                    # Provided direction if two groups
                    if(length(name) == 2)
                    {
                        #Add direction
                        Direction <- apply(tab.acov, 1, function(x, name){
                            p.num <- as.numeric(gsub("< ", "", x["p-value"]))
                            
                            if(p.num <= .05)
                            {
                                if(as.numeric(x[1]) > as.numeric(x[2]))
                                {paste(name[1], ">", name[2], sep = " ")
                                }else{paste(name[1], "<", name[2], sep = " ")}
                            }else{"n.s."}
                        }, name = name)
                        
                        tab.acov <- cbind(tab.acov, Direction)
                    }
                    
                    #Change column name
                    colnames(tab.acov)[length(name)+2] <- "Residual df"
                    colnames(tab.acov)[1:length(name)] <- paste("Adj. M.", name)
                    #Change row names
                    row.names(tab.acov) <- gsub(paste(" ", measures[[j]], sep = ""), "", row.names(tab.acov))
                    
                    # Insert table results
                    res$ANCOVA[[measures[j]]] <- tab.acov
                    
                    # Return groups
                    row.names(groups) <- paste("Group", 1:nrow(groups))
                    res$groups <- groups
                }
                
            }
        }
        
    }else if(test == "ANOVA"){
        
        #Create tables of results
        ##Get ANOVA values
        
        if(ncol(groups) == 1)
        {
            acov.vals <- lapply(temp.res, function(x, extra){
                lapply(x, function(x, extra){
                    x$ANOVA[which(x$ANOVA$Term == "Group"),]
                })
            })
        }
        
        ##Get Residual degress of freedom
        res.df <- lapply(temp.res, function(x){
            lapply(x, function(x){
                x$ANOVA[which(x$ANOVA$Term == "Residuals"),"df"]
            })
        })
        
        ##Get adjusted mean values
        adj.vals <- unlist(lapply(temp.res, function(x){
            lapply(x, function(x){
                means <- as.vector(x$Means$fit)
                names(means) <- x$Means$variables$Group$levels
                means
            })
        }), recursive = FALSE)
        
        adj.vals <- t(simplify2array(adj.vals))
        
        if(length(row.names(adj.vals)) > length(measures))
        {
            row.names(adj.vals) <- paste(rep(gsub("\\)", "", gsub("Proportion \\(", "", props)), each = length(measures)), measures)
            colnames(adj.vals) <- paste("Group", 1:nrow(groups))
        }else{row.names(adj.vals) <- measures}
        
        #Insert adjusted means
        res$Means <- adj.vals
        
        if(ncol(groups) == 1)
        {
            ##Loop through to get tables
            if(length(acov.vals) == 1)
            {
                #Get measures
                meas.val <- unlist(acov.vals, recursive = FALSE)
                #Table measures
                tab.acov <- t(simplify2array(meas.val, higher = FALSE))[,-c(1)]
                #Adjusted means
                tab.acov <- cbind(round(adj.vals, 3), tab.acov)
                #Add residual degrees of freedom
                tab.acov <- as.data.frame(cbind(tab.acov[,c(1:(length(names)+3))], unlist(res.df), tab.acov[,(length(names)+4):ncol(tab.acov)]), stringsAsFactors = FALSE)
                #Recheck names
                name <- colnames(tab.acov)[1:length(name)]
                
                # Provided direction if two groups
                if(length(name) == 2)
                {
                    #Add direction
                    Direction <- apply(tab.acov, 1, function(x, name){
                        p.num <- as.numeric(gsub("< ", "", x["p-value"]))
                        
                        if(p.num <= .05)
                        {
                            if(as.numeric(x[1]) > as.numeric(x[2]))
                            {paste(name[1], ">", name[2], sep = " ")
                            }else{paste(name[1], "<", name[2], sep = " ")}
                        }else{"n.s."}
                    }, name = name)
                    
                    tab.acov <- cbind(tab.acov, Direction)
                }
                
                #Change column name
                colnames(tab.acov)[length(name)+2] <- "Residual df"
                colnames(tab.acov)[1:length(name)] <- paste("Mean", name)
                #Change row names
                row.names(tab.acov) <- measures
                
                # Insert table results
                res$ANOVA <- tab.acov
                
            }else{
                
                for(j in 1:length(measures))
                {
                    #Get measures
                    meas.val <- lapply(acov.vals, function(x){x[[measures[j]]]})
                    #Get residual degrees of freedom
                    res.val <- lapply(res.df, function(x){x[[measures[j]]]})
                    #Table measures
                    tab.acov <- t(simplify2array(meas.val, higher = FALSE))[,-c(1)]
                    #Adjusted means
                    tab.acov <- cbind(round(adj.vals[grep(measures[[j]], row.names(adj.vals)),], 3), tab.acov)
                    #Add residual degrees of freedom
                    tab.acov <- as.data.frame(cbind(tab.acov[,c(1:(length(names)+3))], unlist(res.val), tab.acov[,(length(names)+4):ncol(tab.acov)]), stringsAsFactors = FALSE)
                    #Recheck names
                    name <- colnames(tab.acov)[1:length(name)]
                    
                    # Provided direction if two groups
                    if(length(name) == 2)
                    {
                        #Add direction
                        Direction <- apply(tab.acov, 1, function(x, name){
                            p.num <- as.numeric(gsub("< ", "", x["p-value"]))
                            
                            if(p.num <= .05)
                            {
                                if(as.numeric(x[1]) > as.numeric(x[2]))
                                {paste(name[1], ">", name[2], sep = " ")
                                }else{paste(name[1], "<", name[2], sep = " ")}
                            }else{"n.s."}
                        }, name = name)
                        
                        tab.acov <- cbind(tab.acov, Direction)
                    }
                    
                    #Change column name
                    colnames(tab.acov)[length(name)+2] <- "Residual df"
                    colnames(tab.acov)[1:length(name)] <- paste("Mean", name)
                    #Change row names
                    row.names(tab.acov) <- gsub(paste(" ", measures[[j]], sep = ""), "", row.names(tab.acov))
                    
                    # Insert table results
                    res$ANOVA[[measures[j]]] <- tab.acov
                    
                    # Return groups
                    row.names(groups) <- paste("Group", 1:nrow(groups))
                    res$groups <- groups
                }
                
            }
        }
        
    }else if(test == "t-test"){##t-test
        res <- boot.t.org(temp.res, groups, measures)
    }
    
    return(res)
}
#----

#' Sub-routine function for \code{\link[SemNeT]{test.bootSemNeT}} in Shiny
#' 
#' @description Computes statistical tests for partial bootstrapped
#' networks from \code{\link[SemNeT]{bootSemNeT}}. Automatically
#' computes \emph{t}-tests (\code{\link{t.test}}) or ANOVA
#' (\code{\link{aov}}) including Tukey's HSD for pairwise comparisons
#' (\code{\link{TukeyHSD}})
#' 
#' @param bootSemNeT.obj Object from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param formula Character.
#' A formula for specifying an ANOVA structure. The formula should
#' have the predictor variable as "y" and include the names the variables
#' are grouped by (e.g., \code{formula = "y ~ group_var1 * group_var2"}).
#' See Two-way ANOVA example in examples
#' 
#' @param groups Data frame.
#' A data frame specifying the groups to be input into the formula.
#' The column names should be the variable names of interest. The
#' groups should be in the same order as the groups input into
#' \code{\link[SemNeT]{partboot}}
#' 
#' @return Returns a list containing the objects:
#' 
#' \item{ASPL}{Test statistics for each proportion of nodes remaining for ASPL}
#' 
#' \item{CC}{Test statistics for each proportion of nodes remaining for CC}
#' 
#' \item{Q}{Test statistics for each proportion of nodes remaining for Q}
#' 
#' If two groups:
#' 
#' A matrix in each object has the following columns:
#' 
#' \item{t-statistic}{Statistic from the \code{\link{t.test}}}
#' 
#' \item{df}{Degrees of freedom}
#' 
#' \item{p-value}{\emph{p}-value with values equal to \code{0} being \emph{p} < .001}
#' 
#' \item{d}{Cohen's \emph{d}}
#' 
#' \item{CI95.lower}{Lower bound of the 95 percent confidence interval}
#' 
#' \item{CI95.upper}{Upper bound of the 95 percent confidence interval}
#' 
#' \item{Direction}{Direction of the effect. The argument \code{groups} will
#' specify specifically which group is higher or lower on the measure. If no
#' groups are input, then \code{"d"} and \code{"p"} are used to represent
#' \code{data} and \code{paired} samples from \code{\link[SemNeT]{partboot}}, respectively}
#' 
#' Row names refer to the proportion of nodes remaining in bootstrapped networks
#' 
#' If three or more groups:
#' 
#' A list containing two objects:
#' 
#' \item{ANOVA}{A matrix containing the \emph{F}-statistic, group degrees of freedom,
#' residual degrees of freedom, \emph{p}-value, and partial eta squared {\code{p.eta.sq}}}
#' 
#' \item{HSD}{A matrix containing the differences between each group (\code{diff}),
#' lower (\code{lwr}) and upper (\code{upr}) bounds of the 95% confidence interval,
#' and the adjusted \emph{p}-value (\code{p adj})}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Test: Bootstrapped Network Statistics----
# Updated 18.04.2021
boot.one.testShiny <- function (bootSemNeT.obj, 
                                test = c("ANCOVA", "ANOVA", "t-test"),
                                measures = c("ASPL", "CC", "Q"),
                                formula = NULL, groups = NULL)
{
    #Check for data if formula is not NULL
    if(!is.null(formula))
    {
        if(!exists("groups"))
        {stop("'groups' argument is NULL when 'formula' argument is not. Please input groups.")}
    }
    
    #Get names of networks
    name <- unique(gsub("Net","",gsub("Summ","",gsub("Meas","",names(bootSemNeT.obj)))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Number of input
    len <- length(name)
    
    #Error there are no paired samples
    if(len < 2)
    {stop("Single samples cannot be tested. Use 'randnet.test' for single samples")}
    
    #Handle groups
    if(is.null(groups))
    {groups <- name}
    
    #Enforce matrix
    groups <- as.matrix(groups)
    
    #Check for groups names
    if(is.null(colnames(groups)))
    {colnames(groups) <- ifelse(ncol(groups) == 1, "Group", paste("Group", 1:ncol(groups), sep = ""))}
    
    #Identify iterations
    iter <- bootSemNeT.obj$iter
    
    #%%%%%%%%%%%%%%%%%%%%#
    # SIGNIFICANCE TESTS #
    #%%%%%%%%%%%%%%%%%%%%#
    
    #Input results into list
    tests <- list()
    
    #Check for test
    if(test == "ANCOVA" | test == "ANOVA"){##ANCOVA or ANOVA
        
        #Loop through measures
        for(i in 1:length(measures))
        {
            #Create ANCOVA data frame
            for(j in 1:len)
            {
                #Insert measure values
                meas <- bootSemNeT.obj[[paste(name[j],"Meas",sep="")]][measures[i],]
                
                # Nodes
                #nodes <- unlist(lapply(bootSemNeT.obj[[paste(name[j],"Net",sep="")]], function(x){ncol(x)}))
                
                # Edges
                edges <- unlist(lapply(bootSemNeT.obj[[paste(name[j],"Net",sep="")]], function(x){
                    net <- binarize(x)
                    diag(net) <- 0
                    return(sum(net) / 2)
                }))
                
                #Initialize matrix
                mat <- cbind(rep(name[j], length(meas)), meas, #nodes,
                             edges)
                
                if(j != 1)
                {new.mat <- rbind(new.mat, mat)
                }else{new.mat <- mat}
            }
            
            #Convert to data frame
            aov.obj <- as.data.frame(new.mat, stringsAsFactors = FALSE)
            colnames(aov.obj) <- c("Name", "Measure", #"Nodes",
                                   "Edges")
            aov.obj$Name <- factor(as.character(aov.obj$Name))
            aov.obj[,2:3] <- apply(aov.obj[,2:3], 2, function(x){as.numeric(as.character(x))})
            
            #Organize groups
            aov.obj <- as.data.frame(cbind(aov.obj, rep.rows(groups, iter)), stringsAsFactors = FALSE)
            
            #Get column before groups
            edge.col <- which(colnames(aov.obj) == "Edges")
            
            #Convert groups to factors
            for(g in 1:ncol(groups))
            {aov.obj[,(edge.col+g)] <- as.factor(as.character(aov.obj[,(edge.col+g)]))}
            
            #Remove variables that are all equal
            keep.vars <- apply(aov.obj[,1:ncol(aov.obj)], 2, function(x){length(unique(x)) != 1})
            aov.obj <- aov.obj[,keep.vars]
            
            #Group mean center
            ## See Understanding and misunderstanding group mean centering: a commentary on Kelley et al.'s dangerous practice
            ## Bell, A., Jones, K., & Fairbrother, M. (2018).
            ## \emph{Quality & Quantity Volume} \emph{52}, 2031-2036.
            ##https://doi.org/10.1007/s11135-017-0593-5
            #if("Nodes" %in% names(aov.obj))
            #{
            #  for(g in 1:nrow(groups))
            #  {
            #    if(length(unique((aov.obj$Nodes[which(aov.obj$Group == groups[g,])]))) == 1){
            #      aov.obj$Nodes[which(aov.obj$Group == groups[g,])] <- 0
            #    }else{
            #      aov.obj$Nodes[which(aov.obj$Group == groups[g,])] <- scale(aov.obj$Nodes[which(aov.obj$Group == groups[g,])])
            #    }
            #  }
            #}
            
            if("Edges" %in% names(aov.obj))
            {
                for(g in 1:nrow(groups))
                {
                    if(length(unique((aov.obj$Edges[which(aov.obj$Group == groups[g,])]))) == 1){
                        aov.obj$Edges[which(aov.obj$Group == groups[g,])] <- 0
                    }else{
                        aov.obj$Edges[which(aov.obj$Group == groups[g,])] <- scale(aov.obj$Edges[which(aov.obj$Group == groups[g,])])
                    }
                }
            }
            
            #ANOVA
            if(test == "ANOVA"){
                
                if("Edges" %in% names(aov.obj)){
                    
                    aov.obj$Edges <- NULL
                    
                }
                
            }
            
            #Formula
            if(is.null(formula))
            {formula <- paste("y ~", paste(colnames(groups), collapse = " + "))}
            
            #Replace 'y' with 'Measure'
            formula <- gsub("y", "Measure", formula)
            
            #Split formula to add 'Nodes' and 'Edges'
            split.formula <- unlist(strsplit(formula, split = "~"))
            
            #ANOVA formula
            ##Catch Pathfinder Network method
            #if(all(aov.obj$Nodes - aov.obj$Edges == 1))
            #{aov.formula <- paste(split.formula[1], "~ ", paste(names(keep.vars)[4][keep.vars[4]], collapse = " + "), " +", split.formula[2], sep = "")
            #}else{
            #}
            
            if(test == "ANCOVA"){
                aov.formula <- paste(split.formula[1], "~ ", paste(names(keep.vars)[3][keep.vars[3]], collapse = " + "), " +", split.formula[2], sep = "")
            }else if(test == "ANOVA"){
                aov.formula <- paste(split.formula[1], "~ ", split.formula[2], sep = "")
            }
            
            #ANOVA
            aov.test <- aov(as.formula(aov.formula), data = aov.obj)
            
            #ANCOVA
            acov.test <- car::Anova(aov.test, type = "III")
            
            #Tidy ANCOVA
            tidy.acov <- as.data.frame(broom::tidy(acov.test), stringsAsFactors = FALSE)
            tidy.acov[,-1] <- round(apply(tidy.acov[,-1], 2, as.numeric), 3)
            
            #Get partial etas
            etas <- round(unlist(lapply(acov.test$`Sum Sq`, partial.eta.sq, sum(acov.test$`Sum Sq`[length(acov.test$`Sum Sq`)])))[-c(1,length(acov.test$`Sum Sq`))], 3)
            
            #Attach etas to tidy ANCOVA
            tidy.acov <- as.data.frame(cbind(tidy.acov, c(NA, etas, NA)), stringsAsFactors = FALSE)
            
            #Change column names
            colnames(tidy.acov) <- c("Term", "Sum of Squares", "df", "F-statistic", "p-value", "Partial Eta Squared")
            
            #Change p-values
            tidy.acov$`p-value` <- ifelse(tidy.acov$`p-value` < .001, "< .001", tidy.acov$`p-value`)
            
            # Convert NA to blank values
            tidy.acov <- as.data.frame(apply(tidy.acov, 2, function(x){trimws(ifelse(is.na(x), "", x))}), stringsAsFactors = FALSE)
            
            #Get adjusted means
            adj.means <- effects::allEffects(aov.test)
            
            #Insert ANCOVA
            tests[[paste(measures[i])]]$ANCOVA <- tidy.acov
            
            #Insert adjusted means
            tests[[paste(measures[i])]]$adjustedMeans <- adj.means[[which(names(adj.means) != "Nodes" & names(adj.means) != "Edges")]]
            
            #Get pairwise comparisons
            if(nrow(groups) > 2){
                
                if(ncol(groups) > 1){
                    tests[[paste(measures[i])]]$HSD <- suppressWarnings(TukeyHSD(aov.test))
                }else{
                    tests[[paste(measures[i])]]$HSD <- unlist(suppressWarnings(TukeyHSD(aov.test)), recursive = FALSE)
                }
                
            }
        }
        
    }else if(test == "t-test"){##t-test
        
        #Loop through measures
        for(i in 1:length(measures)){
            
            #Group combinations
            group.comb <- combn(groups, m = 2)
            
            #Result matrix
            res.mat <- matrix(NA, nrow = ncol(group.comb), ncol = 11)
            colnames(res.mat) <- c("df", "t-statistic", "p-value",
                                   "lower.ci", "upper.ci", "d",
                                   "Mean1", "SD1", "Mean2", "SD2",
                                   "Direction")
            row.names(res.mat) <- 1:ncol(group.comb)
            res.mat <- as.data.frame(res.mat)
            
            # Loop through groups
            for(j in 1:ncol(group.comb)){
                
                # Names
                one <- group.comb[1,j]
                two <- group.comb[2,j]
                
                # Groups
                group1 <- bootSemNeT.obj[[grep(paste(one, "Meas", sep = ""),
                                               names(bootSemNeT.obj))]]
                
                group2 <- bootSemNeT.obj[[grep(paste(two, "Meas", sep = ""),
                                               names(bootSemNeT.obj))]]
                
                #Compute t-test
                summ <- t.test(group1[measures[i],],
                               group2[measures[i],],
                               var.equal = TRUE)
                
                # Input results
                row.names(res.mat)[j] <- paste(one, two, sep = "--")
                res.mat[j,1] <- summ$parameter
                res.mat[j,2] <- round(summ$statistic, 3)
                res.mat[j,3] <- ifelse(summ$p.value < .001, "< .001", round(summ$p.value, 3))
                res.mat[j,4] <- round(summ$conf.int[1], 3)
                res.mat[j,5] <- round(summ$conf.int[2], 3)
                res.mat[j,6] <- round(d(group1[measures[i],],
                                        group2[measures[i],]), 3)
                res.mat[j,7] <- round(mean(group1[measures[i],], na.rm = TRUE), 3)
                res.mat[j,8] <- round(sd(group1[measures[i],], na.rm = TRUE), 3)
                res.mat[j,9] <- round(mean(group2[measures[i],], na.rm = TRUE), 3)
                res.mat[j,10] <- round(sd(group2[measures[i],], na.rm = TRUE), 3)
                res.mat[j,11] <- ifelse(
                    summ$p.value > .05,
                    paste(one, "(1) = (2)", two, sep = ""),
                    ifelse(
                        mean(group1[measures[i],], na.rm = TRUE) >
                            mean(group2[measures[i],], na.rm = TRUE),
                        paste(one, "(1) > (2)", two, sep = ""),
                        paste(one, "(1) < (2)", two, sep = "")
                    )
                )
                
            }
            
            tests[[paste(measures[i])]] <- res.mat
            
        }
        
    }
    
    return(tests)
}
#----

#' Plot for \link[SemNeT]{bootSemNeT} in Shiny
#' 
#' @description Plots output from \link[SemNeT]{bootSemNeT}
#' 
#' @param input Object(s) from \code{\link[SemNeT]{bootSemNeT}}
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
#' sim = "cosine", cores = 2, type = "node", method = "TFMG")
#' }
#' # Plot
#' plot(one.result, groups = c("One"))
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#Plot: Bootstrapped Semantic Network Analysis----
# Updated 05.04.2020
plotbootSemNeTShiny <- function (input, groups = NULL, measures = c("ASPL","CC","Q"))
{
    #Check for 'partboot' object
    if(all(unlist(lapply(input, class)) != "bootSemNeT"))
    {stop("Object input into 'bootSemNeT.obj' is not a 'bootSemNeT' object")}
    
    #Number of input
    len <- length(input)
    
    #Get names of networks
    name <- unique(gsub("Net","",gsub("Summ","",gsub("Meas","",names(input[[1]])))))
    
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

#' Random Walk Simulation in Shiny
#' 
#' @description Simulates random walks over two networks to examine the characteristics
#' of spontaneous spreading activation (see Kenett & Austerweil, 2016)
#' 
#' @param dat List.
#' List of networks
#' 
#' @param nameA Character.
#' Name of network A in \code{dat} list
#' 
#' @param nameB Character.
#' Name of network B in \code{dat} list
#' 
#' @param reps Numeric.
#' Number of repetitions of increments in 10 steps.
#' Defaults to \code{20}
#' 
#' @param steps Numeric.
#' Number of random steps to begin with.
#' Defaults to \code{10}
#' 
#' @param iter Numeric.
#' Number of iterations for each random walk.
#' Defaults to \code{10000}
#' 
#' @param cores Numeric.
#' Number of computer processing cores to use for bootstrapping samples.
#' Defaults to \emph{n} - 1 total number of cores.
#' Set to any number between 1 and maximum amount of cores on your computer
#' 
#' @return A result matrix containing the means and standard deviations for
#' several measures as well as \emph{p}-values for a Mann-Whitney U test
#'
#' @references
#' Kenett, Y. N., & Austerweil, J. L. (2016).
#' Examining search processes in low and high creative individuals with random walks.
#' In \emph{Paper presented at the proceedings of the 38th annual meeting of the cognitive science society}. Austin, TX.
#' Retrieved from: \href{http://alab.psych.wisc.edu/papers/files/Kenett16CreativityRW.pdf}{http://alab.psych.wisc.edu/papers/files/Kenett16CreativityRW.pdf}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> and Yoed Kenett <yoedkenett@gmail.com>
#' 
#' @importFrom stats wilcox.test
#' 
#' @noRd
# Random Walks----
# Updated 14.06.2020
randwalkShiny <- function (dat, nameA, nameB, reps = 20, steps = 10,
                      iter = 10000, cores)
{
    #Missing arguments
    if(missing(cores))
    {cores <- parallel::detectCores() - 1
    }else{cores <- cores}
    
    #get networks
    A <- dat[[nameA]]
    B <- dat[[nameB]]
    
    #number of nodes
    nA <- ncol(A)
    nB <- ncol(B)
    
    #starting step
    start <- steps
    
    #binarize matrices
    A <- binarize(A)
    B <- binarize(B)
    diag(A) <- 0
    diag(B) <- 0
    
    #transition matrices
    TA <- matrix(0,nrow=nA,ncol=nA)
    TB <- matrix(0,nrow=nB,ncol=nB)
    
    degA <- colSums(A)
    degB <- colSums(B)
    
    for(i in 1:nA)
        for(j in 1:nA)
        {TA[i,j] <- A[i,j]/degA[i]}
    
    for(i in 1:nB)
        for(j in 1:nB)
        {TB[i,j] <- B[i,j]/degB[i]}
    
    #distance matrices
    DA <- distance(A)
    DB <- distance(B)
    
    # Random walk function
    rw <- function(steps, nA, nB, TA, TB, DA, DB)
    {
        #initialize matrices
        sim <- numeric(length = 2)
        sim5 <- sim
        uniq <- sim
        results <- vector("numeric",17)
        vnA <- numeric(length = steps)
        vnB <- vnA
        visit <- matrix(NA, nrow = 2, ncol = steps)
        
        permA <- sample(nA, 1)
        permB <- sample(nB, 1)
        
        startingNodeA <- permA
        startingNodeB <- permB
        
        vnA[1] <- permA
        vnB[1] <- permB
        
        for(k in 2:steps)
        {
            cdfA <- cumsum(TA[startingNodeA,])
            cdfB <- cumsum(TB[startingNodeB,])
            
            x <- runif(1)
            
            nA <- which(cdfA > x)[1]
            nB <- which(cdfB > x)[1]
            
            vnA[k] <- nA
            vnB[k] <- nB
            
            startingNodeA <- nA
            startingNodeB <- nB
        }
        
        uA <- unique(vnA)
        uB <- unique(vnB)
        
        a <- uA[length(uA)]
        b <- uB[length(uB)]
        
        simA <- exp(-DA[vnA[1],a])
        simB <- exp(-DB[vnB[1],b])
        
        s5ind <- 5
        su <- min(c(length(uA),length(uB)))
        if(s5ind > su)
        {s5ind <- su}
        
        g5ind <- 5
        if(g5ind > s5ind)
        {g5ind <- s5ind}
        
        simA5 <- exp(-DA[vnA[1],uA[s5ind]])
        simB5 <- exp(-DB[vnB[1],uB[s5ind]])
        
        visit[1,] <- vnA
        visit[2,] <- vnB
        
        sim[1] <- simA
        sim5[1] <- simA5
        uniq[1] <- length(uA)
        sim[2] <- simB
        sim5[2] <- simB5
        uniq[2] <- length(uB)
        
        # Initialize result list
        res <- list()
        res$sim <- sim
        res$sim5 <- sim5
        res$uniq <- uniq
        res$visit <- visit
        
        return(res)
    }
    
    # Initialize steps list
    steps <- seq(start, reps*10 , 10)
    
    step.list <- list()
    
    for(i in 1:length(steps))
    {step.list[[i]] <- as.list(rep(steps[i], iter))}
    
    # Initialize parallelization results
    pb.res <- vector("list", length = length(steps))
    
    # Message that this is for random walk
    message(styletext(styletext("\nRandom Walk Analyses\n", defaults = "underline"), defaults = "bold"))
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c("rw", "steps", "step.list", "nA", "nB", "reps",
                                                 "TA", "TB", "DA", "DB", "pb.res"), envir = environment())
    
    #Let user know analysis is starting
    message("Computing random walks...")
    
    for(i in 1:length(steps))
    {
        message(paste("Repetition ", i, " of ", reps, " (", steps[i], " steps)", sep = ""))
        
        
        pb.res[[i]] <- pbapply::pblapply(step.list[[i]], function(x){rw(x, nA, nB, TA, TB, DA, DB)})
    }
    
    parallel::stopCluster(cl)
    
    # Initialize result matrix
    results <- matrix(NA, nrow = reps, ncol = 17)
    
    colnames(results) <- c("Steps",
                           paste("M.uniq",nameA,sep="."), paste("SD.uniq",nameA,sep="."),
                           paste("M.sim",nameA,sep="."), paste("SD.sim",nameA,sep="."),
                           paste("M.sim5",nameA,sep="."), paste("SD.sim5",nameA,sep="."),
                           paste("M.uniq",nameB,sep="."), paste("SD.uniq",nameB,sep="."),
                           paste("M.sim",nameB,sep="."), paste("SD.sim",nameB,sep="."),
                           paste("M.sim5",nameB,sep="."), paste("SD.sim5",nameB,sep="."),
                           "pu", "ps", "ps5", "g5ind")
    
    # Organized into matrices
    for(i in 1:reps)
    {
        sim <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$sim
            }),
            rbind
        ))
        
        sim5 <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$sim5
            }),
            rbind
        ))
        
        uniq <- t(sapply(
            lapply(pb.res[[i]], function(x)
            {
                x$uniq
            }),
            rbind
        ))
        
        #p-values
        pu <- suppressWarnings(wilcox.test(uniq[,1],uniq[,2])$p.value)
        ps <- suppressWarnings(wilcox.test(sim[,1],sim[,2])$p.value)
        ps5 <- suppressWarnings(wilcox.test(sim5[,1],sim5[,2])$p.value)
        
        #results
        results[i,1] <- steps[i]
        results[i,2] <- mean(uniq[,1])
        results[i,3] <- sd(uniq[,1])
        results[i,4] <- mean(sim[,1])
        results[i,5] <- sd(sim[,1])
        results[i,6] <- mean(sim5[,1])
        results[i,7] <- sd(sim5[,1])
        results[i,8] <- mean(uniq[,2])
        results[i,9] <- sd(uniq[,2])
        results[i,10] <- mean(sim[,2])
        results[i,11] <- sd(sim[,2])
        results[i,12] <- mean(sim5[,2])
        results[i,13] <- sd(sim5[,2])
        results[i,14] <- pu
        results[i,15] <- ps
        results[i,16] <- ps5
        results[i,17] <- 5
    }
    
    # Get directions
    ## pu
    dir.pu <- ifelse(results[,"pu"] < .05,
                     ifelse(results[,2] > results[,8],
                            paste(nameA, ">", nameB),
                            paste(nameB, ">", nameA)),
                     "n.s.")
    
    ## ps
    dir.ps <- ifelse(results[,"ps"] < .05,
                     ifelse(results[,4] > results[,10],
                            paste(nameA, ">", nameB),
                            paste(nameB, ">", nameA)),
                     "n.s.")
    
    ## ps5
    dir.ps5 <- ifelse(results[,"ps5"] < .05,
                      ifelse(results[,6] > results[,12],
                             paste(nameA, ">", nameB),
                             paste(nameB, ">", nameA)),
                      "n.s.")
    
    # Change p-values
    short.results <- round(results[,c("Steps","pu","ps","ps5")], 3)
    short.results <- ifelse(short.results < .001, "< .001", short.results)
    
    # Combine p-values and effect directions
    short.results <- cbind(short.results[,1:2], dir.pu,
                           short.results[,3], dir.ps,
                           short.results[,4], dir.ps5)
    
    # Rename columns
    colnames(short.results) <- c("Steps",
                                 "Unique Nodes (p-value)", "Direction",
                                 "Similarity (p-value)", "Direction",
                                 "5-step Similarlity (p-value)", "Direction")
    # Convert to data frame
    short.results <- as.data.frame(short.results)
    
    # Result list
    res <- list()
    res$long <- results
    res$short <- short.results
    
    return(res)
}
#----

#' Spreading Activation Plot in Shiny
#' 
#' @description Generates a plot depicting spreading activation on a network
#' 
#' @param network Matrix or data frame.
#' A network associated with the spreading activation
#' 
#' @param spreadr.object Data frame.
#' Output from \code{\link[spreadr]{spreadr}}
#' 
#' @param time Numeric.
#' Specific time step
#' 
#' @param size Numeric.
#' Size of plot
#' 
#' @return An animated plot of spreading activation
#' 
#' @references
#' Siew, C. S. (2019).
#' spreadr: An R package to simulate spreading activation in a network.
#' \emph{Behavior Research Methods}, \emph{51}, 910-929.
#' https://doi.org/10.3758/s13428-018-1186-5
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> and Cynthia Siew <cynsiewsq@gmail.com>
#' 
#' @noRd
# Spreading Activation Plot----
# Updated 18.06.2020
spreadrShinyPlot <- function (network, spreadr.output, time, size)
{
    # Reset layout
    layout(matrix(1))
    
    # Get 'spreadr' output
    act <- spreadr.output
    
    # Get specific time
    act <- act[act$time == time,]
    
    # Min-max normalize activation
    if(all(act$activation == 0))
    {
        color <- "white"
        trans <- 255
    }else{
        trans <- ((act$activation - min(act$activation)) / (max(act$activation) - min(act$activation))) * 255
        color = "red"
    }
    
    # Convert size to character
    size <- as.character(size)
    
    # Change node size based on size
    vsize <- switch(size,
                    "500" = 4,
                    "900" = 6,
                    "1400" = 8
                    )
    
    # Plot
    qgraph::qgraph(network, layout = "spring", vTrans = trans,
                   color = color, labels = as.factor(colnames(network)),
                   label.prop = 1, vsize = vsize)
}
#----

#' Animate Networks for Spreading Activation from Shiny
#' 
#' @description Uses \code{\link[qgraph]{qgraph}} and \code{\link[animation]{ani.record}}
#' to animate networks. Accepts only one network animation at a time
#' 
#' @param x Shiny result \code{resultShiny$spreadingActivationPlot}
#' 
#' @param ... Additional arguments for \code{\link[animation]{ani.record}}
#' 
#' @return Plots animated networks using \code{\link[qgraph]{qgraph}} and \code{\link[animation]{ani.record}}
#' 
#' @examples
#' 
#' if(interactive())
#' {SemNeTShiny()}
#' 
#' \dontrun{
#'   plot(resultShiny$spreadingActivationPlot[[1]])
#' }
#' 
#' @references 
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012).
#' qgraph: Network visualizations of relationships in psychometric data.
#' \emph{Journal of Statistical Software}, \emph{48}, 1-18.
#' Retrieved from: http://www.jstatsoft.org/v48/i04/
#' 
#' Siew, C. S. Q. (2019).
#' spreadr: An R package to simulate spreading activation in a network.
#' \emph{Behavior Research Methods}, \emph{51}, 910-929.
#' https://doi.org/10.3758/s13428-018-1186-5
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Animate Graphs----
# Updated 16.06.2020
plot.animateShiny <- function (x, ...)
{
    animation::ani.replay(x, ...)
}
#----
