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
        {datalist[[i]] <- NetworkToolbox::binarize(datalist[[i]])}
        
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
#' # Compute networks using NetworkToolbox
#' net1 <- NetworkToolbox::TMFG(cos1)$A
#' net2 <- NetworkToolbox::TMFG(cos2)$A
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
    
    #Message for begin random networks
    message("Generating random networks...", appendLF = FALSE)
    
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
        sig.mat["ASPL",paste(name[i], "(p-value)")] <- round(dnorm(z.aspl, mean = sig.mat["ASPL","M.rand"], sd = sig.mat["ASPL","SD.rand"]),4)
        ##CC
        z.cc <- (meas["CC"] - sig.mat["CC","M.rand"]) / sig.mat["CC","SD.rand"]
        sig.mat["CC",paste(name[i], "(p-value)")] <- round(dnorm(z.cc, mean = sig.mat["CC","M.rand"], sd = sig.mat["CC","SD.rand"]),4)
        ##Q
        z.q <- (meas["Q"] - sig.mat["Q","M.rand"]) / sig.mat["Q","SD.rand"]
        sig.mat["Q",paste(name[i], "(p-value)")] <- round(dnorm(z.q, mean = sig.mat["Q","M.rand"], sd = sig.mat["Q","SD.rand"]),4)
        
        #Insert results
        res[[i]] <- sig.mat
    }
    
    return(res)
    
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
#Updated 05.04.2020
bootSemNeTShiny <- function (dat, method = c("CN", "NRW", "PF", "TMFG"),
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
    
    #Create list of input
    datalist <- dat
    
    #Number of nodes in full data
    full <- unique(unlist(lapply(datalist,ncol)))
    
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
                
                #Error on method
                if(method != "TMFG")
                {stop(paste("bootSemNeT(): Node-wise bootstrap is not supported with the", method, "method", sep = " "))}
                
                #Randomly sample nodes
                rand <- sample(full, (full*prop), replace=FALSE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[,rand]
            }else if(type == "case")
            {
                #Error on method = "TMFG"
                if(method == "TMFG")
                {stop("bootSemNeT(): Case-wise bootstrap is not supported with the TMFG method")}
                
                #Randomly sample nodes
                rand <- sample(nrow(get(name[i], envir = environment())),
                               nrow(get(name[i], envir = environment())),
                               replace=TRUE)
                
                #Input into data list
                new[[count]] <- get(name[i], envir = environment())[rand,]
            }
            
            if(count == iter)
            {break}
        }
        
        #Insert data list
        assign(paste("dl.",name[i],sep=""),new)
    }
    
    #Let user know data generation is finished
    message("done\n")
    
    ############################
    #### COMPUTE SIMILARITY ####
    ############################
    
    if(method == "TMFG")
    {
        #Let user know simliairity is being computed
        message("Computing similarity measures...\n", appendLF = FALSE)
        
        #Parallel processing
        cl <- parallel::makeCluster(cores)
        
        #Export datalist
        parallel::clusterExport(cl = cl, varlist = paste("dl.",name,sep=""), envir = environment())
        
        for(i in 1:length(name))
        {
            #Compute similarity
            newSim <- pbapply::pblapply(X = get(paste("dl.",name[i],sep=""), envir = environment()),
                                        cl = cl,
                                        FUN = similarity,
                                        method = sim)
            
            #Insert similarity list
            assign(paste("sim.",name[i],sep=""),newSim)
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
    
    #Assign network function
    NET.FUNC <- switch(method,
                       TMFG = NetworkToolbox::TMFG,
                       CN = CN,
                       NRW = NRW,
                       PF = PF)
    
    #Let user know networks are being computed
    message("Estimating networks...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = c(paste("sim.",name,sep=""), "NET.FUNC"),
                            envir = environment())
    
    for(i in 1:length(name))
    {
        #Compute networks
        newNet <- pbapply::pblapply(X = get(paste("sim.",name[i],sep=""), envir = environment()),
                                    cl = cl,
                                    FUN = NET.FUNC)
        
        #Insert network list
        assign(paste("net.",name[i],sep=""),newNet)
    }
    
    #Stop Cluster
    parallel::stopCluster(cl)
    
    #Grab networks
    for(i in 1:length(name))
    {
        #Initialize semantic network list
        Semnet <- list()
        
        #Loop through networks
        if(method == "TMFG")
        {
            for(j in 1:iter)
            {Semnet[[j]] <- get(paste("net.",name[i],sep=""), envir = environment())[[j]]$A}
        }else{
            for(j in 1:iter)
            {Semnet[[j]] <- get(paste("net.",name[i],sep=""), envir = environment())[[j]]}
        }
        
        #Insert semantic network list
        assign(paste("Semnet.",name[i],sep=""),Semnet)
    }
    
    ##############################
    #### COMPUTING STATISTICS ####
    ##############################
    
    #Let user know networks are being computed
    message("Computing statistics...\n", appendLF = FALSE)
    
    #Parallel processing
    cl <- parallel::makeCluster(cores)
    
    #Export datalist
    parallel::clusterExport(cl = cl, varlist = paste("Semnet.",name[i],sep=""), envir = environment())
    
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
#Test: Partial Bootstrapped Network Statistics----
# Updated 05.04.2020
test.bootSemNeTShiny <- function (input, formula = NULL, groups = NULL)
{
    #Names of groups
    name <- unique(gsub("Summ","",gsub("Meas","",names(input[[1]]))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Length of groups
    len <- length(name)
    
    #Initialize result list
    res <- list()
    
    #Number of input
    if(length(input)==1)
    {
        res <- boot.one.testShiny(input[[1]])
    }else{
        
        if(len == 2)
        {
            #Initialize measure lists
            aspl <- list()
            cc <- list()
            q <- list()
            
            #Initialize row names
            aspl.row <- vector("character", length = length(input))
            cc.row <- vector("character", length = length(input))
            q.row <- vector("character", length = length(input))
            
            #Loop through input
            for(i in 1:length(input))
            {
                #Compute tests
                test <- boot.one.testShiny(input[[i]])
                
                #ASPL
                aspl[[i]] <- test$ASPL
                aspl.row[i] <- row.names(aspl[[i]])
                aspl.col <- colnames(aspl[[i]])
                
                #CC
                cc[[i]] <- test$CC
                cc.row[i] <- row.names(cc[[i]])
                cc.col <- colnames(cc[[i]])
                
                #Q
                q[[i]] <- test$Q
                q.row[i] <- row.names(q[[i]])
                q.col <- colnames(q[[i]])
            }
            
            #Convert to matrices
            aspl <- t(sapply(aspl, rbind))
            cc <- t(sapply(cc, rbind))
            q <- t(sapply(q, rbind))
            
            #Name rows
            row.names(aspl) <- aspl.row
            row.names(cc) <- cc.row
            row.names(q) <- q.row
            
            #Name columns
            colnames(aspl) <- aspl.col
            colnames(cc) <- cc.col
            colnames(q) <- q.col
            
            #Input results
            res$ASPL <- as.data.frame(aspl)
            res$CC <- as.data.frame(cc)
            res$Q <- as.data.frame(q)
            
        }else{
            
            #Initialize measure lists
            aspl <- list()
            cc <- list()
            q <- list()
            hsd <- list()
            
            #Initialize row names
            aspl.row <- vector("character", length = length(input))
            cc.row <- vector("character", length = length(input))
            q.row <- vector("character", length = length(input))
            
            #Loop through input
            for(i in 1:length(input))
            {
                #Identify proportion of nodes remaining
                perc <- input[[i]]$prop
                
                #Compute tests
                test <- boot.one.testShiny(input[[i]], formula = formula, groups = groups)
                
                if(is.null(formula))
                {
                    #ASPL
                    aspl[[i]] <- test$ASPL$ANOVA
                    aspl.row[i] <- row.names(aspl[[i]])
                    aspl.col <- colnames(aspl[[i]])
                    hsd$ASPL[[aspl.row[i]]] <- test$ASPL$HSD
                    
                    #CC
                    cc[[i]] <- test$CC$ANOVA
                    cc.row[i] <- row.names(cc[[i]])
                    cc.col <- colnames(cc[[i]])
                    hsd$CC[[cc.row[i]]] <- test$CC$HSD
                    
                    #Q
                    q[[i]] <- test$Q$ANOVA
                    q.row[i] <- row.names(q[[i]])
                    q.col <- colnames(q[[i]])
                    hsd$Q[[q.row[i]]] <- test$Q$HSD
                }else{
                    #ASPL
                    aspl[[sprintf("%1.2f", perc)]]$ANOVA <- test$ASPL$ANOVA[[1]]
                    aspl[[sprintf("%1.2f", perc)]]$HSD <- test$ASPL$HSD[[1]]
                    
                    #CC
                    cc[[sprintf("%1.2f", perc)]] <- test$CC$ANOVA[[1]]
                    cc[[sprintf("%1.2f", perc)]] <- test$CC$HSD[[1]]
                    
                    #Q
                    q[[sprintf("%1.2f", perc)]] <- test$Q$ANOVA[[1]]
                    q[[sprintf("%1.2f", perc)]] <- test$Q$HSD[[1]]
                    
                    hsd <- NULL
                }
            }
            
            if(is.null(formula))
            {
                #Convert to matrices
                aspl <- t(sapply(aspl, round, 4))
                cc <- t(sapply(cc, round, 4))
                q <- t(sapply(q, round, 4))
                
                #Name rows
                row.names(aspl) <- aspl.row
                row.names(cc) <- cc.row
                row.names(q) <- q.row
                
                #Name columns
                colnames(aspl) <- aspl.col
                colnames(cc) <- cc.col
                colnames(q) <- q.col
            }
            
            #Input results
            res$ASPL <- aspl
            res$CC <- cc
            res$Q <- q
            res$HSD <- hsd
        }
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
# Updated 05.04.2020
boot.one.testShiny <- function (bootSemNeT.obj, formula = NULL, groups = NULL)
{
    #Check for data if formula is not NULL
    if(!is.null(formula))
    {
        if(!exists("groups"))
        {stop("'groups' argument is NULL when 'formula' argument is not. Please input groups.")}
    }
    
    #Get names of networks
    name <- unique(gsub("Summ","",gsub("Meas","",names(bootSemNeT.obj))))
    
    #Remove proportion and iter
    name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
    attr(name, "na.action") <- NULL
    
    #Number of input
    len <- length(name)
    
    #Error there are no paired samples
    if(len < 2)
    {stop("Single samples cannot be tested. Use 'randnet.test' for single samples")}
    
    #Identify prop of nodes remaining
    perc <- bootSemNeT.obj$prop
    
    if(is.null(perc))
    {perc <- 1.00}
    
    #Identify iterations
    iter <- bootSemNeT.obj$iter
    
    ############################
    #### SIGNIFICANCE TESTS ####
    ############################
    
    #t-test
    if(len == 2)
    {
        ##Function for Cohen's d
        d <- function(samp1,samp2)
        {
            samp1 <- as.vector(samp1)
            samp2 <- as.vector(samp2)
            
            num <- (mean(samp2)-mean(samp1))
            denom <- sqrt(((sd(samp1)^2)+(sd(samp2)^2))/2)
            
            cohensd <- abs(num/denom)
            
            return(cohensd)
        }
        
        ##ASPL Tests
        aspl <- matrix(NA, nrow = 1, ncol = 8)
        row.names(aspl) <- sprintf("%1.2f", perc)
        colnames(aspl) <- c("t-statistic", "df", "p-value", "d", "Difference",
                            "CI95.lower", "CI95.upper","Direction")
        #ASPL
        one.aspl <- bootSemNeT.obj[[paste(name[1],"Meas",sep="")]]["ASPL",]
        two.aspl <- bootSemNeT.obj[[paste(name[2],"Meas",sep="")]]["ASPL",]
        
        #t-test
        test <- t.test(one.aspl, two.aspl, var.equal = TRUE)
        
        #Input results into table
        aspl[sprintf("%1.2f", perc),1] <- round(as.numeric(test$statistic),3)
        aspl[sprintf("%1.2f", perc),2] <- round(as.numeric(test$parameter),3)
        aspl[sprintf("%1.2f", perc),3] <- round(as.numeric(test$p.value),3)
        aspl[sprintf("%1.2f", perc),4] <- round(as.numeric(d(one.aspl,two.aspl)),3)
        aspl[sprintf("%1.2f", perc),5] <- round(as.numeric(mean(one.aspl)-mean(two.aspl)),3)
        aspl[sprintf("%1.2f", perc),6] <- round(as.numeric(test$conf.int[1]),3)
        aspl[sprintf("%1.2f", perc),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {aspl[sprintf("%1.2f", perc),8] <- "n.s."
        }else{
            aspl[sprintf("%1.2f", perc),8] <- ifelse(sign(test$statistic)==1,
                                                         paste(name[1],">",name[2],sep=" "),
                                                         paste(name[2],">",name[1],sep=" ")
            )
        }
        
        ##CC Tests
        cc <- matrix(NA, nrow = 1, ncol = 8)
        row.names(cc) <- sprintf("%1.2f", perc)
        colnames(cc) <- c("t-statistic", "df", "p-value", "d", "Difference",
                          "CI95.lower", "CI95.upper","Direction")
        #CC
        one.cc <- bootSemNeT.obj[[paste(name[1],"Meas",sep="")]]["CC",]
        two.cc <- bootSemNeT.obj[[paste(name[2],"Meas",sep="")]]["CC",]
        
        #t-test
        test <- t.test(one.cc, two.cc, var.equal = TRUE)
        
        #Input results into table
        cc[sprintf("%1.2f", perc),1] <- round(as.numeric(test$statistic),3)
        cc[sprintf("%1.2f", perc),2] <- round(as.numeric(test$parameter),3)
        cc[sprintf("%1.2f", perc),3] <- round(as.numeric(test$p.value),3)
        cc[sprintf("%1.2f", perc),4] <- round(as.numeric(d(one.cc,two.cc)),3)
        cc[sprintf("%1.2f", perc),5] <- round(as.numeric(mean(one.cc)-mean(two.cc)),3)
        cc[sprintf("%1.2f", perc),6] <- round(as.numeric(test$conf.int[1]),3)
        cc[sprintf("%1.2f", perc),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {cc[sprintf("%1.2f", perc),8] <- "n.s."
        }else{
            cc[sprintf("%1.2f", perc),8] <- ifelse(sign(test$statistic)==1,
                                                       paste(name[1],">",name[2],sep=" "),
                                                       paste(name[2],">",name[1],sep=" ")
            )
        }
        
        ##Q Tests
        q <- matrix(NA, nrow = 1, ncol = 8)
        row.names(q) <- sprintf("%1.2f", perc)
        colnames(q) <- c("t-statistic", "df", "p-value", "d", "Difference",
                         "CI95.lower", "CI95.upper","Direction")
        #Q
        one.q <- bootSemNeT.obj[[paste(name[1],"Meas",sep="")]]["Q",]
        two.q <- bootSemNeT.obj[[paste(name[2],"Meas",sep="")]]["Q",]
        
        #t-test
        test <- t.test(one.q, two.q, var.equal = TRUE)
        
        #Input results into table
        q[sprintf("%1.2f", perc),1] <- round(as.numeric(test$statistic),3)
        q[sprintf("%1.2f", perc),2] <- round(as.numeric(test$parameter),3)
        q[sprintf("%1.2f", perc),3] <- round(as.numeric(test$p.value),3)
        q[sprintf("%1.2f", perc),4] <- round(as.numeric(d(one.q,two.q)),3)
        q[sprintf("%1.2f", perc),5] <- round(as.numeric(mean(one.q)-mean(two.q)),3)
        q[sprintf("%1.2f", perc),6] <- round(as.numeric(test$conf.int[1]),3)
        q[sprintf("%1.2f", perc),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {q[sprintf("%1.2f", perc),8] <- "n.s."
        }else{
            q[sprintf("%1.2f", perc),8] <- ifelse(sign(test$statistic)==1,
                                                      paste(name[1],">",name[2],sep=" "),
                                                      paste(name[2],">",name[1],sep=" ")
            )
        }
        
        #Input results into list
        tests <- list()
        tests$ASPL <- as.data.frame(aspl, stringsAsFactors = FALSE)
        tests$CC <- as.data.frame(cc, stringsAsFactors = FALSE)
        tests$Q <- as.data.frame(q, stringsAsFactors = FALSE)
        
    }else{ #ANOVA
        
        ##Function for partial eta squared
        partial.eta <- function(ESS, TSS)
        {
            p.e <- ESS/TSS
            
            return(p.e)
        }
        
        ##ASPL Tests
        if(is.null(formula))
        {
            aspl <- matrix(NA, nrow = 1, ncol = 5)
            row.names(aspl) <- sprintf("%1.2f", perc)
            colnames(aspl) <- c("F-statistic", "group.df", "residual.df", "p-value", "p.eta.sq")
        }else{
            aspl <- list()
            hsd <- list()
        }
        
        #Initialize group object
        new.aspl <- vector("numeric", length = iter)
        
        #ASPL
        for(i in 1:len)
        {
            #Insert ASPL values
            new.aspl <- bootSemNeT.obj[[paste(name[i],"Meas",sep="")]]["ASPL",]
            
            #Initialize matrix
            mat <- cbind(rep(name[i], length(new.aspl)),new.aspl)
            
            if(i != 1)
            {new.mat <- rbind(new.mat,mat)
            }else{new.mat <- mat}
        }
        
        #Convert to data frame
        aov.obj <- as.data.frame(new.mat, stringsAsFactors = FALSE)
        colnames(aov.obj) <- c("Group", "Measure")
        aov.obj$Group <- as.factor(as.character(aov.obj$Group))
        aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
        
        # Check for groups
        if(!is.null(groups))
        {
            aov.obj <- as.data.frame(cbind(aov.obj,groups), stringsAsFactors = FALSE)
            colnames(aov.obj) <- c("Group", "Measure", colnames(groups))
            aov.obj$Group <- as.factor(as.character(aov.obj$Group))
            aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
            
            for(g in 1:ncol(groups))
            {aov.obj[,(2+g)] <- as.factor(as.character(aov.obj[,(2+g)]))}
        }
        
        #ANOVA
        if(!is.null(formula))
        {
            test <- aov(as.formula(gsub("y", "Measure", formula)), data = aov.obj)
            aspl[[sprintf("%1.2f", perc)]] <- summary(test)[[1]]
            hsd[[sprintf("%1.2f", perc)]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            aspl[sprintf("%1.2f", perc),"F-statistic"] <- round(test.summ$`F value`[1],3)
            aspl[sprintf("%1.2f", perc),"group.df"] <- test.summ$Df[1]
            aspl[sprintf("%1.2f", perc),"residual.df"] <- test.summ$Df[2]
            aspl[sprintf("%1.2f", perc),"p-value"] <- test.summ$`Pr(>F)`[1]
            aspl[sprintf("%1.2f", perc),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
            #Tukey's HSD
            if(test.summ$`Pr(>F)`[1] < .05)
            {hsd <- TukeyHSD(test)$Group
            }else{hsd <- "ANOVA was not significant"}
        }
        
        #List for ASPL
        ASPL <- list()
        ASPL$ANOVA <- aspl
        ASPL$HSD <- hsd
        
        ##CC Tests
        if(is.null(formula))
        {
            cc <- matrix(NA, nrow = 1, ncol = 5)
            row.names(cc) <- sprintf("%1.2f", perc)
            colnames(cc) <- c("F-statistic", "group.df", "residual.df", "p-value", "p.eta.sq")
        }else{
            cc <- list()
            hsd <- list()
        }
        
        #Initialize group object
        new.cc <- vector("numeric", length = iter)
        
        #CC
        for(i in 1:len)
        {
            #Insert CC values
            new.cc <- bootSemNeT.obj[[paste(name[i],"Meas",sep="")]]["CC",]
            
            #Initialize matrix
            mat <- cbind(rep(name[i], length(new.cc)),new.cc)
            
            if(i != 1)
            {new.mat <- rbind(new.mat,mat)
            }else{new.mat <- mat}
        }
        
        #Convert to data frame
        aov.obj <- as.data.frame(new.mat, stringsAsFactors = FALSE)
        colnames(aov.obj) <- c("Group", "Measure")
        aov.obj$Group <- as.factor(as.character(aov.obj$Group))
        aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
        
        # Check for groups
        if(!is.null(groups))
        {
            aov.obj <- as.data.frame(cbind(aov.obj,groups), stringsAsFactors = FALSE)
            colnames(aov.obj) <- c("Group", "Measure", colnames(groups))
            aov.obj$Group <- as.factor(as.character(aov.obj$Group))
            aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
            
            for(g in 1:ncol(groups))
            {aov.obj[,(2+g)] <- as.factor(as.character(aov.obj[,(2+g)]))}
        }
        
        #ANOVA
        if(!is.null(formula))
        {
            test <- aov(as.formula(gsub("y", "Measure", formula)), data = aov.obj)
            cc[[sprintf("%1.2f", perc)]] <- summary(test)[[1]]
            hsd[[sprintf("%1.2f", perc)]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            cc[sprintf("%1.2f", perc),"F-statistic"] <- round(test.summ$`F value`[1],3)
            cc[sprintf("%1.2f", perc),"group.df"] <- test.summ$Df[1]
            cc[sprintf("%1.2f", perc),"residual.df"] <- test.summ$Df[2]
            cc[sprintf("%1.2f", perc),"p-value"] <- test.summ$`Pr(>F)`[1]
            cc[sprintf("%1.2f", perc),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
            #Tukey's HSD
            if(test.summ$`Pr(>F)`[1] < .05)
            {hsd <- TukeyHSD(test)$Group
            }else{hsd <- "ANOVA was not significant"}
        }
        
        #List for CC
        CC <- list()
        CC$ANOVA <- cc
        CC$HSD <- hsd
        
        ##Q Tests
        if(is.null(formula))
        {
            q <- matrix(NA, nrow = 1, ncol = 5)
            row.names(q) <- sprintf("%1.2f", perc)
            colnames(q) <- c("F-statistic", "group.df", "residual.df", "p-value", "p.eta.sq")
        }else{
            q <- list()
            hsd <- list()
        }
        
        #Initialize group object
        new.q <- vector("numeric", length = iter)
        
        #Q
        for(i in 1:len)
        {
            #Insert Q values
            new.q <- bootSemNeT.obj[[paste(name[i],"Meas",sep="")]]["Q",]
            
            #Initialize matrix
            mat <- cbind(rep(name[i], length(new.q)),new.q)
            
            if(i != 1)
            {new.mat <- rbind(new.mat,mat)
            }else{new.mat <- mat}
        }
        
        #Convert to data frame
        aov.obj <- as.data.frame(new.mat, stringsAsFactors = FALSE)
        colnames(aov.obj) <- c("Group", "Measure")
        aov.obj$Group <- as.factor(as.character(aov.obj$Group))
        aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
        
        # Check for groups
        if(!is.null(groups))
        {
            aov.obj <- as.data.frame(cbind(aov.obj,groups), stringsAsFactors = FALSE)
            colnames(aov.obj) <- c("Group", "Measure", colnames(groups))
            aov.obj$Group <- as.factor(as.character(aov.obj$Group))
            aov.obj$Measure <- as.numeric(as.character(aov.obj$Measure))
            
            for(g in 1:ncol(groups))
            {aov.obj[,(2+g)] <- as.factor(as.character(aov.obj[,(2+g)]))}
        }
        
        #ANOVA
        if(!is.null(formula))
        {
            test <- aov(as.formula(gsub("y", "Measure", formula)), data = aov.obj)
            q[[sprintf("%1.2f", perc)]] <- summary(test)[[1]]
            hsd[[sprintf("%1.2f", perc)]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            q[sprintf("%1.2f", perc),"F-statistic"] <- round(test.summ$`F value`[1],3)
            q[sprintf("%1.2f", perc),"group.df"] <- test.summ$Df[1]
            q[sprintf("%1.2f", perc),"residual.df"] <- test.summ$Df[2]
            q[sprintf("%1.2f", perc),"p-value"] <- test.summ$`Pr(>F)`[1]
            q[sprintf("%1.2f", perc),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
            #Tukey's HSD
            if(test.summ$`Pr(>F)`[1] < .05)
            {hsd <- TukeyHSD(test)$Group
            }else{hsd <- "ANOVA was not significant"}
        }
        
        #List for Q
        Q <- list()
        Q$ANOVA <- q
        Q$HSD <- hsd
        
        #Input results into list
        tests <- list()
        tests$ASPL <- ASPL
        tests$CC <- CC
        tests$Q <- Q
    }
    
    # Band-aid fix for case bootstrap (instead of node)
    if(bootSemNeT.obj$type == "case")
    {
        tests <- t(sapply(tests,as.data.frame))
        
        tests[,-which(colnames(tests) == "Direction")] <- apply(tests[,-which(colnames(tests) == "Direction")],2,as.numeric)
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