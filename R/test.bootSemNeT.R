#' Statistical tests for \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @description Computes statistical tests for bootstrapped
#' networks from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param ... Object(s) from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param test Character.
#' Type of statistical test to be used.
#' 
#' \itemize{
#' 
#' \item{\code{"t-test"}}
#' {Computes t-tests for all measures}
#' 
#' \item{\code{"ANOVA"}}
#' {Computes ANOVAs and includes
#' Tukey's HSD for pairwise comparisons (\code{\link{TukeyHSD}})}
#' 
#' \item{\code{"ANCOVA"}}
#' {Computes ANCOVAs that control for the number of nodes
#' and edges in the networks and includes adjusted means
#' and Tukey's HSD for pairwise comparisons (\code{\link{TukeyHSD}})}}
#' 
#' @param measures Character.
#' Network measures to be tested.
#' 
#' \itemize{
#' 
#' \item{\code{\link[SemNeT]{ASPL}}}
#' {Average shortest path length}
#' 
#' \item{\code{\link[SemNeT]{CC}}}
#' {Clustering coefficient}
#' 
#' \item{\code{\link[SemNeT]{Q}}}
#' {Modularity statistic}
#' 
#' }
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
#' @examples
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' two <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' two.result <- bootSemNeT(one, two, prop = .50, iter = 100,
#' sim = "cosine", cores = 2, type = "node", method = "TMFG")
#' }
#' # Compute tests
#' test.bootSemNeT(two.result)
#' 
#' \donttest{
#' # Two-way ANOVA example
#' ## Simulated data
#' hihi <- sim.fluency(50, 500)
#' hilo <- sim.fluency(50, 500)
#' lohi <- sim.fluency(50, 500)
#' lolo <- sim.fluency(50, 500)
#' 
#' ## Create groups
#' groups <- matrix(
#' c("high", "high",
#' "high", "low",
#' "low", "high",
#' "low", "low"
#' ), ncol = 2, byrow = TRUE)
#' 
#' ## Change column names (variable names)
#' colnames(groups) <- c("gf","caq")
#' 
#' ## Run partial bootstrap networks
#' boot.fifty <- bootSemNeT(hihi, hilo, lohi, lolo, prop = .50,
#' type = "node", method = "TMFG", cores = 2, iter = 100)
#' boot.sixty <- bootSemNeT(hihi, hilo, lohi, lolo, prop = .60,
#' type = "node", method = "TMFG", cores = 2, iter = 100)
#' 
#' ## Compute tests
#' test.bootSemNeT(boot.fifty, boot.sixty,
#' test = "ANOVA", formula = "y ~ gf*caq", groups = groups)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils combn
#' 
#' @export
#Test: Bootstrapped Network Statistics----
# Updated 22.06.2021
test.bootSemNeT <- function (...,
                             test = c("ANCOVA", "ANOVA", "t-test"),
                             measures = c("ASPL", "CC", "Q"),
                             formula = NULL, groups = NULL)
{
    #Missing arguments
    if(missing(measures))
    {measures <- c("ASPL", "CC", "Q")
    }else{measures <- match.arg(measures)}
    
    if(missing(test)){
        test <- "t-test"
    }else{test <- match.arg(test)}
    
    #Obtain ... in a list
    input <- list(...)
    
    #Names of groups
    name <- unique(gsub("Net", "", gsub("Summ","",gsub("Meas","",names(input[[1]])))))
    
    #Remove proportion and itetations
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