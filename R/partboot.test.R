#' Statistical tests for \code{\link[SemNeT]{partboot}}
#' 
#' @description Computes statistical tests for partial bootstrapped
#' networks from \code{\link[SemNeT]{partboot}}. Automatically
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
#' \code{\link[SemNeT]{partboot}}
#' 
#' @param ... Object(s) from \code{\link[SemNeT]{partboot}}
#' 
#' @return Returns a list containing the objects:
#' 
#' \item{ASPL}{Test statistics for each percentage of nodes remaining for ASPL}
#' 
#' \item{CC}{Test statistics for each percentage of nodes remaining for CC}
#' 
#' \item{Q}{Test statistics for each percentage of nodes remaining for Q}
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
#' Row names refer to the percentage of nodes remaining in bootstrapped networks
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
#' two.result <- partboot(one, two, percent = .50, iter = 1000,
#' sim = "cosine", cores = 2)
#' }
#' # Compute tests
#' partboot.test(two.result)
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
#' hihi.group <- cbind(rep("high",nrow(hihi)),rep("high",nrow(hihi)))
#' hilo.group <- cbind(rep("high",nrow(hilo)),rep("low",nrow(hilo)))
#' lohi.group <- cbind(rep("low",nrow(lohi)),rep("high",nrow(lohi)))
#' lolo.group <- cbind(rep("low",nrow(lolo)),rep("low",nrow(lolo)))
#' 
#' ## Bind groups into single data frame
#' groups <- rbind(hihi.group,
#'                 hilo.group,
#'                 lohi.group,
#'                 lolo.group)
#' 
#' ## Change column names (variable names)
#' colnames(groups) <- c("gf","caq")
#' 
#' ## Change groups into data frame
#' groups <- as.data.frame(groups)
#' 
#' ## Run partial bootstrap networks
#' boot.fifty <- partboot(hihi, hilo, lohi, lolo, percent = .50)
#' boot.sixty <- partboot(hihi, hilo, lohi, lolo, percent = .60)
#' 
#' ## Compute tests
#' partboot.test(boot.fifty, boot.sixty, formula = "y ~ gf*caq", groups = groups)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Test: Partial Bootstrapped Network Statistics----
partboot.test <- function (..., formula = NULL, groups = NULL)
{
    #Obtain ... in a list
    input <- list(...)
    
    #Names of groups
    name <- unique(gsub("Summ","",gsub("Meas","",names(input[[1]]))))
    
    #Remove percent and iter
    name <- na.omit(gsub("iter",NA,gsub("percent",NA,name)))
    attr(name, "na.action") <- NULL
    
    #Length of groups
    len <- length(name)
    
    #Initialize result list
    res <- list()
    
    #Number of input
    if(length(input)==1)
    {
        res <- partboot.one.test(input[[1]])
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
                test <- partboot.one.test(input[[i]])
                
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
                #Identify percent of nodes remaining
                perc <- input[[i]]$percent
                
                #Compute tests
                test <- partboot.one.test(input[[i]], formula = formula, groups = groups)
                
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
                    aspl[[paste(perc*100,"%",sep="")]]$ANOVA <- test$ASPL$ANOVA[[1]]
                    aspl[[paste(perc*100,"%",sep="")]]$HSD <- test$ASPL$HSD[[1]]
                    
                    #CC
                    cc[[paste(perc*100,"%",sep="")]] <- test$CC$ANOVA[[1]]
                    cc[[paste(perc*100,"%",sep="")]] <- test$CC$HSD[[1]]
                    
                    #Q
                    q[[paste(perc*100,"%",sep="")]] <- test$Q$ANOVA[[1]]
                    q[[paste(perc*100,"%",sep="")]] <- test$Q$HSD[[1]]
                    
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