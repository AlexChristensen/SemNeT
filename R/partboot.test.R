#' Wrapper function for \code{\link[SemNeT]{partboot.test}}
#' 
#' @description Computes statistical tests for partial bootstrapped
#' networks from \code{\link[SemNeT]{partboot}}. Automatically
#' computes \emph{t}-tests (\code{\link{t.test}}) or ANOVA
#' (\code{\link{aov}}) including Tukey's HSD for pairwise comparisons
#' (\code{\link{TukeyHSD}})
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
#' lower (\code{lwr}) and upper (\code{upr}) bounds of the 95% confidence interval,
#' and the adjusted \emph{p}-value (\code{p adj})}
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
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Test: Partial Bootstrapped Network Statistics----
partboot.test <- function (...)
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
                #Compute tests
                test <- partboot.one.test(input[[i]])
                
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
            }
            
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