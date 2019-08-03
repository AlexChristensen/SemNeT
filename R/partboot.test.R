#' Test for partboot
#' 
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' 
#' @param ... Objects from \code{\link[SemNeT]{partboot}}
#' 
#' @param groups Character.
#' Labels for groups in the order they were entered
#' in \code{\link[SemNeT]{partboot}} (e.g.,
#' \code{data} = first,
#' \code{paired} = second)
#' 
#' @return Returns a list containing the objects:
#' 
#' \item{ASPL}{Test statistics for each percentage of nodes remaining for ASPL}
#' 
#' \item{CC}{Test statistics for each percentage of nodes remaining for CC}
#' 
#' \item{Q}{Test statistics for each percentage of nodes remaining for Q}
#' 
#' The matrix in each object has the following columns:
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
#' @examples
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' two <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' two.result <- partboot(data = one, paired = two, percent = .50, iter = 1000,
#' sim = "cosine", cores = 2)
#' }
#' # Compute tests
#' partboot.test(two.result, groups = c("One", "Two"))
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats t.test
#' 
#' @export
#Test: Partial Bootstrapped Network Statistics----
partboot.test <- function (..., groups = NULL)
{
    #Obtain ... in a list
    input <- list(...)
    
    #Get names of networks
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    #Get groups
    if(is.null(groups))
    {groups <- c("d","p")}

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
    
    #Error there are no paired samples
    if(all(!paired))
    {stop("Single samples cannot be tested. Use 'randnet.test' for single samples")}
    
    #Identify which samples can be tested
    if(len != length(which(paired)))
    {
        if(length(which(!paired))==1)
        {
            message(
            paste("Input",paste("'",name[which(!paired)],"'", sep = ""),
                  "did not have paired samples. Significance tests were not computed for this input.")
            )
        }else if(length(which(!paired))==2)
        {
            message(
            paste("Inputs",paste("'",name[which(!paired)],"'", sep = "", collapse = " and "),
                  "did not have paired samples. Significance tests were not computed for these inputs.")
            )
        }else{
            
            start <- paste("'",name[which(!paired)][-length(which(!paired))],"'",sep = "", collapse = ", ")
            end <- paste(", and '",name[which(!paired)][length(which(!paired))],"'",sep="")
            
            message(
                paste("Inputs",paste(start,end, sep = ""),
                  "did not have paired samples. Significance tests were not computed for these inputs.")
            )
        }
    }
    
    #Grab testing samples
    ##Target samples
    test.samps <- list()
    target.samps <- which(paired)
    
    for(i in 1:length(which(paired)))
    {test.samps[[i]] <- input[[target.samps[i]]]}
    
    #New length
    new.len <- length(test.samps)
    
    #Identify percent of nodes remaining
    perc <- vector("numeric", length = new.len)
    
    for(i in 1:new.len)
    {perc[i] <- test.samps[[i]]$percent}
    
    ############################
    #### SIGNIFICANCE TESTS ####
    ############################
    
    #Initialize list for tests
    tests <- list()
    
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
    aspl <- matrix(NA, nrow = new.len, ncol = 7)
    row.names(aspl) <- paste(perc*100,"%",sep="")
    colnames(aspl) <- c("t-statistic", "df", "p-value", "d",
                        "CI95.lower", "CI95.upper","Direction")
    
    for(i in 1:new.len)
    {
        #Target sample
        target <- test.samps[[i]]
        
        #ASPL
        data.aspl <- target$dataMeas["ASPL",]
        paired.aspl <- target$pairedMeas["ASPL",]
        
        #t-test
        test <- t.test(data.aspl, paired.aspl, var.equal = TRUE)
        
        #Input results into table
        aspl[paste(perc[i]*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        aspl[paste(perc[i]*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        aspl[paste(perc[i]*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        aspl[paste(perc[i]*100,"%",sep=""),4] <- round(as.numeric(d(data.aspl,paired.aspl)),3)
        aspl[paste(perc[i]*100,"%",sep=""),5] <- round(as.numeric(test$conf.int[1]),3)
        aspl[paste(perc[i]*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {aspl[paste(perc[i]*100,"%",sep=""),7] <- "n.s."
        }else{
            aspl[paste(perc[i]*100,"%",sep=""),7] <- ifelse(sign(test$statistic)==1,
                                                            paste(groups[1],">",groups[2],sep=" "),
                                                            paste(groups[2],">",groups[1],sep=" ")
            )
        }
    }
    
    ##CC Tests
    cc <- matrix(NA, nrow = new.len, ncol = 7)
    row.names(cc) <- paste(perc*100,"%",sep="")
    colnames(cc) <- c("t-statistic", "df", "p-value", "d",
                        "CI95.lower", "CI95.upper","Direction")
    
    for(i in 1:new.len)
    {
        #Target sample
        target <- test.samps[[i]]
        
        #CC
        data.cc <- target$dataMeas["CC",]
        paired.cc <- target$pairedMeas["CC",]
        
        #t-test
        test <- t.test(data.cc, paired.cc, var.equal = TRUE)
        
        #Input results into table
        cc[paste(perc[i]*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        cc[paste(perc[i]*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        cc[paste(perc[i]*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        cc[paste(perc[i]*100,"%",sep=""),4] <- round(as.numeric(d(data.cc,paired.cc)),3)
        cc[paste(perc[i]*100,"%",sep=""),5] <- round(as.numeric(test$conf.int[1]),3)
        cc[paste(perc[i]*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {cc[paste(perc[i]*100,"%",sep=""),7] <- "n.s."
        }else{
            cc[paste(perc[i]*100,"%",sep=""),7] <- ifelse(sign(test$statistic)==1,
                                                            paste(groups[1],">",groups[2],sep=" "),
                                                            paste(groups[2],">",groups[1],sep=" ")
            )
        }
    }
    
    ##Q Tests
    q <- matrix(NA, nrow = new.len, ncol = 7)
    row.names(q) <- paste(perc*100,"%",sep="")
    colnames(q) <- c("t-statistic", "df", "p-value", "d",
                      "CI95.lower", "CI95.upper","Direction")
    
    for(i in 1:new.len)
    {
        #Target sample
        target <- test.samps[[i]]
        
        #Q
        data.q <- target$dataMeas["Q",]
        paired.q <- target$pairedMeas["Q",]
        
        #t-test
        test <- t.test(data.q, paired.q, var.equal = TRUE)
        
        #Input results into table
        q[paste(perc[i]*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        q[paste(perc[i]*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        q[paste(perc[i]*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        q[paste(perc[i]*100,"%",sep=""),4] <- round(as.numeric(d(data.q,paired.q)),3)
        q[paste(perc[i]*100,"%",sep=""),5] <- round(as.numeric(test$conf.int[1]),3)
        q[paste(perc[i]*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {q[paste(perc[i]*100,"%",sep=""),7] <- "n.s."
        }else{
            q[paste(perc[i]*100,"%",sep=""),7] <- ifelse(sign(test$statistic)==1,
                                                            paste(groups[1],">",groups[2],sep=" "),
                                                            paste(groups[2],">",groups[1],sep=" ")
            )
        }
    }
    
    #Input results into list
    tests$ASPL <- as.data.frame(aspl)
    tests$CC <- as.data.frame(cc)
    tests$Q <- as.data.frame(q)
    
    return(tests)
}
#----