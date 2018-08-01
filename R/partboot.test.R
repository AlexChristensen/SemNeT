#' Test for partboot
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' @param object An object from \link[SemNeT]{partboot}
#' @return Returns test statistics for specified measures
#' @examples
#' #finalize rmatA
#' finalCmat <- finalize(convmat)
#' #finalize rmatB
#' finalRmat <- finalize(rmat)
#'
#' #equate rmatA and rmatB
#' eq1 <- equate(finalCmat,finalRmat)
#' 
#' #obtain respective equated response matrices
#' eqCmat <- eq1$rmatA
#' eqRmat <- eq1$rmatB
#' 
#' \donttest{
#' results <- partboot(eqCmat, eqRmat, corr = "cosine", cores = 4)
#' 
#' partboot.test(results, paired = TRUE, labels = labs)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom stats t.test
#' @export
#Test: Partial Bootstrapped Network Statistical----
partboot.test <- function (object)
{
    groups <- length(object)
    
    if(groups==2)
    {
        names <- ls(object$bootDataMeas)
        
        if("randASPL" %in% names)
        {names <- names[-which(names=="randASPL")]}
        
        if("randCC" %in% names)
        {names <- names[-which(names=="randCC")]}
        
        test.table <- matrix(0, nrow=length(names),ncol=7)
        test.table <- as.data.frame(test.table)
        
        row.names(test.table) <- names
        colnames(test.table) <- c("t-stat","df","p-value","Cohen's d","95CI.lower","95CI.upper","Paired Group")
        
        d <- function(samp1,samp2)
        {
            samp1 <- as.vector(samp1)
            samp2 <- as.vector(samp2)
            
            num <- (mean(samp2)-mean(samp1))
            denom <- sqrt(((sd(samp1)^2)+(sd(samp2)^2))/2)
            
            cohensd <- abs(num/denom)
            
            return(cohensd)
        }
        
        if("ASPL" %in% names)
        {
            test <- t.test(object$bootDataMeas$ASPL,object$bootPairedMeas$ASPL,var.equal=TRUE)
            
            test.table["ASPL",1] <- as.numeric(test$statistic)
            test.table["ASPL",2] <- as.numeric(test$parameter)
            test.table["ASPL",3] <- as.numeric(test$p.value)
            test.table["ASPL",4] <- as.numeric(d(object$bootDataMeas$ASPL,object$bootPairedMeas$ASPL))
            test.table["ASPL",5] <- as.numeric(test$conf.int[1])
            test.table["ASPL",6] <- as.numeric(test$conf.int[2])
            test.table["ASPL",7] <- ifelse(sign(test$statistic)==1,"<",">")
        }
        
        if("CC" %in% names)
        {
            test <- t.test(object$bootDataMeas$CC,object$bootPairedMeas$CC,var.equal=TRUE)
            
            test.table["CC",1] <- as.numeric(test$statistic)
            test.table["CC",2] <- as.numeric(test$parameter)
            test.table["CC",3] <- as.numeric(test$p.value)
            test.table["CC",4] <- as.numeric(d(object$bootDataMeas$CC,object$bootPairedMeas$CC))
            test.table["CC",5] <- as.numeric(test$conf.int[1])
            test.table["CC",6] <- as.numeric(test$conf.int[2])
            test.table["CC",7] <- ifelse(sign(test$statistic)==1,"<",">")
        }
        
        if("MNS" %in% names)
        {
            test <- t.test(object$bootDataMeas$MNS,object$bootPairedMeas$MNS,var.equal=TRUE)
            
            test.table["MNS",1] <- as.numeric(test$statistic)
            test.table["MNS",2] <- as.numeric(test$parameter)
            test.table["MNS",3] <- as.numeric(test$p.value)
            test.table["MNS",4] <- as.numeric(d(object$bootDataMeas$MNS,object$bootPairedMeas$MNS))
            test.table["MNS",5] <- as.numeric(test$conf.int[1])
            test.table["MNS",6] <- as.numeric(test$conf.int[2])
            test.table["MNS",7] <- ifelse(sign(test$statistic)==1,"<",">")
        }
        
        if("Q" %in% names)
        {
            test <- t.test(object$bootDataMeas$Q,object$bootPairedMeas$Q,var.equal=TRUE)
            
            test.table["Q",1] <- as.numeric(test$statistic)
            test.table["Q",2] <- as.numeric(test$parameter)
            test.table["Q",3] <- as.numeric(test$p.value)
            test.table["Q",4] <- as.numeric(d(object$bootDataMeas$Q,object$bootPairedMeas$Q))
            test.table["Q",5] <- as.numeric(test$conf.int[1])
            test.table["Q",6] <- as.numeric(test$conf.int[2])
            test.table["Q",7] <- ifelse(sign(test$statistic)==1,"<",">")
        }
        
        if("S" %in% names)
        {
            test <- t.test(object$bootDataMeas$S,object$bootPairedMeas$S,var.equal=TRUE)
            
            test.table["S",1] <- as.numeric(test$statistic)
            test.table["S",2] <- as.numeric(test$parameter)
            test.table["S",3] <- as.numeric(test$p.value)
            test.table["S",4] <- as.numeric(d(object$bootDataMeas$S,object$bootPairedMeas$S))
            test.table["S",5] <- as.numeric(test$conf.int[1])
            test.table["S",6] <- as.numeric(test$conf.int[2])
            test.table["S",7] <- ifelse(sign(test$statistic)==1,"<",">")
        }
        
        test.table[,1:6] <- as.data.frame(round(test.table[,1:6],5))
    }else if(groups>2)
    {
        
        ####IN PROGRESS####
        
        message("UNDER CONSTRUCTION")
        
        if(groups<2)
        {
            eta <- function(anova)
            {
                summ <- summary(anova)[1][[1]]
                
                sst <- sum(summ[[2]])
                
                len <- nrow(summ)
                
                eta2 <- vector("numeric",length=len)
                
                for(i in 1:len)
                {eta2[i] <- summ[[2]][i]/sst}
                
                return(eta2)
            }
            
            names <- ls(object$bootDataMeas)
            
            if("randASPL" %in% names)
            {names <- names[-which(names=="randASPL")]}
            
            if("randCC" %in% names)
            {names <- names[-which(names=="randCC")]}
            
            test.table <- list()
            
            
            
            if("ASPL" %in% names)
            {
                #test <- aov(ASPL ~  , data = object)
                
                test.table["ASPL",1] <- as.numeric(test$statistic)
                test.table["ASPL",2] <- as.numeric(test$parameter)
                test.table["ASPL",3] <- as.numeric(test$p.value)
                test.table["ASPL",4] <- as.numeric(d(object$bootDataMeas$ASPL,object$bootPairedMeas$ASPL))
                test.table["ASPL",5] <- as.numeric(test$conf.int[1])
                test.table["ASPL",6] <- as.numeric(test$conf.int[2])
                test.table["ASPL",7] <- ifelse(sign(test$statistic)==1,"<",">")
            }
            
            if("CC" %in% names)
            {
                test <- t.test(object$bootDataMeas$CC,object$bootPairedMeas$CC,var.equal=TRUE)
                
                test.table["CC",1] <- as.numeric(test$statistic)
                test.table["CC",2] <- as.numeric(test$parameter)
                test.table["CC",3] <- as.numeric(test$p.value)
                test.table["CC",4] <- as.numeric(d(object$bootDataMeas$CC,object$bootPairedMeas$CC))
                test.table["CC",5] <- as.numeric(test$conf.int[1])
                test.table["CC",6] <- as.numeric(test$conf.int[2])
                test.table["CC",7] <- ifelse(sign(test$statistic)==1,"<",">")
            }
            
            if("MNS" %in% names)
            {
                test <- t.test(object$bootDataMeas$MNS,object$bootPairedMeas$MNS,var.equal=TRUE)
                
                test.table["MNS",1] <- as.numeric(test$statistic)
                test.table["MNS",2] <- as.numeric(test$parameter)
                test.table["MNS",3] <- as.numeric(test$p.value)
                test.table["MNS",4] <- as.numeric(d(object$bootDataMeas$MNS,object$bootPairedMeas$MNS))
                test.table["MNS",5] <- as.numeric(test$conf.int[1])
                test.table["MNS",6] <- as.numeric(test$conf.int[2])
                test.table["MNS",7] <- ifelse(sign(test$statistic)==1,"<",">")
            }
            
            if("Q" %in% names)
            {
                test <- t.test(object$bootDataMeas$Q,object$bootPairedMeas$Q,var.equal=TRUE)
                
                test.table["Q",1] <- as.numeric(test$statistic)
                test.table["Q",2] <- as.numeric(test$parameter)
                test.table["Q",3] <- as.numeric(test$p.value)
                test.table["Q",4] <- as.numeric(d(object$bootDataMeas$Q,object$bootPairedMeas$Q))
                test.table["Q",5] <- as.numeric(test$conf.int[1])
                test.table["Q",6] <- as.numeric(test$conf.int[2])
                test.table["Q",7] <- ifelse(sign(test$statistic)==1,"<",">")
            }
            
            if("S" %in% names)
            {
                test <- t.test(object$bootDataMeas$S,object$bootPairedMeas$S,var.equal=TRUE)
                
                test.table["S",1] <- as.numeric(test$statistic)
                test.table["S",2] <- as.numeric(test$parameter)
                test.table["S",3] <- as.numeric(test$p.value)
                test.table["S",4] <- as.numeric(d(object$bootDataMeas$S,object$bootPairedMeas$S))
                test.table["S",5] <- as.numeric(test$conf.int[1])
                test.table["S",6] <- as.numeric(test$conf.int[2])
                test.table["S",7] <- ifelse(sign(test$statistic)==1,"<",">")
            }
        }
        
    }
    
    return(test.table)
}
#----