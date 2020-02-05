#' Wrapper function for \code{\link[SemNeT]{partboot.test}}
#' 
#' @description Computes statistical tests for partial bootstrapped
#' networks from \code{\link[SemNeT]{partboot}}. Automatically
#' computes \emph{t}-tests (\code{\link{t.test}}) or ANOVA
#' (\code{\link{aov}}) including Tukey's HSD for pairwise comparisons
#' (\code{\link{TukeyHSD}})
#' 
#' @param partboot.obj Object from \code{\link[SemNeT]{partboot}}
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
#' partboot.one.test(two.result)
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
#' 
#' ## Compute tests
#' partboot.one.test(boot.fifty, formula = "y ~ gf*caq", groups = groups)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats t.test aov TukeyHSD as.formula
#' 
#' @export
#Test: Partial Bootstrapped Network Statistics----
partboot.one.test <- function (partboot.obj, formula = NULL, groups = NULL)
{
    #Check for 'partboot' object
    if(class(partboot.obj) != "partboot")
    {stop("Object input into 'partboot.obj' is not a 'partboot' object")}
    
    #Check for data if formula is not NULL
    if(!is.null(formula))
    {
        if(!exists("groups"))
        {stop("'groups' argument is NULL when 'formula' argument is not. Please input groups.")}
    }
    
    #Get names of networks
    name <- unique(gsub("Summ","",gsub("Meas","",names(partboot.obj))))
    
    #Remove percent and iter
    name <- na.omit(gsub("iter",NA,gsub("percent",NA,name)))
    attr(name, "na.action") <- NULL
    
    #Number of input
    len <- length(name)
    
    #Error there are no paired samples
    if(len < 2)
    {stop("Single samples cannot be tested. Use 'randnet.test' for single samples")}
    
    #Identify percent of nodes remaining
    perc <- partboot.obj$percent
    
    #Identify iterations
    iter <- partboot.obj$iter
    
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
        row.names(aspl) <- paste(perc*100,"%",sep="")
        colnames(aspl) <- c("t-statistic", "df", "p-value", "d", "Difference",
                            "CI95.lower", "CI95.upper","Direction")
        #ASPL
        one.aspl <- partboot.obj[[paste(name[1],"Meas",sep="")]]["ASPL",]
        two.aspl <- partboot.obj[[paste(name[2],"Meas",sep="")]]["ASPL",]
            
        #t-test
        test <- t.test(one.aspl, two.aspl, var.equal = TRUE)
            
        #Input results into table
        aspl[paste(perc*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        aspl[paste(perc*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        aspl[paste(perc*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        aspl[paste(perc*100,"%",sep=""),4] <- round(as.numeric(d(one.aspl,two.aspl)),3)
        aspl[paste(perc*100,"%",sep=""),5] <- round(as.numeric(mean(one.aspl)-mean(two.aspl)),3)
        aspl[paste(perc*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[1]),3)
        aspl[paste(perc*100,"%",sep=""),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {aspl[paste(perc*100,"%",sep=""),8] <- "n.s."
        }else{
            aspl[paste(perc*100,"%",sep=""),8] <- ifelse(sign(test$statistic)==1,
                                                         paste(name[1],">",name[2],sep=" "),
                                                         paste(name[2],">",name[1],sep=" ")
            )
        }
        
        ##CC Tests
        cc <- matrix(NA, nrow = 1, ncol = 8)
        row.names(cc) <- paste(perc*100,"%",sep="")
        colnames(cc) <- c("t-statistic", "df", "p-value", "d", "Difference",
                            "CI95.lower", "CI95.upper","Direction")
        #CC
        one.cc <- partboot.obj[[paste(name[1],"Meas",sep="")]]["CC",]
        two.cc <- partboot.obj[[paste(name[2],"Meas",sep="")]]["CC",]
        
        #t-test
        test <- t.test(one.cc, two.cc, var.equal = TRUE)
        
        #Input results into table
        cc[paste(perc*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        cc[paste(perc*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        cc[paste(perc*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        cc[paste(perc*100,"%",sep=""),4] <- round(as.numeric(d(one.cc,two.cc)),3)
        cc[paste(perc*100,"%",sep=""),5] <- round(as.numeric(mean(one.cc)-mean(two.cc)),3)
        cc[paste(perc*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[1]),3)
        cc[paste(perc*100,"%",sep=""),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {cc[paste(perc*100,"%",sep=""),8] <- "n.s."
        }else{
            cc[paste(perc*100,"%",sep=""),8] <- ifelse(sign(test$statistic)==1,
                                                         paste(name[1],">",name[2],sep=" "),
                                                         paste(name[2],">",name[1],sep=" ")
            )
        }
        
        ##Q Tests
        q <- matrix(NA, nrow = 1, ncol = 8)
        row.names(q) <- paste(perc*100,"%",sep="")
        colnames(q) <- c("t-statistic", "df", "p-value", "d", "Difference",
                          "CI95.lower", "CI95.upper","Direction")
        #Q
        one.q <- partboot.obj[[paste(name[1],"Meas",sep="")]]["Q",]
        two.q <- partboot.obj[[paste(name[2],"Meas",sep="")]]["Q",]
        
        #t-test
        test <- t.test(one.q, two.q, var.equal = TRUE)
        
        #Input results into table
        q[paste(perc*100,"%",sep=""),1] <- round(as.numeric(test$statistic),3)
        q[paste(perc*100,"%",sep=""),2] <- round(as.numeric(test$parameter),3)
        q[paste(perc*100,"%",sep=""),3] <- round(as.numeric(test$p.value),3)
        q[paste(perc*100,"%",sep=""),4] <- round(as.numeric(d(one.q,two.q)),3)
        q[paste(perc*100,"%",sep=""),5] <- round(as.numeric(mean(one.q)-mean(two.q)),3)
        q[paste(perc*100,"%",sep=""),6] <- round(as.numeric(test$conf.int[1]),3)
        q[paste(perc*100,"%",sep=""),7] <- round(as.numeric(test$conf.int[2]),3)
        
        if(round(as.numeric(test$p.value),3) > .05)
        {q[paste(perc*100,"%",sep=""),8] <- "n.s."
        }else{
            q[paste(perc*100,"%",sep=""),8] <- ifelse(sign(test$statistic)==1,
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
            row.names(aspl) <- paste(perc*100,"%",sep="")
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
            new.aspl <- partboot.obj[[paste(name[i],"Meas",sep="")]]["ASPL",]
            
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
            aspl[[paste(perc*100,"%",sep="")]] <- summary(test)[[1]]
            hsd[[paste(perc*100,"%",sep="")]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            aspl[paste(perc*100,"%",sep=""),"F-statistic"] <- round(test.summ$`F value`[1],3)
            aspl[paste(perc*100,"%",sep=""),"group.df"] <- test.summ$Df[1]
            aspl[paste(perc*100,"%",sep=""),"residual.df"] <- test.summ$Df[2]
            aspl[paste(perc*100,"%",sep=""),"p-value"] <- test.summ$`Pr(>F)`[1]
            aspl[paste(perc*100,"%",sep=""),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
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
            row.names(cc) <- paste(perc*100,"%",sep="")
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
            new.cc <- partboot.obj[[paste(name[i],"Meas",sep="")]]["CC",]
            
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
            cc[[paste(perc*100,"%",sep="")]] <- summary(test)[[1]]
            hsd[[paste(perc*100,"%",sep="")]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            cc[paste(perc*100,"%",sep=""),"F-statistic"] <- round(test.summ$`F value`[1],3)
            cc[paste(perc*100,"%",sep=""),"group.df"] <- test.summ$Df[1]
            cc[paste(perc*100,"%",sep=""),"residual.df"] <- test.summ$Df[2]
            cc[paste(perc*100,"%",sep=""),"p-value"] <- test.summ$`Pr(>F)`[1]
            cc[paste(perc*100,"%",sep=""),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
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
            row.names(q) <- paste(perc*100,"%",sep="")
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
            new.q <- partboot.obj[[paste(name[i],"Meas",sep="")]]["Q",]
            
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
            q[[paste(perc*100,"%",sep="")]] <- summary(test)[[1]]
            hsd[[paste(perc*100,"%",sep="")]] <- TukeyHSD(test)
        }else{
            test <- aov(Measure ~ Group, data = aov.obj)
            
            test.summ <- summary(test)[[1]]
            
            #Input results into table
            q[paste(perc*100,"%",sep=""),"F-statistic"] <- round(test.summ$`F value`[1],3)
            q[paste(perc*100,"%",sep=""),"group.df"] <- test.summ$Df[1]
            q[paste(perc*100,"%",sep=""),"residual.df"] <- test.summ$Df[2]
            q[paste(perc*100,"%",sep=""),"p-value"] <- test.summ$`Pr(>F)`[1]
            q[paste(perc*100,"%",sep=""),"p.eta.sq"] <- partial.eta(test.summ$`Sum Sq`[1],sum(test.summ$`Sum Sq`))
            
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
    
    return(tests)
}
#----