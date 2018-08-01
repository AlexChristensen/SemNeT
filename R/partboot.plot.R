#' Plot for partboot
#' @description Bootstraps (without replacement) the nodes in the network and computes global network characteristics
#' @param object An object from \link[SemNeT]{partboot}
#' @param paired Is object from a paired \link[SemNeT]{partboot}?
#' @param CI Confidence intervals to use for plot.
#' Defaults to .975
#' @param labels Labels to be used in plot.
#' Defaults to NULL.
#' Typed responses will be requested if NULL
#' @param measures Measures to be plotted
#' @return Returns plots for the specified measures
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
#' #labels
#' labs <- c("eqCmat","eqRmat")
#' partboot.plot(results, paired = TRUE, labels = labs)
#' }
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @importFrom stats qnorm sd
#' @export
#Plot: Partial Bootstrapped Semantic Network Analysis----
partboot.plot <- function (object, paired = FALSE, CI = .975, labels = NULL,
                           measures = c("ASPL","CC","Q","S","MeanStrength"))
{
    if(missing(measures))
    {measures <- c("ASPL","CC","Q","S","MeanStrength")
    }else{measures <- match.arg(measures,several.ok=TRUE)}
    
    ci <- CI
    
    #Names in object
    objnames <- ls(object)
    
    bootData <- list()
    
    #Extract data measures
    if(!"bootDataMeas" %in% objnames)
    {
        len <- length(objnames)
        
        subnames <- ls(object[[1]])
        
        if("bootDataMeas" %in% subnames)
        {
            for(i in 1:len)
            {
                bootData[[i]] <- object[[i]][["bootDataMeas"]]
            }
        }
    }else{len <- 1
    bootData[[1]] <- object$bootDataMeas
    }
    
    #Extract paired measures
    if(paired)
    {
        bootPaired <- list()
        
        if(!"bootPairedMeas" %in% objnames)
        {
            len <- length(objnames)
            
            subnames <- ls(object[[1]])
            
            if("bootPairedMeas" %in% subnames)
            {
                for(i in 1:len)
                {
                    bootPaired[[i]] <- object[[i]][["bootPairedMeas"]]
                }
            }
        }else{bootPaired[[1]] <- object$bootPairedMeas}
    }
    
    #Number of samples in data
    sampsData <- vector(mode="numeric",length=len)
    
    for(i in 1:len)
    {sampsData[i] <- nrow(bootData[[i]])}
    
    #Number of samples in paired data
    if(paired)
    {
        sampsPaired <- vector(mode="numeric",length=len)
        
        for(i in 1:len)
        {sampsPaired[i] <- nrow(bootPaired[[i]])}
    }
    
    #Plots
    
    plot <- list()
    
    if("ASPL" %in% measures)
    {
        plot$aspl <- suppressWarnings(
            org.plot(bootData, bootPaired,
                     sampsData, sampsPaired,
                     len = len,
                     measname = "Average Shortest Path Length",
                     netmeas = "ASPL", pall = "Spectral",
                     paired = paired, CI = ci, labels = labels)
        )
        
        if(!is.null(plot$aspl$labels))
        {
            labels <- plot$aspl$labels
            plot$aspl <- suppressWarnings(
                org.plot(bootData, bootPaired,
                         sampsData, sampsPaired,
                         len = len,
                         measname = "Average Shortest Path Length",
                         netmeas = "ASPL", pall = "Spectral",
                         paired = paired, CI = ci, labels = labels)
            )
        }
    }
    
    
    
    if("CC" %in% measures)
    {
        plot$cc <- suppressWarnings(
            org.plot(bootData, bootPaired,
                     sampsData, sampsPaired,
                     len = len,
                     measname = "Clustering Coefficient",
                     netmeas = "CC", pall = "Spectral",
                     paired = paired, CI = ci, labels = labels)
        )
        
        if(!is.null(plot$cc$labels))
        {
            labels <- plot$cc$labels
            plot$cc <- suppressWarnings(
                org.plot(bootData, bootPaired,
                         sampsData, sampsPaired,
                         len = len,
                         measname = "Clustering Coefficient",
                         netmeas = "CC", pall = "Spectral",
                         paired = paired, CI = ci, labels = labels)
            )
        }
    }
    
    if("Q" %in% measures)
    {
        plot$q <- suppressWarnings(
            org.plot(bootData, bootPaired,
                     sampsData, sampsPaired,
                     len = len,
                     measname = "Modularity",
                     netmeas = "Q", pall = "Spectral",
                     paired = paired, CI = ci, labels = labels)
        )
        
        if(!is.null(plot$q$labels))
        {
            labels <- plot$q$labels
            plot$q <- suppressWarnings(
                org.plot(bootData, bootPaired,
                         sampsData, sampsPaired,
                         len = len,
                         measname = "Modularity",
                         netmeas = "Q", pall = "Spectral",
                         paired = paired, CI = ci, labels = labels)
            )
        }
    }
    
    if("S" %in% measures)
    {
        plot$s <- suppressWarnings(
            org.plot(bootData, bootPaired,
                     sampsData, sampsPaired,
                     len = len,
                     measname = "Small-worldness",
                     netmeas = "S", pall = "Spectral",
                     paired = paired, CI = ci, labels = labels)
        )
        
        if(!is.null(plot$s$labels))
        {
            labels <- plot$s$labels
            plot$s <- suppressWarnings(
                org.plot(bootData, bootPaired,
                         sampsData, sampsPaired,
                         len = len,
                         measname = "Small-worldness",
                         netmeas = "S", pall = "Spectral",
                         paired = paired, CI = ci, labels = labels)
            )
        }
    }
    
    if("MeanStrength" %in% measures)
    {
        plot$mns <- suppressWarnings(
            org.plot(bootData, bootPaired,
                     sampsData, sampsPaired,
                     len = len,
                     measname = "Mean Network Strength",
                     netmeas = "MNS",
                     pall = "Spectral",
                     paired = paired, CI = ci, labels = labels)
        )
        
        if(!is.null(plot$mns$labels))
        {
            labels <- plot$mns$labels
            plot$mns <- suppressWarnings(
                org.plot(bootData, bootPaired,
                         sampsData, sampsPaired,
                         len = len,
                         measname = "Mean Network Strength",
                         netmeas = "MNS", pall = "Spectral",
                         paired = paired, CI = ci, labels = labels)
            )
        }
    }
    
    return(plot)
}
#----