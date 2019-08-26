#' Organization function for partboot.plot
#' 
#' @description A wrapper function used in \link[SemNeT]{partboot}. Not to be used individually
#' 
#' @param input List.
#' Input data from \link[SemNeT]{partboot}
#' 
#' @param len Numeric.
#' Number of bootstrapped samples (percentages)
#' 
#' @param measures Character.
#' Network measures to be entered
#' 
#' @param name Character.
#' Name(s) of object(s) used from \link[SemNeT]{partboot}
#' 
#' @param groups Character.
#' Names for the group(s)
#' 
#' @param netmeas Character.
#' Abbreviated network measure name that should be plotted
#' 
#' @return Returns plots for the specified measures
#' 
#' @examples
#' #### NOT INTENDED FOR INDIVIDUAL USE ####
#' #### WRAPPER FUNCTION ####
#' 
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' one.result <- partboot(data = one, percent = .50, iter = 1000,
#' sim = "cosine", cores = 2)
#' }
#' # Plot
#' org.plot(input = list(one.result), name = "data",
#' len = 1, groups = "One", netmeas = "ASPL")
#' 
#' @references
#' Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., Kievit, R. (2018).
#' Raincloud plots: A multi-platform tool for robust data visualization.
#' \emph{PeerJ Preprints}, \emph{6}, e27137v1.
#' \href{https://doi.org/10.7287/peerj.preprints.27137v1}{10.7287/peerj.preprints.27137v1}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats qnorm
#' 
#' @export
#Partial Bootstrapped Semantic Network Analysis----
org.plot <- function (input, len, measures, name, groups, netmeas)
{
    #CRAN CHECKS
    group <- NULL; y <- NULL; x <- NULL; width <- NULL
    violinwidth <- NULL; xmax <- NULL; xminv <- NULL
    xmaxv <- NULL; percent <- NULL
    
    #Missing arguments
    if(missing(measures))
    {measures <- c("ASPL","CC","Q")
    }else{measures <- match.arg(measures,several.ok=TRUE)}
    
    ###########################
    #### FLAT VIOLIN PLOTS ####
    ###########################
    
    #SEE: https://pdfs.semanticscholar.org/a38b/df3803b1cd00d57f69516be1d60a3c8688c9.pdf
    #AND: https://github.com/RainCloudPlots/RainCloudPlots
    
    "%||%" <- function(a, b) {
        if (!is.null(a)) a else b
    }
    
    geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                                 position = "dodge", trim = TRUE, scale = "area",
                                 show.legend = NA, inherit.aes = TRUE, ...) {
        layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomFlatViolin,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                trim = trim,
                scale = scale,
                ...
            )
        )
    }
    
    GeomFlatViolin <-
        ggproto("GeomFlatViolin", Geom,
                setup_data = function(data, params) {
                    data$width <- data$width %||%
                        params$width %||% (resolution(data$x, FALSE) * 0.9)
                    
                    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                    data %>%
                        group_by(group) %>%
                        mutate(
                            ymin = min(y),
                            ymax = max(y),
                            xmin = x,
                            xmax = x + width / 2
                        )
                },
                
                draw_group = function(data, panel_scales, coord) {
                    # Find the points for the line to go all the way around
                    data <- transform(data,
                                      xminv = x,
                                      xmaxv = x + violinwidth * (xmax - x)
                    )
                    
                    # Make sure it's sorted properly to draw the outline
                    newdata <- rbind(
                        plyr::arrange(transform(data, x = xminv), y),
                        plyr::arrange(transform(data, x = xmaxv), -y)
                    )
                    
                    # Close the polygon: set first and last point the same
                    # Needed for coord_polar and such
                    newdata <- rbind(newdata, newdata[1, ])
                    
                    #ggname("geom_flat_violin", ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord))
                    ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord)
                },
                
                draw_key = draw_key_polygon,
                
                default_aes = aes(
                    weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, linetype = "solid"
                ),
                
                required_aes = c("x", "y")
        )
    
    ###############################
    #### SET UP DATA FOR PLOTS ####
    ###############################
    
    # Initialize Percent and Iterations
    perc <- vector("numeric", length = len)
    it <- perc
    iter <- input[[1]]$iter
    
    for(i in 1:len)
    {
        perc[i] <- input[[i]]$percent
        it[i] <- input[[i]]$iter
    }
    
    plot.mat <- matrix(NA, nrow = sum(it)*length(name), ncol = 2 + length(measures))
    colnames(plot.mat) <- c("group","percent",measures)
    
    #Grab measures
    meas <- matrix(NA, nrow = 1, ncol = length(measures))
    
    for(j in 1:length(name))
    {
        for(i in 1:len)
        {meas <- rbind(meas,t(input[[i]][[paste(name[j],"Meas",sep="")]]))}
    }
    
    meas <- meas[-1,]
    
    plot.mat[,"group"] <- rep(1:length(name), each = len * iter)
    
    plot.mat[,"percent"] <- rep(rep(perc, each = iter), length(name))
    plot.mat[,3:(2+length(measures))] <- meas
    
    #Convert to data frame
    plot.mat <- as.data.frame(plot.mat, stringsAsFactors = TRUE)
    
    #Select network measure of interest
    plot.mat.select <- plot.mat[,c("group","percent",netmeas)]
    colnames(plot.mat.select)[3] <- "netmeas"
    plot.mat.select$group <- as.factor(as.character(plot.mat.select$group))
    plot.mat.select$percent <- as.factor(as.character(plot.mat.select$percent))
    
    
    ##############
    #### PLOT ####
    ##############
    
    # Label Setups
    ## Measures
    if(netmeas=="ASPL")
    {full.meas <- "Average Shortest Path Length"
    }else if(netmeas=="CC")
    {full.meas <- "Clustering Coefficient"
    }else if(netmeas=="Q")
    {full.meas <- "Modularity"}
    
    ##Groups
    if(is.null(groups))
    {groups <- name}
    
    # Rainclouds for repeated measures, continued
    pl <- ggplot(plot.mat.select, aes(x = percent, y = netmeas, fill = group)) +
        
        geom_flat_violin(aes(fill = group),position = position_nudge(x = 0.05, y = 0),
                        adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
        
        geom_point(aes(x = as.numeric(percent)-.125, y = netmeas, colour = group),
                   position = position_jitter(width = .05), alpha = .3, size = 1, shape = 20) +
        
        geom_boxplot(aes(x = percent, y = netmeas, fill = group),outlier.shape = NA,
                     alpha = .5, width = .1, colour = "black") +
        
        scale_colour_brewer(name = "Groups", labels = groups, palette = "Dark2") +
        
        scale_fill_brewer(name = "Groups", labels = groups, palette = "Dark2") +
        
        labs(title = paste("Bootstrapped Node-drop Results:",netmeas,sep=" "),
             subtitle = paste(iter,"Samples",sep = " "),
             x = "Percent of Nodes\nRemaining (%)",
             y = paste(full.meas," (",netmeas,")",sep="")) +
        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              plot.title = element_text(face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              legend.title = element_text(face = "bold")) +
        
        coord_flip()
    
    return(pl)
}
#----