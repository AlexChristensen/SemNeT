#' Organization function for partboot.plot
#' @description A function used in \link[SemNetCleaner]{partboot}. Not to be used individually
#' @param bootData bootData from \link[SemNetCleaner]{partboot}
#' @param bootPaired bootPaired from \link[SemNetCleaner]{partboot}
#' @param sampsData sampsData from \link[SemNetCleaner]{partboot}
#' @param sampsPaired sampsPaired from \link[SemNetCleaner]{partboot}
#' @param len Number of samples in data list
#' @param measname Full network measure name
#' @param netmeas Abbreviated network measure name
#' @param pall Color palette to be used from \code{\link{RColorBrewer}}
#' @param paired Is samples paired?
#' @param CI Confidence interval to be used
#' @param labels Labels to be used in plot.
#' @return Returns plots for the specified measures
#' @examples
#' ###NOT FOR INDIVIDUAL USE###
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats qnorm
#' @export
#Partial Bootstrapped Semantic Network Analysis----

org.plot <- function (bootData, bootPaired,
                      sampsData, sampsPaired, len,
                      measname, netmeas, pall,
                      paired, CI, labels)

{
    #set variables for binding check
    #FIGURE OUT HOW TO PASS THESE THROUGH CHECK
    V1 <- NULL
    V2 <- NULL
    V4 <- NULL
    group <- NULL
    y <- NULL
    x <- NULL
    width <- NULL
    violinwidth <- NULL
    xmax <- NULL
    xmaxv <- NULL
    xminv <- NULL
    
    
    if(missing(bootPaired))
    {bootPaired <- NULL}
    
    if(missing(sampsPaired))
    {sampsPaired <- NULL}
    
    if(missing(labels))
    {labels <- NULL}
    
    ###Raindrop plot functions
    ###Huge thank you to Hadley Wickham and Micah Allen
    ###Code taken from: 
    ###https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
    
    raincloud_theme = ggplot2::theme(
        text = ggplot2::element_text(size = 10),
        axis.title.x = ggplot2::element_text(size = 16),
        axis.title.y = ggplot2::element_text(size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
        legend.title=ggplot2::element_text(size=16),
        legend.text=ggplot2::element_text(size=16),
        legend.position = "right",
        plot.title = ggplot2::element_text(lineheight=.8, face="bold", size = 16),
        panel.border = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid')
    )
    
    "%||%" <- function(a, b) {
        if (!is.null(a)) a else b
    }
    
    ##from ggplot2
    ggname <- function (prefix, grob) {
        grob$name <- grid::grobName(grob, prefix)
        grob
    }
    
    geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                                 position = "dodge", trim = TRUE, scale = "area",
                                 show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
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
        ggplot2::ggproto("GeomFlatViolin", ggplot2::Geom,
                setup_data = function(data, params) {
                    data$width <- data$width %||%
                        params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
                    
                    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                    data %>%
                        group_by(group) %>%
                        dplyr::mutate(ymin = min(y),
                               ymax = max(y),
                               xmin = x,
                               xmax = x + width / 2)
                    
                },
                
                draw_group = function(data, panel_scales, coord) {
                    # Find the points for the line to go all the way around
                    data <- transform(data, xminv = x,
                                      xmaxv = x + violinwidth * (xmax - x))
                    
                    # Make sure it's sorted properly to draw the outline
                    newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                                     plyr::arrange(transform(data, x = xmaxv), -y))
                    
                    # Close the polygon: set first and last point the same
                    # Needed for coord_polar and such
                    newdata <- rbind(newdata, newdata[1,])
                    
                    ggname("geom_flat_violin", ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord))
                },
                
                draw_key = ggplot2::draw_key_polygon,
                
                default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                  alpha = NA, linetype = "solid"),
                
                required_aes = c("x", "y")
        )
    
    
    #Sample sort
    totalData <- sum(sampsData)
    if(paired)
    {
        totalPaired <- sum(sampsPaired)
        cases <- totalData + totalPaired
        n <- (cases/len)/2
        meas <- as.data.frame(matrix(0, nrow = cases, ncol = 2))
        meas2 <- as.data.frame(matrix(0, nrow = cases/2, ncol = 4))
    }else{
        cases <- totalData
        n <- cases/len
        meas <- as.data.frame(matrix(0, nrow = cases, ncol = 2))
        meas2 <- as.data.frame(matrix(0, nrow = cases, ncol = 2))
    }

        if(!paired)
        {
            ends <- cumsum(sampsData)
            starts <- ends - (sampsData) + 1
        
            for(i in 1:length(sampsData))
            {
                meas[starts[i]:ends[i],2] <- bootData[[i]][netmeas]
            
                meas2 <- meas
            }
        
            for(i in 1:length(sampsData))
            {
                meas[starts[i]:ends[i],1] <- as.numeric(rep(i, sampsData[i]))
                meas2[starts[i]:ends[i],1] <- as.numeric(rep(i, sampsData[i]))
            }
            
            meas<-meas[order(meas[,1],decreasing=TRUE),]
            meas2<-meas[order(meas2[,1],decreasing=TRUE),]
            
            meas[,1] <- as.factor(meas[,1])
            meas2[,1] <- as.factor(meas2[,1])
            
            summ <- plyr::ddply(meas2, "V1", plyr::summarise,
                                mean = mean(V2),
                                sd = sd(V2),
                                se = sd(V2)/sqrt(n))
            
            lower <- vector(mode="numeric",length=len)
            upper <- vector(mode="numeric",length=len)
            
            for(i in 1:len)
            {
                subbs <- subset(meas2,meas2$V1==i)
                lower[i] <- mean(subbs$V2) - (qnorm(CI)*sd(subbs$V2)/sqrt(nrow(subbs)))
                upper[i] <- mean(subbs$V2) + (qnorm(CI)*sd(subbs$V2)/sqrt(nrow(subbs)))
            }
            
            summ <- as.data.frame(cbind(summ,lower,upper))
            
        }else{
            sampsComb <- c(sampsData,sampsPaired)
        
            ends <- cumsum(sampsComb)
        
            starts <- ends - (sampsComb) + 1
        
            endsData <- cumsum(sampsData)
            startsData <- endsData - (sampsData) + 1
        
            endsPaired <- cumsum(sampsPaired)
            startsPaired <- endsPaired - (sampsPaired) + 1
        
            is.even <- function(x) x %% 2 == 0
        
            for(i in 1:length(sampsComb))
            {
                if(!is.even(i))
                {
                    meas[starts[i]:ends[i],2] <- bootData[[ceiling(i/2)]][netmeas]
                    meas2[startsData[ceiling(i/2)]:endsData[ceiling(i/2)],2] <- bootData[[ceiling(i/2)]][netmeas]
                }else{
                    meas[starts[i]:ends[i],2] <- bootPaired[[i/2]][netmeas]
                    meas2[startsPaired[i/2]:endsPaired[i/2],4] <- bootPaired[[ceiling(i/2)]][netmeas]
                }
            }
        
            tot <- sampsData + sampsPaired
            totEN <- cumsum(tot)
            totST <- totEN - (tot) + 1
        
            for(i in 1:length(tot))
            {
                meas[totST[i]:totEN[i],1] <- c(rep(as.numeric(paste(i,1,sep=".")), tot[i]/2),rep(as.numeric(paste(i,2,sep=".")), tot[i]/2))
                
                meas2[startsData[i]:endsData[i],1] <- rep(as.numeric(paste(i,1,sep=".")), sampsData[i])
                meas2[startsData[i]:endsData[i],3] <- rep(as.numeric(paste(i,2,sep=".")), sampsData[i])
            }
            
            meas[,1] <- as.factor(meas[,1])
            meas2[,1] <- as.factor(meas2[,1])

            sum1 <- plyr::ddply(meas2, "V1", plyr::summarise,
                            mean = mean(V2),
                            sd = sd(V2),
                            se = sd(V2)/sqrt(n))
        
            sum2 <- plyr::ddply(meas2, "V3", plyr::summarise,
                            mean = mean(V4),
                            sd = sd(V4),
                            se = sd(V4)/sqrt(n))
                            
            lower.1 <- vector(mode="numeric",length=len)
            upper.1 <- vector(mode="numeric",length=len)
            lower.2 <- vector(mode="numeric",length=len)
            upper.2 <- vector(mode="numeric",length=len)
            
            for(i in 1:len)
            {
                subbs <- subset(meas2,meas2$V1==as.numeric(paste(i,1,sep=".")))
                lower.1[i] <- mean(subbs$V2) - (qnorm(CI)*sd(subbs$V2)/sqrt(nrow(subbs)))
                upper.1[i] <- mean(subbs$V2) + (qnorm(CI)*sd(subbs$V2)/sqrt(nrow(subbs)))
                lower.2[i] <- mean(subbs$V4) - (qnorm(CI)*sd(subbs$V4)/sqrt(nrow(subbs)))
                upper.2[i] <- mean(subbs$V4) + (qnorm(CI)*sd(subbs$V4)/sqrt(nrow(subbs)))
            }
            
            
            colnames(sum2)[1] <- colnames(sum1)[1]
        
            sum1[,1] <- as.numeric(sum1[,1])
            if(all(sum1[,1]%%1==0))
            {
                for(i in 1:(nrow(sum1)))
                {sum1[i,1] <- rep(as.numeric(paste(i,1,sep="."),nrow(sum1)))}
            }
            sum2[,1] <- as.numeric(sum2[,1])
        
            sum1 <- as.data.frame(cbind(sum1,lower.1,upper.1))
            
            sum2 <- as.data.frame(cbind(sum2,lower.2,upper.2))
            
            colnames(sum1)[5:6] <- c("lower","upper")
            colnames(sum2)[5:6] <- c("lower","upper")
            
            summ <- rbind(sum1,sum2)
        
            summ[,1] <- as.factor(summ[,1])
        }
        
        g <- ggplot2::ggplot(data = meas, ggplot2::aes(y = V2, x = V1, fill = V1)) +
            geom_flat_violin(position = ggplot2::position_nudge(x = .2, y = 0), alpha = 1) +
            ggplot2::geom_point(ggplot2::aes(y = V2, color = V1), position = ggplot2::position_jitter(width = .15), size = .5, alpha = 1) +
            ggplot2::geom_errorbar(data = summ, ggplot2::aes(ymin = lower, ymax = upper, y = mean), position = ggplot2::position_nudge(x = 0.3), width = .2) +
            ggplot2::geom_point(data = summ, ggplot2::aes(x = V1, y = mean), position = ggplot2::position_nudge(x = 0.3), size = 2.5) +
            ggplot2::expand_limits(x = 5.25) +
            ggplot2::guides(fill = FALSE) +
            ggplot2::guides(color = FALSE) +
            ggplot2::scale_color_brewer(palette = pall) +
            ggplot2::scale_fill_brewer(palette = pall) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw() +
            raincloud_theme
        
        if(is.null(labels))
        {
            g <- g + ggplot2::labs(x = "Percent of Nodes Remaining",
                 y = paste(measname," (",netmeas,")",sep=""),
                 title = paste("Boostrapped Node-drop Results: ",netmeas,sep=""),
                 subtitle = paste(n, "Samples"))
            print(g)
        }else{
            g <- g + ggplot2::labs(x = "Percent of Nodes Remaining",
                          y = paste(measname," (",netmeas,")",sep=""),
                          title = paste("Boostrapped Node-drop Results: ",netmeas,sep=""),
                          subtitle = paste(n, "Samples")) +
                ggplot2::scale_x_discrete(labels=labels)
        }
        
        #labels function
            if(is.null(labels))
            {
                ans <- menu(c("Yes","No"),title="Define y-axis labels?")
                {
                    if(ans==1)
                    {
                        labels <- paste(summ[,1])
                        
                        labels <- labels[order(labels)]
                        
                        num <- length(labels)
                        
                        resp <- vector(length=num)
                        
                        for(i in 1:length(labels))
                        {resp[i] <- readline(paste("New label for ",labels[i],":"))}
                        
                        labels <- resp
                        
                        g <- g + ggplot2::scale_x_discrete(labels=labels)
                    }
                }
            }
            
            return(list(plot = g, labels = labels))
}
#----