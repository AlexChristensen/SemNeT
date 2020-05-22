#' Changes Bad Responses to NA
#' 
#' @description A sub-routine to determine whether responses are good or bad.
#' Bad responses are replaced with missing (\code{NA}). Good responses are returned.
#' 
#' @param word Character.
#' A word to be tested for whether it is bad
#' 
#' @param ... Vector.
#' Additional responses to be considered bad
#' 
#' @return If response is bad, then returns \code{NA}.
#' If response is valid, then returns the response
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Change Bad Responses----
# Updated 15.04.2020
bad.response <- function (word, ...)
{
  # Other bad responses
  others <- unlist(list(...))
  
  # Bad responses
  bad <- c(NA, "NA", "", " ", "  ", others)
  
  # If there is no longer a response
  if(length(word)==0)
  {word <- NA}
  
  for(i in 1:length(word))
  {
    #if bad, then convert to NA
    if(word[i] %in% bad)
    {word[i] <- NA}
  }
  
  return(word)
}

#' Responses to binary matrix
#' 
#' @description Converts the response matrix to binary response matrix
#' 
#' @param resp Response matrix.
#' A response matrix of verbal fluency or linguistic data
#' 
#' @return A list containing objects for each participant and their responses
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Change response matrix to binary response matrix
#' binmat <- resp2bin(clean$responses$clean)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Response matrix to binary matrix
# Updated 15.04.2020
resp2bin <- function (resp)
{
  # Data matrix
  mat <- as.matrix(resp)
  
  # Replace bad responses with NA
  mat <- bad.response(mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Number of cases
  n <- nrow(mat)
  
  # Initialize binary matrix
  bin.mat <- matrix(0, nrow = n, ncol = length(uniq.resp))
  colnames(bin.mat) <- uniq.resp
  row.names(bin.mat) <- row.names(resp)
  
  # Loop through and replace
  for(i in 1:n)
  {bin.mat[i,na.omit(match(mat[i,], colnames(bin.mat)))] <- 1}
  
  return(bin.mat)
}

#' Binary Responses to Character Responses
#' @description Converts the binary response matrix into characters for each participant
#' 
#' @param rmat Binary matrix.
#' A binarized response matrix of verbal fluency or linguistic data
#' 
#' @param to.data.frame Boolean.
#' Should output be a data frame where participants are columns?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to convert output to data frame
#' 
#' @return A list containing objects for each participant and their responses
#' 
#' @examples
#' # Toy example
#' raw <- open.animals[c(1:10),-c(1:3)]
#' 
#' # Clean and prepocess data
#' clean <- textcleaner(raw, partBY = "row", dictionary = "animals")
#' 
#' # Change binary response matrix to word response matrix
#' charmat <- bin2resp(clean$binary)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Binary to Response----
# Updated 15.04.2020
bin2resp <- function (rmat, to.data.frame = FALSE)
{
  #grab response names
  name <- colnames(rmat)
  
  #number of responses
  n <- ncol(rmat)
  
  #initialize matrix
  mat <- matrix(NA,nrow=nrow(rmat),ncol=ncol(rmat))
  
  #loop for each name
  for(i in 1:n)
  {mat[,i] <- ifelse(rmat[,i]==1,name[i],NA)}
  
  #number of participants
  p <- nrow(rmat)
  
  #initialize participant list
  part <- list()
  
  #loop for each participant
  for(j in 1:p)
  {
    resps <- na.omit(mat[j,])
    attributes(resps)$na.action <- NULL
    part[[row.names(rmat)[j]]] <- resps
  }
  
  #convert output to data frame
  if(to.data.frame)
  {
    nlen <- vector("numeric",length=length(part))
    
    num <- length(nlen)
    
    for(i in 1:num)
    {nlen[i] <- length(part[[i]])}
    
    mlen <- max(nlen)
    
    part.df <- matrix("",nrow=mlen,ncol=num)
    
    for(i in 1:num)
    {
      reps <- mlen - nlen[i]
      
      part.df[,i] <- c(unlist(part[[i]]),rep("",reps))
    }
    
    part <- as.data.frame(part.df)
    colnames(part) <- row.names(rmat)
    part <- t(part)
  }
  
  return(part)
}

#' Organization function for \link[SemNeT]{plot.bootSemNeT}
#' 
#' @description A sub-routine used in \code{\link[SemNeT]{plot.bootSemNeT}}. Not to be used individually
#' 
#' @param input List.
#' Input data from \code{\link[SemNeT]{bootSemNeT}}
#' 
#' @param len Numeric.
#' Number of bootstrapped samples
#' 
#' @param measures Character.
#' Network measures to be entered
#' 
#' @param name Character.
#' Name(s) of object(s) used from \code{\link[SemNeT]{bootSemNeT}}
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
#' #### SUB-ROUTINE ####
#' 
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' one.result <- bootSemNeT(data = one, prop = .50, iter = 1000,
#' sim = "cosine", cores = 2, method = "TMFG", type = "node")
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
#' @noRd
# Plot: Bootstrapped Network Statistics----
#Updated 30.03.2020
org.plot <- function (input, len, measures, name, groups, netmeas)
{
  
  ##Groups
  if(is.null(groups))
  {groups <- name}
  
  #CRAN CHECKS
  group <- NULL; y <- NULL; x <- NULL; width <- NULL
  violinwidth <- NULL; xmax <- NULL; xminv <- NULL
  xmaxv <- NULL; prop <- NULL
  
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
  
  # Initialize Proportion and Iterations
  perc <- vector("numeric", length = len)
  it <- perc
  iter <- input[[1]]$iter
  
  for(i in 1:len)
  {
    if(input[[i]]$type == "node")
    {
      perc[i] <- input[[i]]$prop
      type <- "node"
    }else{type <- "case"}
    
    it[i] <- input[[i]]$iter
  }
  
  plot.mat <- matrix(NA, nrow = sum(it)*length(name), ncol = 2 + length(measures))
  colnames(plot.mat) <- c("group","prop",measures)
  
  if(input[[i]]$type == "case")
  {plot.mat <- plot.mat[,-2]}
  
  #Grab measures
  meas <- matrix(NA, nrow = 1, ncol = length(measures))
  
  for(j in 1:length(name))
  {
    for(i in 1:len)
    {meas <- rbind(meas,t(input[[i]][[paste(name[j],"Meas",sep="")]]))}
  }
  
  meas <- meas[-1,]
  
  plot.mat[,"group"] <- rep(1:length(name), each = len * iter)
  
  if(input[[i]]$type == "node")
  {
    plot.mat[,"prop"] <- rep(rep(perc, each = iter), length(name))
    plot.mat[,3:(2+length(measures))] <- meas
  }else{plot.mat[,2:(1+length(measures))] <- meas}
  
  #Convert to data frame
  plot.mat <- as.data.frame(plot.mat, stringsAsFactors = TRUE)
  
  #Select network measure of interest
  if(input[[i]]$type == "node")
  {
    plot.mat.select <- plot.mat[,c("group","prop",netmeas)]
    colnames(plot.mat.select)[3] <- "netmeas"
  }else{
    plot.mat.select <- plot.mat[,c("group",netmeas)]
    colnames(plot.mat.select)[2] <- "netmeas"
  }
  
  plot.mat.select$group <- as.factor(as.character(plot.mat.select$group))
  
  #Initialize count
  count <- 0
  
  #Descriptives
  if(input[[i]]$type == "node")
  {
    plot.mat.desc <- matrix(NA, nrow = (length(groups) * length(perc)), ncol = 6)
    colnames(plot.mat.desc) <- c("group", "prop", "mean", "se", "lower.ci", "upper.ci")
    
    for(i in 1:length(groups))
      for(j in 1:length(perc))
      {
        #Update count
        count <- count + 1
        
        #Target
        target.group <- which(plot.mat.select$group == i)
        target.perc <- target.group[which(plot.mat.select$prop[target.group] == perc[j])]
        target.data <- plot.mat.select[target.perc, "netmeas"]
        
        plot.mat.desc[count, "group"] <- i
        plot.mat.desc[count, "prop"] <- perc[j]
        plot.mat.desc[count, "mean"] <- mean(target.data)
        plot.mat.desc[count, "se"] <- sd(target.data)
        plot.mat.desc[count, "lower.ci"] <- plot.mat.desc[count, "se"] * 1.96
        plot.mat.desc[count, "upper.ci"] <- plot.mat.desc[count, "se"] * 1.96
      }
  }else{
    plot.mat.desc <- matrix(NA, nrow = length(groups), ncol = 5)
    colnames(plot.mat.desc) <- c("group", "mean", "se", "lower.ci", "upper.ci")
    
    for(i in 1:length(groups))
    {
      #Update count
      count <- count + 1
      
      #Target
      target.group <- which(plot.mat.select$group == i)
      target.data <- plot.mat.select[target.group, "netmeas"]
      
      plot.mat.desc[count, "group"] <- i
      plot.mat.desc[count, "mean"] <- mean(target.data)
      plot.mat.desc[count, "se"] <- sd(target.data)
      plot.mat.desc[count, "lower.ci"] <- plot.mat.desc[count, "se"] * 1.96
      plot.mat.desc[count, "upper.ci"] <- plot.mat.desc[count, "se"] * 1.96
    }
  }
  
  #Convert to data frame
  plot.desc <- as.data.frame(plot.mat.desc, stringsAsFactors = TRUE)
  plot.desc$group <- as.factor(as.character(plot.desc$group))
  
  #Change to integer values
  if(type == "node")
  {
    plot.mat.select$prop <- sprintf("%1.2f",plot.mat.select$prop)
    plot.desc$prop <- sprintf("%1.2f", plot.desc$prop)
    plot.mat.select$prop <- as.factor(as.character(plot.mat.select$prop))
    plot.desc$prop <- as.factor(as.character(plot.desc$prop))
  }
  
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
  
  # Rainclouds for repeated measures, continued
  if(type == "node")
  {
    pl <- ggplot(plot.mat.select, aes(x = prop, y = netmeas, fill = group)) +
      
      geom_flat_violin(aes(fill = group),position = position_nudge(x = 0.05, y = 0),
                       adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
      
      geom_point(aes(x = as.numeric(prop)-.125, y = netmeas, colour = group),
                 position = position_jitter(width = .05), alpha = .3, size = 1, shape = 20) +
      
      geom_boxplot(aes(x = prop, y = netmeas, fill = group),outlier.shape = NA,
                   alpha = .5, width = .1, colour = "black") +
      
      geom_point(data = plot.desc, aes(x = prop, y = mean),
                 position = position_nudge(x = -.125),
                 alpha = 1, pch = 21, size = 3) +
      
      #geom_errorbar(data = plot.desc, aes(x = prop, y = mean, ymin = mean - lower.ci, ymax = mean + upper.ci),
      #              position = position_nudge(x = -.25, y = .05),
      #              colour = "black", width = 0.1, size = 0.8, alpha = .5) +
      
      scale_colour_brewer(name = "Groups", labels = groups, palette = "Dark2") +
      
      scale_fill_brewer(name = "Groups", labels = groups, palette = "Dark2") +
      
      labs(title = paste("Bootstrap Node-drop Results:",netmeas,sep=" "),
           subtitle = paste(iter,"Samples",sep = " "),
           x = "Proportion of\nNodes Remaining",
           y = paste(full.meas," (",netmeas,")",sep="")) +
      
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 16),
            axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 14)) +
      
      scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
      
      coord_flip()
    
  }else{
    pl <- ggplot(plot.mat.select, aes(x = group, y = netmeas, fill = group)) +
      
      geom_flat_violin(aes(fill = group),position = position_nudge(x = 0.05, y = 0),
                       adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
      
      geom_point(aes(x = group, y = netmeas, colour = group),
                 position = position_jitter(width = .05), alpha = .3, size = 1, shape = 20) +
      
      geom_boxplot(aes(x = group, y = netmeas, fill = group),outlier.shape = NA,
                   alpha = .5, width = .1, colour = "black") +
      
      geom_point(data = plot.desc, aes(x = group, y = mean),
                 position = position_nudge(x = -.125),
                 alpha = 1, pch = 21, size = 3) +
      
      #geom_errorbar(data = plot.desc, aes(x = prop, y = mean, ymin = mean - lower.ci, ymax = mean + upper.ci),
      #              position = position_nudge(x = -.25, y = .05),
      #              colour = "black", width = 0.1, size = 0.8, alpha = .5) +
      
      scale_colour_brewer(name = "Groups", labels = groups, palette = "Dark2") +
      
      scale_fill_brewer(name = "Groups", labels = groups, palette = "Dark2") +
      
      scale_x_discrete(label = name) +
      
      labs(title = paste("Bootstrap Case-wise Results:",netmeas,sep=" "),
           subtitle = paste(iter,"Samples",sep = " "),
           x = "Groups",
           y = paste(full.meas," (",netmeas,")",sep="")) +
      
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 16),
            axis.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 14)) +
      
      scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
      
      coord_flip()
  }
  
  return(pl)
}
#----

#' Generates a Random Network
#' 
#' @description Generates a random binary network
#' 
#' @param nodes Numeric.
#' Number of nodes in random network
#' 
#' @param edges Numeric.
#' Number of edges in random network
#' 
#' @param A Matrix or data frame.
#' An adjacency matrix (i.e., network) to be used to estimated a random network with
#' fixed edges (allows for asymmetric network estimation)
#' 
#' @return Returns an adjacency matrix of a random network
#' 
#' @examples
#' rand <- randnet(10, 27)
#' 
#' @references 
#' Rubinov, M., & Sporns, O. (2010). 
#' Complex network measures of brain connectivity: Uses and interpretations. 
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#' 
#' Csardi, G., & Nepusz, T. (2006).
#' The \emph{igraph} software package for complex network research.
#' \emph{InterJournal, Complex Systems}, 1695.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Random Network----
# Updated 20.04.2020
randnet <- function (nodes = NULL, edges = NULL, A = NULL)
{
  if(is.null(A))
  {
    # Initialize matrix
    mat <- matrix(1, nrow = nodes, ncol = nodes)
    
    # Set diagonal to zero
    diag(mat) <- 0
    
    # Indices of upper diagonal
    ind <- ifelse(upper.tri(mat) == TRUE, 1, 0)
    i <- which(ind == 1)
    
    # Sample zeros and ones
    rp <- sample(length(i))
    # Get indices
    irp <- i[rp]
    
    # Initialize random matrix
    rand <- matrix(0, nrow = nodes, ncol = nodes)
    
    # Insert edges
    rand[irp[1:edges]] <- 1
    
    # Make symmetric
    rand <- rand + t(rand)
    
  }else{
    
    # Make diagonal of network zero
    diag(A) <- 0
    
    # Compute degree
    degrees <- NetworkToolbox::degree(A)
    
    # Get degrees based on directed or undirected
    # Use igraph
    if(is.list(degrees))
    {rand <- as.matrix(igraph::as_adj(igraph::sample_degseq(out.deg = degrees$outDegree, in.deg = degrees$inDegree, method = "vl")))
    }else{rand <- as.matrix(igraph::as_adj(igraph::sample_degseq(out.deg = degrees, method = "vl")))}
  }
  
  return(rand)
}

#' Sub-routine function for \code{\link[SemNeT]{test.bootSemNeT}}
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
#' @examples
#' # Simulate Dataset
#' one <- sim.fluency(20)
#' two <- sim.fluency(20)
#' \donttest{
#' # Run partial bootstrap networks
#' two.result <- bootSemNeT(one, two, prop = .50, iter = 1000,
#' sim = "cosine", cores = 2, method = "TMFG", type = "node")
#' }
#' # Compute tests
#' boot.one.test(two.result)
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
#' boot.fifty <- bootSemNeT(hihi, hilo, lohi, lolo, prop = .50, method = "TMFG", type = "node")
#' 
#' ## Compute tests
#' boot.one.test(boot.fifty, formula = "y ~ gf*caq", groups = groups)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats t.test aov TukeyHSD as.formula
#' 
#' @noRd
# Test: Bootstrapped Network Statistics----
# Updated 21.05.2020
boot.one.test <- function (bootSemNeT.obj, formula = NULL, groups = NULL)
{
  #Check for 'partboot' object
  if(class(bootSemNeT.obj) != "bootSemNeT")
  {stop("Object input into 'bootSemNeT.obj' is not a 'bootSemNeT' object")}
  
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
      aov.obj <- as.data.frame(cbind(aov.obj, rep.rows(groups, iter)), stringsAsFactors = FALSE)
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
      aov.obj <- as.data.frame(cbind(aov.obj, rep.rows(groups, iter)), stringsAsFactors = FALSE)
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
      aov.obj <- as.data.frame(cbind(aov.obj,rep.rows(groups, iter)), stringsAsFactors = FALSE)
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

#' Co-occurence (sub-routine for Goni)
#' 
#' @description Computes co-occurence of responses within
#' a given window
#' 
#' @param data Matrix or data frame.
#' Preprocessed verbal fluency data
#' 
#' @param window Numeric.
#' Size of window to look for co-occurences in
#' 
#' @return A binary matrix 
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Co-occurrence matrix----
# Updated 26.03.2020
cooccur <- function(data, window = 2)
{
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace "" with NA
  mat <- ifelse(mat == "", NA, mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Initialize number matrix
  num.mat <- mat
  
  # Replace responses with numbers
  for(i in 1:nrow(mat))
  {num.mat[i,] <- match(mat[i,], uniq.resp)}
  
  # Convert to numeric
  num.mat <- apply(num.mat,2,as.numeric)
  
  # Add NAs the length of window to matrix
  num.mat <- as.matrix(cbind(matrix(NA, nrow = nrow(num.mat), ncol = window),
                             num.mat,
                             matrix(NA, nrow = nrow(num.mat), ncol = window)))
  
  # Co-occurence matrix
  co.mat <- matrix(0, nrow = length(uniq.resp), ncol = length(uniq.resp))
  
  # Loop through each word
  for(i in 1:length(uniq.resp))
    for(j in 1:nrow(data))
    {
      # Find word position
      pos <- which(i == num.mat[j,])
      
      if(length(pos) != 0)
      {
        # Word neighbors
        for(k in 1:length(pos))
        {
          if(k == 1)
          {pos_neigh <- c((pos[k] - window:1), pos[k] + 1:window)
          }else{pos_neigh <- c(pos_neigh, c((pos[k] - window:1), pos[k] + 1:window))}
        }
        
        # Get neighboring words
        words <- num.mat[j, pos_neigh]
        
        # Unique words (excluding self)
        words <- setdiff(unique(words[!is.na(words)]),i)
        
        # Count co-occurence
        co.mat[i, words] <- co.mat[i, words] + 1
      }
    }
  
  return(co.mat)
}

#' Relative frequency (sub-routine for Goni)
#' 
#' @description Computes relative frequency of each response
#' 
#' @param data Matrix or data frame.
#' Preprocessed verbal fluency data
#' 
#' @return A vector of relative frequencies
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Relative frequency----
# Updated 26.03.2020
rel.freq <- function(data)
{
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace "" with NA
  mat <- ifelse(mat == "", NA, mat)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Initialize matrix
  rel_freq <- numeric(length(uniq.resp))
  
  # Input into matrix
  for(i in 1:length(uniq.resp))
  {
    # Initialize count
    count <- 0
    
    # Target word
    target <- uniq.resp[i]
    
    # Loop through cases
    for(j in 1:nrow(mat))
    {
      # If target word is in case,
      # then count it
      if(length(which(target == mat[j,])) != 0)
      {count <- count + 1}
    }
    
    # Insert proportion of responses
    rel_freq[i] <- count / nrow(mat)
    
  }
  
  return(rel_freq)
}

#' Statistical Co-occurence (sub-routine for Goni)
#' 
#' @description Computes the statistical co-occurence of responses+36
#' 60.23
#' 
#' @param data Matrix or data frame.
#' Preprocessed verbal fluency data
#' 
#' @param window Numeric.
#' Size of window to look for co-occurences in.
#' Defaults to \code{2}
#' 
#' @param alpha Numeric.
#' Significance value.
#' Defaults to \code{.05}
#' 
#' @return A binary matrix 
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats binom.test
#' 
#' @noRd
# Statistical Co-occurrence----
# Updated 27.03.2020
stat.cooccur <- function(data, window = 2, alpha = .05)
{
  # Check if the matrix is numeric
  if(any(apply(data, 2, is.numeric)))
  {stop("CN(): Only a response matrix can be used as input for 'data'")}
  
  # Data matrix
  mat <- as.matrix(data)
  
  # Replace bad responses with NA
  mat <- bad.response(mat)
  
  # Number of particpants
  m <- nrow(mat)
  
  # Average number of responses
  n <- round(mean(rowSums(!is.na(mat))),2)
  
  # Unique responses
  uniq.resp <- sort(na.omit(unique(as.vector(mat))))
  
  # Number of unique responses
  num_words <- length(uniq.resp)
  
  # Adjacency matrix
  adj <- matrix(0, nrow = num_words, ncol = num_words)
  
  # Compute relative frequencies
  rel_freq <- rel.freq(data)
  
  # Compute co-occurrences
  co_occ <- cooccur(data, window = window)
  
  # Probability of random co-occurrence
  rand_co <- (2 / (n * (n-1))) * ((window * n) - (window * (window + 1)/2))
  
  # Intialize words studied
  words_studied <- numeric(num_words)
  
  for(i in 1:(num_words-1))
    for(j in (i+1):num_words)
    {
      # Number of co-occurrences
      num_co <- co_occ[i,j]
      
      if(num_co > 1)
      {
        # Update words studied
        words_studied[i] <- 1
        words_studied[j] <- 1
        
        # Threshold
        thresh <- rel_freq[i] * rel_freq[j] * rand_co
        
        # Compute binomial confidence interval
        int <- binom.test(num_co, m, alpha)$conf.int
        
        if(thresh < int[1])
        {
          adj[i,j] <- 1
          adj[j,i] <- 1
        }
      }
    }
  
  # Provide column names
  colnames(adj) <- uniq.resp
  row.names(adj) <- uniq.resp
  
  return(adj)
}



#' @noRd
### cosine.R from lsa package (now archived)
###
### 2009-09-14:
###   * added vector vs. matrix comparison
###     to reduce data load when looking for associations
### 2005-11-21:
###   * added lazy calculation:
###     calc only below diagonale; diag = 1; add t(co)
###   * sqrt() over crossprod(x) and crossprod(y)
### 2005-11-09:
###   * bugfix cosvecs
###   * integrated cosvecs into cosine by doing type dependant processing
### 2005-08-26:
###   * rewrote cosvecs function to crossprod
### 
cosine <- function( x, y=NULL ) {
  
  if ( is.matrix(x) && is.null(y) ) {
    
    co = array(0,c(ncol(x),ncol(x)))
    f = colnames( x )
    dimnames(co) = list(f,f)
    
    for (i in 2:ncol(x)) {
      for (j in 1:(i-1)) {
        co[i,j] = cosine(x[,i], x[,j])
      }
    }
    co = co + t(co)
    diag(co) = 1
    
    return (as.matrix(co))
    
  } else if ( is.vector(x) && is.vector(y) ) {
    return ( crossprod(x,y) / sqrt( crossprod(x)*crossprod(y) ) )
  } else if ( is.vector(x) && is.matrix(y) ) {
    
    co = vector(mode='numeric', length=ncol(y))
    names(co) = colnames(y)
    for (i in 1:ncol(y)) {
      co[i] = cosine(x,y[,i]) 
    }
    return(co)
    
  }	else {
    stop("argument mismatch. Either one matrix or two vectors needed as input.")
  }
  
}

#' @noRd
# Function to replicate rows----
# Updated 21.05.2020
rep.rows <- function (mat, times)
{
  # New matrix
  new.mat <- matrix(NA, nrow = 0, ncol = ncol(mat))
  
  # Loop through rows of matrix
  for(i in 1:nrow(mat))
  {new.mat <- rbind(new.mat, matrix(rep(mat[i,], times = times), ncol = ncol(mat), byrow = TRUE))}
  
  # Rename new matrix
  colnames(new.mat) <- colnames(mat)
  
  return(new.mat)
}
