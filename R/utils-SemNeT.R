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
# Updated 16.09.2020
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
  {
    target.resps <- na.omit(match(mat[i,], colnames(bin.mat)))
    bin.mat[i,target.resps] <- 1:length(target.resps)
  }
  
  # Result list
  res <- list()
  res$binary <- binarize(bin.mat)
  res$order <- bin.mat
  
  class(res) <- "resp2bin"
  
  return(res)
}

#' @noRd
# Sets up TMFG for bootstrap and permutation----
# Updated 01.09.2020
tmfg_setup <- function(..., minCase)
{
  if(is.null(minCase))
  {minCase <- 2}
  
  name <- as.character(substitute(list(...)))
  name <- name[-which(name=="list")]
  
  dat <- list(...)
  
  for(i in 1:length(dat))
  {
    if(is.character(unlist(dat[[i]])))
    {dat[[i]] <- resp2bin(dat[[i]])$binary}
    
    dat[[i]] <- finalize(dat[[i]], minCase)
  }
  
  names(dat) <- name
  
  eq <- equateShiny(dat)
  
  return(eq)
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
# Updated 16.09.2020
bin2resp <- function (rmat, to.data.frame = FALSE)
{
  # Check for resp2bin class
  if(class(rmat) == "resp2bin" || {all(apply(rmat, 2, is.numeric)) && max(rmat) > 1})
  {
    # Get ordered responses
    ordered <- rmat$order
    
    # Maximum responses
    max.resp <- max(rowSums(binarize(ordered)))
    
    # Initialize matrix
    mat <- matrix(NA, nrow = nrow(ordered), ncol = max.resp)
    colnames(mat) <- paste("Response_", formatC(1:max.resp,
                                                digits = nchar(max.resp) - 1,
                                                flag = 0,
                                                format = "d"), sep = "")
    row.names(mat) <- row.names(ordered)
    
    for(i in 1:nrow(ordered))
    {
      target <- ordered[i,]
      
      responses <- target[as.vector(target != 0)]
      
      named.responses <- names(responses[order(responses)])
      
      mat[i,1:length(named.responses)] <- named.responses
    }
    
  }else{
    
    # Maximum responses
    max.resp <- max(rowSums(rmat))
    
    # Initialize matrix
    mat <- matrix(NA, nrow = nrow(rmat), ncol = max.resp)
    colnames(mat) <- paste("Response_", formatC(1:max.resp,
                                                digits = nchar(max.resp) - 1,
                                                flag = 0,
                                                format = "d"), sep = "")
    row.names(mat) <- row.names(rmat)
    
    for(i in 1:nrow(rmat))
    {
      target <- rmat[i,]
      
      responses <- target[as.vector(target != 0)]
      
      named.responses <- names(responses)
      
      mat[i,1:length(named.responses)] <- named.responses
    }
  }
  
  if(to.data.frame)
  {mat <- as.data.frame(mat, stringsAsFactors = FALSE)}
  
  return(mat)
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
# Updated 03.09.2020
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
    degrees <- colSums(binarize(A))
    
    # Get degrees based on directed or undirected
    # Use igraph
    rand <- as.matrix(igraph::as_adj(igraph::sample_degseq(out.deg = degrees, method = "vl")))
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
# Updated 25.09.2020
boot.one.test <- function (bootSemNeT.obj, measures = c("ASPL", "CC", "Q"), formula = NULL, groups = NULL)
{
  #Check for 'bootSemNeT' object
  if(class(bootSemNeT.obj) != "bootSemNeT")
  {stop("Object input into 'bootSemNeT.obj' is not a 'bootSemNeT' object")}
  
  #Check for data if formula is not NULL
  if(!is.null(formula))
  {
    if(!exists("groups"))
    {stop("'groups' argument is NULL when 'formula' argument is not. Please input groups.")}
  }
  
  #Get names of networks
  name <- unique(gsub("Net","",gsub("Summ","",gsub("Meas","",names(bootSemNeT.obj)))))
  
  #Remove proportion and iter
  name <- na.omit(gsub("type",NA,gsub("iter",NA,gsub("prop",NA,name))))
  attr(name, "na.action") <- NULL
  
  #Number of input
  len <- length(name)
  
  #Error there are no paired samples
  if(len < 2)
  {stop("Single samples cannot be tested. Use 'randnet.test' for single samples")}
  
  #Handle groups
  if(is.null(groups))
  {groups <- name}
  
  #Enforce matrix
  groups <- as.matrix(groups)
  
  #Check for groups names
  if(is.null(colnames(groups)))
  {colnames(groups) <- ifelse(ncol(groups) == 1, "Group", paste("Group", 1:ncol(groups), sep = ""))}
  
  #Identify iterations
  iter <- bootSemNeT.obj$iter
  
  ############################
  #### SIGNIFICANCE TESTS ####
  ############################
  
  #Input results into list
  tests <- list()
  
  #Loop through measures
  for(i in 1:length(measures))
  {
    #Create ANCOVA data frame
    for(j in 1:len)
    {
      #Insert measure values
      meas <- bootSemNeT.obj[[paste(name[j],"Meas",sep="")]][measures[i],]
      
      # Nodes
      nodes <- unlist(lapply(bootSemNeT.obj[[paste(name[j],"Net",sep="")]], function(x){ncol(x)}))
      
      # Edges
      edges <- unlist(lapply(bootSemNeT.obj[[paste(name[j],"Net",sep="")]], function(x){
        net <- binarize(x)
        diag(net) <- 0
        return(sum(net) / 2)
      }))
      
      #Initialize matrix
      mat <- cbind(rep(name[j], length(meas)), meas, nodes, edges)
      
      if(j != 1)
      {new.mat <- rbind(new.mat, mat)
      }else{new.mat <- mat}
    }
    
    #Convert to data frame
    aov.obj <- as.data.frame(new.mat, stringsAsFactors = FALSE)
    colnames(aov.obj) <- c("Name", "Measure", "Nodes", "Edges")
    aov.obj$Name <- factor(as.character(aov.obj$Name))
    aov.obj[,2:4] <- apply(aov.obj[,2:4], 2, function(x){as.numeric(as.character(x))})
    
    #Organize groups
    aov.obj <- as.data.frame(cbind(aov.obj, rep.rows(groups, iter)), stringsAsFactors = FALSE)
    
    #Get column before groups
    edge.col <- which(colnames(aov.obj) == "Edges")
    
    #Convert groups to factors
    for(g in 1:ncol(groups))
    {aov.obj[,(edge.col+g)] <- as.factor(as.character(aov.obj[,(edge.col+g)]))}
    
    #Remove variables that are all equal
    keep.vars <- apply(aov.obj[,1:ncol(aov.obj)], 2, function(x){length(unique(x)) != 1})
    aov.obj <- aov.obj[,keep.vars]
    
    #Group mean center
    ## See Understanding and misunderstanding group mean centering: a commentary on Kelley et al.'s dangerous practice
    ## Bell, A., Jones, K., & Fairbrother, M. (2018).
    ## \emph{Quality & Quantity Volume} \emph{52}, 2031-2036.
    ##https://doi.org/10.1007/s11135-017-0593-5
    if("Nodes" %in% names(aov.obj))
    {
      for(g in 1:nrow(groups))
      {aov.obj$Nodes[which(aov.obj$Group == groups[g,])] <- scale(aov.obj$Nodes[which(aov.obj$Group == groups[g,])])}
    }
    
    if("Edges" %in% names(aov.obj))
    {
      for(g in 1:nrow(groups))
      {aov.obj$Edges[which(aov.obj$Group == groups[g,])] <- scale(aov.obj$Edges[which(aov.obj$Group == groups[g,])])}
    }
    
    #Formula
    if(is.null(formula))
    {formula <- paste("y ~", paste(colnames(groups), collapse = " + "))}
    
    #Replace 'y' with 'Measure'
    formula <- gsub("y", "Measure", formula)
    
    #Split formula to add 'Nodes' and 'Edges'
    split.formula <- unlist(strsplit(formula, split = "~"))
    
    #ANOVA formula
    ##Catch Pathfinder Network method
    if(all(aov.obj$Nodes - aov.obj$Edges == 1))
    {aov.formula <- paste(split.formula[1], "~ ", paste(names(keep.vars)[4][keep.vars[4]], collapse = " + "), " +", split.formula[2], sep = "")
    }else{aov.formula <- paste(split.formula[1], "~ ", paste(names(keep.vars)[3:4][keep.vars[3:4]], collapse = " + "), " +", split.formula[2], sep = "")}
    
    #ANOVA
    aov.test <- aov(as.formula(aov.formula), data = aov.obj)
    
    #ANCOVA
    acov.test <- car::Anova(aov.test, type = "III")
    
    #Tidy ANCOVA
    tidy.acov <- as.data.frame(broom::tidy(acov.test), stringsAsFactors = FALSE)
    tidy.acov[,-1] <- round(apply(tidy.acov[,-1], 2, as.numeric), 3)
    
    #Get partial etas
    etas <- round(unlist(lapply(acov.test$`Sum Sq`, partial.eta.sq, sum(acov.test$`Sum Sq`[length(acov.test$`Sum Sq`)])))[-c(1,length(acov.test$`Sum Sq`))], 3)
    
    #Attach etas to tidy ANCOVA
    tidy.acov <- as.data.frame(cbind(tidy.acov, c(NA, etas, NA)), stringsAsFactors = FALSE)
    
    #Change column names
    colnames(tidy.acov) <- c("Term", "Sum of Squares", "df", "F-statistic", "p-value", "Partial Eta Squared")
    
    #Change p-values
    tidy.acov$`p-value` <- ifelse(tidy.acov$`p-value` < .001, "< .001", tidy.acov$`p-value`)
    
    # Convert NA to blank values
    tidy.acov <- as.data.frame(apply(tidy.acov, 2, function(x){trimws(ifelse(is.na(x), "", x))}), stringsAsFactors = FALSE)
    
    #Get adjusted means
    adj.means <- effects::allEffects(aov.test)
    
    #Insert ANCOVA
    tests[[paste(measures[i])]]$ANCOVA <- tidy.acov
    
    #Insert adjusted means
    tests[[paste(measures[i])]]$adjustedMeans <- adj.means[[which(names(adj.means) != "Nodes" & names(adj.means) != "Edges")]]
    
    #Get pairwise comparisons
    if(length(unique(groups[,1])) > 2)
    {tests[[paste(measures[i])]]$HSD <- unlist(suppressWarnings(TukeyHSD(aov.test)), recursive = FALSE)}
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

#' Statistical Co-occurence (sub-routine for \code{\link[SemNeT]{CN}})
#' 
#' @description Computes the statistical co-occurence of responses
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
# Updated 16.09.2020
stat.cooccur <- function(data, window = 2, alpha = .05)
{
  # Check if the matrix is numeric
  if(any(apply(data, 2, is.numeric)))
  {
    if(max(range(data)) == 1)
    {stop("CN(): Only a response matrix or ordered numeric matrix can be used as input for 'data'")
    }else{data <- bin2resp(data)}
  }
  
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

#' Generalized Topological Overlap (sub-routine for \code{\link[SemNeT]{CN}})
#' 
#' @description Computes the genearlized topological overlap
#' 
#' @param A Matrix or data frame.
#' A \code{\link[SemNeT]{CN}} estimated matrix
#' 
#' @param steps Numeric.
#' Number of connections away from original node.
#' Defaults to \code{2}
#' 
#' @return A weighted topological overlap matrix
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Generalized Topological Overlap----
# Updated 13.09.2020
gtom <- function (A, steps = 2)
{
  # Initial matrix state
  bm <- A
  bmAux <- bm
  numNodes <- ncol(A)
  
  # Compute generalized topological overlap
  for(j in 2:steps)
  {
    for(i in 1:numNodes)
    {
      # Neighbors of node i
      neighbors <- unname(which(bm[i,] == 1))
      neighbors <- as.vector(neighbors)
      
      if(length(neighbors) == 1)
      {bm.neighbors <- t(as.matrix(bm[neighbors,]))
      }else{bm.neighbors <- bm[neighbors,]}
      
      # Neighbors of neighbors of node i
      neighborsOfNeighbors <- unname(unlist(apply(bm.neighbors, 1, function(x){which(x == 1)})))
      # All neighbors
      allNeighbors <- setdiff(unique(neighborsOfNeighbors),i)
      # Input into new matrix
      bmAux[i,allNeighbors] <- 1
      bmAux[allNeighbors,i] <- 1
    }
    
    bm <- bmAux
  }
  
  numeratorMatrix <- bm %*% bm + A + diag(1, ncol(A))
  
  bmSum <- colSums(bm)
  
  repmat <- matrix(rep(bmSum, ncol(A)), nrow = ncol(A), byrow = TRUE)
  
  trepmat <- t(repmat)
  
  denominatorMatrix <- -A +  pmin(repmat, trepmat) + 1
  
  GTOM <- numeratorMatrix / denominatorMatrix
  
  return(GTOM)
}

#' Enrich Network (sub-routine for \code{\link[SemNeT]{CN}})
#' 
#' @description Connections all nodes within their respective modules
#' 
#' @param A Matrix or data frame.
#' A \code{\link[SemNeT]{CN}} estimated matrix
#' 
#' @param GTOM Matrix or data frame.
#' Output from \code{\link[SemNeT]{gtom}}
#' 
#' @return An enriched \code{\link[SemNeT]{CN}} network
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats hclust cutree as.dist
#' 
#' @noRd
# Enrich Network----
# Updated 13.09.2020
enrich.network <- function(A, GTOM)
{
  # Get dissimilarity matrix
  dis.sim <- as.dist(1 - GTOM, upper = TRUE)
  
  # Get linkage
  Z <- hclust(d = dis.sim, method = "average")
  
  # Cut based on Goni et al. (2011)
  modules <- cutree(Z, h = 0.35)
  
  # Number of modules
  numModules <- max(modules)
  
  for(i in 1:numModules)
  {
    neighMod <- which(modules == i)
    A[neighMod, neighMod] <- 1
  }
  
  # Remove diagonal
  diag(A) <- 0
  
  return(A)
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

#' @noRd
# Partial eta squared----
# Updated 02.09.2020
partial.eta.sq <- function(ESS, RSS)
{ESS / (ESS + RSS)}

#' Convert Network(s) to igraph's Format
#' @description Converts single or multiple networks into \code{\link{igraph}}'s format for network analysis
#' 
#' @param A Adjacency matrix (network matrix) or brain connectivity array
#' (from \code{\link[NetworkToolbox]{convertConnBrainMat}})
#' 
#' @param neural Is input a brain connectivity array (i.e., m x m x n)?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to convert each brain connectivity matrix
#' 
#' @return Returns a network matrix in \code{\link{igraph}}'s format or
#' returns a list of brain connectivity matrices each of which have been
#' convert to \code{\link{igraph}}'s format
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(neoOpen, normal = FALSE)$A
#' 
#' igraphNetwork <- convert2igraph(A)
#' 
#' \dontrun{ 
#' neuralarray <- convertConnBrainMat()
#' 
#' igraphNeuralList <- convert2igraph(neuralarray, neural = TRUE)
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Convert matrices to igraph format (from NetworkToolbox)----
# Updated 02.09.2020
convert2igraph <- function (A, neural = FALSE)
{
  if(!neural)
  {
    net <- igraph::as.igraph(qgraph::qgraph(A,DoNotPlot=TRUE))
    return(net)
  }else if(neural)
  {
    netlist <- list()
    
    n<-length(A)/nrow(A)/ncol(A)
    
    pb <- txtProgressBar(max=n, style = 3)
    
    for(i in 1:n)
    {
      netlist[[i]] <- igraph::as.igraph(qgraph::qgraph(A[,,i],DoNotPlot=TRUE))
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    return(netlist)
  }
}
#----

#' Distance
#' @description Computes distance matrix of the network
#' 
#' @param A An adjacency matrix of network data
#' 
#' @param weighted Is the network weighted?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} for weighted measure of distance
#' 
#' @return A distance matrix of the network
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(neoOpen, normal = FALSE)$A
#' 
#' #Unweighted
#' Du <- distance(A)
#' 
#' #Weighted
#' Dw <- distance(A, weighted = TRUE)
#' 
#' @references 
#' Rubinov, M., & Sporns, O. (2010). 
#' Complex network measures of brain connectivity: Uses and interpretations. 
#' \emph{NeuroImage}, \emph{52}, 1059-1069.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Distance (SemNeT)----
# Updated 02.09.2020
distance<-function (A, weighted = FALSE)
{
  if(nrow(A)!=ncol(A))
  {stop("Input not an adjacency matrix")}
  if(!weighted)
  {B<-ifelse(A!=0,1,0)
  l<-1
  Lpath<-B
  D<-B
  Idx<-matrix(TRUE,nrow=nrow(B),ncol=ncol(B))
  while(any(Idx))
  {
    l<-l+1
    Lpath<-(as.matrix(Lpath))%*%(as.matrix(B))
    for(e in 1:nrow(Lpath))
      for(w in 1:ncol(Lpath))
        Idx[e,w]<-(Lpath[e,w]!=0&&(D[e,w]==0))
    D[Idx]<-l
  }
  
  D[!D]<-Inf
  diag(D)<-0
  }else if(weighted){
    G<-ifelse(1/A==Inf,0,1/A)
    
    if(any(G==-Inf))
    {G<-ifelse(G==-Inf,0,G)}
    
    if(any(!G==t(G)))
    {if(max(abs(G-t(G)))<1e-10)
    {G<-(G+G)/2}}
    
    n<-ncol(G)
    D<-matrix(Inf,nrow=n,ncol=n)
    diag(D)<-0
    B<-matrix(0,nrow=n,ncol=n)
    
    for(u in 1:n)
    {
      S<-matrix(TRUE,nrow=n,ncol=1)
      L1<-G
      V<-u
      while(TRUE)
      {
        S[V]<-0
        L1[,V]<-0
        for(v in V)
        {
          W<-which(L1[v,]!=0)
          d<-apply(rbind(D[u,W],(D[u,v]+L1[v,W])),2,min)    
          wi<-apply(rbind(D[u,W],(D[u,v]+L1[v,W])),2,which.min)
          D[u,W]<-d
          ind<-W[wi==2]
          B[u,ind]<-B[u,v]+1
        }
        
        minD<-suppressWarnings(min(D[u,S==TRUE]))
        if(length(minD)==0||is.infinite(minD)){break}
        
        V<-which(D[u,]==minD)
      }
    }
  }
  
  D<-ifelse(D==Inf,0,D)
  
  colnames(D)<-colnames(A)
  row.names(D)<-colnames(A)
  return(D)
}
#----

#' Binarize Network
#' 
#' @description Converts weighted adjacency matrix to a binarized adjacency matrix
#' 
#' @param A An adjacency matrix of network data (or an array of matrices)
#' 
#' @return Returns an adjacency matrix of 1's and 0's
#' 
#' @examples
#' # Pearson's correlation only for CRAN checks
#' A <- TMFG(neoOpen, normal = FALSE)$A
#' 
#' neoB <- binarize(A)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Binarize (SemNeT)----
# Updated 02.09.2020
binarize <- function (A)
{
  A <- as.matrix(A)
  bin <- ifelse(A!=0,1,0)
  row.names(bin) <- row.names(A)
  colnames(bin) <- colnames(A)
  
  return(bin)
}
#----
