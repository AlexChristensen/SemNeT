#' @title Plot for \code{\link{bootstrap_SemNeT}}
#'
#' @description Plots each measure's bootstrap distribution as a raincloud plot
#' (Allen et al., 2018): a half violin, boxplot, and the jittered bootstrap values,
#' with one panel per measure and the groups side by side. A bootstrap confidence
#' interval of each group's value is drawn below its boxplot
#'
#' @param x A \code{\link{bootstrap_SemNeT}} object
#'
#' @param measures Character.
#' One or more of \code{"aspl"}, \code{"cc"}, and \code{"q"}.
#' Defaults to all three
#'
#' @param ci Boolean (length = 1).
#' Whether to draw each group's confidence interval below its boxplot.
#' Defaults to \code{TRUE}.
#' Node bootstraps (\code{type = "node"}) subsample words, which changes the
#' measures themselves, so no interval is drawn for them
#'
#' @param ci_level Numeric (length = 1).
#' Confidence level between 0 and 1 (exclusive).
#' Defaults to \code{.95}
#'
#' @param ci_type Character (length = 1).
#' Type of interval, as in \code{\link{bootstrap_test_SemNeT}}.
#' Defaults to \code{"normal"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"normal"} --- The group's value for the full data plus and minus
#' the normal quantile times the bootstrap standard error. Needs the full data's
#' observed value (falls back to the bootstrap mean without it)
#'
#' \item \code{"bc"} --- Bias-corrected percentile interval. Needs the full
#' data's observed value (falls back to \code{"percentile"} without it)
#'
#' \item \code{"percentile"} --- Percentiles of the bootstrap values
#'
#' }
#'
#' @param ... Not used
#'
#' @return Returns a \code{ggplot} object (printed at the console and
#' compatible with \code{shiny::renderPlot()})
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' # Get openness data
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Bootstrap networks
#' boot_result <- bootstrap_SemNeT(
#'   low = low, high = high, iter = 100, cores = 2,
#'   type = "case", method = "tmfg"
#' )
#'
#' # Plot
#' plot(boot_result)
#' }
#'
#' @references
#' \strong{Raincloud plots} \cr
#' Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit, R. A. (2018).
#' Raincloud plots: a multi-platform tool for robust data visualization.
#' \emph{PeerJ Preprints}, \emph{6}, e27137v1.
#' \doi{10.7287/peerj.preprints.27137v1}
#'
#' @export
# Plot bootstrap ----
# Updated 22.10.2026
plot.bootstrap_SemNeT <- function(
    x, measures = c("aspl", "cc", "q"), ci = TRUE, ci_level = .95,
    ci_type = c("normal", "bc", "percentile"), ...
)
{

  # Check for {ggplot2}
  if(!requireNamespace("ggplot2", quietly = TRUE)){

    # Send error
    stop(
      "In `plot.bootstrap_SemNeT()`: the 'ggplot2' package is required to draw this plot but isn't installed.\n",
      "Install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )

  }

  # Remove {ggplot2} variable notes
  group <- value <- NULL

  # Argument errors
  ci_type <- match.arg(ci_type)
  measures <- plot_bootstrap_SemNeT_errors(x, measures, ci, ci_level)

  # Check for node bootstrap
  if(ci && identical(x$meta$type, "node")){

    # Send message
    message(
      "In `plot.bootstrap_SemNeT()`: node bootstraps subsample words, so their spread is not ",
      "a confidence interval -- no interval is drawn."
    )

    # Turn off intervals
    ci <- FALSE

  }

  # Get group names
  group_names <- names(x$groups)

  # Get measure labels
  measure_labels <- toupper(measures)

  # Set up plot data
  plot_data <- do.call(rbind, lapply(group_names, function(group_name){

    # Loop over measures
    do.call(rbind, lapply(seq_along(measures), function(i){

      # Set up data of group and measure
      data.frame(
        group = group_name,
        measure = measure_labels[i],
        value = x$groups[[group_name]]$measures[measures[i], ],
        stringsAsFactors = FALSE
      )

    }))

  }))

  # Set order of groups
  plot_data$group <- factor(plot_data$group, levels = group_names)

  # Set order of measures
  plot_data$measure <- factor(plot_data$measure, levels = measure_labels)

  # Get mean of each group and measure
  summary_data <- stats::aggregate(value ~ group + measure, data = plot_data, FUN = mean)

  # Set up observed data
  observed_data <- do.call(rbind, lapply(group_names, function(group_name){

    # Get observed measures
    observed <- x$groups[[group_name]]$observed$measures

    # Check for no observed measures
    if(is.null(observed)){

      # Return nothing
      return(NULL)

    }

    # Set up data of group
    data.frame(
      group = group_name, measure = measure_labels, value = unname(observed[measures]),
      stringsAsFactors = FALSE
    )

  }))

  # Check for observed data
  if(!is.null(observed_data)){

    # Keep finite values
    observed_data <- observed_data[is.finite(observed_data$value), , drop = FALSE]

    # Set order of groups
    observed_data$group <- factor(observed_data$group, levels = group_names)

    # Set order of measures
    observed_data$measure <- factor(observed_data$measure, levels = measure_labels)

  }

  # Set up interval data
  ci_data <- NULL

  # Check for intervals
  if(ci){

    # Get interval of each group and measure
    ci_data <- do.call(rbind, lapply(group_names, function(group_name){

      # Loop over measures
      do.call(rbind, lapply(seq_along(measures), function(i){

        # Get bootstrap values (the plot drops non-finite values too)
        values <- x$groups[[group_name]]$measures[measures[i], ]
        values <- values[is.finite(values)]

        # Get observed value
        observed <- x$groups[[group_name]]$observed$measures[measures[i]]

        # Check for usable observed value
        has_observed <- length(observed) == 1 && is.finite(observed)

        # Get interval
        interval <- if(ci_type == "normal"){

          # Normal interval around observed value
          normal_interval(values, ci_level, if(has_observed) unname(observed) else mean(values))

        }else{

          # Percentile interval (bias-corrected with observed value)
          z0 <- if(ci_type == "bc" && has_observed) bc_z0(values, unname(observed)) else 0
          bc_interval(values, ci_level, z0)

        }

        # Set up data of group and measure
        data.frame(
          group = group_name, measure = measure_labels[i],
          center = if(ci_type == "normal" && has_observed) unname(observed) else mean(values),
          lower = interval[1], upper = interval[2], stringsAsFactors = FALSE
        )

      }))

    }))

    # Set order of groups
    ci_data$group <- factor(ci_data$group, levels = group_names)

    # Set order of measures
    ci_data$measure <- factor(ci_data$measure, levels = measure_labels)

  }

  # Set up plot
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, fill = group)) +

    # Add half violins
    geom_flat_violin(
      ggplot2::aes(fill = group), position = ggplot2::position_nudge(x = 0.15),
      adjust = 1.5, trim = FALSE, alpha = 0.5, colour = NA
    ) +

    # Add raw values
    ggplot2::geom_point(
      ggplot2::aes(colour = group), position = ggplot2::position_jitter(width = 0.05),
      alpha = 0.3, size = 1, shape = 20
    ) +

    # Add boxplots
    ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, colour = "black", outlier.shape = NA) +

    # Add means
    ggplot2::geom_point(
      data = summary_data, ggplot2::aes(x = group, y = value),
      position = ggplot2::position_nudge(x = -0.15), inherit.aes = FALSE, pch = 21, size = 3
    ) +

    # Add observed values
    {
      if(!is.null(observed_data) && nrow(observed_data) > 0){
        ggplot2::geom_point(
          data = observed_data, ggplot2::aes(x = group, y = value),
          position = ggplot2::position_nudge(x = -0.15), inherit.aes = FALSE,
          shape = 23, size = 3, fill = "black"
        )
      }
    } +

    # Add confidence intervals below boxplots
    {
      if(!is.null(ci_data)){
        list(
          ggplot2::geom_errorbar(
            data = ci_data, ggplot2::aes(x = group, ymin = lower, ymax = upper, colour = group),
            position = ggplot2::position_nudge(x = -0.3), inherit.aes = FALSE,
            width = 0.08, linewidth = 0.8
          ),
          ggplot2::geom_point(
            data = ci_data, ggplot2::aes(x = group, y = center, colour = group),
            position = ggplot2::position_nudge(x = -0.3), inherit.aes = FALSE, size = 2
          )
        )
      }
    } +

    # Set fill colors
    ggplot2::scale_fill_brewer(palette = "Dark2") +

    # Set point colors
    ggplot2::scale_colour_brewer(palette = "Dark2") +

    # Facet by measure (free axis because of flipped coordinates)
    ggplot2::facet_wrap(~ measure, scales = "free_x") +

    # Set labels
    ggplot2::labs(
      title = "Bootstrap Results", subtitle = paste(x$meta$iter, "Samples"),
      caption = wrap_caption(
        "Open circle: bootstrap mean.",
        if(!is.null(observed_data) && nrow(observed_data) > 0) "Black diamond: value for the full data.",
        if(!is.null(ci_data)){
          paste0(
            "Bar below each box: ", format(100 * ci_level), "% ",
            switch(ci_type, normal = "normal", bc = "bias-corrected percentile", percentile = "percentile"),
            " interval of the group's value."
          )
        },
        "The spread of the violin and box is not a confidence interval."
      ),
      x = "Group", y = "Value"
    ) +

    # Set theme
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "none",
      plot.caption = ggplot2::element_text(size = 10.5)
    ) +

    # Flip coordinates
    ggplot2::coord_flip()

  # Return plot
  return(plot)

}

#' @noRd
# Errors ----
# Updated 22.10.2026
plot_bootstrap_SemNeT_errors <- function(x, measures, ci, ci_level)
{

  # 'x' errors
  if(!inherits(x, "bootstrap_SemNeT")){

    # Send error
    stop(
      "In `plot.bootstrap_SemNeT()`: 'x' is not a `bootstrap_SemNeT` object (from `bootstrap_SemNeT()`).",
      call. = FALSE
    )

  }

  # 'measures' errors
  typeof_error(measures, "character", "plot.bootstrap_SemNeT")

  # Check for unknown measures
  if(!all(measures %in% c("aspl", "cc", "q"))){

    # Send error
    stop("In `plot.bootstrap_SemNeT()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'.", call. = FALSE)

  }

  # 'ci' errors
  length_error(ci, 1, "plot.bootstrap_SemNeT")
  typeof_error(ci, "logical", "plot.bootstrap_SemNeT")

  # 'ci_level' errors
  length_error(ci_level, 1, "plot.bootstrap_SemNeT")
  typeof_error(ci_level, "numeric", "plot.bootstrap_SemNeT")

  # Check for 'ci_level' outside of (0, 1)
  if(ci_level <= 0 || ci_level >= 1){

    # Send error
    stop("In `plot.bootstrap_SemNeT()`: 'ci_level' must be between 0 and 1 (exclusive).", call. = FALSE)

  }

  # Return 'measures'
  return(measures)

}

#' @noRd
# Wrap caption ----
# Updated 22.10.2026
wrap_caption <- function(...)
{

  # Return caption wrapped over lines
  return(paste(strwrap(paste(..., sep = " "), width = 80), collapse = "\n"))

}

#' @noRd
# Half violin geom ----
# Updated 12.09.2026
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "dodge", trim = TRUE, scale = "area",
                              show.legend = NA, inherit.aes = TRUE, ...)
{

  # Return layer
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale, ...)
  )

}

#' @noRd
# Half violin ggproto ----
# Updated 12.09.2026
GeomFlatViolin <- ggplot2::ggproto(
  "GeomFlatViolin", ggplot2::Geom,

  setup_data = function(data, params){

    # Set width
    data$width <- data$width %||% params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)

    # Group by panel and group
    grouping <- interaction(data$PANEL, data$group, drop = TRUE)

    # Set bottom of violin
    data$ymin <- ave(data$y, grouping, FUN = min)

    # Set top of violin
    data$ymax <- ave(data$y, grouping, FUN = max)

    # Set left of violin
    data$xmin <- data$x

    # Set right of violin
    data$xmax <- data$x + data$width / 2

    # Return data
    return(data)

  },

  draw_group = function(data, panel_scales, coord){

    # Get points around the violin
    data <- transform(data, xminv = x, xmaxv = x + violinwidth * (xmax - x))

    # Get left edge ascending and right edge descending
    newdata <- rbind(
      transform(data[order(data$y), ], x = xminv),
      transform(data[order(-data$y), ], x = xmaxv)
    )

    # Close polygon
    newdata <- rbind(newdata, newdata[1, ])

    # Draw polygon
    ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord)

  },

  draw_key = ggplot2::draw_key_polygon,

  default_aes = ggplot2::aes(
    weight = 1, colour = "grey20", fill = "white", linewidth = 0.5,
    alpha = NA, linetype = "solid"
  ),

  required_aes = c("x", "y")

)

#' @noRd
# Null default ----
# Updated 12.09.2026
`%||%` <- function(a, b)
{

  # Return first unless NULL
  if(!is.null(a)) a else b

}
