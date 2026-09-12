#' @title Plot for \code{\link{bootstrap_SemNeT}}
#'
#' @description Plots the bootstrapped distribution of each requested measure
#' as a raincloud plot (Allen, Poggiali, Whitaker, Marshall, & Kievit, 2018) --
#' a half (flat) violin showing the full distribution's shape, paired with a
#' boxplot and the raw jittered bootstrap values -- one panel per measure,
#' grouped side by side
#'
#' @param x A \code{bootstrap_SemNeT} object from \code{\link{bootstrap_SemNeT}}
#'
#' @param measures Character.
#' One or more of \code{"aspl"}, \code{"cc"}, \code{"q"}.
#' Defaults to all three
#'
#' @param ... Currently unused
#'
#' @return A \code{ggplot} object, returned visibly (so it prints automatically
#' at the console, and renders correctly inside \code{shiny::renderPlot()})
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
#' Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., & Kievit, R. A. (2018).
#' Raincloud plots: a multi-platform tool for robust data visualization.
#' \emph{PeerJ Preprints}, \emph{6}, e27137v1.
#' \doi{10.7287/peerj.preprints.27137v1}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Plot for bootstrap_SemNeT() results
# Updated 12.09.2026
plot.bootstrap_SemNeT <- function(x, measures = c("aspl", "cc", "q"), ...)
{

  # 'ggplot2' is the one dependency this plot genuinely needs -- kept as a
  # `Suggests` (checked here, not loaded package-wide) so the core package
  # still installs/loads without it, matching how the Shiny app's own
  # Suggests-only dependencies are checked right before the feature that
  # needs them (see `SemNeTShiny()`)
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop(
      "In `plot.bootstrap_SemNeT()`: the 'ggplot2' package is required to draw this plot but isn't installed.\n",
      "Install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )
  }

  # `ggplot2::aes()` below refers to `group`/`value` as bare (tidy-eval)
  # column names, not R variables -- harmless, but `R CMD check`'s static
  # analysis can't tell the difference and flags them as undefined globals.
  # Binding them to `NULL` here silences that false positive without pulling
  # in `rlang` just for `.data$group`/`.data$value`
  group <- value <- NULL

  # Validate and coerce input before any computation begins
  measures <- plot_bootstrap_SemNeT_errors(x, measures)

  group_names <- names(x$groups)
  measure_labels <- toupper(measures)

  # One row per bootstrap iteration per group per measure -- the long format
  # `ggplot2` needs to facet by measure and group/colour by group at once
  plot_data <- do.call(rbind, lapply(group_names, function(group_name){
    do.call(rbind, lapply(seq_along(measures), function(i){
      data.frame(
        group = group_name,
        measure = measure_labels[i],
        value = x$groups[[group_name]]$measures[measures[i], ],
        stringsAsFactors = FALSE
      )
    }))
  }))

  plot_data$group <- factor(plot_data$group, levels = group_names)
  plot_data$measure <- factor(plot_data$measure, levels = measure_labels)

  # Per group/measure mean, plotted as a single marker over the cloud of raw values
  summary_data <- stats::aggregate(value ~ group + measure, data = plot_data, FUN = mean)

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, fill = group)) +

    geom_flat_violin(
      ggplot2::aes(fill = group), position = ggplot2::position_nudge(x = 0.15),
      adjust = 1.5, trim = FALSE, alpha = 0.5, colour = NA
    ) +

    ggplot2::geom_point(
      ggplot2::aes(colour = group), position = ggplot2::position_jitter(width = 0.05),
      alpha = 0.3, size = 1, shape = 20
    ) +

    ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, colour = "black", outlier.shape = NA) +

    ggplot2::geom_point(
      data = summary_data, ggplot2::aes(x = group, y = value),
      position = ggplot2::position_nudge(x = -0.15), inherit.aes = FALSE, pch = 21, size = 3
    ) +

    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_colour_brewer(palette = "Dark2") +

    # `coord_flip()` below swaps how the panel axes are displayed, so freeing
    # the axis that ends up horizontal (each measure's own value range) means
    # specifying "free_x" here, not "free_y" -- confirmed empirically: with
    # "free_y" every facet kept the same (widest-measure's) value range
    ggplot2::facet_wrap(~ measure, scales = "free_x") +

    ggplot2::labs(
      title = "Bootstrap Results", subtitle = paste(x$meta$iter, "Samples"),
      x = "Group", y = "Value"
    ) +

    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "none"
    ) +

    ggplot2::coord_flip()

  return(plot)

}

#' @noRd
# Argument errors for `plot.bootstrap_SemNeT()`
# Updated 06.09.2026
plot_bootstrap_SemNeT_errors <- function(x, measures)
{

  # 'x' must be a `bootstrap_SemNeT` object
  if(!inherits(x, "bootstrap_SemNeT")){
    stop(
      "In `plot.bootstrap_SemNeT()`: 'x' is not a `bootstrap_SemNeT` object (from `bootstrap_SemNeT()`).",
      call. = FALSE
    )
  }

  # 'measures' must be one or more of the documented options
  object_error(measures, "vector", "plot.bootstrap_SemNeT")
  typeof_error(measures, "character", "plot.bootstrap_SemNeT")

  if(!all(measures %in% c("aspl", "cc", "q"))){
    stop("In `plot.bootstrap_SemNeT()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'.", call. = FALSE)
  }

  return(measures)

}

#' @noRd
# A half ("flat") violin geom for `ggplot2` -- draws only the right half of a
# standard violin, which is what lets a raincloud plot place the violin,
# boxplot, and jittered raw points side by side without overlapping. Ported
# from the original package's own hand-rolled version (itself adapted from
# the raincloud-plots reference implementation, see `plot.bootstrap_SemNeT()`'s
# `@references`), rewritten here against base R only -- the original leaned
# on `dplyr::group_by()`/`mutate()` for one grouped min/max computation and
# `plyr::arrange()` for one sort-by-column call, both trivial in base R
# (`ave()` and `order()`), so this needs only the one dependency the plot
# itself can't do without (`ggplot2`), not three more just for this helper
# Updated 12.09.2026
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "dodge", trim = TRUE, scale = "area",
                              show.legend = NA, inherit.aes = TRUE, ...)
{

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
# The `ggproto` object backing `geom_flat_violin()` above
# Updated 12.09.2026
GeomFlatViolin <- ggplot2::ggproto(
  "GeomFlatViolin", ggplot2::Geom,

  setup_data = function(data, params){

    data$width <- data$width %||% params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)

    # Bounding rectangle per violin (one group = one violin), via base R's
    # `ave()` instead of a `dplyr::group_by()`/`mutate()` pipe. `setup_data()`
    # runs once over the *whole* layer's data, across every facet panel at
    # once -- grouping by `data$group` alone (as the original's `dplyr`
    # version did) pools a group's rows across every panel, so its ymin/ymax
    # (and, in turn, per-panel scale training with `facet_wrap(scales =
    # "free_x")`) gets contaminated by other panels' ranges. Including `PANEL`
    # in the grouping keeps each facet's bounding box -- and thus its free
    # scale -- correctly local to that facet
    grouping <- interaction(data$PANEL, data$group, drop = TRUE)
    data$ymin <- ave(data$y, grouping, FUN = min)
    data$ymax <- ave(data$y, grouping, FUN = max)
    data$xmin <- data$x
    data$xmax <- data$x + data$width / 2

    return(data)

  },

  draw_group = function(data, panel_scales, coord){

    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x, xmaxv = x + violinwidth * (xmax - x))

    # Sorted ascending (left edge) then descending (right edge) by `y`, via
    # base R `order()` instead of `plyr::arrange()`, to draw the outline
    newdata <- rbind(
      transform(data[order(data$y), ], x = xminv),
      transform(data[order(-data$y), ], x = xmaxv)
    )

    # Close the polygon: set first and last point the same
    newdata <- rbind(newdata, newdata[1, ])

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
# `a %||% b`: `a` unless it's `NULL`, in which case `b`
# Updated 12.09.2026
`%||%` <- function(a, b)
{
  if(!is.null(a)) a else b
}
