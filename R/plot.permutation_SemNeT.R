#' @title Plot for \code{\link{permutation_SemNeT}}
#'
#' @description Plots each measure's permutation null distribution as a raincloud
#' plot (Allen et al., 2018): a half violin, boxplot, and the jittered permuted
#' differences (the first sample minus the second), with one panel per measure.
#' The observed difference is marked and the central range of the null
#' distribution is drawn below the boxplot, so an observed difference outside
#' that range corresponds to a small \emph{p}-value
#'
#' @param x A \code{\link{permutation_SemNeT}} object
#'
#' @param measures Character.
#' One or more of the measures tested in \code{x} (\code{"aspl"}, \code{"cc"},
#' and \code{"q"}).
#' Defaults to \code{NULL} (every measure tested)
#'
#' @param null_range Boolean (length = 1).
#' Whether to draw the central range of the null distribution below its
#' boxplot.
#' Defaults to \code{TRUE}
#'
#' @param level Numeric (length = 1).
#' Proportion of the permuted differences inside the null range, between 0 and
#' 1 (exclusive); the range is bounded by the \code{(1 - level) / 2} and
#' \code{1 - (1 - level) / 2} quantiles.
#' Defaults to \code{.95}
#'
#' @param ... Not used
#'
#' @return Returns a \code{ggplot} object (printed at the console and
#' compatible with \code{shiny::renderPlot()})
#'
#' @details The null range is not a confidence interval: it describes the
#' differences expected when group membership does not matter, not the
#' precision of an estimate (see \code{\link{plot.bootstrap_SemNeT}} for that).
#' The permuted differences include the observed one (the first permutation).
#'
#' The \emph{p}-value in each panel's title is the one stored in \code{x}, so it
#' follows the \code{alternative} used in \code{\link{permutation_SemNeT}}
#' (the range and violin are not adjusted for it).
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' # Get openness data
#' low <- open.clean[which(open.group == "Low"), ]
#' high <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Perform permutation test
#' perm_test <- permutation_SemNeT(
#'   low, high, method = "tmfg", measure = c("aspl", "cc", "q"),
#'   iter = 100, cores = 2, seed = 1234
#' )
#'
#' # Plot
#' plot(perm_test)
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
# Plot permutation ----
# Updated 22.10.2026
plot.permutation_SemNeT <- function(x, measures = NULL, null_range = TRUE, level = .95, ...)
{

  # Check for {ggplot2}
  if(!requireNamespace("ggplot2", quietly = TRUE)){

    # Send error
    stop(
      "In `plot.permutation_SemNeT()`: the 'ggplot2' package is required to draw this plot but isn't installed.\n",
      "Install it with `install.packages(\"ggplot2\")`.",
      call. = FALSE
    )

  }

  # Remove {ggplot2} variable notes
  group <- value <- lower <- upper <- observed <- NULL

  # Argument errors
  measures <- plot_permutation_SemNeT_errors(x, measures, null_range, level)

  # Get differences as a matrix with a column per measure
  differences <- as.matrix(x$differences)

  # Name measures
  colnames(differences) <- row.names(x$result)

  # Get group names
  group_names <- names(x$result)[1:2]

  # Get measure labels with p-values
  p_values <- format_p_value(x$result[measures, "p_value"])
  measure_labels <- paste0(
    toupper(measures), ": p ", ifelse(substr(p_values, 1, 1) == "<", p_values, paste0("= ", p_values))
  )

  # Set up plot data
  plot_data <- do.call(rbind, lapply(seq_along(measures), function(i){

    # Set up data of measure
    data.frame(
      group = "Difference", measure = measure_labels[i],
      value = differences[, measures[i]], stringsAsFactors = FALSE
    )

  }))

  # Set order of measures
  plot_data$measure <- factor(plot_data$measure, levels = measure_labels)

  # Set up summary data (null mean and observed difference)
  summary_data <- do.call(rbind, lapply(seq_along(measures), function(i){

    # Set up data of measure
    data.frame(
      group = "Difference", measure = measure_labels[i],
      mean = mean(differences[, measures[i]], na.rm = TRUE),
      observed = differences[1, measures[i]], stringsAsFactors = FALSE
    )

  }))

  # Set order of measures
  summary_data$measure <- factor(summary_data$measure, levels = measure_labels)

  # Keep finite observed differences
  observed_data <- summary_data[is.finite(summary_data$observed), , drop = FALSE]

  # Set up null range data
  range_data <- NULL

  # Check for null range
  if(null_range){

    # Get probabilities of range
    probabilities <- c((1 - level) / 2, 1 - (1 - level) / 2)

    # Get range of each measure
    range_data <- do.call(rbind, lapply(seq_along(measures), function(i){

      # Get quantiles
      bounds <- quantile(differences[, measures[i]], probs = probabilities, na.rm = TRUE, names = FALSE)

      # Set up data of measure
      data.frame(
        group = "Difference", measure = measure_labels[i],
        lower = bounds[1], upper = bounds[2], stringsAsFactors = FALSE
      )

    }))

    # Set order of measures
    range_data$measure <- factor(range_data$measure, levels = measure_labels)

  }

  # Set up plot
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = group, y = value, fill = group)) +

    # Add half violin
    geom_flat_violin(
      ggplot2::aes(fill = group), position = ggplot2::position_nudge(x = 0.15),
      adjust = 1.5, trim = FALSE, alpha = 0.5, colour = NA
    ) +

    # Add raw values
    ggplot2::geom_point(
      ggplot2::aes(colour = group), position = ggplot2::position_jitter(width = 0.05),
      alpha = 0.3, size = 1, shape = 20
    ) +

    # Add boxplot
    ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, colour = "black", outlier.shape = NA) +

    # Add reference line of no difference
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +

    # Add null mean
    ggplot2::geom_point(
      data = summary_data, ggplot2::aes(x = group, y = mean),
      position = ggplot2::position_nudge(x = -0.15), inherit.aes = FALSE, pch = 21, size = 3
    ) +

    # Add observed difference
    {
      if(nrow(observed_data) > 0){
        ggplot2::geom_point(
          data = observed_data, ggplot2::aes(x = group, y = observed),
          position = ggplot2::position_nudge(x = -0.15), inherit.aes = FALSE,
          shape = 23, size = 3, fill = "black"
        )
      }
    } +

    # Add null range below boxplot
    {
      if(!is.null(range_data)){
        ggplot2::geom_errorbar(
          data = range_data, ggplot2::aes(x = group, ymin = lower, ymax = upper, colour = group),
          position = ggplot2::position_nudge(x = -0.3), inherit.aes = FALSE,
          width = 0.08, linewidth = 0.8
        )
      }
    } +

    # Set fill color
    ggplot2::scale_fill_brewer(palette = "Dark2") +

    # Set point color
    ggplot2::scale_colour_brewer(palette = "Dark2") +

    # Facet by measure (free axis because of flipped coordinates)
    ggplot2::facet_wrap(~ measure, scales = "free_x", ncol = 1) +

    # Set labels
    ggplot2::labs(
      title = "Permutation Results",
      subtitle = paste0(
        nrow(differences), " Permutations (incl. observed): ", group_names[1], " - ", group_names[2],
        " (H1: ", x$result$alternative[1], ")"
      ),
      caption = wrap_caption(
        "Open circle: mean of the permuted differences. Black diamond: observed difference.",
        if(!is.null(range_data)){
          paste0(
            "Bar below the box: central ", format(100 * level), "% of the permuted differences."
          )
        },
        "Dotted line: no difference. The spread is the null distribution, not a confidence interval."
      ),
      x = NULL, y = paste0("Difference (", group_names[1], " - ", group_names[2], ")")
    ) +

    # Set theme
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
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
plot_permutation_SemNeT_errors <- function(x, measures, null_range, level)
{

  # 'x' errors
  if(!inherits(x, "permutation_SemNeT")){

    # Send error
    stop(
      "In `plot.permutation_SemNeT()`: 'x' is not a `permutation_SemNeT` object (from `permutation_SemNeT()`).",
      call. = FALSE
    )

  }

  # Get measures tested
  tested <- row.names(x$result)

  # Check for no measures
  if(is.null(measures)){

    # Use measures tested
    measures <- tested

  }

  # 'measures' errors
  typeof_error(measures, "character", "plot.permutation_SemNeT")

  # Check for measures not tested
  if(!all(measures %in% tested)){

    # Send error
    stop(
      "In `plot.permutation_SemNeT()`: 'measures' must only contain measures tested in 'x' (",
      paste0("'", tested, "'", collapse = ", "), ").",
      call. = FALSE
    )

  }

  # 'null_range' errors
  length_error(null_range, 1, "plot.permutation_SemNeT")
  typeof_error(null_range, "logical", "plot.permutation_SemNeT")

  # 'level' errors
  length_error(level, 1, "plot.permutation_SemNeT")
  typeof_error(level, "numeric", "plot.permutation_SemNeT")

  # Check for 'level' outside of (0, 1)
  if(level <= 0 || level >= 1){

    # Send error
    stop("In `plot.permutation_SemNeT()`: 'level' must be between 0 and 1 (exclusive).", call. = FALSE)

  }

  # Return 'measures'
  return(unique(measures))

}
