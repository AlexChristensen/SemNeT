#' @title Plot for \code{\link{bootstrap_SemNeT}}
#'
#' @description Plots the bootstrapped distribution of each requested measure,
#' one panel per measure, boxplots grouped side by side
#'
#' @param x A \code{bootstrap_SemNeT} object from \code{\link{bootstrap_SemNeT}}
#'
#' @param measures Character.
#' One or more of \code{"aspl"}, \code{"cc"}, \code{"q"}.
#' Defaults to all three
#'
#' @param ... Additional arguments passed on to \code{\link[graphics]{boxplot}}
#'
#' @return Plots one panel per measure and returns \code{x} invisibly
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
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom graphics boxplot par
#'
#' @export
# Plot for bootstrap_SemNeT() results
# Updated 06.09.2026
plot.bootstrap_SemNeT <- function(x, measures = c("aspl", "cc", "q"), ...)
{

  # Validate and coerce input before any computation begins
  measures <- plot_bootstrap_SemNeT_errors(x, measures)

  group_names <- names(x$groups)

  # One panel per measure, arranged side by side -- a base-graphics boxplot
  # in place of the original's custom `ggplot2`/`dplyr` raincloud plot
  # (a hand-rolled `ggproto` geom, ~300 lines on its own): the aggressive
  # dependency trim this refactor already applied elsewhere (dropping
  # `qgraph` from `convert2igraph()`, `plyr`/`philentropy` package-wide)
  # applies just as much here, and a boxplot shows the same bootstrapped
  # spread-and-median information without reintroducing `ggplot2`, `dplyr`,
  # and `magrittr` for this one plot
  previous_par <- par(mfrow = c(1, length(measures)))
  on.exit(par(previous_par), add = TRUE)

  for(measure in measures){

    values <- lapply(group_names, function(group_name) x$groups[[group_name]]$measures[measure, ])
    names(values) <- group_names

    boxplot(values, main = toupper(measure), ylab = measure, xlab = "Group", ...)

  }

  return(invisible(x))

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
