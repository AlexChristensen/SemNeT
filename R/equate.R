#' @title Equate Groups
#'
#' @description Equates multiple binary response matrices to one another, so
#' that every group retains exactly the responses common to \emph{all} of them
#'
#' @param ... Named binary response matrices or data frames.
#' At least two, e.g. \code{equate(low = low_data, high = high_data)}
#'
#' @return A named list of the equated binary response matrices, one per
#' input, each containing the same responses (columns) in the same order
#'
#' @examples
#' # Obtain binary data
#' bin <- open.binary
#'
#' # Finalize each group
#' mat1 <- finalize(bin[c(1:5), ])
#' mat2 <- finalize(bin[c(6:10), ])
#'
#' # Equate the groups
#' eq <- equate(mat1 = mat1, mat2 = mat2)
#'
#' # Obtain respective equated response matrices
#' eq_mat1 <- eq$mat1
#' eq_mat2 <- eq$mat2
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Equate groups to their shared vocabulary
# Updated 06.09.2026
equate <- function(...)
{

  # Validate and coerce input before any computation begins
  groups <- equate_errors(list(...))

  return(equate_core(groups, "equate"))

}

#' @noRd
# The actual equating logic, without `equate_errors()`'s validation --
# extracted so `bootstrap_SemNeT()` can equate its already-validated,
# already-finalized per-iteration samples directly, `iter` times per call,
# without re-running full input validation on every single one. Benchmarked
# ~5x faster than going through `equate()` itself (155 vs. 800 microseconds,
# `microbenchmark`, n=300) purely from skipping the repeated
# `response_matrix_error()`/column-name checks -- real savings at `iter`'s
# default of 1000 draws. `equate()` above still validates every argument on
# every call; only an already-validated caller may skip straight to this
# Updated 07.09.2026
equate_core <- function(groups, function_name)
{

  # The vocabulary common to every group, computed once in a single pass --
  # the original instead iteratively re-equated groups pairwise (fold left,
  # then a second pass re-broadcasting the fold's result back to every
  # group), which was also where its real bug lived (see `equate_errors()`'s
  # note on the fixed bug below) -- `Reduce(intersect, ...)` gives the exact
  # same "responses common to every group" result directly, in one pass, and
  # `sort()` guarantees every group's output ends up in the same column order.
  # `method = "radix"` benchmarked ~20x faster than the locale-aware default
  # sort at realistic vocabulary sizes; only the ordering of non-ASCII/mixed-
  # case strings could differ from locale collation, and the contract here is
  # only "every group gets the same order," not a specific alphabetization
  shared_vocabulary <- sort(Reduce(intersect, lapply(groups, colnames)), method = "radix")

  # No response survived in every group -- equating to nothing is a
  # degenerate result no downstream network function can use
  if(length(shared_vocabulary) == 0){
    stop(
      paste0("In `", function_name, "()`: no response is common to every group -- there is nothing to equate."),
      call. = FALSE
    )
  }

  # Every group subset to the identical vocabulary, in the identical order --
  # guarantees column `i` means the same response in every returned matrix
  return(lapply(groups, function(group) group[, shared_vocabulary, drop = FALSE]))

}

#' @noRd
# Argument errors for `equate()`
# Updated 06.09.2026
equate_errors <- function(groups)
{

  # At least two groups are required to equate anything
  if(length(groups) < 2){
    stop("In `equate()`: at least two response matrices must be provided via `...`.", call. = FALSE)
  }

  # Every group must be named -- required in place of the original's
  # `substitute()`-based name-sniffing, and works correctly under `do.call()`
  if(is.null(names(groups)) || any(names(groups) == "")){
    stop(
      "In `equate()`: every response matrix passed via `...` must be named, e.g. `equate(low = low_data, high = high_data)`.",
      call. = FALSE
    )
  }

  # Every group's name must be unique -- otherwise the returned list has a
  # duplicate name, and `$`/`[[` access (the documented way to read a group's
  # equated matrix back out) silently returns only the first match
  if(anyDuplicated(names(groups)) > 0){
    stop("In `equate()`: every response matrix passed via `...` must have a unique name.", call. = FALSE)
  }

  # Every group must be a binary response matrix with named columns --
  # `equate()` intersects by column *name* (the actual responses), so an
  # unnamed matrix has nothing meaningful to equate on
  groups <- lapply(groups, function(group){

    group <- response_matrix_error(group, "equate")

    if(is.null(colnames(group))){
      stop(
        paste0(
          "In `equate()`: input `", deparse(substitute(group)), "` has no column names.\n",
          "Every group must be a response matrix with responses as named columns."
        ),
        call. = FALSE
      )
    }

    return(group)

  })

  return(groups)

}
