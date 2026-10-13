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
# Equate groups ----
# Updated 06.09.2026
equate <- function(...)
{

  # Argument errors
  groups <- equate_errors(list(...))

  # Return equated groups
  return(equate_core(groups, "equate"))

}

#' @noRd
# Equate groups without errors ----
# Updated 07.09.2026
equate_core <- function(groups, function_name)
{

  # Get responses shared by every group
  shared_vocabulary <- sort(Reduce(intersect, lapply(groups, colnames)), method = "radix")

  # Check for no shared responses
  if(length(shared_vocabulary) == 0){

    # Send error
    stop(
      paste0("In `", function_name, "()`: no response is common to every group -- there is nothing to equate."),
      call. = FALSE
    )

  }

  # Return groups with shared responses
  return(lapply(groups, function(group) group[, shared_vocabulary, drop = FALSE]))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
equate_errors <- function(groups)
{

  # '...' errors
  if(length(groups) < 2){

    # Send error
    stop("In `equate()`: at least two response matrices must be provided via `...`.", call. = FALSE)

  }

  # Check for unnamed groups
  if(is.null(names(groups)) || any(names(groups) == "")){

    # Send error
    stop(
      "In `equate()`: every response matrix passed via `...` must be named, e.g. `equate(low = low_data, high = high_data)`.",
      call. = FALSE
    )

  }

  # Check for duplicate names
  if(anyDuplicated(names(groups)) > 0){

    # Send error
    stop("In `equate()`: every response matrix passed via `...` must have a unique name.", call. = FALSE)

  }

  # Check each group
  groups <- lapply(groups, function(group){

    # Check for response matrix
    group <- response_matrix_error(group, "equate")

    # Check for raw responses
    if(!is.numeric(group)){

      # Send error
      stop(
        "In `equate()`: every group must be a binary response matrix (see `responses2binary()`), not raw word responses.",
        call. = FALSE
      )

    }

    # Check for response names
    if(is.null(colnames(group))){

      # Send error
      stop(
        paste0(
          "In `equate()`: input `", deparse(substitute(group)), "` has no column names.\n",
          "Every group must be a response matrix with responses as named columns."
        ),
        call. = FALSE
      )

    }

    # Check for duplicate responses
    if(anyDuplicated(colnames(group)) > 0){

      # Send error
      stop("In `equate()`: every group's response (column) names must be unique.", call. = FALSE)

    }

    # Return group
    return(group)

  })

  # Return 'groups' (in case of data frames)
  return(groups)

}
