#' @title Read a Response or Group File in Any Supported Format
#'
#' @description Multi-format file reader dispatching on a file's extension:
#' \code{.rds}, \code{.RData}, \code{.csv}, \code{.txt}, \code{.xls}/\code{.xlsx}
#' (via \code{readxl}), \code{.sav} (via \code{foreign}), \code{.mat} (via
#' \code{R.matlab}), or no/unrecognized extension (read as plain delimited
#' text). Used by the Shiny app's upload widgets, and usable directly for the
#' same purpose outside Shiny
#'
#' @param datapath Character.
#' Path to the file's actual content on disk (e.g. Shiny's \code{input$file$datapath})
#'
#' @param filename Character.
#' The file's original name, used only to determine its format from its
#' extension -- kept separate from \code{datapath} since Shiny's own upload
#' temp files don't necessarily carry a matching extension
#'
#' @param header Boolean.
#' Does the file have a header row (for delimited-text and spreadsheet formats)?
#' Defaults to \code{TRUE}
#'
#' @param sep Character.
#' Field delimiter (for delimited-text formats).
#' Defaults to \code{","}
#'
#' @return The file's contents as a data frame (or the single object it
#' contains, for \code{.rds}/\code{.RData})
#'
#' @examples
#' path <- tempfile(fileext = ".csv")
#' write.csv(data.frame(x = 1:3, y = c("a", "b", "c")), path, row.names = FALSE)
#'
#' read_uploaded_file(path, "responses.csv")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Read uploaded file ----
# Updated 10.09.2026
read_uploaded_file <- function(datapath, filename, header = TRUE, sep = ",")
{

  # Argument errors
  read_uploaded_file_errors(datapath, filename, header, sep)

  # Get file extension
  extension <- tolower(tools::file_ext(filename))

  # Get package needed to read extension
  format_package <- switch(
    extension, xlsx = "readxl", xls = "readxl", sav = "foreign", mat = "R.matlab", NULL
  )

  # Check for missing package
  if(!is.null(format_package) && !requireNamespace(format_package, quietly = TRUE)){

    # Send error
    stop(
      "In `read_uploaded_file()`: reading a \".", extension, "\" file requires the '", format_package, "' package. ",
      "Install it with `install.packages(\"", format_package, "\")`.",
      call. = FALSE
    )

  }

  # Check for RData
  if(extension == "rdata"){

    # Set up private environment
    holder <- new.env()

    # Load file into environment
    load(datapath, envir = holder)

    # Get objects
    objects <- as.list(holder)

    # Check for one object
    if(length(objects) != 1){

      # Send error
      stop(
        "In `read_uploaded_file()`: an \".RData\" file must contain exactly one object.",
        call. = FALSE
      )

    }

    # Return object
    return(objects[[1]])

  }

  # Check for no extension
  if(extension == ""){

    # Return table
    return(utils::read.table(datapath, header = header, sep = sep))

  }

  # Return data
  return(switch(
    extension,
    rds = readRDS(datapath),
    csv = utils::read.csv(datapath, header = header, sep = sep, as.is = TRUE),
    xlsx = as.data.frame(readxl::read_xlsx(datapath, col_names = header)),
    xls = as.data.frame(readxl::read_xls(datapath, col_names = header)),
    sav = foreign::read.spss(datapath, to.data.frame = TRUE, stringsAsFactors = FALSE),
    mat = as.data.frame(R.matlab::readMat(datapath)),
    txt = utils::read.table(datapath, header = header, sep = sep),
    stop(
      paste0("In `read_uploaded_file()`: file extension \".", extension, "\" is not supported."),
      call. = FALSE
    )
  ))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
read_uploaded_file_errors <- function(datapath, filename, header, sep)
{

  # 'datapath' errors
  length_error(datapath, 1, "read_uploaded_file")
  typeof_error(datapath, "character", "read_uploaded_file")

  # 'filename' errors
  length_error(filename, 1, "read_uploaded_file")
  typeof_error(filename, "character", "read_uploaded_file")

  # 'header' errors
  length_error(header, 1, "read_uploaded_file")
  typeof_error(header, "logical", "read_uploaded_file")

  # 'sep' errors
  length_error(sep, 1, "read_uploaded_file")
  typeof_error(sep, "character", "read_uploaded_file")

}

#' @title Find Usable \code{textcleaner} Objects in the Global Environment
#'
#' @description Scans \code{.GlobalEnv} for objects inheriting class
#' \code{"textcleaner"} \emph{and} \code{"fluency"} (from SemNetCleaner's
#' own \code{textcleaner()}/\code{textcleaner_shiny()} -- a sibling
#' package) -- used by the Shiny app's Load Data tab to offer loading one
#' directly, as a second data source alongside its own file-upload widget.
#'
#' Restricted to \code{type = "fluency"} results: a \code{type = "free"}
#' result's \code{$data$clean} is a long-format response/cue/ID table, a
#' different shape than the participant x response-slot word matrix this
#' package's own \code{responses2binary()} (and every bundled example
#' dataset) expects -- not something to silently reshape without a real
#' design decision, so it's excluded here rather than offered and failing
#' downstream.
#'
#' @return A character vector of object names in \code{.GlobalEnv} that are
#' usable \code{textcleaner} results (possibly empty)
#'
#' @examples
#' find_textcleaner_objects()
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Find {SemNetCleaner} objects ----
# Updated 10.09.2026
find_textcleaner_objects <- function()
{

  # Get objects in global environment
  candidates <- ls(envir = .GlobalEnv)

  # Identify fluency {SemNetCleaner} objects
  is_usable_textcleaner <- vapply(candidates, function(name){

    # Get object
    obj <- tryCatch(get(name, envir = .GlobalEnv), error = function(e) NULL)

    # Check for classes
    inherits(obj, "textcleaner") && inherits(obj, "fluency")

  }, logical(1))

  # Return usable objects
  candidates[is_usable_textcleaner]

}
