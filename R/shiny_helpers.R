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
# Multi-format file reader for the Shiny app's upload widgets -- dispatches on
# the *original* file name's extension (not the temp `datapath` Shiny stores
# the upload under, which isn't guaranteed to carry a matching extension),
# but reads the actual uploaded content from `datapath`. Real bug fixed
# relative to the pre-2.0.0 Shiny app's `read.data()`, which inferred the
# extension from `datapath` itself.
# Exported for the same reason as `responses_to_binary()` (see helpers.R):
# an unexported (`@noRd`) helper called directly from `inst/Shiny/` is
# invisible under a real `library(SemNeT)` install, and only appeared to
# work because Phase 7's own testing used `devtools::load_all()` throughout
# Updated 10.09.2026
read_uploaded_file <- function(datapath, filename, header = TRUE, sep = ",")
{

  # Extension is read off the name the user's browser sent, lower-cased so
  # ".CSV"/".csv" are treated the same way
  extension <- tolower(tools::file_ext(filename))

  # These readers are `Suggests`, not `Imports` -- checked here, per format,
  # rather than requiring every possible format's package just to launch the
  # app (most users only ever upload one format)
  format_package <- switch(
    extension, xlsx = "readxl", xls = "readxl", sav = "foreign", mat = "R.matlab", NULL
  )

  if(!is.null(format_package) && !requireNamespace(format_package, quietly = TRUE)){
    stop(
      "In `read_uploaded_file()`: reading a \".", extension, "\" file requires the '", format_package, "' package. ",
      "Install it with `install.packages(\"", format_package, "\")`.",
      call. = FALSE
    )
  }

  if(extension == "rdata"){

    # Loaded into a private environment, not `.GlobalEnv` -- the Shiny app
    # never touches the user's real global environment (see `SHINY_PLAN.md`'s
    # "Deferred / Dropped" section)
    holder <- new.env()
    load(datapath, envir = holder)
    objects <- as.list(holder)

    if(length(objects) != 1){
      stop(
        "In `read_uploaded_file()`: an \".RData\" file must contain exactly one object.",
        call. = FALSE
      )
    }

    return(objects[[1]])

  }

  # A bare/missing extension falls through to the same plain-text reader as
  # ".txt" -- `switch()` can't take an empty string as a case label directly,
  # so that's normalized here instead
  if(extension == ""){
    return(utils::read.table(datapath, header = header, sep = sep))
  }

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
