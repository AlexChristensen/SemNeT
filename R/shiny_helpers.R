#' @noRd
# Multi-format file reader for the Shiny app's upload widgets -- dispatches on
# the *original* file name's extension (not the temp `datapath` Shiny stores
# the upload under, which isn't guaranteed to carry a matching extension),
# but reads the actual uploaded content from `datapath`. Real bug fixed
# relative to the pre-2.0.0 Shiny app's `read.data()`, which inferred the
# extension from `datapath` itself
# Updated 07.09.2026
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
      "In the SemNeT Shiny app: reading a \".", extension, "\" file requires the '", format_package, "' package. ",
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
        "In the SemNeT Shiny app: an uploaded `.RData` file must contain exactly one object.",
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
      paste0("In the SemNeT Shiny app: file extension \".", extension, "\" is not supported."),
      call. = FALSE
    )
  ))

}
