# Package startup hook

# Package startup ----
# Updated 14.10.2026
.onAttach <- function(libname, pkgname)
{

  # Set up version message (bold and underlined)
  msg <- style_text(style_text(paste("\nSemNeT (version ", packageVersion("SemNeT"), ")", sep = ""), defaults = "underline"), defaults = "bold")

  # Add tutorial link
  msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')

  # Add issue tracker link
  msg <- paste(msg, "For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNeT/issues>\n")

  # Add bootstrap testing change notice
  msg <- paste(
    msg,
    "\nIMPORTANT: Bootstrap group tests changed in version 2.1.0. Earlier versions applied t-tests/ANOVA/ANCOVA",
    "to bootstrap samples as independent observations, inflating statistics by about sqrt(iter).",
    "\nAs of version 2.1.0, bootstrap_test_SemNeT() returns confidence intervals of within-iteration group differences.",
    "\nRecompute earlier bootstrap results. Run and report both the bootstrap (bootstrap_test_SemNeT())",
    "and permutation (permutation_SemNeT()) procedures.\nDetails: <https://github.com/AlexChristensen/SemNeT/issues/16>\n"
  )

  # Send message to console
  packageStartupMessage(msg)

}

#' @noRd
# Style text ----
# Updated 06.09.2026
style_text <- function(text, defaults = c("bold", "italics", "highlight", "underline", "strikethrough"))
{

  # Check console for text styling
  console_supports_text <- system_check()

  # Check for no support
  if(!console_supports_text$text){

    # Return plain text
    return(text)

  }

  # Check for missing style
  if(missing(defaults)){

    # Set no style
    style_number <- 0

  }else{

    # Get ANSI code of style
    style_number <- switch(
      defaults,
      bold = 1,
      italics = 3,
      underline = 4,
      highlight = 7,
      strikethrough = 9
    )

  }

  # Return styled text
  return(paste("\033[", style_number, ";m", text, "\033[0m", sep = ""))

}

#' @noRd
# System check ----
# Updated 06.09.2026
system_check <- function(...)
{

  # Get operating system
  operating_system <- unname(tolower(Sys.info()["sysname"]))

  # Check for RStudio
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"

  # Assume text styling is supported
  supports_text <- TRUE

  # Check for unsupported console
  if(!is_rstudio && operating_system != "linux"){

    # Set no support
    supports_text <- FALSE

  }

  # Return checks
  return(
    list(
      operating_system = operating_system,
      is_rstudio = is_rstudio,
      text = supports_text
    )
  )

}
