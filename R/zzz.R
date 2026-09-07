#' @importFrom utils packageVersion
# Package startup hook

# Print a styled startup message when the package is attached
# Updated 06.09.2026
.onAttach <- function(libname, pkgname)
{

  # Build the version line, bolded and underlined
  msg <- style_text(style_text(paste("\nSemNeT (version ", packageVersion("SemNeT"), ")", sep = ""), defaults = "underline"), defaults = "bold")

  # Append the tutorial link
  msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')

  # Append the issue tracker link
  msg <- paste(msg, "For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNeT/issues>\n")

  # Send the message to the console
  packageStartupMessage(msg)

}

#' @noRd
# Wrap text in ANSI styling codes, when the console supports it
# Updated 06.09.2026
style_text <- function(text, defaults = c("bold", "italics", "highlight", "underline", "strikethrough"))
{

  # Check whether the current console supports ANSI text styling
  console_supports_text <- system_check()

  # Bail out with the plain text if styling is not supported
  if(!console_supports_text$text){
    return(text)
  }

  # No style requested defaults to no styling code
  if(missing(defaults)){
    style_number <- 0
  }else{

    # Map the style name onto its ANSI code number
    style_number <- switch(
      defaults,
      bold = 1,
      italics = 3,
      underline = 4,
      highlight = 7,
      strikethrough = 9
    )

  }

  # Wrap the text in the ANSI escape codes and reset at the end
  return(paste("\033[", style_number, ";m", text, "\033[0m", sep = ""))

}

#' @noRd
# Check the operating system and console for ANSI text-styling support
# Updated 06.09.2026
system_check <- function(...)
{

  # Lower-cased operating system name
  operating_system <- unname(tolower(Sys.info()["sysname"]))

  # Whether the session is running inside RStudio
  is_rstudio <- Sys.getenv("RSTUDIO") == "1"

  # ANSI styling is assumed supported by default
  supports_text <- TRUE

  # Non-RStudio, non-Linux consoles are assumed not to support it
  if(!is_rstudio && operating_system != "linux"){
    supports_text <- FALSE
  }

  # Return the three checks as a named list
  return(
    list(
      operating_system = operating_system,
      is_rstudio = is_rstudio,
      text = supports_text
    )
  )

}
