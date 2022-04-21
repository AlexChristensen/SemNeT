#' @importFrom utils packageVersion

.onload <- function(libname, pkgname)
{library.dynam("SemNeT",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- styletext(styletext(paste("\nSemNeT (version ", packageVersion("SemNeT"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    msg <- paste(msg,'\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNeT/issues>\n")
    msg <- paste(msg,"For a GUI experience, check out the Shiny app: SemNeTShiny()\n")
    packageStartupMessage(msg)
}