.onload <- function(libname, pkgname)
{library.dynam("SemNeT",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- styletext(styletext("\nSemNeT", defaults = "underline"), defaults = "bold")
    msg <- paste(msg,'\nFor help getting started, see <https://doi.org/10.31234/osf.io/eht87> \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNeT/issues>\n")
    msg <- paste(msg,"For a GUI experience, check out the Shiny app: SemNeTShiny()\n")
    packageStartupMessage(msg)
}