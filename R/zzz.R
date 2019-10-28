.onload <- function(libname, pkgname)
{library.dynam("SemNeT",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- paste('For help getting started, type `browseVignettes("SemNeT")` \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNeT/issues>")
    packageStartupMessage(msg)
}