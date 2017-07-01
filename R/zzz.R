.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("This is unimod version",
                              packageVersion("unimod")))
}
