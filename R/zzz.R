.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("\nThis is unimod version",
                              packageVersion("unimod"),"\n"))
}
