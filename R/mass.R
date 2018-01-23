#' Calculate mass for a given aminoacid sequence
#'
#' @param x character, sequence
#' @param type character, mono or average mass
#' @return list of numeric (mass values)
#' @noRd
.aamass <- function(x, type=c("mono", "average")) {
    if (match.arg(type) == "mono") {
        m <- aminoacids$MonoMass
    } else {
        m <- aminoacids$AvgMass
    }
    names(m) <- aminoacids$OneLetter
    lapply(.string2character(x), function(xx)m[xx])
}
