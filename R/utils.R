#' character to composition
#'
#' @param x character
#' @return named numeric
#' @noRd
.character2composition <- function(x) {
    if (!is.character(x)) {
        stop("'x' must be a 'character'")
    }
    s <- strsplit(x, "[[:space:]]")[[1L]]
    nms <- gsub("[^[:alpha:]]+", "", s)
    v <- as.integer(gsub("[^[:digit:]]+", "", s))
    v[is.na(v)] <- 1L
    names(v) <- nms
    v
}

#' Composition to character
#'
#' @param x numeric, named
#' @return character
#' @noRd
.composition2character <- function(x) {
    if (!is.numeric(x)) {
        stop("'x' must be a 'numeric'")
    }
    if (is.null(names(x))) {
        stop("'x' must be a named 'numeric'")
    }
    paste0(names(x), "(", x, ")", collapse=" ")
}
