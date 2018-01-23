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

#' Splits a string into a character vector
#'
#' @param x character, (AAString), (AAStringSet)
#' @return list of single characters
#' @noRd
.string2character <- function(x) {
    if (is(x, "AAStringSetList")) {
        x <- unlist(x)
    }
    ## if we always apply as.character the names of original characters are
    ## removed
    if (!is.character(x)) {
        x <- as.character(x)
    }
    strsplit(x, split=character(), fixed=TRUE)
}
