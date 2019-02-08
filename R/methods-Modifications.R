setClassUnion(
    "PeptideSequences", 
    c("character", "AAString", "AAStringSet")
)
setMethod(
    ".countSites", 
    signature(object="ModificationFixed", pepseq="PeptideSequences"),
    function(object, pepseq) {

    nchar(pepseq) - nchar(gsub(object@site, "", pepseq, fixed=TRUE))
    ## while looking a little bit odd it is much faster than:
    # vapply(
    #     gregexpr(pattern=site, text=x, fixed=fixed),
    #     function(rx)sum(rx > 0L), double(1L),
    #     USE.NAMES=FALSE
    # )

})

setMethod(
    ".countSites", 
    signature(object="ModificationTerm", pepseq="PeptideSequences"),
    function(object, pepseq)rep.int(1L, length(pepseq))
)

setMethod(
    ".countSites",
    signature(object="ModificationFixedLocalized", pepseq="PeptideSequences"),
    function(object, pepseq) {
    vapply(
        .sitePos(object, pepseq), 
        function(p) {
            sum(
                p[, 1L] %in% object@globalIndex, 
                seq_len(nrow(p)) %in% object@siteIndex,
                na.rm=TRUE
            )
        },
        NA_real_
    )
})

setMethod(
    ".countSites",
    signature(object="ModificationFixedRegExp", pepseq="PeptideSequences"),
    function(object, pepseq) {
    vapply(.sitePos(object, pepseq), nrow, NA_real_)
})

setMethod(
    ".countSites",
    signature(object="ModificationVariable", pepseq="PeptideSequences"),
    function(object, pepseq) {
    n <- callNextMethod()
    .possibleCounts <- function(nn)c(
        0L,
        seq_len(min(object@siteMaxN * n, object@globalMaxN, na.rm=TRUE))
    )

    if (length(n) > 1L) {
        lapply(n, .possibleCounts)
    } else {
        .possibleCounts(n)
    }
})

setMethod(
    ".pepseq",
    signature(object="Modification", pepseq="PeptideSequences"),
    function(object, pepseq)pepseq # do nothing
)

setMethod(
    ".pepseq",
    signature(object="ModificationFixedRegExp", pepseq="PeptideSequences"),
    function(object, pepseq) {
    vapply(
        pepseq, gsub, NA_character_, 
        pattern=object@site, replacement=object@replacement
    )
})

