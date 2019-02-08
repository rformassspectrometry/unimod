if (is.null(getGeneric(".countSites"))) {
    setGeneric(".countSites",
        function(object, pepseq)standardGeneric(".countSites"))
}

if (is.null(getGeneric(".pepseq"))) {
    setGeneric(".pepseq",
        function(object, pepseq)standardGeneric(".pepseq"))
}
