#' Parse <aa> tag.
#' @param xml xml_document (returned by .unimodDb)
#' @return data.frame
#' @noRd
.aminoacids <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    aaNodes <- xml2::xml_find_all(xml, "//umod:amino_acids/umod:aa")
    aaAttrs <- xml2::xml_attrs(aaNodes)
    eeAttrs <- unlist(xml2::xml_attrs(
        xml2::xml_find_all(xml, "//umod:amino_acids/umod:aa/umod:element")
    ))

    aa <- do.call(rbind, aaAttrs)

    numbers <- as.integer(eeAttrs["number" == names(eeAttrs)])
    names(numbers) <- eeAttrs["symbol" == names(eeAttrs)]
    numbers <- split(numbers, rep.int(aa[, "title"], xml2::xml_length(aaNodes)))
    numbers[["-"]] <- integer()

    allElements <- unique(unlist(lapply(numbers, names)))

    em <- do.call(rbind, lapply(aa[, "title"], function(a) {
        e <- numbers[[a]]
        d <- setdiff(allElements, names(e))
        e[d] <- 0L
        e
    }))

    d <- data.frame(
        OneLetter=aa[, "title"],
        ThreeLetter=aa[, "three_letter"],
        FullName=aa[, "full_name"],
        AvgMass=as.double(aa[, "avge_mass"]),
        MonoMass=as.double(aa[, "mono_mass"]),
        row.names=aa[, "title"],
        stringsAsFactors=FALSE
    )
    cbind(d, em)
}

## TODO: save data.frame aminoacids as public available data set
## `data(aminoacids)`
