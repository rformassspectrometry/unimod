#' Internal character id for unimod entries
#'
#' @param name character, unimod title
#' @param site character, unimod site
#' @param position character, unimod position
#' @param neutralLoss character, "0"/"1"
#' @return character
#' @noRd
.characterId <- function(name, site, position, neutralLoss) {
    stopifnot(is.character(name))
    stopifnot(is.character(site))
    stopifnot(is.character(position))
    stopifnot(is.character(neutralLoss))
    paste0(
        name, ":",
        ifelse(grepl("Protein", position), "P-", ""),
        site,
        ifelse(neutralLoss == "1", ":NL", "")
    )
}

#' Internal function to query delta mass.
#'
#' @param xml xml_nodeset, <mod>
#' @return double
#' @noRd
.delta <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    node <- xml2::xml_find_first(xml, ".//umod:delta")
    xml2::xml_attrs(node)[c("avge_mass", "mono_mass", "composition")]
}

#' Internal function to turn unimod xml into a data.frame.
#'
#' @param xml xml_document (returned by .unimodDb)
#' @return data.frame
#' @noRd
.modifications <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    nodes <- xml2::xml_find_all(xml, "//umod:mod")
    u  <- lapply(nodes, function(n) {
        td <- c(.title(n), .delta(n))
        sp <- .specificity(n)
        m <- cbind(matrix(td, ncol=length(td), nrow=nrow(sp), byrow=TRUE,
                          dimnames=list(c(), names(td))), sp, neutralLoss="0")
        nl <- .neutralLoss(n)
        if (!is.null(nl) && nrow(nl)) {
            nlrepl <- match(
                paste(nl[, "site"], nl[, "spec_group"]),
                paste(m[, "site"], m[, "spec_group"])
            )
            nl <- cbind(nl, neutralLoss="1")
            mnr <- nrow(m)
            m <- rbind(m, m[nlrepl,,drop=FALSE])
            m[(mnr + 1L):nrow(m), colnames(nl)] <- nl
        }
        m
    })
    u <- do.call(rbind, u)
    data.frame(
        Id=as.numeric(u[, "record_id"]),
        Name=u[, "title"],
        Description=u[, "full_name"],
        Composition=u[, "composition"],
        AvgMass=as.numeric(u[, "avge_mass"]),
        MonoMass=as.numeric(u[, "mono_mass"]),
        Site=u[, "site"],
        Position=factor(u[, "position"]),
        Classification=factor(u[, "classification"]),
        SpecGroup=as.numeric(u[, "spec_group"]),
        LastModified=u[, "date_time_modified"],
        Approved=as.logical(as.numeric(u[, "approved"])),
        Hidden=as.logical(as.numeric(u[, "hidden"])),
        stringsAsFactors=FALSE)
}

#' Internal function to query neutral loss.
#'
#' @param xml xml_nodeset, <umod:specificity>
#' @return matrix
#' @noRd
.neutralLoss <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    nodes <- xml2::xml_find_all(xml, ".//umod:NeutralLoss")
    nl <- do.call(rbind, lapply(nodes, function(nd) {
        c(
            xml2::xml_attrs(xml2::xml_parent(nd))[c("site", "spec_group")],
            xml2::xml_attrs(nd)[c("avge_mass", "mono_mass", "composition")]
        )
    }))
    nl[nl[, "composition"] != "0",, drop=FALSE]
}

#' Internal function to query specificity.
#'
#' @param xml xml_nodeset, <mod>
#' @return matrix
#' @noRd
.specificity <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    nodes <- xml2::xml_find_all(xml, ".//umod:specificity")
    sp <- do.call(rbind, xml2::xml_attrs(nodes))
    sp[order(as.numeric(sp[, "spec_group"])),, drop=FALSE]
}

#' Internal function to query title/id/lastmod.
#'
#' @param xml xml_nodeset, <mod>
#' @return double
#' @noRd
.title <- function(xml) {
    stopifnot(requireNamespace("xml2"))
    xml2::xml_attrs(xml)[c("record_id", "title", "full_name",
                           "date_time_modified", "approved")]
}
