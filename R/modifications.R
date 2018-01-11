#' internal function to query title/id/lastmod
#'
#' @param xml xml_nodeset, <mod>
#' @return double
#' @noRd
.title <- function(xml) {
  setNames(xml_attrs(xml)[c("record_id", "title", "full_name",
                            "date_time_modified", "approved")],
           c("id", "name", "description", "lastModified", "approved"))
}

#' internal function to query delta mass
#'
#' @param xml xml_nodeset, <mod>
#' @return double
#' @noRd
.delta <- function(xml) {
  node <- xml_find_first(xml, ".//umod:delta")
  setNames(as.double(xml_attrs(node)[c("avge_mass", "mono_mass")]),
           c("avgMass", "monoMass"))
}

#' internal function to query composition
#'
#' @param xml xml_nodeset, <mod>
#' @return list, delta masses and composition as named vector
#' @noRd
.composition <- function(xml) {
  nodes <- xml_find_all(xml, ".//umod:delta/umod:element")
  composition <- do.call(rbind, xml_attrs(nodes))
  setNames(as.integer(composition[, "number"]), composition[, "symbol"])
}

#' internal function to query specificity
#'
#' @param xml xml_nodeset, <mod>
#' @return data.frame
#' @noRd
.specificity <- function(xml) {
  nodes <- xml_find_all(xml, ".//umod:specificity")
  sp <- do.call(rbind, xml_attrs(nodes))
  sp <- sp[order(as.numeric(sp[, "spec_group"])),, drop=FALSE]
  data.frame(site=sp[, "site"],
             position=sp[, "position"],
             classification=sp[, "classification"],
             hidden=sp[, "hidden"] == "1",
             group=as.integer(sp[, "spec_group"]),
             stringsAsFactors=FALSE, row.names=seq(nrow(sp)))
}

#' internal function to query references
#'
#' @param xml xml_nodeset, <mod>
#' @return data.frame
#' @noRd
.xref <- function(xml) {
  nodes <- xml_find_all(xml, ".//umod:xref")
  data.frame(text=xml_text(xml_find_all(nodes, ".//umod:text")),
             source=xml_text(xml_find_all(nodes, ".//umod:source")),
             url=xml_text(xml_find_all(nodes, ".//umod:url")),
             stringsAsFactors=FALSE)
}
