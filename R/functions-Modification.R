#' @describeIn Modification-class Constructor
#' @export
#Modification <- function(title, name, id=-1L,
#                         lastModified=character(),
#                         approved=logical(),
#                         deltaAvgMass,
#                         deltaMonoMass,
#                         composition=double(),
#                         specificity,
#                         refs=data.frame(stringsAsFactors=FALSE)) {
#  new("Modification",
#      title=title,
#      name=name,
#      id=id,
#      lastModified=lastModified,
#      approved=approved,
#      deltaAvgMass=deltaAvgMass,
#      deltaMonoMass=deltaMonoMass,
#      composition=composition,
#      specificity=specificity,
#      refs=refs)
#}
Modification <- function(...) {
  new("Modification", ...)
}

#' internal function to create Modification from xml
#'
#' @param xml xml_nodeset, <mod>
#' @return Modification
#' @noRd
.Modification <- function(xml) {
  title <- .title(xml)
  delta <- .delta(xml)
  Modification(id=unname(as.integer(title["id"])),
               name=unname(unname(title["name"])),
               description=unname(title["description"]),
               lastModified=unname(title["lastModified"]),
               approved=unname(title["approved"] == "1"),
               deltaAvgMass=unname(delta["avgMass"]),
               deltaMonoMass=unname(delta["monoMass"]),
               composition=.composition(xml),
               specificity=.specificity(xml),
               refs=.xref(xml))
}

#' @param object Modification
#' @return integer, accession number/id.
#' @noRd
accession <- accessions # just an alias

#' Modification validation function
#'
#' @param object Modification
#' @return logical (TRUE) or character of error messages
#' @noRd
.validModification <- function(object) {
  msg <- character()

  if (length(object@deltaMonoMass) != 1L ||
      !is.double(object@deltaMonoMass)) {
    msg <- c(msg, "'deltaMonoMass' has to be a 'double' of length one.")
  }
  if (!nrow(object@specificity)) {
    msg <- c(msg, "'specificity' should not be empty.")
  }
  if (!all(c("site", "position") %in% colnames(object@specificity))) {
    msg <- c(msg, "'specificity' doesn't have a 'site' or 'position' column.")
  }
  if (length(msg)) {
    msg
  } else {
    TRUE
  }
}
