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
  Modification(title=unname(unname(title["title"])),
               name=unname(title["name"]),
               id=unname(as.integer(title["id"])),
               lastModified=unname(title["lastModified"]),
               approved=unname(title["approved"] == "1"),
               deltaAvgMass=unname(delta["avgMass"]),
               deltaMonoMass=unname(delta["monoMass"]),
               composition=.composition(xml),
               specificity=.specificity(xml),
               refs=.xref(xml))
}
