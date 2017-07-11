#' @describeIn Modification-class Constructor
#' @param \ldots arguments passed to the internal constructor.
#' @export
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

#' @rdname Modification-class
#' @return integer, accession number/id.
#' @aliases accession id accessions accessions,Modification-method
#' @export
accession <- id <- accessions # just an alias

#' @rdname Modification-class
#' @param object Modification
#' @return double, named vector
#' @export
composition <- function(object) {
  .isModification(object)
  object@composition
}

#' @describeIn Modification-class mass
#' @return double, delta average mass
#' @export
deltaAvgMass <- function(object) {
  .isModification(object)
  object@deltaAvgMass
}

#' @describeIn Modification-class mass
#' @return double, delta monoisotopic mass
#' @export
deltaMonoMass <- function(object) {
  .isModification(object)
  object@deltaMonoMass
}

#' @describeIn Modification-class name
#' @return character, PSI name
#' @export
name <- names # just an alias

#' @describeIn Modification-class references
#' @return data.frame, with
#' @export
references <- function(object) {
  .isModification(object)
  object@refs
}

#' @describeIn Modification-class specificity
#' @param hidden logical, show hidden specificities (default: TRUE)?
#' @return data.frame, with specificities
#' @export
specificity <- function(object, hidden=TRUE, ...) {
  .isModification(object)
  if (!hidden && "hidden" %in% colnames(object@specificity)) {
    object@specificity[!object@specificity$hidden,,drop=FALSE]
  } else {
    object@specificity
  }
}

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
