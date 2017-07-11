#' Test for Modification class
#' @param object object to test
#' @return TRUE if object is a Modification otherwise fails with an error
#' @noRd
.isModification <- function(object) {
  if (!isTRUE(is(object, "Modification"))) {
    stop("'object' has to be an 'Modification' object.")
  }
  TRUE
}

#' Read unimod.xml from inst/extdata.
#' @return xml_document
#' @noRd
.unimodDb <- function() {
  f <- system.file(file.path("extdata", "unimod.xml"), package="unimod")
  read_xml(f)
}
