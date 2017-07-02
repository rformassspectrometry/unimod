#' Read unimod.xml from inst/extdata.
#' @return xml_document
#' @noRd
.unimodDb <- function() {
  f <- system.file(file.path("extdata", "unimod.xml"), package="unimod")
  read_xml(f)
}
