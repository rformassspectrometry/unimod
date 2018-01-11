#' Fetch and read unimod.xml from the web.
#' @return xml_document
#' @noRd
.unimodDb <- function() {
    xml2::read_xml("http://www.unimod.org/xml/unimod.xml")
}
