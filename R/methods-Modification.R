#' @param object Modification
#' @noRd
setMethod("show", "Modification", function(object) {
  cat(class(object), " (version: ",
      paste0(classVersion(object)[[class(object)]], collapse="."),
      ")\n", sep="")
  invisible(NULL)
})

