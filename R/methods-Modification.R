setMethod("accessions", "Modification", function(object, ...) {
  object@id
})

#' @rdname Modification-class
setMethod("dim", "Modification", function(x) {
  dim(specificity(x))
})

#' @rdname Modification-class
setMethod("length", "Modification", function(x) {
  nrow(specificity(x))
})

setMethod("mass", "Modification", function(object, ...) {
  deltaMonoMass(object, ...)
})

#' @param x Modification object
#' @rdname Modification-class
setMethod("names", "Modification", function(x) {
  x@name
})

setMethod("show", "Modification", function(object) {
  cat("- General:\n")
  group <- c(objc="Class",
             acce="Accession number/id",
             name="PSI-MS/Interim Name",
             desc="Description",
             comp="Composition",
             setNames(paste("Delta", c("Average", "Monoisotopic"), "Mass"),
                      c("avgm", "mono")),
             appr="Approved")
  value <- c(objc=class(object),
             acce=object@id,
             name=object@name,
             desc=object@description,
             comp=paste0(names(object@composition), "(", object@composition, ")",
                    collapse=" "),
             avgm=object@deltaAvgMass,
             mono=object@deltaMonoMass,
             appr=object@approved)
  cat(paste0("  ", format(group[names(group) %in% names(value)],
                          justify="left"), ": ",
             format(value, justify="right"), collapse="\n"), "\n")
  cat("- Specificity:\n")
  print(object@specificity)
  cat("- References: use 'references(object)'\n")

  invisible(NULL)
})
