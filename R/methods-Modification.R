setMethod("accessions", "Modification", function(object, ...) {
  id(object)
})

setMethod("approved", "Modification", function(object, ...) {
  object@approved
})

setMethod("composition", "Modification", function(object, ...) {
  object@composition
})

setMethod("description", "Modification", function(object, ...) {
  object@description
})

setMethod("deltaAvgMass", "Modification", function(object, ...) {
  object@deltaAvgMass
})

setMethod("deltaMonoMass", "Modification", function(object, ...) {
  object@deltaMonoMass
})

#' @rdname Modification-class
setMethod("dim", "Modification", function(x) {
  dim(specificity(x))
})

setMethod("id", "Modification", function(object, ...) {
  object@id
})

#' @rdname Modification-class
setMethod("length", "Modification", function(x) {
  nrow(specificity(x))
})

setMethod("mass", "Modification", function(object, ...) {
  deltaMonoMass(object, ...)
})

setMethod("name", "Modification", function(object) {
  object@name
})

#' @param x Modification object
#' @rdname Modification-class
setMethod("names", "Modification", function(x) {
  x@name
})

setMethod("references", "Modification", function(object, ...) {
  object@refs
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

setMethod("specificity", "Modification", function(object, all=TRUE, ...) {
  if (!all && "hidden" %in% colnames(object@specificity)) {
    object@specificity[!object@specificity$hidden,,drop=FALSE]
  } else {
    object@specificity
  }
})
