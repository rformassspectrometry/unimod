setMethod("accessions", "Modification", function(object, ...) {
  id(object)
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

setMethod("dim", "Modification", function(x) {
  dim(specificity(x))
})

setMethod("id", "Modification", function(object, ...) {
  object@id
})

setMethod("length", "Modification", function(x) {
  nrow(specificity(x))
})

setMethod("mass", "Modification", function(object, ...) {
  deltaMonoMass(object, ...)
})

setMethod("name", "Modification", function(object) {
  object@name
})

setMethod("names", "Modification", function(x) {
  x@name
})

setMethod("references", "Modification", function(object, ...) {
  object@refs
})

setMethod("show", "Modification", function(object) {
  cat("- General:\n")
  group <- c(paste(class(object), "version"),
             "Accession number/id",
             "PSI-MS/Interim Name",
             "Description",
             "Composition",
             paste("Delta", c("Average", "Monoisotopic"), "Mass"))
  value <- c(paste0(classVersion(object)[[class(object)]], collapse="."),
             object@id, object@name, object@description,
             paste0(names(object@composition), "(", object@composition, ")",
                    collapse=" "),
             object@deltaAvgMass, object@deltaMonoMass)
  cat(paste0("  ", format(group, justify="left"), ": ",
             format(value, justify="right"), collapse="\n"), "\n")
  cat("- Specificity:\n")
  print(object@specificity)
  cat("- References: use 'references(object)'\n")

  invisible(NULL)
})

setMethod("specificity", "Modification", function(object, ...) {
  object@specificity
})
