#' @param object Modification
#' @noRd
setMethod("show", "Modification", function(object) {
  cat("- General:\n")
  group <- c(paste(class(object), "version"),
             "Accession number/id",
             "PSI-MS/Interim Name:",
             "Composition",
             paste("Delta", c("Average", "Monoisotopic"), "Mass"))
  value <- c(paste0(classVersion(object)[[class(object)]], collapse="."),
             object@id, object@name,
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

