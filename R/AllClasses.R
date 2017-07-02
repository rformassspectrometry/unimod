#' Modification class
#'
#' @slot id integer, unimod record_id.
#' @slot name character, unimod PSI-MS name/title.
#' @slot description character, unimod full_name.
#' @slot lastModified character, time of last modification.
#' @slot approved logical, approved?
#' @slot deltaAvgMass double, unimod delta avge_mass.
#' @slot deltaMonoMass double, unimod delta mono_mass.
#' @slot composition integer, named vector. The names are the titles of unimod
#' elements. The items are the number of the elements.
#' @slot specificity data.frame, with site, position and classification of the
#' modification.
#' @slot refs data.frame, references listed in the unimod database.
#'
setClass("Modification",
  slots=c(id="integer",
          name="character",
          description="character",
          lastModified="character",
          approved="logical",
          deltaAvgMass="numeric",
          deltaMonoMass="numeric",
          composition="integer",
          specificity="data.frame",
          refs="data.frame"),
  contains=c("VersionedBiobase"),
  prototype=prototype(new("VersionedBiobase",
                          versions=c(Modification="0.0.1")),
                      id=integer(),
                      name=character(),
                      description=character(),
                      lastModified=character(),
                      approved=logical(),
                      deltaAvgMass=double(),
                      deltaMonoMass=double(),
                      composition=integer(),
                      specificity=data.frame(stringsAsFactors=FALSE),
                      refs=data.frame(stringsAsFactors=FALSE)
  )
)
