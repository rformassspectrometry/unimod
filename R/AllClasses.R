#' The Modification Class
#'
#'
#' @details TODO
#'
#' @slot TODO
#'
#' @return TODO
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @examples
# setClass(
#    "Modification",
#    slots=c(
#        id="character",
#        mass="numeric",
#        fixed="logical",
#        rxPattern="character",
#        rxReplacement="character",
#        rxFixed="logical",
#        maximalNumberOfVariableSites="integer",
#        proteoform="character"
#    )
#    #,
#    #prototype=prototype(
#    #    mass=double(),
#    #    location=numeric(),
#    #    pattern=character(),
#    #    replacement=character(),
#    #    fixed=logical(),
#    #    maximalNumberOfVariableSites=integer(),
#    #    proteoform=character()
#    #    #validity=.validateModification
#    #)
#)

setClass(
    "Modification",
    contains="VIRTUAL",
    slots=c(
        id="character",
        name="character",
        unimodId="numeric",
        mass="numeric"
    )
)

setClass(
    "ModificationFixed",
    contains="Modification",
    slots=c(
        site="character"
    )
)

setClass(
    "ModificationFixedLocalized",
    contains="ModificationFixed",
    slots=c(
        globalIndex="numeric",
        siteIndex="numeric"
    )
)

setClass(
    "ModificationFixedRegExp",
    contains="ModificationFixed",
    slots=c(
        replacement="character"
    )
)

setClass(
    "ModificationTerm",
    contains="Modification"
)

setClass(
    "ModificationNterm",
    contains="ModificationTerm"
)

setClass(
    "ModificationCterm",
    contains="ModificationTerm"
)

setClass(
    "ModificationVariable",
    contains="ModificationFixedLocalized",
    slots=c(
        globalMaxN="numeric",
        siteMaxN="numeric"
    )
)
