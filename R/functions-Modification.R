UnimodModification <- function(id, location=numeric(), fixed=TRUE, 
                               globalIndex=integer(),
                               siteIndex=integer(),
                               globalMaxN=5L,
                               siteMaxN=3L,
                               massType=c("MonoMass", "AvgMass"), 
                               verbose=interactive()) {
    massType <- match.arg(massType)

    mod <- modifications[id,]

    if (is.na(mod$Id)) {
        stop("Could not find any modification for ", id)
    }
    if (nrow(mod) > 1L) {
        stop(id, " was not unique.")
    }

    verifiedIds <- c(
        ## default verified:
        ## 1: Acetyl
        "Acetyl:K", "Acetyl:N-term", "Acetyl:C", "Acetyl:S",
        "Acetyl:P-N-term", "Acetyl:T", "Acetyl:Y", "Acetyl:H",
        "Acetyl:R",
        ## 4: Carbamidomethyl
        "Carbamidomethyl:C", "Carbamidomethyl:K",
        "Carbamidomethyl:N-term", "Carbamidomethyl:H",
        "Carbamidomethyl:D", "Carbamidomethyl:E", "Carbamidomethyl:S",
        "Carbamidomethyl:T", "Carbamidomethyl:Y", "Carbamidomethyl:U",
        "Carbamidomethyl:M", "Carbamidomethyl:M:NL",
        ## 21: Phospho
        "Phospho:T", "Phospho:S", "Phospho:Y", "Phospho:D",
        "Phospho:H", "Phospho:C", "Phospho:R", "Phospho:K",
        "Phospho:T:NL", "Phospho:S:NL",
        ## 765: Met-loss
        "Met-loss:P-M",
        ## 766: Met-loss+Acetyl
        "Met-loss+Acetyl:P-M"
    )

    if (!mod$Id %in% verifiedIds) {
        warning( 
            "Applying the default rule for the modification: ", mod$Id,
            "\nPlease create an issue on: https://github.com/",
            "ComputationalProteomicsUnit/unimod/issues/new ",
            "\nto let us implement the correct rule or if the default ",
            "one is already correct we could remove this message."
        )
    }

    if (id == "Met-loss:P-M") {
        ## 765: Met-loss
        # N-terminal initiator methionine is removed by a methionine
        # aminopeptidase from proteins where the residue following the
        # methionine is Ala, Cys, Gly, Pro, Ser, Thr or Val. This is
        # generally the final N-terminal state for proteins where the
        # following residue was a Cys, Pro or Val.
        list(new(
            "ModificationFixedRegExp",
            id=mod$Id,
            mass=mod[, massType],
            site="^M([ACGPSTV])",
            replacement="\\1"
        ))
    } else if (id == "Met-loss+Acetyl:P-M") {
        ## 766: Met-loss+Acetyl
        # The N-terminal initiator methionine is removed by a methionine
        # aminopeptidase from proteins whose residue following the
        # methionine is Ala, Cys, Gly, Pro, Ser, Thr or Val. Proteins
        # whose following residue was Ala, Gly, Ser or Thr are then
        # acetylated by an N(alpha)-acetyltransferase on the new
        # N-terminus.
        list(
            new("ModificationFixedRegExp",
                id=mod$Id,
                unimodId=mod$UnimodId,
                name=mod$Name,
                mass=mod[, massType],
                site="^M([AGST])",
                replacement="\\1"
            ),
            new("ModificationFixedRegExp",
                id="Met-loss:P-M",
                unimodId=mod$UnimodId,
                name=mod$Name,
                mass=modifications["Met-loss:P-M", massType],
                site="^M([CPV])",
                replacement="\\1"
            )
        )
    } else if (endsWith(mod$Site, "term")) {
        new(paste0("Modification", substr(mod$Site, 1L, 1L), "term"), 
            id=mod$Id,
            unimodId=mod$UnimodId,
            name=mod$Name,
            mass=mod[, massType]
        )
    } else if (fixed) {
        if (!length(globalIndex) && !length(siteIndex)) {
            new("ModificationFixed",
                id=mod$Id,
                unimodId=mod$UnimodId,
                name=mod$Name,
                mass=mod[, massType],
                site=mod$Site
            )
        } else {
            new("ModificationFixedLocalized",
                id=mod$Id,
                unimodId=mod$UnimodId,
                name=mod$Name,
                mass=mod[, massType],
                site=mod$Site,
                globalIndex=globalIndex,
                siteIndex=siteIndex
           )
        }
    } else {
        new("ModificationVariable",
            id=mod$Id,
            unimodId=mod$UnimodId,
            name=mod$Name,
            mass=mod[, massType],
            site=mod$Site,
            globalIndex=globalIndex,
            siteIndex=siteIndex,
            globalMaxN=globalMaxN,
            siteMaxN=siteMaxN
       )
    }
}

deltaMass <- function(m, ps) {
    if (length(ps) > 1L && 
        any(vapply(m, inherits, TRUE, "ModificationVariable"))) {
        stop(
            "Vectorized calculation of variable modifications for multiple ",
            "sequences isn't supported."
        )
    }
    lapply(m, function(mm).countSites(mm, ps) * mm@mass)
}

pepseq <- function(m, ps) {
    lapply(ps, function(pps) {
        for (i in seq(along=m)) {
            pps <- .pepseq(m[[i]], pps)
        }
        pps
    })
}

proteoform <- function(m, ps) {
    lapply(ps, function(pps) {
        for (i in seq(along=m)) {
            pps <- .proteoform(m[[i]], pps)
        }
        pps
    })
}

.sitePos <- function(m, ps) {
    lapply(
        gregexpr(m@site, ps, fixed=!inherits(m, "ModificationFixedRegExp")),
        function(rx) {
            if (rx[1L] > 0L) {
                matrix(c(rx, rx + attr(rx, "match.length")), ncol=2L)
            } else {
                matrix(nrow=0L, ncol=2L)
            }
        }
    )
}
