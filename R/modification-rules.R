#' Regular expression rules for modifications.
#'
#' Merged to the unimod modifications data.frame; Perl Expressions allowed.
#'
#' @noRd
.modificationRules <- matrix(
    dimnames=list(c(), c("Id", "Pattern", "Replacement")),
    ncol=3L, byrow=TRUE,
    ## Rules:
    c(
        # 1: Acetyl
        "Acetyl:K",         "K",    NA_character_,
        "Acetyl:N-term",    "^\\w", NA_character_,
        "Acetyl:C",         "C",    NA_character_,
        "Acetyl:S",         "S",    NA_character_,
        "Acetyl:P-N-term",  "^\\w", NA_character_,
        "Acetyl:T",         "T",    NA_character_,
        "Acetyl:Y",         "Y",    NA_character_,
        "Acetyl:H",         "H",    NA_character_,
        "Acetyl:R",         "R",    NA_character_,

        # 4: Carbamidomethyl
        "Carbamidomethyl:C",        "C",    NA_character_,
        "Carbamidomethyl:K",        "K",    NA_character_,
        "Carbamidomethyl:N-term",   "^\\w", NA_character_,
        "Carbamidomethyl:H",        "H",    NA_character_,
        "Carbamidomethyl:D",        "D",    NA_character_,
        "Carbamidomethyl:E",        "E",    NA_character_,
        "Carbamidomethyl:S",        "S",    NA_character_,
        "Carbamidomethyl:T",        "T",    NA_character_,
        "Carbamidomethyl:Y",        "Y",    NA_character_,
        "Carbamidomethyl:U",        "U",    NA_character_,
        "Carbamidomethyl:M",        "M",    NA_character_,
        "Carbamidomethyl:M:NL",     "M",    NA_character_,

        # 21: Phospho
        "Phospho:T",    "T",    NA_character_,
        "Phospho:S",    "S",    NA_character_,
        "Phospho:Y",    "Y",    NA_character_,
        "Phospho:D",    "D",    NA_character_,
        "Phospho:H",    "H",    NA_character_,
        "Phospho:C",    "C",    NA_character_,
        "Phospho:R",    "R",    NA_character_,
        "Phospho:K",    "K",    NA_character_,
        "Phospho:T:NL", "T",    NA_character_,
        "Phospho:S:NL", "S",    NA_character_,

        # 765 Met-loss
        "Met-loss:P-M", "^M([ACGPSTV])", "\\1",

        # 766 Met-loss+Acetyl
        "Met-loss+Acetyl:P-M", "^M([ACGPSTV])", "\\1"
    )
)
