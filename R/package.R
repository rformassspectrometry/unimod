#' Managing amino acid modifications for mass spectrometry in R.
#'
#' An interface to the community supported database for amino
#' acid/protein modifications using mass spectrometry.
#'
#' @details
#'
#' Unimod is a public domain database, distributed under a copyleft
#' licence: 'a copyright notice that permits unrestricted redistribution
#' and modification, provided that all copies and derivatives retain the
#' same permissions.'
#'
#' The aim is to create a community supported, comprehensive database of
#' protein modifications for mass spectrometry applications. That is,
#' accurate and verifiable values, derived from elemental compositions,
#' for the mass differences introduced by all types of natural and
#' artificial modifications. Other important information includes any
#' mass change, (neutral loss), that occurs during MS/MS analysis, and
#' site specificity, (which residues are susceptible to modification and
#' any constraints on the position of the modification within the protein
#' or peptide).
#'
#' The goal is not to place this information in a biological
#' context. There are already many high quality sources of biological
#' information about post-translational modifications:
#'
#' \itemize{
#'  \item{RESID for post-translational modifications}
#'  \item{Swiss-Prot protein sequence database}
#'  \item{Prosite database of protein families and domains}
#'  \item{Glycan Database from the Consortium for Functional Glycomics}
#' }
#'
#' Other compilations of modifications with a mass spectrometry bias:
#'
#' \itemize{
#'  \item{Delta Mass}
#'  \item{FindMod}
#' }
#'
#' @references
#' Source and more details: \url{http://www.unimod.org/unimod_help.html}
#'
#' @docType package
#' @name unimod-package
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @references \url{https://github.com/ComputationalProteomicsUnit/unimod/}
#' @keywords package
#'
#' @import methods
#' @importClassesFrom Biobase Versioned
#' @importFrom Biobase classVersion description
#' @importFrom ProtGenerics accessions mass
#' @importFrom utils packageVersion
#' @importFrom stats setNames
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attrs xml_text
#' xml_length
NULL
