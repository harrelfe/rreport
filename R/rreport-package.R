#' This package creates reports.
#'
#' @author Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' Maintainer: Charles Dupont \email{charles.dupont@@vanderbilt.edu}
#'
#' @importFrom chron dates seq.dates chron years days hours minutes seconds
#' @importFrom lattice bwplot
#' @importFrom rms survplot
#' @importFrom survival survfit Surv
#' @import Hmisc
#' @docType package
#' @aliases rreport package-rreport
#' @name rreport
NULL

# The caching and check for conflicts require looking for a pattern of objects; the search may be avoided by defining an object ‘.noGenerics’
# see ?library
.noGenerics <- TRUE

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("rreport library by Frank E Harrell Jr\n\nType library(help='rreport') to see overall documentation.\n\n")
}

# questions?
# Surv may come from rms
# accrualReport: no visible binding for global variable ‘code.infig’
# aeReport: no visible binding for global variable ‘weeks’
# completenessReport: no visible binding for global variable ‘compFullCaptionDone’
# freqReport: no visible binding for global variable ‘name’
# rangeCheck: no visible binding for global variable ‘dataframe’
# there is no "gtype" (interactive/pdf/ps)
# endPlot
# putFig
# startPlot

# mixedvarReport: no visible global function definition for ‘Key’

# S3 methods shown with full name in documentation object 'getReferenceObject':
#   ‘print.latexReference’
# 
# S3 methods shown with full name in documentation object 'floor.chron':
#   ‘floor.chron’ ‘ceiling.chron’
# 
# The \usage entries for S3 methods should use the \method markup and not their full name.
# See the chapter ‘Writing R documentation files’ in the ‘Writing R Extensions’ manual.
# * checking Rd contents ... WARNING
# Argument items with no description in Rd object 'getReferenceObject':
#   ‘refD’ ‘newMarker’ ‘keyword’ ‘label’
