#' Synthetic Longitudinal EDSS and SDMT Data
#'
#' Artificially generated toy data set including Extended Disability Status Scale (EDSS)
#' and Symbol Digit Modalities Test (SDMT) scores in a small cohort of example patients
#' to illustrate the use of the package.
#'
#' @name toydata_visits
#'
#' @docType data
#'
#' @usage data(toydata_visits)
#'
#' @format An object of class `data.frame`, with columns:
#' \describe{
#'  \item{id}{Subject IDs.}
#'  \item{date}{Visit dates.}
#'  \item{EDSS}{Synthetic EDSS scores (values between 0 and 10).}
#'  \item{SDMT}{Synthetic SDMT scores (values between 0 and 110).}
#' }
#' @references This data set was artificially created for the `msprog` package.
#' @keywords datasets
#' @examples
#'
#' head(toydata_visits)
#'
NULL



#' Synthetic Relapse Data
#'
#' Artificially generated relapse onset dates for some example patients in [toydata_visits]
#' to illustrate the use of the package.
#'
#' @name toydata_relapses
#'
#' @docType data
#'
#' @usage data(toydata_relapses)
#'
#' @format An object of class `data.frame`, with columns:
#' \describe{
#'  \item{id}{Subject IDs.}
#'  \item{date}{Relapse onset dates.}
#' }
#' @references This data set was artificially created for the `msprog` package.
#' @keywords datasets
#' @examples
#'
#' data(toydata_relapses)
#' head(toydata_relapses)
#'
NULL

