#' Synthetic Longitudinal EDSS and SDMT Data
#'
#' Small, artificially generated toy data set providing Extended Disability Status Scale (EDSS)
#' and Symbol Digit Modalities Test (SDMT) information on 4 example patients
#' to illustrate the use of the package.
#'
#' @name toydata_visits
#'
#' @docType data
#'
#' @usage data(toydata_visits)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{id}{Subject ID}
#'  \item{date}{The visit date}
#'  \item{EDSS}{A value between 0 and 10}
#'  \item{SDMT}{A value between 0 and 110}
#' }
#' @references This data set was artificially created for the `msprog` package.
#' @keywords datasets
#' @examples
#'
#' data(toydata_visits)
#' head(toydata_visits)
#'
NULL



#' Synthetic Relapse Data
#'
#' Artificially generated relapse dates for some example patients in `toydata_visits` to illustrate the use of the package.
#'
#' @name toydata_relapses
#'
#' @docType data
#'
#' @usage data(toydata_relapses)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{id}{Subject ID}
#'  \item{date}{The relapse date}
#' }
#' @references This data set was artificially created for the `msprog` package.
#' @keywords datasets
#' @examples
#'
#' data(toydata_relapses)
#' head(toydata_relapses)
#'
NULL

