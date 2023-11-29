
#' Definition of progression deltas for different tests.
#'
#' \code{compute_delta} returns the minimum delta to be considered as a valid change
#' from baseline of an outcome measure (EDSS, NHPT, T25FW, or SDMT).
#'
#' @param baseline Outcome value at baseline.
#' @param outcome One of: \cr
#' \itemize{
##' \item{\code{'edss'}}{ (Extended Disability Status Scale ) [default];}
#'  \item{\code{'nhpt'}}{ (Nine-Hole Peg Test);}
#'  \item{\code{'t25fw'}}{ (Timed 25-Foot Walk);}
#'  \item{\code{'sdmt'}}{ (Symbol Digit Modalities Test).}
#'  }
#'
#' @return Minimum delta corresponding to valid change from the provided baseline value. Specifically:
#' \itemize{
#'  \item{EDSS: }{1.5 if \code{baseline==0}, 1 if \code{0<baseline<=5}, 0.5 if \code{baseline>=5.5};}
#'  \item{NHPT and T25FW: }{20\% of \code{baseline};}
#'  \item{SDMT: }{either 3 points or 10\% of \code{baseline}.}
#'  }
#' @export compute_delta
#' @examples
#' compute_delta(4.5)
#' compute_delta(55, outcome='sdmt')
compute_delta <- function(baseline, outcome='edss') {

  if (outcome == 'edss') {
    if (baseline >= 0 & baseline < 0.5) {
      return(1.5)
    } else if (baseline >= 0.5 & baseline < 5.5) {
      return(1.0)
    } else if (baseline >= 5.5 & baseline <= 10) {
      return(0.5)
    } else {
      stop('invalid EDSS score')
    }
  } else if (outcome %in% c('nhpt', 't25fw')) {
    if (baseline<0) {
      stop('invalid ', toupper(outcome),' score')
    }
    if (outcome=='nhpt' & baseline>300) {
      warning('NHPT score >300')
    }
    else if (outcome=='t25fw' & baseline>180) {
      warning('T25FW score >180')
    }
    return(baseline/5)
  } else if (outcome == 'sdmt') {
    if (baseline<0 || baseline>110) {
      stop('invalid SDMT score')
    }
    return(min(unlist(c(baseline/10, 3))))
  } else {
  stop('invalid outcome type')
    }
}
