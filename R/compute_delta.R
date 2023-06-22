
#' Definition of progression deltas for different tests.
#'
#' `compute_delta` returns the minimum delta to be considered as a valid change
#' from baseline of an outcome measure (EDSS, NHPT, T25FW, or SDMT).
#'
#' @param baseline Outcome value at baseline.
#' @param outcome One of: \cr
#'  'edss' (Extended Disability Status Scale ) [default]; \cr
#'  'nhpt' (Nine-Hole Peg Test) \cr
#'  'nhptD' (Nine-Hole Peg Test, dominant hand) \cr
#'  'nhptND' (Nine-Hole Peg Test, non-dominant hand) \cr
#'  't25fw' (Timed 25-Foot Walk) \cr
#'  'sdmt' (Symbol Digit Modalities Test).
#'
#' @return Minimum delta corresponding to valid change from the provided baseline value.
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
      stop('invalid EDSS baseline')
    }
  } else if (outcome %in% c('nhpt', 'nhptD', 'nhptND', 't25fw')) {
    return(baseline/5)
  } else if (outcome == 'sdmt') {
    return(min(baseline/10, 3))
  }
}
