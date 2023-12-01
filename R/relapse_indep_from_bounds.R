
#' Define relapse-free intervals for PIRA definition.
#'
#' \code{relapse_indep_from_bounds} organises the given interval bounds around baseline, event, and confirmation
#' into a named list to be given as argument \code{relapse_indep} to function \code{MSprog}.
#'
#' If the right end is \code{NULL}, the interval is assumed to extend up to the left end of the next interval.
#' If the left end is \code{NULL}, the interval is assumed to extend up to the right end of the previous interval.
#'
#' @param b0 Days before baseline (\code{>=0}).
#' @param b1 Days after baseline (\code{>=0}), or \code{NULL}.
#' @param e0 Days before event (\code{>=0}), or \code{NULL}.
#' @param e1 Days after event (\code{>=0}), or \code{NULL}.
#' @param c0 Days before confirmation (\code{>=0}), or \code{NULL}.
#' @param c1 Days after confirmation (\code{>=0}).
#'
#' @return A named list to be given as argument \code{relapse_indep} to function \code{MSprog}
#' @export relapse_indep_from_bounds
#' @examples
#' # [Muller JAMA Neurol 2023](high-specificity def) No relapses between baseline and confirmation:
#' relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,NULL,NULL,0)
#' # [, ]Muller JAMA Neurol 2023] No relapses within event-90dd->event+30dd and within confirmation-90dd->confirmation+30dd:
#' relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)
#' # [Kappos JAMA Neurol 2020] No relapses within baseline->event+30dd and within confirmation+-30dd:
#' relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,30,30,30)
relapse_indep_from_bounds <- function(b0, b1, e0, e1, c0, c1) {
  for (p in c(b0, b1, e0, e1, c0, c1)) {
    if (!is.null(p) & p<0) {
      stop('invalid bounds: either provide `NULL` or a non-negative number')
    }
  }
  list('bl'=list(b0, b1), 'event'=list(e0, e1), 'conf'=list(c0, c1))
}
