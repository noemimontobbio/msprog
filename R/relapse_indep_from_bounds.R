
#' Define relapse-free intervals for PIRA definition.
#'
#' `relapse_indep_from_bounds()` organises the given interval bounds around baseline, event, and confirmation
#' into a named list to be given as argument `relapse_indep` to function [MSprog()].
#'
#' If the right end is `NULL`, the interval is assumed to extend up to the left end of the next interval.
#' If the left end is `NULL`, the interval is assumed to extend up to the right end of the previous interval.
#'
#' @param b0 Days before baseline (`>=0`).
#' @param b1 Days after baseline (`>=0`), or `NULL`.
#' @param e0 Days before event (`>=0`), or `NULL`.
#' @param e1 Days after event (`>=0`), or `NULL`.
#' @param c0 Days before confirmation (`>=0`), or `NULL`.
#' @param c1 Days after confirmation (`>=0`).
#' @param use_end_dates If `TRUE`, only the right bounds (`e1`, `c1`) are used,
#' as the right bounds will be defined by the onset-to-end interval of each relapse.
#' This option is only relevant when relapse end dates are available.
#'
#' @references
#' \[1\] Müller J, Cagol A, Lorscheider J, Tsagkas C, Benkert P, Yaldizli Ö, et al.
#' Harmonizing definitions for progression independent of relapse activity in multiple sclerosis: A systematic review.
#' JAMA Neurol. 2023;80:1232–45. \cr\cr
#' \[2\] Kappos L, Wolinsky JS, Giovannoni G, Arnold DL, Wang Q, Bernasconi C, et al.
#' Contribution of relapse-independent progression vs relapse-associated worsening to overall confirmed disability
#' accumulation in typical relapsing multiple sclerosis in a pooled analysis of 2 randomized clinical trials.
#' JAMA Neurol. 2020;77:1132–40.
#'
#' @return A named list to be given as argument `relapse_indep` to function [MSprog()]
#' @export relapse_indep_from_bounds
#' @examples
#' # No relapses between baseline and confirmation (high-specificity definition from [1]):
#' relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,NULL,NULL,0)
#' # No relapses within event-90dd->event+30dd
#' # and within confirmation-90dd->confirmation+30dd [1]:
#' relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)
#' # No relapses within baseline->event+30dd and within confirmation+-30dd [2]:
#' relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,30,30,30)
relapse_indep_from_bounds <- function(b0=0, b1=0, e0=90, e1=30, c0=90, c1=30, use_end_dates=F) {
  for (p in c(b0, b1, e0, e1, c0, c1)) {
    if (!is.null(p) & p<0) {
      stop('invalid bounds: please either provide `NULL` or a non-negative number')
    }
  }

  if (is.null(b0) & !use_end_dates) {
    stop('`b0` cannot be `NULL`: please provide a non-negative number')
  }
  if (is.null(c1)) {
    stop('`c1` cannot be `NULL`: please provide a non-negative number')
  }

  for (p in c(e1, c1)) {
  if (use_end_dates & (is.null(p) || p<0)) {
    stop('invalid bounds: please provide a non-negative number')
    }
  }

  if (use_end_dates) {
    list('event'=e1, 'conf'=c1)
  } else {
  list('bl'=list(b0, b1), 'event'=list(e0, e1), 'conf'=list(c0, c1))
  }
}

