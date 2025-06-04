
#' Define relapse-free intervals for PIRA definition.
#'
#' `relapse_indep_from_bounds()` organises the given interval bounds
#' into a named list to be given as argument `relapse_indep` to function [MSprog()].
#' The relapse-free intervals may be anchored to (any subset of) the following three data-driven checkpoints.
#' \itemize{
#' \item{`'prec'`: a visit preceding the event: can be (i) the current baseline, (ii) the last visit before the event,
#' or (iii) the last pre-worsening visit (`i` such that `outcome[event] - outcome[i] >= delta_fun(outcome[i])`,
#' and same for the confirmation visit);}
#' \item{`'event'`: the disability worsening event;}
#' \item{`'conf'`: the (first) confirmation visit.}
#' }
#'
#' If both ends of an interval are 0 (e.g., if both `p0=0` and `p1=0`), the checkpoint is ignored. If the right end is `NULL`, the interval is assumed to extend up to the left end of the next interval.
#' If the left end is `NULL`, the interval is assumed to extend up to the right end of the previous interval.
#' Here are some examples:
#' \itemize{
#' \item{No relapses from 90dd before to 30dd after the event, and from 90dd before to 30dd after the confirmation \[1\]:
#' \cr`relapse_indep_from_bounds(e0=90,e1=30,c0=90,c1=30)`;}
#' \item{No relapses between baseline and confirmation (high-specificity definition from \[1\]):
#' \cr`relapse_indep_from_bounds(p0=0,p1=NULL,e0=NULL,e1=NULL,c0=NULL,c1=0)`;}
#' \item{No relapses from baseline to 30dd after the event, and within confirmation+-30dd \[2\]:
#' \cr`relapse_indep_from_bounds(p0=0,p1=NULL,e0=NULL,e1=30,c0=30,c1=30)`.}
#' }
#'
#' @param p0 Days before preceding visit (`>=0`).
#' @param p1 Days after preceding visit (`>=0`), or `NULL`.
#' @param e0 Days before event (`>=0`), or `NULL`.
#' @param e1 Days after event (`>=0`), or `NULL`.
#' @param c0 Days before confirmation (`>=0`), or `NULL`.
#' @param c1 Days after confirmation (`>=0`).
#' @param prec_type Which visit to use as "preceding visit". Must be one of:
#' \itemize{
#' \item{`'baseline'`: the current baseline;}
#' \item{`'last'`: the last visit before the event;}
#' \item{`'last_lower'`: the last pre-worsening visit, i.e.,
#' the last visit `i` where `outcome[event] - outcome[i] >= delta_fun(outcome[i])`.}
#' }
#' @param use_end_dates If `TRUE`, only the right bounds (`e1`, `c1`) are used,
#' as the right bounds will be defined by the onset-to-end interval of each relapse.
#' This option is only relevant when relapse \emph{end} dates are available.
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
relapse_indep_from_bounds <- function(p0=0, p1=0, e0=0, e1=0, c0=0, c1=0, prec_type='baseline', use_end_dates=F) {
  if (!(prec_type %in% c('baseline', 'last', 'last_lower'))) {
    stop('invalid value for `prec_type` argument, please provide one of: \'baseline\', \'last\', \'last_lower\'')
  }
  for (p in c(p0, p1, e0, e1, c0, c1)) {
    if (!is.null(p) & p<0) {
      stop('invalid bounds: please either provide `NULL` or a non-negative number')
    }
  }

  if (is.null(p0) & !use_end_dates) {
    stop('`p0` cannot be `NULL`: please provide a non-negative number')
  }
  if (is.null(c1) & !use_end_dates) {  #if (is.null(c1))
    stop('`c1` cannot be `NULL`: please provide a non-negative number')
  }

  # for (p in c(e1, c1)) {
  # if (use_end_dates & (is.null(p) || p<0)) {
  #   stop('invalid bounds: please provide a non-negative number')
  #   }
  # }

  if (use_end_dates) {
    list('event'=e1, 'conf'=c1)
  } else {
  list('prec'=list(p0, p1), 'event'=list(e0, e1), 'conf'=list(c0, c1), 'prec_type'=prec_type)
  }
}

