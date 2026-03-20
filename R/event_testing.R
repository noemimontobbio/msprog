
#' Default minimum clinically meaningful shift for different scales.
#'
#' Compute the minimum clinically meaningful score change as a function of
#' the reference value for some widely used scales (EDSS, NHPT, T25FW, or SDMT),
#' according to the most established rule for each of these outcomes.
#'
#' Default thresholds are meant to apply to all versions of each test (e.g.,
#' dominant or non-dominant hand for NHPT, best time or mean of two trials, etc.).
#'
#' @param baseline Outcome value at baseline.
#' @param outcome One of: \cr
#' \itemize{
#' \item{`"edss"` (Extended Disability Status Scale, default);}
#'  \item{`"nhpt"` (Nine-Hole Peg Test);}
#'  \item{`"t25fw"` (Timed 25-Foot Walk);}
#'  \item{`"sdmt"` (Symbol Digit Modalities Test).}
#'  }
#' @return Minimum clinically meaningful change from the provided baseline value. Specifically:
#' \itemize{
#'  \item{EDSS: 1.5 if `baseline`=0, 1 if 0<`baseline`<=5.0, 0.5 if `baseline`>5.0}
#'  \item{NHPT and T25FW: 20`%` of `baseline`}
#'  \item{SDMT: either 4 points or 20`%` of `baseline`.}
#'  }
#' @export compute_delta
#' @examples
#' compute_delta(4.5) # default outcome is "edss"
#' compute_delta(55, outcome="sdmt")
compute_delta <- function(baseline, outcome="edss") {

  if (outcome == "edss") {
    if (baseline == 0) {
      return(1.5)
    } else if (baseline > 0 & baseline <= 5) {
      return(1.0)
    } else if (baseline > 5 & baseline <= 10) {
      return(0.5)
    } else {
      stop("invalid EDSS score")
    }
  } else if (outcome %in% c("nhpt", "t25fw")) {
    if (baseline<0) {
      stop("invalid ", toupper(outcome), " score")
    }
    if (outcome == "nhpt" & baseline > 300) {
      warning("NHPT score >300")
    }
    else if (outcome == "t25fw" & baseline > 180) {
      warning("T25FW score >180")
    }
    return(baseline/5)
  } else if (outcome == "sdmt") {
    if (baseline < 0 || baseline > 110) {
      stop("invalid SDMT score")
    }
    return(min(unlist(c(baseline/5, 4))))
  } else {
  stop("invalid outcome type")
    }
}







#' Compare value to reference.
#'
#' Check if an outcome value determines a valid worsening, or improvement,
#' or change, from a given reference value.
#'
#' @param x Outcome value to test.
#' @param baseline Outcome value at baseline.
#' @param type One of: \cr
#' \itemize{
##' \item{`"wors"` (worsening);}
#'  \item{`"impr"` (improvement);}
#'  \item{`"change"` (any valid change).}
#'  }
#' @param outcome One of: \cr
#' \itemize{
##' \item{`"edss"` (Extended Disability Status Scale, default);}
#'  \item{`"nhpt"` (Nine-Hole Peg Test);}
#'  \item{`"t25fw"` (Timed 25-Foot Walk);}
#'  \item{`"sdmt"` (Symbol Digit Modalities Test);}
#'  \item{`"custom"` (only accepted when specifying non-`NULL` `worsening`
#'  -- and `delta_fun` as well, if `sub_threshold=FALSE`).}
#'  }
#'  Outcome type determines the direction of worsening (see `worsening` argument)
#'  and selects the default definition of clinically meaningful change given the reference value
#'  (using the built-in function [compute_delta()]).
#'  The latter can be replaced by a custom function using the `delta_fun` argument.
#' @param worsening The direction of worsening (`"increase"` if higher values
#' correspond to worse disease course, `"decrease"` otherwise).
#' This argument is only used when `outcome` is set to `"custom"`.
#' Otherwise, `worsening` is automatically set to `"increase"` for EDSS, NHPT, T25FW,
#'  and to `"decrease"` for SDMT.
#' @param delta_fun Custom function specifying the minimum clinically meaningful
#' change in the outcome measure from the provided reference value.
#' The function provided must take a numeric value (reference score) as input,
#' and return a numeric value corresponding to the minimum relevant shift from baseline, see example below.
#' If `outcome` is set to `"custom"`, a custom delta function must be specified by the user.
#' For other values of `outcome`, if no `delta_fun` is specified, the built-in function [compute_delta()] is used internally.
#' The argument is ignored if `sub_threshold=TRUE`.
#' @param sub_threshold If `TRUE`, any confirmed worsening, or improvement, or change in the outcome measure is valid,
#'  regardless of `delta_fun`.
#' @return A boolean value specifying if a valid event was found.
#' @export is_event
#' @examples
#' is_event(x=4.5, baseline=4, type="wors", outcome="edss")
#' is_event(x=50, baseline=57, type="wors", outcome="sdmt")
#' is_event(x=3, baseline=3.5, type="impr", outcome="edss", sub_threshold=TRUE)
is_event <- function(x, baseline, type, outcome="edss", worsening=NULL,
                     delta_fun=NULL, sub_threshold=FALSE) {
  outcome <- match.arg(
    tolower(outcome),
    c("edss", "nhpt", "t25fw", "sdmt", "custom")
  )
  if (outcome %in% c("edss", "nhpt", "t25fw")) {
    worsening <- "increase"
  } else if (outcome == "sdmt") {
    worsening <- "decrease"
  } else if (is.null(worsening)) {
    stop('If using `outcome="custom"`, please specify worsening direction.')
  } else if (is.null(delta_fun)) {
    stop('If using `outcome="custom"`, please specify a delta function (`delta_fun` argument).')
  }

  improvement <- if (worsening == "decrease") "increase" else "decrease"

  if (sub_threshold) {
    event_sign <- list(
      increase = x > baseline,
      decrease = x < baseline,
      change = x != baseline
    )
  } else {
    if (is.null(delta_fun)) {
      fun_tmp <- compute_delta
    } else {
      fun_tmp <- function(baseline, outcome) {
        tryCatch({
          delta_fun(baseline, outcome)
        }, error = function(e) {
          delta_fun(baseline)
        })
      }
    }

    event_sign <- list(
      increase = x - baseline >= fun_tmp(baseline, outcome),
      decrease = x - baseline <= - fun_tmp(baseline, outcome),
      change = abs(x - baseline) >= fun_tmp(baseline, outcome)
    )
  }

  event <- list(
    wors = event_sign[[worsening]],
    impr = event_sign[[improvement]],
    change = event_sign[["change"]]
  )

  return(event[[type]])
}

