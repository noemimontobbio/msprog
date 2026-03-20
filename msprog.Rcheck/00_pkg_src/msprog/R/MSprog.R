
#' Assess multiple sclerosis disability course from longitudinal data.
#'
#' Detect and characterise confirmed disability worsening (CDW) or improvement
#' (CDI) events of an outcome measure (EDSS, NHPT, T25FW, or SDMT; or any custom outcome)
#' based on repeated assessments through time (and on the dates of acute episodes, if any).
#' The events are detected sequentially by scanning the outcome values in chronological order.
#' Several qualitative and quantitative options are given as arguments that can be set
#' by the user and reported as a complement to the results to ensure reproducibility.
#'
#' @param data Data frame containing longitudinal data, including: subject ID, outcome value, date of visit.
#' @param subj_col Name of data column with subject ID.
#' @param value_col Name of data column with outcome value.
#' @param date_col Name of data column with date of visit.
#' @param outcome Specifies the outcome type. Must be one of the following:
#' \itemize{
#'  \item `"edss"` (Expanded Disability Status Scale)
#'  \item `"nhpt"` (Nine-Hole Peg Test)
#'  \item `"t25fw"` (Timed 25-Foot Walk)
#'  \item `"sdmt"` (Symbol Digit Modalities Test)
#'  \item `"custom"` (only accepted when specifying custom `delta_fun` and `worsening`).
#'  }
#'  Outcome type triggers internal checks on value range,
#'  determines the direction of worsening (see `worsening` argument),
#'  and selects the default definition of clinically meaningful change given
#'  the reference value (using the built-in function [compute_delta()]).
#'  The latter can be replaced by a custom function using the `delta_fun` argument.
#' @param relapse Optional data frame containing longitudinal data, including: subject ID and relapse date.
#' @param rsubj_col Name of subject ID column for relapse data, if different from outcome data.
#' @param rdate_col Name of onset date column for relapse data, if different from outcome data.
#' @param renddate_col Name of end date column for relapse data (if present).
#' @param subjects Subset of subjects (list of IDs). If none is specified, all subjects listed in `data` are included.
#' @param delta_fun Custom function specifying the minimum clinically meaningful
#' change in the outcome measure from the provided reference value.
#' The function provided must take a numeric value (reference score) as input,
#' and return a numeric value corresponding to the minimum shift from baseline, see example below.
#' If `outcome` is set to `"custom"`, a custom delta function must be specified by the user.
#' For other values of `outcome`, if no `delta_fun` is specified,  the built-in function [compute_delta()] is used internally.
#' @param worsening The direction of worsening (`"increase"` if higher values correspond to worse disease course, `"decrease"` otherwise).
#'
#' The given value is only used when `outcome` is set to `"custom"`. Otherwise, `worsening` is automatically set to
#' `"increase"` if `outcome` is set to `"edss"`, `"nhpt"`, `"t25fw"`,
#'  and to `"decrease"` if `outcome` is set to `"sdmt"`.
#' @param event Character string specifying which events to detect. Must be one of the following.
#' \itemize{
#' \item `"firstCDW"` (first CDW, default).
#' \item `"firstCDI"` (first CDI).
#' \item `"multiple"` (all confirmed events in chronological order).
#' \item `"firstPIRA"` (first PIRA).
#' \item `"firstRAW"` (first RAW).
#' \item `"first"` (only the very first confirmed event -- CDI or CDW).
#' }
#' @param baseline Specifies the baseline scheme. Must be one of the following.
#' \itemize{
#' \item `"fixed"`: first valid outcome value, default.
#' \item `"roving"`: updated after each CDI or CDW event to the visit determined by `proceed_from`;
#' suitable for a multiple-event setting (i.e., when `event` is set to `"multiple"`)
#' or when searching for a specific type of CDW
#' (i.e., when `event` is set to `"firstPIRA"` or `"firstRAW"`) -- not recommended for randomised data.
#' \item `"roving_impr"`: updated after every CDI (to the visit determined by `proceed_from`);
#' suitable for a first-CDW setting to discard fluctuations around baseline -- not recommended for multiple events, or for randomised data.
#' \item `"roving_wors"`: updated after every CDW (to the visit determined by `proceed_from`);
#' suitable when searching for a specific type of CDW (i.e., when `event` is set to `"firstPIRA"` or `"firstRAW"`).
#' }
#' @param proceed_from After detecting a confirmed disability event, continue searching:
#' \itemize{
#' \item from the next visit after the first qualifying confirmation visit if `proceed_from="firstconf"`
#' \item from the next visit after the event onset if `proceed_from="event"`.
#' }
#' If `baseline` is set to `"roving"`, `"roving_impr"`, or `"roving_wors"`,
#' when rebaselining after a confirmed disability event, the baseline is moved to:
#' \itemize{
#' \item the first qualifying confirmation visit if `proceed_from="firstconf"`
#' \item the event visit if `proceed_from="event"`.
#' }
#' @param sub_threshold_rebl This argument is only used if `baseline` is not set to `"fixed"`.
#' It controls whether and which "sub-threshold" events (i.e., \emph{confirmed}
#' changes in the outcome measure below the clinically meaningful threshold) can
#' trigger a re-baseline.
#' Must be one of the following.
#' \itemize{
#' \item `"none"` (default): only use *clinically meaningful* confirmed changes for re-baseline.
#' \item `"event"`: any confirmed sub-threshold event can potentially trigger a re-baseline.
#' \item `"improvement"`: any confirmed sub-threshold improvement can potentially trigger a re-baseline.
#' \item `"worsening"`: any confirmed sub-threshold worsening can potentially trigger a re-baseline.
#' }
#' See `delta_fun` argument and [compute_delta()] function for more details.
#' @param bl_geq This argument is only used if the baseline is moved.
#' If `TRUE`, the new reference value must always be greater or equal than the previous one;
#' when it is not, the old reference value is assigned to it \[2\].
#' @param relapse_rebl If `TRUE`, re-baseline after every relapse \[2\].
#' @param skip_local_extrema This argument is only used if the baseline is moved.
#' It controls function behaviour in the presence of local minima or maxima.
#'
#' A visit `i` is a local minimum point for outcome `x` if
#'
#' `x[i+1]>x[i]` and `x[i-1]>x[i]`.
#'
#' Local maxima are defined similarly.
#'
#' A visit `i` is a \emph{strict} local minimum point for outcome `x` if
#'
#' `x[i+1]-x[i]>=delta_fun(x[i])`;
#'
#' `x[i-1]-x[i]>=delta_fun(x[i])`.
#'
#' Strict local maxima are defined similarly.
#'
#' When `x[i]=x[i-2]`, visit `i` is \emph{not} considered a local
#' extremum point even if the above conditions hold.
#' This controls for cases where the outcome has an undulating course.
#' The following argument values are accepted.
#' \itemize{
#' \item `"none"` (default): local extrema are always accepted as valid baseline values.
#' \item `"strict"`: the baseline cannot be placed at a \emph{strict} local minimum or maximum.
#' \item `"all"`: the baseline cannot be placed at a local minimum or maximum.
#' }
#' @param validconf_col Name of data column specifying which visits can (`TRUE`) or cannot (`FALSE`) be used as confirmation visits.
#' The input data does not necessarily have to include such a column.
#' If `validconf_col=NULL`, all visits are potentially used as confirmation visits.
#' @param conf_days Period before confirmation (days). Can be a single value, or
#' list-like of any length if considering multiple windows.
#' If `length(conf_days) > 1` (e.g., `conf_days=c(12*7, 24*7)`),
#' the function detects events confirmed at \emph{either} time point (e.g.,
#' "confirmed at 12 \emph{or} 24 weeks") with the relative tolerance (as per `conf_tol_days`).
#' @param conf_tol_days Tolerance window for confirmation visit (days).
#' Can be an integer (same lower and upper tolerance)
#' or list-like of length 2 (different lower and upper tolerance).
#' The right end of the interval (upper tolerance) can be set to `Inf`
#' (confirmation window unbounded on the right
#' -- e.g., "confirmed over 12 *or more* weeks").
#' @param require_sust_days Minimum number of days over which a confirmed change must be sustained
#' (i.e., confirmed at \emph{all} visits occurring in the specified period) to be retained as an event.
#' Events sustained for the remainder of the follow-up period are always retained regardless of follow-up duration.
#' If `require_sust_days=Inf`, events are retained only when sustained for the remainder of the follow-up period.
#'
#' (Warning: if `check_intermediate` is set to `FALSE`, sustained change will be established based \emph{only on the end} of the specified period.)
#' @param check_intermediate If `TRUE` (default), events are confirmed \emph{over all intermediate visits}
#' up to the confirmation visit.
#'
#' If set to `FALSE` (not recommended in most cases, as it may discard meaningful fluctuations),
#' events will be confirmed \emph{only at} the specified confirmation visit
#' (and \emph{only at the end} of the period defined by `require_sust_days`, if any).
#' @param relapse_to_bl Minimum distance (days) from the onset of a relapse for a visit to be used as baseline.
#' Can be an integer (minimum distance from \emph{last} relapse onset) or list-like of length 2
#' (minimum distance from \emph{last} relapse onset, minimum distance from \emph{next} relapse onset).
#' Note that setting the distance to zero means keeping the baseline where it is regardless of surrounding relapses.
#'
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse onset
#' is automatically set to the specific relapse duration, unless `relapse_to_bl` (or `relapse_to_bl[1]`) is zero
#' (in which case relapse timing does not affect baseline placement).
#'
#' If the designated baseline does not respect this constraint, the baseline is moved to the next available visit.
#' @param relapse_to_event Minimum distance (days) from the onset of a relapse for an event to be considered as such.
#' Can be an integer (minimum distance from \emph{last} relapse onset) or list-like of length 2
#' (minimum distance from \emph{last} relapse onset, minimum distance from \emph{next} relapse onset).
#' Note that setting the distance to zero means retaining the event regardless of surrounding relapses.
#'
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse onset
#' is automatically set to the specific relapse duration, unless `relapse_to_event` (or `relapse_to_event[1]`) is zero
#' (in which case relapse timing does not affect event validation).
#' @param relapse_to_conf Minimum distance (days) from the onset of a relapse for a visit to be a valid confirmation visit.
#' Can be an integer (minimum distance from \emph{last} relapse onset) or list-like of length 2
#' (minimum distance from \emph{last} relapse onset, minimum distance from \emph{next} relapse onset).
#' Note that setting the distance to zero means using any visit for confirmation regardless of surrounding relapses.
#'
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse onset
#' is automatically set to the specific relapse duration, unless `relapse_to_conf` (or `relapse_to_conf[1]`) is zero
#' (in which case relapse timing does not affect selection of confirmation visits).
#' @param relapse_assoc Maximum distance (days) from the onset of a relapse for a CDW event to be classified as RAW.
#' Can be an integer (maximum distance from \emph{last} relapse onset) or list-like of length 2
#' (maximum distance from \emph{last} relapse onset, maximum distance from \emph{next} relapse onset).
#' If relapse end dates are available (`renddate_col`), the maximum distance from last relapse
#' is automatically set to the specific relapse duration.
#' @param relapse_indep Specifies relapse-free intervals for PIRA definition.
#' Must be a named list
#' \cr`list(prec=list(p0, p1), event=list(e0, e1), conf=list(c0, c1))`\cr
#' specifying the intervals around (any subset of) three checkpoints:
#' \enumerate{
#' \item a preceding visit, e.g., baseline or last visit before the worsening (`p0` and `p1`)
#' \item the event onset (`e0` and `e1`)
#' \item the first available confirmation visit (`c0` and `c1`).
#' }
#' The auxiliary function `relapse_indep_from_bounds()` can be used to organise
#' interval bounds into a named list correctly, by calling:
#' \cr`relapse_indep_from_bounds(p0, p1, e0, e1, c0, c1)`\cr
#' See [relapse_indep_from_bounds()] function docs for more details on how to define the intervals.
#' If relapse end dates are available (`renddate_col`), it is possible to also define PIRA based on those
#' by setting `use_end_dates=TRUE` in [relapse_indep_from_bounds()].
#' @param impute_last_visit Imputation probability for worsening events occurring
#' at the last available visit (i.e., with no confirmation).
#' Unconfirmed worsening events occurring at the last visit are never imputed if `impute_last_visit=0`;
#' they are always imputed if `impute_last_visit=1`;
#' they are imputed with probability `p`, `0<p<1`, if `impute_last_visit=p`.
#' If a value `N>1` is passed, unconfirmed worsening events are imputed only if occurring within `N` days of follow-up
#' (e.g., in case of early discontinuation).
#' @param date_format Format of dates in the `date_col` and `rdate_col` columns of the input data.
#' Can be specified as:
#' \itemize{
#' \item Standard format for dates (e.g., \code{"\%d-\%m-\%Y"}; see [strptime()] docs for correct syntax).
#' \item `'day'` if dates in are given as "days from start" (the starting point can be different for each subject
#' -- e.g., days from randomisation in a clinical trial); negative values are accepted.
#' }
#' If not specified, function [as.Date()] will try to infer it automatically.
#' @param include_dates If `TRUE`, `output$results` will include the dates of:
#' \itemize{
#' \item event onset (`'date'` column)
#' \item the current baseline (`'bl_date'` column)
#' \item the last visit before event onset at a clinically meaningful score distance from it (`'last_delta_date'` column)
#' \item the confirmation visit(s) (`'conf<c>_date'` and `'PIRA_conf<c>_date'` columns for each `c` in `conf_days`)
#' }
#' @param include_values If `TRUE`,  `output$results` will include the outcome value at:
#' \itemize{
#' \item event onset (`'value'` column)
#' \item the current baseline (`'bl_value'` column)
#' \item the last visit before event onset at a clinically meaningful score distance from it (`'last_delta_value'` column)
#' \item the confirmation visit(s) (`'conf<c>_value'` and `'PIRA_conf<c>_value'` columns for each `c` in `conf_days`)
#' }
#' @param include_stable If `TRUE`, subjects with no confirmed events are included in `output$results`,
#' with `time2event` = total follow up.
#' @param verbose One of:
#' \itemize{
#'  \item{0 (print no info);}
#'  \item{1 (print concise info, default);}
#'  \item{2 (print extended info).}
#'  }
#'
#' @references
#' \[1\] M\"uller J, Cagol A, Lorscheider J, Tsagkas C, Benkert P, Yaldizli \"O, et al.
#' Harmonizing definitions for progression independent of relapse activity in multiple sclerosis: A systematic review.
#' JAMA Neurol. 2023;80:1232--45. \cr\cr
#' \[2\] Kappos L, Wolinsky JS, Giovannoni G, Arnold DL, Wang Q, Bernasconi C, et al.
#' Contribution of relapse-independent progression vs relapse-associated worsening to overall confirmed disability
#' accumulation in typical relapsing multiple sclerosis in a pooled analysis of 2 randomized clinical trials.
#' JAMA Neurol. 2020;77:1132--40.
#'
#'
#' @return An object of class `MSprogOutput` with the following attributes:
#' \itemize{
#' \item{`event_count`: a data frame containing the event sequence detected for each subject, and the counts for each event type.}
#' \item{`results`: a data frame with extended info on each event for all subjects.}
#' \item{`settings`: a list containing all the arguments used to compute the output.}
#' \item{`unconfirmed`: a data frame with info on unconfirmed events (initial change from baseline, but no confirmation)
#' for all subjects.}
#' }
#' For a detailed description of output data frames, see `?MSprogOutput`.
#'
#' @importFrom stats na.omit setNames complete.cases rbinom
#' @importFrom dplyr %>% group_by slice n mutate across ungroup
#' @importFrom rlang .data
#' @export
#' @examples
#' # 1. EDSS course
#' output <- MSprog(toydata_visits, subj_col='id', value_col='EDSS', date_col='date', outcome='edss',
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output$results) # extended info on each event for all subjects
#' print(output$event_count) # summary of event sequence for each subject
#' # 2. SDMT course
#' output <- MSprog(toydata_visits, subj_col='id', value_col='SDMT', date_col='date', outcome='sdmt',
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output$results) # extended info on each event for all subjects
#' print(output$event_count) # summary of event sequence for each subject
#' # 3. SDMT course, with a custom delta function
#' my_sdmt_delta <- function(reference_value) {min(c(reference_value/10, 3))}
#' output <- MSprog(toydata_visits, subj_col='id', value_col='SDMT', date_col='date', outcome='sdmt',
#'     delta_fun=my_sdmt_delta,
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output$results) # extended info on each event for all subjects
#' print(output$event_count) # summary of event sequence for each subject
MSprog <- function(data, subj_col, value_col, date_col, outcome,
                   relapse=NULL, rsubj_col=NULL, rdate_col=NULL, renddate_col=NULL,
                   subjects=NULL, delta_fun=NULL, worsening=NULL,
                   event=c('firstCDW', 'firstCDI', 'multiple', 'firstPIRA', 'firstRAW', 'first'),
                   baseline=c('fixed', 'roving', 'roving_impr', 'roving_wors'),
                   proceed_from=c('firstconf', 'event'),
                   sub_threshold_rebl=c('none', 'event', 'improvement', 'worsening'),
                   bl_geq=FALSE, relapse_rebl=FALSE, skip_local_extrema=c('none', 'strict', 'all'),
                   validconf_col=NULL, conf_days=12*7, conf_tol_days=c(7,2*365.25),
                   require_sust_days=0, check_intermediate=T,
                   relapse_to_bl=30, relapse_to_event=0, relapse_to_conf=30,
                   relapse_assoc=90, relapse_indep=NULL,
                   impute_last_visit=0, date_format=NULL,
                   include_dates=FALSE, include_values=FALSE, include_stable=TRUE,
                   verbose=1
                   ) {

  # SETUP

  warnings <- list()

  ###########################
  # CHECKS ON ARGUMENT VALUES

  # If conf_tol_days is a single value, duplicate it (equal left and right tolerance)
  if (length(conf_tol_days)==1) {
    conf_tol_days <- c(conf_tol_days, conf_tol_days)
  }

  # If relapse_to_bl is a single value, set right bound to zero
  if (length(relapse_to_bl)==1) {
    relapse_to_bl <- c(relapse_to_bl, 0)
  }
  # If relapse_to_event is a single value, set right bound to zero
  if (length(relapse_to_event)==1) {
    relapse_to_event <- c(relapse_to_event, 0)
  }
  # If relapse_to_conf is a single value, set right bound to zero
  if (length(relapse_to_conf)==1) {
    relapse_to_conf <- c(relapse_to_conf, 0)
  }
  # If relapse_assoc is a single value, set right bound to zero
  if (length(relapse_assoc)==1) {
    relapse_assoc <- c(relapse_assoc, 0)
  }

  outcome <- match.arg(
    tolower(outcome),
    c("edss", "nhpt", "t25fw", "sdmt", "custom")
  )

  event <- match.arg(event)
  baseline <- match.arg(baseline)
  proceed_from <- match.arg(proceed_from)
  sub_threshold_rebl <- match.arg(sub_threshold_rebl)
  skip_local_extrema <- match.arg(skip_local_extrema)

  # end of checks
  ###########################

  if (!is.null(renddate_col)) {
    relapse_to_bl[1] <- if (relapse_to_bl[1] == 0) 0 else Inf
    relapse_to_event[1] <- if (relapse_to_event[1] == 0) 0 else Inf
    relapse_to_conf[1] <- if (relapse_to_conf[1] == 0) 0 else Inf
    relapse_assoc[1] <- Inf
  }

  # If no column names are specified for the relapse file, use the main ones
  if (is.null(rsubj_col)) {
    rsubj_col <- subj_col
  }
  if (is.null(rdate_col)) {
    rdate_col <- date_col
  }

  # If no `validconf_col` is specified, create a dummy one
  if (is.null(validconf_col) || !validconf_col %in% names(data)) {
    validconf_col <- 'validconf'
    data$validconf <- TRUE
  } else {
    data[[validconf_col]] <- as.logical(data[[validconf_col]])
  }

  # Create empty relapse file if none is provided
  if (is.null(relapse)) {
    relapse <- data.frame(matrix(nrow=0, ncol=2))
    names(relapse) <- c(rsubj_col, rdate_col)
    renddate_col <- NULL
    relapse_rebl <- FALSE
  }

  # Convert outcome values to numeric
  data[[value_col]] <- as.numeric(data[[value_col]])

  # Remove missing values from columns of interest
  data <- data[complete.cases(data[ , c(subj_col, value_col, date_col, validconf_col)]), ]
  if (is.null(renddate_col)) {
    relapse <- relapse[complete.cases(relapse[, c(rsubj_col, rdate_col)]), ]
  } else {
  relapse <- relapse[complete.cases(relapse[, c(rsubj_col, rdate_col, renddate_col)]), ]
  }

  # Convert dates to Date format
  if (is.null(date_format)) {
    tryCatch({
    data[[date_col]] <- as.Date(data[[date_col]])
    relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]])
    if (!is.null(renddate_col)) {
      relapse[[renddate_col]] <- as.Date(relapse[[renddate_col]])
    }
    }, error=function(e) {
      stop("Failed to infer format for date columns; please provide correct format via `date_format` argument.")
      NULL
    }
    )
  } else if (date_format == 'day') {
    tryCatch({
    data[[date_col]] <- as.numeric(data[[date_col]])
    relapse[[rdate_col]] <- as.numeric(relapse[[rdate_col]])
    if (!is.null(renddate_col)) {
      relapse[[renddate_col]] <- as.numeric(relapse[[renddate_col]])
      if (nrow(relapse) > 0 & all(is.na(relapse[[rdate_col]]))) {
        stop("Failed to intepret relapse end-date column as numeric (number of days, as per `date_format='day'`)")
      }
    }
    }, error=function(e) {
      stop("Failed to intepret date columns as numeric (number of days, as per `date_format='day'`)")
      NULL
    }
    )
    # If conversion to numeric fails silently (all NAs):
    if (all(is.na(data[[date_col]])) | (nrow(relapse) > 0 & all(is.na(relapse[[rdate_col]])))) {
      stop("Failed to intepret date columns as numeric (number of days, as per `date_format='day'`)")
    }
  } else {
  tryCatch({
  data[[date_col]] <- as.Date(data[[date_col]], format=date_format)
  relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]], format=date_format)
  if (!is.null(renddate_col)) {
    relapse[[renddate_col]] <- as.Date(relapse[[renddate_col]], format=date_format)
  }
  }, error=function(e) {
    stop('Failed to intepret date columns as "', date_format, '" as specified in `date_format` argument.')
    NULL
  }
  )
  }

  # Local function to display dates/days
  display_date <- function(day, start) {
    if (is.na(day) | is.null(day)) {
      return("")
    }
    if (!is.null(date_format) && date_format == 'day') {
      paste("day", day)
    } else {
      as.character(start + day)
    }
  }

  # Local function to re-convert numeric to date (vectorised)
  num_to_date <- function(day, start) {
    if (!is.null(date_format) && date_format == "day") {
      return(day)
    }
    out <- as.Date(rep(NA_real_, length(day)))
    ok <- !is.na(day)
    out[ok] <- start + day[ok]
    out
  }

  # Restrict to subset of subjects
  if (!is.null(subjects)) {
  data <- data[data[[subj_col]] %in% subjects,]
  relapse <- relapse[relapse[[rsubj_col]] %in% subjects,]
  }

  if (nrow(data)==0) {
    stop('Empty data. Did you pass an empty/invalid `subjects` argument?')
  }

  # Convert dates to days from global minimum
  if (is.null(date_format) || date_format != 'day') {
  if (nrow(relapse)>0) {
    global_start <- min(min(data[[date_col]]), min(relapse[[rdate_col]]))
  } else {global_start <- min(data[[date_col]])}
  data[[date_col]] <- as.numeric(difftime(data[[date_col]], global_start), units='days')
  relapse[[rdate_col]] <- as.numeric(difftime(relapse[[rdate_col]], global_start), units='days')
  if (!is.null(renddate_col)) {
    relapse[[renddate_col]] <- as.numeric(difftime(relapse[[renddate_col]], global_start), units='days')
  }
  } else {
    global_start <- NULL
  }

  # Check if values are in correct range
  if (outcome!='custom') {
    if (any(data[[value_col]]<0)) {
      stop('negative ', outcome,' scores')
    }
    else if (outcome=='edss' && any(data[[value_col]]>10)) {
      stop('EDSS scores >10')
    }
    else if (outcome=='sdmt' && any(data[[value_col]]>110)) {
      stop('SDMT scores >110')
    }
    else if (outcome=='nhpt' && any(data[[value_col]]>300)) {
      warnings <- c(warnings, 'NHPT scores >300')
    }
    else if (outcome=='t25fw' && any(data[[value_col]]>180)) {
      warnings <- c(warnings, 'T25FW scores >180')
    }
  }

  # Set direction of worsening
  if (outcome %in% c('edss', 'nhpt', 't25fw')) {
    worsening <- 'increase'
  } else if (outcome=='sdmt') {
    worsening <- 'decrease'
  } else if (is.null(worsening)) {
    stop('If using `outcome="custom"`, please specify the direction of worsening (`worsening` argument, \"increase\" or \"decrease\").')
  } else if (is.null(delta_fun)) {
    stop('If using `outcome="custom"`, please provide a custom delta function (`delta_fun` argument).')
  } else {
    worsening <- match.arg(worsening, c('increase', 'decrease'))
  }

  if (impute_last_visit < 0) {
    stop('`impute_last_visit` must be nonnegative')
  } else if (impute_last_visit<=1) {
    # If impute_last_visit is a probability, set no limit to follow-up length (Inf)
    impute_max_fu <- Inf
  } else {
    # If impute_last_visit is a follow-up time, save the value and set probability to 1
    impute_max_fu <- impute_last_visit
    impute_last_visit <- 1
  }

  # Define local is_event() function
  isevent_loc <- function(x, bl, type='wors', st=FALSE) {
    is_event(x, bl, type=type, outcome=outcome, worsening=worsening,
             sub_threshold=st, delta_fun=delta_fun)
  }

  # Define function detecting last_delta
  find_last_delta_idx <- function(data_id, change_idx, conf_idx) {
    valid_ref <- FALSE
    iref <- change_idx
    while (iref > 1 && !valid_ref) {
      iref <- iref - 1
      #
      if (skip_local_extrema != "none") {
        prec <- if (iref == 1) data_id[[value_col]][iref] else data_id[[value_col]][iref - 1]
        prec2 <- if (iref <= 2) -1 else data_id[[value_col]][iref - 2]
        subs <- if (iref == nvisits) data_id[[value_col]][iref] else data_id[[value_col]][iref + 1]
        vis <- data_id[[value_col]][iref]
        local_extr <- ((isevent_loc(prec, bl=vis, type='wors', st=skip_local_extrema=='all')
                        && isevent_loc(subs, bl=vis, type='wors', st=skip_local_extrema=='all')) || (
                          isevent_loc(prec, bl=vis, type='impr', st=skip_local_extrema=='all')
                          && isevent_loc(subs, bl=vis, type='impr', st=skip_local_extrema=='all'))
        ) && (vis!=prec2)
      } else {
        local_extr <- FALSE
      }
      event_ok <- isevent_loc(data_id[[value_col]][change_idx],
                              bl=data_id[[value_col]][iref], type='wors')
      if (is.null(conf_idx)) {
        conf_ok <- TRUE
      } else {
        conf_ok <- if (check_intermediate) all(sapply((change_idx + 1):conf_idx[[1]],
                          function(x) isevent_loc(data_id[[value_col]][x], bl=data_id[[value_col]][iref], type='wors'))  # worsening is confirmed at (all visits up to) first valid date
                      ) else isevent_loc(data_id[[value_col]][conf_idx[[1]]], bl=data_id[[value_col]][iref], type='wors') # worsening is confirmed at first valid date
      }
      valid_ref <- event_ok && conf_ok && !local_extr
    }
    if (valid_ref) iref else 1
  }

  # Define function creating empty event template #_ev_
  make_empty_event <- function() {
    ev <- list(
      event_type = "",
      event_index = NA_integer_,
      date = if (!is.null(date_format) && date_format == "day") NaN else as.Date(NA),
      value = NaN,
      bl_date = if (!is.null(date_format) && date_format == "day") NaN else as.Date(NA),
      bl_value = NaN,
      last_delta_date = if (!is.null(date_format) && date_format == "day") NaN else as.Date(NA),
      last_delta_value = NaN,
      bl2event = NaN,
      time2event = NaN,
      sust_days = 0,
      sust_last = FALSE #0L
    )
    for (cd in conf_days) {
      ev[[paste0("conf", cd, "_date")]] <- if (!is.null(date_format) && date_format == "day") NaN else as.Date(NA)
      ev[[paste0("conf", cd, "_value")]] <- NaN
      ev[[paste0("PIRA_conf", cd, "_date")]] <- if (!is.null(date_format) && date_format == "day") NaN else as.Date(NA)
      ev[[paste0("PIRA_conf", cd, "_value")]] <- NaN
    }
    ev
  }

  # Define a confirmation window for each value of conf_days
  conf_window <- lapply(conf_days, function(t) {
    lower <- as.integer(t) - conf_tol_days[[1]]
      upper <- as.integer(t) + conf_tol_days[[2]]
    return(c(lower, upper))
  })

  # Define relapse-free intervals for PIRA definition
  if (is.null(relapse_indep)) {
    relapse_indep <- relapse_indep_from_bounds(p0=0, p1=0, e0=90, e1=30, c0=90, c1=30)
  }

  #################################################################
  # Assess disability course

  # Make subject ID a character for safer indexing
  data[[subj_col]] <- as.character(data[[subj_col]])
  relapse[[rsubj_col]] <- as.character(relapse[[rsubj_col]])

  all_subj <- unique(data[[subj_col]])
  nsub <- length(all_subj)

  # Initialise results data.frame
  #####_ev_
  subject_results <- vector("list", nsub)
  names(subject_results) <- all_subj
  #####_ev_
  # max_nevents <- round(max(table(data[[subj_col]]))/2)
  # allcol <- c(subj_col, 'nevent', 'event_type', 'CDW_type', 'total_fu', 'time2event', 'bl2event',
  #             'date', 'value', 'bl_date', 'bl_value', 'last_delta_date', 'last_delta_value',
  #             # paste0('conf', conf_days), paste0('PIRA_conf', conf_days),
  #             paste0('conf', conf_days, '_date'), paste0('conf', conf_days, '_value'),
  #             paste0('PIRA_conf', conf_days, '_date'), paste0('PIRA_conf', conf_days, '_value'),
  #             'sust_days', 'sust_last')
  # results_df <- data.frame(matrix(nrow=nsub*max_nevents, ncol=length(allcol)))
  # colnames(results_df) <- allcol
  # # Initialize columns with the correct types/values
  # results_df[[subj_col]] <- rep(all_subj, each=max_nevents)
  # results_df$nevent <- rep(1:max_nevents, times=nsub)
  # results_df$event_type <- ''  # character
  # results_df$CDW_type <- ''  # character
  # if (!is.null(date_format) && date_format == 'day') {
  # results_df$date <- NaN # numeric
  # results_df$bl_date <- NaN # numeric
  # results_df$last_delta_date <- NaN # numeric
  # for (cd in conf_days) {
  #   results_df[[paste0('conf', cd, '_date')]] <- NaN # numeric
  #   results_df[[paste0('PIRA_conf', cd, '_date')]] <- NaN # numeric
  # }
  # } else {
  #   results_df$date <- as.Date(NA)  # Date
  #   results_df$bl_date <- as.Date(NA)  # Date
  #   results_df$last_delta_date <- as.Date(NA)  # Date
  #   for (cd in conf_days) {
  #     results_df[[paste0('conf', cd, '_date')]] <- as.Date(NA)  # Date
  #     results_df[[paste0('PIRA_conf', cd, '_date')]] <- as.Date(NA)  # Date
  #   }
  # }
  # results_df$bl_value <- NaN # numeric
  # results_df$value <- NaN  # numeric
  # results_df$last_delta_value <- NaN  # numeric
  # results_df$total_fu <- 0  # numeric
  # results_df$time2event <- NaN  # numeric
  # results_df$bl2event <- NaN  # numeric
  # for (cd in conf_days) {
  #   # results_df[[paste0('conf', cd)]] <- 0
  #   # results_df[[paste0('PIRA_conf', cd)]] <- 0
  #   results_df[[paste0('conf', cd, '_value')]] <- NaN # numeric
  #   results_df[[paste0('PIRA_conf', cd, '_value')]] <- NaN # numeric
  # }
  # results_df$sust_days <- 0
  # results_df$sust_last <- 0
  #####_ev_

  summary <- data.frame(matrix(nrow=nsub, ncol=5))
  colnames(summary) <- c('event_sequence', 'CDI', 'CDW', 'RAW', 'PIRA')
  rownames(summary) <- all_subj
  summary$event_sequence <- ""
  summary$CDI <- 0L
  summary$CDW <- 0L
  summary$RAW <- 0L
  summary$PIRA <- 0L

  unconf <- list()
  unconf_idx <- 1

  total_fu <- setNames(rep(0, nsub), all_subj)

  for (subjid in all_subj) {

    data_id <- data[data[[subj_col]] == subjid, ]

    # If more than one visit occur on the same day, only keep last
    ucounts <- table(data_id[, date_col])
    if (any(ucounts > 1)) {
      data_id <- data_id %>%
        group_by(.data[[date_col]]) %>%
        slice(n()) %>%
        ungroup()
    }

    # Sort visits in chronological order
    order_tmp <- order(data_id[[date_col]])
    if (any(order_tmp != seq_len(nrow(data_id)))) {
      data_id <- data_id[order_tmp, ]
    }

    nvisits <- nrow(data_id)
    first_visit <- min(data_id[[date_col]])
    relapse_id <- relapse[relapse[[rsubj_col]] == subjid, ]
    relapse_id <- relapse_id[relapse_id[[rdate_col]] >= first_visit - relapse_to_bl[1], ]  # ignore relapses occurring before first visit
    relapse_dates <- relapse_id[[rdate_col]] # relapse onset_dates
    nrel <- length(relapse_dates)

    total_fu[subjid] <- data_id[[date_col]][nvisits] - data_id[[date_col]][1]


    # Print info
    if (verbose == 2) {
      message("\nSubject #", subjid, ": ", nvisits, " visit", (if (nvisits == 1) "" else "s"),
              ", ", nrel, " relapse", (if (nrel == 1) "" else "s"))
      if (any(ucounts > 1)) {
        message("Found multiple visits in the same day: only keeping last.")
      }
      if (any(order_tmp != seq_len(nrow(data_id)))) {
        message("Visits not listed in chronological order: sorting them.")
      }
    }

    # Compute distance from relapses
    if (length(relapse_dates) > 0) {
      # relapse_df <- data.frame(split(rep(relapse_dates, each=nrow(data_id)),
      #                                rep(1:length(relapse_dates), each=nrow(data_id))))
      # relapse_df$visit <- data_id[[date_col]]
      # dist <- (relapse_df %>% mutate(across(1:length(relapse_dates),
      #                           ~ as.numeric(.x - visit))))[1:length(relapse_dates)]
      dist <- outer(- data_id[[date_col]], relapse_dates, "+")
      distm <- - dist
      distp <- dist
      distm[distm < 0] <- Inf
      distp[distp < 0] <- Inf
      data_id$closest_rel_before <- apply(distm, 1, min)  #if (all(is.na(distm))) Inf else apply(distm, 1, min, na.rm=TRUE)
      if (!is.null(renddate_col)) {
        relapse_duration <- relapse_id[[renddate_col]] - relapse_id[[rdate_col]]
        which_rel <- apply(distm, 1, which.min)  #if (all(is.na(distm))) Inf else apply(distm, 1, which.min)
        data_id$closest_rel_duration <- relapse_duration[which_rel]
        data_id$outside_influence <- data_id$closest_rel_duration < data_id$closest_rel_before
        data_id[data_id$outside_influence, 'closest_rel_before'] <- Inf
          }
      data_id$closest_rel_after <- apply(distp, 1, min)  #if (all(is.na(distp))) Inf else apply(distp, 1, min, na.rm=TRUE)
    } else {
      data_id$closest_rel_before <- Inf
      data_id$closest_rel_after <- Inf
    }

    # Initialise results columns
    #####_ev_
    events <- list()
    event_type <- character(0)
    event_index <- integer(0)
    #####_ev_
    # event_type <- ""
    # event_index <- NULL
    # bl_date <- edate <- last_delta_date <- bl_value <- evalue <- last_delta_value <- time2event <- bl2event <- sustd <- sustl <- vector()
    # # conf <- pira_conf <- list()
    # conf_date <- conf_value <- pira_conf_date <- pira_conf_value <- list()
    # for (m in conf_days) {
    #   # conf[[as.character(m)]] <- vector()
    #   conf_date[[as.character(m)]] <- vector()
    #   conf_value[[as.character(m)]] <- vector()
    #   # pira_conf[[as.character(m)]] <- vector()
    #   pira_conf_date[[as.character(m)]] <- vector()
    #   pira_conf_value[[as.character(m)]] <- vector()
    # }
    #####_ev

    # Initialise variables
    bl_idx <- 1 # Baseline index
    search_idx <- 2 # Index of where we are in the search
    proceed <- 1 # "proceed with search" flag
    bl_last <- NULL # Previous baseline value
    irel <- 1 # Current relapse index
    if (nrel > 0) {
      for (r in 1:nrel) {
        if (relapse_dates[r] > data_id[[date_col]][bl_idx]) {
          irel <- r  #change_idx <- r
          break
        }
      }
    }

    if (verbose == 2) {
      message("Baseline at visit no.", bl_idx)
    }

    n_iter <- 0

    while (proceed) {

      n_iter <- n_iter + 1

      if (n_iter > 10000) {stop('Something got stuck: infinite loop')}

      # Set baseline (skip if local extremum or within relapse influence)
      if (skip_local_extrema!="none") {
        prec <- if (bl_idx == 1) data_id[[value_col]][bl_idx] else data_id[[value_col]][bl_idx - 1]
        prec2 <- if (bl_idx <= 2) -1 else data_id[[value_col]][bl_idx - 2]
        subs <- if (bl_idx == nvisits) data_id[[value_col]][bl_idx] else data_id[[value_col]][bl_idx + 1]
        vis <- data_id[[value_col]][bl_idx]
        local_extr <- ((isevent_loc(prec, bl=vis, type='wors', st=skip_local_extrema == 'all')
                       && isevent_loc(subs, bl=vis, type='wors', st=skip_local_extrema == 'all')) || (
                        isevent_loc(prec, bl=vis, type='impr', st=skip_local_extrema == 'all')
                        && isevent_loc(subs, bl=vis, type='impr', st=skip_local_extrema == 'all'))
                      ) && (vis!=prec2)
      } else {
        local_extr <- FALSE
      }
      while (proceed
             && (data_id[['closest_rel_before']][bl_idx] < relapse_to_bl[1]
             || data_id[['closest_rel_after']][bl_idx] < relapse_to_bl[2]
             || local_extr)) {
        if (verbose == 2) {
          message("Baseline (visit no.", bl_idx, ") is ",
                  if (local_extr) "a local extremum: " else "within relapse influence: ",
                  "moved to visit no.", bl_idx + 1)
        }
        bl_idx <- bl_idx + 1
        #
        if (bl_idx <= nvisits & skip_local_extrema != "none") {
          prec <- if (bl_idx == 1) data_id[[value_col]][bl_idx] else data_id[[value_col]][bl_idx - 1]
          prec2 <- if (bl_idx <= 2) -1 else data_id[[value_col]][bl_idx - 2]
          subs <- if (bl_idx==nvisits) data_id[[value_col]][bl_idx] else data_id[[value_col]][bl_idx + 1]
          vis <- data_id[[value_col]][bl_idx]
          local_extr <- ((isevent_loc(prec, bl=vis, type='wors', st=skip_local_extrema=='all')
                         && isevent_loc(subs, bl=vis, type='wors', st=skip_local_extrema=='all')) || (
                        isevent_loc(prec, bl=vis, type='impr', st=skip_local_extrema=='all')
                        && isevent_loc(subs, bl=vis, type='impr', st=skip_local_extrema=='all'))
                        ) && (vis!=prec2)
        } else {
          local_extr <- FALSE
        }
        #
        if (bl_idx > nvisits - 1) {
          proceed <- 0
          if (verbose == 2) {
            message("Not enough visits left: end process")
          }
        }
      }

      # If `relapse_rebl` is enabled, update relapse index to next relapse after baseline
      if (relapse_rebl && !is.na(irel) && irel <= nrel && bl_idx <= nvisits) {
        idx_tmp <- NA
        for (x in irel:nrel) {
          if (relapse_dates[x] > data_id[[date_col]][bl_idx]) {
            idx_tmp <- x
            break
          }
        }
        irel <- idx_tmp
      }

      # If baseline was moved after current search index, move search index:
      if (search_idx <= bl_idx) {search_idx <- bl_idx + 1}
      if (verbose == 2) {
        message("Searching for events from visit no.",
                if (search_idx > nvisits) "-" else search_idx, " on")
      }

      if (bl_idx > nvisits) {
        bl_idx <- nvisits
        proceed <- 0
        if (verbose == 2) {
          message("Not enough visits left: end process")
        }
      } else if (bl_geq && !is.null(bl_last) && bl_last > data_id[[value_col]][bl_idx]) {
        # Kappos2020 (by Sean Yiu)
        data_id[bl_idx, value_col] <- bl_last
      }

      bl <- data_id[bl_idx, ]
      bl_last <- bl[[value_col]]

      # Event detection
      change_idx <- NA
      if (search_idx <= nvisits) {
      for (x in search_idx:nvisits) {
          if (isevent_loc(data_id[[value_col]][x], bl=bl[[value_col]], type='change',
                          st=sub_threshold_rebl!='none') &
              data_id[['closest_rel_before']][x] >= relapse_to_event[1]
               && data_id[['closest_rel_after']][x] >= relapse_to_event[2]) {
            change_idx <- x
            break
          }
      }
      }

      if (is.na(change_idx) || change_idx > nvisits) {
        proceed <- 0
        if (verbose == 2) {
          message("No ", if (outcome != "custom") outcome else "outcome", " change in any subsequent visit: end process")
        }
      } else if (relapse_rebl && !is.na(irel) && irel <= nrel
                 && data_id[[date_col]][change_idx]
                    > relapse_dates[irel] + (if (event=='firstPIRA') 0 else relapse_assoc[1])) {
        # If `relapse_rebl`is enabled and the detected change from baseline has crossed the next relapse,
        # the baseline needs to be moved after that relapse (post-relapse re-baseline below).
        # (unless search_idx is after the relapse but within its influence as per `relapse_assoc[0]`: could be a RAW)
        search_idx <- change_idx
      } else {
        if (change_idx == nvisits) {
          conf_idx <- list()
          conf_t <- list() # for impute_last_visit
          } else {
        conf_idx <- lapply(conf_window, function(t) {
          match_idx <- numeric(0)
          for (x in (change_idx + 1):nvisits) {
            if (data_id[[date_col]][x] - data_id[[date_col]][change_idx] >= t[1]
                && data_id[[date_col]][x] - data_id[[date_col]][change_idx] <= t[2]  # date in confirmation range
                && data_id[['closest_rel_before']][x] >= relapse_to_conf[1]  # occurring out of influence of last relapse
                && data_id[['closest_rel_after']][x] >= relapse_to_conf[2]  # occurring out of influence of next relapse
                && data_id[[validconf_col]][x]  # can be used as confirmation
                ) {
              match_idx <- append(match_idx, x)
            }
          }
          match_idx
        })
        conf_t <- list()
        for (i in seq_along(conf_days)) {
          conf_t[[as.character(conf_days[i])]] <- conf_idx[[i]]
        }
        conf_idx <- unique(unlist(conf_idx))
        }
        if (verbose == 2) {
          message("Found ", if (outcome != 'custom') toupper(outcome) else 'outcome',
                  " change at visit no.", change_idx, " (",
                  display_date(data_id[[date_col]][change_idx], global_start),
                       "); potential confirmation visits available: no.", paste(conf_idx, collapse=", "))
        }


        # Confirmation
        # ============

        # CONFIRMED IMPROVEMENT:
        # --------------------
        if (isevent_loc(data_id[[value_col]][change_idx], bl=bl[[value_col]], type='impr') # value improved (>delta) from baseline
            && length(conf_idx) > 0  # confirmation visits available
            && ifelse(check_intermediate,
            all(sapply((change_idx + 1):conf_idx[[1]], function(x) isevent_loc(data_id[[value_col]][x], bl=bl[[value_col]],
                       type='impr'))),  # improvement is confirmed at (all visits up to) first valid date
            isevent_loc(data_id[[value_col]][conf_idx[[1]]], bl=bl[[value_col]], type='impr'))  # improvement is confirmed at first valid date
            && !((event %in% c('firstCDW', 'firstPIRA', 'firstRAW')) && baseline %in% c('fixed', 'roving_wors'))
            ) {

          # First visit at which improvement is not sustained:
          if (conf_idx[[1]]==nvisits) {
            next_nonsust <- NA
            } else {
              next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                  bl=bl[[value_col]], type='impr'))[1] + conf_idx[[1]]
          }
          # Discard potential confirmation visits if these occur out of sustained interval:
          if (!is.na(next_nonsust)) {
            conf_idx <- conf_idx[conf_idx < next_nonsust]
          }

          # The confirmed improvement can still be rejected if `require_sust_days>0`.
          # The `valid_ev` flag indicates whether the event can (1) or cannot (0) be retained:
          valid_ev <- 1
          if (require_sust_days>0) {
              if ((!check_intermediate)
                  && data_id[[date_col]][nvisits] - data_id[[date_col]][change_idx] >= require_sust_days # if follow-up lasts at least require_sust_days
                  ) {
                sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                      - data_id[[date_col]][change_idx] >= require_sust_days)[1] + change_idx  # first visit occurring at least `require_sust_days` from event
              } else {
                sust_vis <- nvisits  # if follow-up lasts before `require_sust_days`, check at last visit
                }
            valid_ev <- if (check_intermediate) {is.na(next_nonsust) || (data_id[[date_col]][next_nonsust]
                          - data_id[[date_col]][change_idx]) >= require_sust_days  # improvement sustained up to end of follow-up, or for `require_sust_days`
                } else isevent_loc(data_id[[value_col]][sust_vis], bl=bl[[value_col]], type='impr')  # improvement confirmed at `sust_vis` (last visit, or first visit after `require_sust_days`)
          }

          # If the event is retained (as per `require_sust_days`), we store the info:
          if (valid_ev) {

            # Find last visit at clinically meaningful score distance from event
            lastdelta_idx <- find_last_delta_idx(data_id, change_idx,
                                  if (change_idx == nvisits) NULL else conf_idx)
            lastdelta <- data_id[lastdelta_idx,]

            sust_idx <- if (is.na(next_nonsust)) nvisits else next_nonsust - 1
            #####_ev_
            ev <- make_empty_event()
            ev$event_type <- "CDI"
            ev$event_index <- change_idx
            ev$date <- data_id[[date_col]][change_idx]
            ev$value <- data_id[[value_col]][change_idx]
            ev$bl_date <- bl[[date_col]]
            ev$bl_value <- bl[[value_col]]
            ev$last_delta_date <- lastdelta[[date_col]]
            ev$last_delta_value <- lastdelta[[value_col]]
            ev$bl2event <- data_id[[date_col]][change_idx] - bl[[date_col]]
            ev$time2event <- data_id[[date_col]][change_idx] - data_id[[date_col]][1]
            for (cm in names(conf_t)) {
              confirmed_at <- intersect(conf_t[[cm]], conf_idx)
              if (length(confirmed_at)==0) {
                conf_t[[cm]] <- NULL #conf_t <- within(conf_t, rm(list=cm))
              } else {
                ev[[paste0("conf", cm, "_date")]] <- data_id[[date_col]][min(confirmed_at)]
                ev[[paste0("conf", cm, "_value")]] <- data_id[[value_col]][min(confirmed_at)]
              }
            }
            ev$sust_days <- data_id[[date_col]][sust_idx] - data_id[[date_col]][change_idx]
            ev$sust_last <- sust_idx == nvisits
            #####_ev_
            # event_type <- c(event_type, "CDI")
            # event_index <- c(event_index, change_idx)
            # bl_date <- c(bl_date, display_date(bl[[date_col]], global_start))
            # bl_value <- c(bl_value, bl[[value_col]])
            # edate <- c(edate, display_date(data_id[[date_col]][change_idx], global_start))
            # evalue <- c(evalue, data_id[[value_col]][change_idx])
            # last_delta_date <- c(last_delta_date, display_date(lastdelta[[date_col]], global_start))
            # last_delta_value <- c(last_delta_value, lastdelta[[value_col]])
            # bl2event <- c(bl2event, data_id[[date_col]][change_idx] - bl[[date_col]])
            # time2event <- c(time2event, data_id[[date_col]][change_idx] - data_id[[date_col]][1])
            # for (cm in names(conf_t)) {
            #   confirmed_at <- intersect(conf_t[[cm]], conf_idx)
            #   if (length(confirmed_at)==0) {
            #     conf_t <- within(conf_t, rm(list=cm))
            #     conf_date[[cm]] <- c(conf_date[[cm]], display_date(NA, global_start))
            #     conf_value[[cm]] <- c(conf_value[[cm]], NaN)
            #   } else {
            #     conf_date[[cm]] <- c(conf_date[[cm]], display_date(data_id[[date_col]][min(confirmed_at)], global_start))
            #     conf_value[[cm]] <- c(conf_value[[cm]], data_id[[value_col]][min(confirmed_at)])
            #   }
            #   # conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
            #   # pira_conf[[cm]] <- c(pira_conf[[cm]], 0)
            #   pira_conf_date[[cm]] <- c(pira_conf_date[[cm]], display_date(NA, global_start))
            #   pira_conf_value[[cm]] <- c(pira_conf_value[[cm]], NaN)
            #   }
            # sustd <- c(sustd, data_id[[date_col]][sust_idx] - data_id[[date_col]][change_idx])
            # sustl <- c(sustl, as.integer(sust_idx == nvisits))
            #####_ev_
            # Print progress info
            if (verbose == 2) {
              message("Found ", if (outcome != 'custom') paste0(toupper(outcome), "-") else "",
                      "CDI (visit no.", change_idx, ", ",
                      display_date(data_id[[date_col]][change_idx], global_start),
                           ") confirmed at ", paste(names(conf_t), collapse=" and "), " days, sustained up to visit no.", sust_idx,
                           " (", display_date(data_id[[date_col]][sust_idx], global_start), ")")
            }

            events[[length(events) + 1]] <- ev
            event_type <- c(event_type, ev$event_type)
            event_index <- c(event_index, ev$event_index)

          } else {
            # If the event is NOT retained (as per `require_sust_days`), we proceed.
            if (verbose == 2) {
              message("Change confirmed but not sustained over ",
                      if (require_sust_days < Inf) paste(">=", require_sust_days, "days") else "entire follow-up",
                      ": proceed with search")
            }
            unconf[[unconf_idx]] <- setNames(list(subjid,
                                                  data_id[[date_col]][change_idx],
                                                  data_id[[value_col]][change_idx],
                                                  bl[[date_col]],
                                                  bl[[value_col]],
                                                  data_id[['closest_rel_before']][change_idx],
                                                  data_id[['closest_rel_after']][change_idx]),
                                             c(subj_col, 'date', 'value', 'bl_date', 'bl_value',
                                               'closest_rel_before', 'closest_rel_after'))
            unconf_idx <- unconf_idx + 1
          }

          # For each m in `conf_days`, only keep the earliest available confirmation visit:
          conf_idx <- unname(sapply(names(conf_t), function(cm) {
            x <- intersect(conf_t[[cm]], conf_idx)
            if (length(x)) min(x)
          }))

          if (baseline %in% c('roving', 'roving_impr')) { #_r_#
            # In a roving baseline setting, the baseline is moved after the confirmed event (even if it is not sustained):
            newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx
            bl_idx <- newref}
          else if (valid_ev) {
              newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx
            } else {
              # If the event is not retained (as per `require_sust_days`),
              # proceed with search starting from event, regardless of `proceed_from`:
              newref <- change_idx
            }

          if (verbose==2 & baseline!='fixed') {
            message("Baseline at visit no.", bl_idx)
          }

          # Move the search index.
          search_idx <- newref + 1
        }

      # Confirmed sub-threshold improvement: RE-BASELINE
      # ------------------------------------------------
      else if (length(conf_idx) > 0 # confirmation visits available
          && isevent_loc(data_id[[value_col]][change_idx], bl=bl[[value_col]], type='impr', st=TRUE) # (sub-threshold) improvement from baseline
          && ifelse(check_intermediate,
          all(sapply((change_idx + 1):conf_idx[[1]], function(x)
            isevent_loc(data_id[[value_col]][x], bl=bl[[value_col]], type='impr', st=TRUE))), # (sub-threshold) improvement is confirmed over (all visits up to) first valid date
            isevent_loc(data_id[[value_col]][conf_idx[[1]]], bl=bl[[value_col]], type='impr', st=TRUE)) # (sub-threshold) improvement is confirmed at first valid date
          && baseline %in% c('roving', 'roving_impr') #_r_#
          && sub_threshold_rebl %in% c('event', 'improvement')
              ) {
              newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx

              # Set new baseline after event:
              bl_idx <- newref
              # Move search index
              search_idx <- newref + 1

              if (verbose == 2) {
                message("Found confirmed sub-threshold ", if (outcome != 'custom') toupper(outcome), 'outcome',
                        " improvement (visit no.", change_idx, ")")
                message("Baseline at visit no.", bl_idx)
              }
            }

      # CONFIRMED WORSENING:
      # -------------------
      else if (isevent_loc(data_id[[value_col]][change_idx], bl=bl[[value_col]], type='wors')  # value worsened (>delta) from baseline

         && ((length(conf_idx) > 0 # confirmation visits available
           && ifelse(check_intermediate,
              all(sapply((change_idx + 1):conf_idx[[1]],
               function(x) isevent_loc(data_id[[value_col]][x], bl=bl[[value_col]], type='wors'))),  # worsening is confirmed at (all visits up to) first valid date
              isevent_loc(data_id[[value_col]][conf_idx[[1]]], bl=bl[[value_col]], type='wors')) # worsening is confirmed at first valid date
          ) || (data_id[[date_col]][change_idx] - data_id[[date_col]][1] <= impute_max_fu
                && rbinom(1,1,impute_last_visit)
                && change_idx == nvisits))
         ) {
                 if (change_idx == nvisits) {  # i.e., when imputing event at last visit
                   conf_idx <- c(nvisits)
                 }

                # First visit at which worsening is not sustained:
                if (conf_idx[[1]] == nvisits) {
                  next_nonsust <- NA
                  } else {
                    next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl=bl[[value_col]],
                                          type='wors'))[1] + conf_idx[[1]] }
                # Discard potential confirmation visits if these occur out of sustained interval:
                if (!is.na(next_nonsust)) {
                  conf_idx <- conf_idx[conf_idx < next_nonsust] }  # confirmed dates

                # The confirmed worsening can still be rejected if `require_sust_days>0`.
                # The `valid_ev` flag indicates whether the event can (1) or cannot (0) be retained:
                valid_ev <- 1
                if (require_sust_days>0) {
                  if ((!check_intermediate)
                      && data_id[[date_col]][nvisits] - data_id[[date_col]][change_idx] >= require_sust_days) { # follow-up lasts at least `require_sust_days`
                    sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                                      - data_id[[date_col]][change_idx] >= require_sust_days)[1] + change_idx
                    } else {sust_vis <- nvisits}
                  valid_ev <- if (check_intermediate) {is.na(next_nonsust) || (data_id[[date_col]][next_nonsust] -
                                data_id[[date_col]][change_idx]) >= require_sust_days # worsening sustained up to end of follow-up, or for `require_sust_days`
                    } else isevent_loc(data_id[[value_col]][sust_vis], bl=bl[[value_col]], type='wors') # worsening confirmed at last visit, or first visit after `require_sust_days`
                }

                # If the event is retained (as per `require_sust_days`),
                # 1. we check if it's PIRA/RAW;
                # 2. we store the info.
                if (valid_ev) {


                  #####_ev_
                  ev <- make_empty_event()
                  #####_ev_

                  # Find last visit at clinically meaningful score distance from event
                  lastdelta_idx <- find_last_delta_idx(data_id, change_idx,
                                                       if (change_idx == nvisits) NULL else conf_idx)
                  lastdelta <- data_id[lastdelta_idx,]

                  sust_idx <- if (is.na(next_nonsust)) nvisits else next_nonsust - 1

                  if (data_id[['closest_rel_before']][change_idx] <= relapse_assoc[1]
                      || data_id[['closest_rel_after']][change_idx] <= relapse_assoc[2]) {
                    # A) Event is relapse-associated.
                    if (event=='firstPIRA' && baseline %in% c('fixed', 'roving_impr')) {
                      search_idx <- change_idx + 1 # skip this event if only searching for PIRA with no CDW-driven re-baseline
                      next
                    }
                    #####_ev_
                    ev$event_type <- "RAW"
                    ev$event_index <- change_idx
                    #####_ev_
                    # event_type <- c(event_type, 'RAW')
                    # event_index <- c(event_index, change_idx)
                    #####_ev_
                  } else {
                    # B) event is not relapse-associated.
                    if (event=='firstRAW' && baseline %in% c('fixed', 'roving_impr')) {
                      search_idx <- change_idx + 1 # skip this event if only searching for RAW with no CDW-driven re-baseline
                      next
                    }

                    # The detected CDW is not RAW. Let's check if it's PIRA.

                    if (is.null(renddate_col) # either relapse end dates are not provided
                        || length(relapse_indep[['event']]==2)) { # or they are provided but not used for PIRA (`use_end_dates=FALSE` in `relapse_indep_from_bounds`)

                      # Compute intervals that must be relapse-free for PIRA definition
                      if (relapse_indep[['prec_type']]=='baseline') {
                        prec <- bl
                      } else if (relapse_indep[['prec_type']]=='last') {
                        prec <- data_id[change_idx - 1,]  # last visit before the event
                      } else if (relapse_indep[['prec_type']]=='last_delta') {
                        prec <- lastdelta # last pre-worsening visit
                      } else {
                        stop('\'', relapse_indep[['prec_type']], '\' is an invalid value for \'prec_type\' in `relapse_indep`')
                      }
                      left <- right <- list() # left/right borders of relapse-free intervals
                      for (iic in 1:length(conf_idx)) {
                        left[[iic]] <- list() # left borders of relapse-free intervals for current confirmation visit
                        right[[iic]] <- list() # right borders of relapse-free intervals for current confirmation visit
                        ic <- conf_idx[[iic]]
                        for (point in c('prec', 'event', 'conf')) {
                          t <- if (point == 'prec') prec[[date_col]] else if (point == 'event') {
                            data_id[[date_col]][change_idx]} else data_id[[date_col]][ic]

                          if (!is.null(relapse_indep[[point]][[1]])) {
                            t0 <- t - relapse_indep[[point]][[1]]
                          } # for prec, a value for t0 is always set;
                            # for the other points, if not set, the values is recovered from previous interval.

                          if (!is.null(relapse_indep[[point]][[2]])) {
                            t1 <- t + relapse_indep[[point]][[2]]
                            # t1 is set as soon as a right bound is found.

                            if (t1 > t0) {
                              left[[iic]] <- append(left[[iic]], t0)
                              right[[iic]] <- append(right[[iic]], t1)
                            }
                          }
                        }
                      }

                      # Check if confirmation visits meet relapse-free interval rules
                      rel_inbetween <- sapply(1:length(conf_idx), function(iic) {
                        any(sapply(1:length(left[[iic]]), function(j) {
                          any((left[[iic]][j] <= relapse_dates) & (relapse_dates <= right[[iic]][j]))
                        }))
                      })
                    } else {  # if using relapse end dates!

                      if (data_id[['closest_rel_before']][change_idx] < Inf
                          || data_id[['closest_rel_after']][change_idx] <= relapse_indep[['event']][[2]]) {
                        # (relapse-free interval around event not satisfied)
                        rel_inbetween <- rep(TRUE, length(conf_idx))
                      } else {
                        # Check relapse-free interval around each confirmation visit:
                      rel_inbetween <- sapply(1:length(conf_idx), function(iic) {
                        data_id[['closest_rel_before']][iic] < Inf || data_id[['closest_rel_after']][iic] <= relapse_indep[['conf']][[2]]
                      })
                      }
                    }

                    # Store info on PIRA:
                    pconf_idx <- conf_idx[!rel_inbetween] # PIRA confirmation visits (a subset of original confirmation visits)
                    if (length(pconf_idx) > 0) {
                      pconf_t <- conf_t
                      for (cm in names(conf_t)) {
                        confirmed_at <- intersect(pconf_t[[cm]], pconf_idx)
                        if (length(confirmed_at)==0) {
                          pconf_t[[cm]] <- NULL #pconf_t <- within(pconf_t, rm(list=cm))
                          #####_ev_
                          # pira_conf_date[[cm]] <- c(pira_conf_date[[cm]], display_date(NA, global_start))
                          # pira_conf_value[[cm]] <- c(pira_conf_value[[cm]], NaN)
                          #####_ev_
                        } else {
                          #####_ev_
                          ev[[paste0("PIRA_conf", cm, "_date")]] <- data_id[[date_col]][min(confirmed_at)]
                          ev[[paste0("PIRA_conf", cm, "_value")]] <- data_id[[value_col]][min(confirmed_at)]
                          #####_ev_
                          # pira_conf_date[[cm]] <- c(pira_conf_date[[cm]], display_date(data_id[[date_col]][min(confirmed_at)], global_start))
                          # pira_conf_value[[cm]] <- c(pira_conf_value[[cm]], data_id[[value_col]][min(confirmed_at)])
                          #####_ev_
                        }
                        # pira_conf[[cm]] <- c(pira_conf[[cm]], as.integer(length(confirmed_at)>0))
                      }
                      #####_ev_
                      ev$event_type <- "PIRA"
                      ev$event_index <- change_idx
                      #####_ev_
                      # event_type <- c(event_type, 'PIRA')
                      # event_index <- c(event_index, change_idx)
                      #####_ev_
                    } else {
                      #####_ev_
                      ev$event_type <- "CDW"
                      ev$event_index <- change_idx
                      #####_ev_
                      # event_type <- c(event_type, 'CDW')
                      # event_index <- c(event_index, change_idx)
                      #####_ev_
                    }
                  }

                  # Store info:
                  #####_ev_
                  ev$date <- data_id[[date_col]][change_idx]
                  ev$value <- data_id[[value_col]][change_idx]
                  ev$bl_date <- bl[[date_col]]
                  ev$bl_value <- bl[[value_col]]
                  ev$last_delta_date <- lastdelta[[date_col]]
                  ev$last_delta_value <- lastdelta[[value_col]]
                  ev$bl2event <- data_id[[date_col]][change_idx] - bl[[date_col]]
                  ev$time2event <- data_id[[date_col]][change_idx] - data_id[[date_col]][1]
                  for (cm in names(conf_t)) {
                    confirmed_at <- intersect(conf_t[[cm]], conf_idx)
                    if (length(confirmed_at)==0) {
                      conf_t[[cm]] <- NULL #conf_t <- within(conf_t, rm(list=cm))
                    } else {
                      ev[[paste0("conf", cm, "_date")]] <- data_id[[date_col]][min(confirmed_at)]
                      ev[[paste0("conf", cm, "_value")]] <- data_id[[value_col]][min(confirmed_at)]
                    }
                  }
                  ev$sust_days <- data_id[[date_col]][sust_idx] - data_id[[date_col]][change_idx]
                  ev$sust_last <- sust_idx == nvisits
                  #####_ev_
                  # if (event_type[length(event_type)] != 'PIRA') {
                  #   for (m in conf_days) {
                  #       # pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], 0)
                  #       pira_conf_date[[as.character(m)]] <- c(pira_conf_date[[as.character(m)]], display_date(NA, global_start))
                  #       pira_conf_value[[as.character(m)]] <- c(pira_conf_value[[as.character(m)]], NaN)
                  #       }
                  # }
                  # bl_date <- c(bl_date, display_date(bl[[date_col]], global_start))
                  # bl_value <- c(bl_value, bl[[value_col]])
                  # edate <- c(edate, display_date(data_id[[date_col]][change_idx], global_start))
                  # evalue <- c(evalue, data_id[[value_col]][change_idx])
                  # last_delta_date <- c(last_delta_date, display_date(lastdelta[[date_col]], global_start))
                  # last_delta_value <- c(last_delta_value, lastdelta[[value_col]])
                  # bl2event <- c(bl2event, data_id[[date_col]][change_idx] - bl[[date_col]])
                  # time2event <- c(time2event, data_id[[date_col]][change_idx] - data_id[[date_col]][1])
                  # for (cm in names(conf_t)) {
                  #   confirmed_at <- intersect(conf_t[[cm]], conf_idx)
                  #   if (length(confirmed_at)==0) {
                  #     conf_t <- within(conf_t, rm(list=cm))
                  #     conf_date[[cm]] <- c(conf_date[[cm]], display_date(NA, global_start))
                  #     conf_value[[cm]] <- c(conf_value[[cm]], NaN)
                  #   } else {
                  #   conf_date[[cm]] <- c(conf_date[[cm]], display_date(data_id[[date_col]][min(confirmed_at)], global_start))
                  #   conf_value[[cm]] <- c(conf_value[[cm]], data_id[[value_col]][min(confirmed_at)])
                  #   }
                  #   # conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
                  # }
                  # sustd <- c(sustd, data_id[[date_col]][sust_idx] - data_id[[date_col]][change_idx])
                  # sustl <- c(sustl, as.integer(sust_idx == nvisits))
                  #####_ev_

                  # Print info
                  if (verbose == 2) {
                    message("Found ", if (outcome != 'custom') paste0(toupper(outcome), "-") else "", "CDW",
                            if (ev$event_type == "CDW") "" else paste0(" (", ev$event_type, ")"),
                            " (visit no.", change_idx, ", ",
                            display_date(data_id[[date_col]][change_idx], global_start),
                            if (length(conf_t) > 0) paste0(") confirmed at ",
                            paste(ifelse(ev$event_type == 'PIRA', names(pconf_t), names(conf_t)), collapse=" and "),
                                " days, sustained up to visit no.", sust_idx,
                                 " (", display_date(data_id[[date_col]][sust_idx], global_start), ")") else {
                            ") occurring at last assessment (no confirmation)"}
                            )
                  }

                  events[[length(events) + 1]] <- ev
                  event_type <- c(event_type, ev$event_type)
                  event_index <- c(event_index, ev$event_index)

                  } else {
                    # If the event is NOT retained (as per `require_sust_days`), we proceed.
                    for (cm in names(conf_t)) {
                      confirmed_at <- intersect(conf_t[[cm]], conf_idx)
                      if (length(confirmed_at)==0) {
                        conf_t[[cm]] <- NULL #conf_t <- within(conf_t, rm(list=cm))
                      }
                    }
                    if (verbose == 2) {
                      message("Change confirmed but not sustained over ",
                          if (require_sust_days < Inf) paste(">=", require_sust_days, "days") else "entire follow-up",
                      ": proceed with search")
                    }
                    unconf[[unconf_idx]] <- setNames(list(subjid,
                                                          data_id[[date_col]][change_idx],
                                                          data_id[[value_col]][change_idx],
                                                          bl[[date_col]],
                                                          bl[[value_col]],
                                                          data_id[['closest_rel_before']][change_idx],
                                                          data_id[['closest_rel_after']][change_idx]),
                                                     c(subj_col, 'date', 'value', 'bl_date', 'bl_value',
                                                       'closest_rel_before', 'closest_rel_after'))
                    unconf_idx <- unconf_idx + 1
                  }


                if (length(conf_t) > 0) {
                  # For each m in conf_days, only keep the earliest available confirmation visit:
                  conf_idx <- unname(sapply(names(conf_t), function(cm) {
                    x <- intersect(conf_t[[cm]], conf_idx)
                    if (length(x)) min(x)
                  }))

                  if (baseline %in% c('roving', 'roving_wors')) {
                    # In a roving baseline setting, the baseline is moved after the confirmed event (even if it is not sustained):
                    newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx
                    bl_idx <- newref
                    } else if (!valid_ev
                               || (ev$event_type != 'PIRA' && event == 'firstPIRA')
                               || (ev$event_type != 'RAW' && event == 'firstRAW')
                    ) {
                    # If the event is NOT retained as per `require_sust_days`,
                    # or != required event type,
                    # proceed with search starting from event, regardless of `proceed_from`:
                    newref <- change_idx
                  } else {
                    # If baseline is static, only update `newref` (to be used for search index):
                    newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx
                  }

                  # Move the search index.
                  search_idx <- newref + 1

                } else { # worsening occurring at last visit (length(conf_t)==0)
                  search_idx <- change_idx + 1
                }

              if (verbose == 2 & baseline!='fixed') {
                message("Baseline at visit no.", bl_idx)
              }


          }

       # Confirmed sub-threshold worsening: RE-BASELINE
       # ------------------------------------------------
      else if (length(conf_idx) > 0
               && isevent_loc(data_id[[value_col]][change_idx], bl=bl[[value_col]], type='wors', st=TRUE)
               && ifelse(check_intermediate,
                    all(sapply((change_idx + 1):conf_idx[[1]], function(x)
                      isevent_loc(data_id[[value_col]][x], bl=bl[[value_col]], type='wors', st=TRUE))), # (sub-threshold) worsening confirmed over (all visits up to) first valid date
                    isevent_loc(data_id[[value_col]][conf_idx[[1]]], bl=bl[[value_col]], type='wors', st=TRUE))  # (sub-threshold) worsening confirmed at first valid date
               && baseline %in% c('roving', 'roving_wors')
               && sub_threshold_rebl %in% c('event', 'worsening')
               ) {
                  newref <- if (proceed_from=='firstconf') conf_idx[[1]] else change_idx

                  # Set new baseline after the event:
                  bl_idx <- newref
                  # Move search index
                  search_idx <- newref + 1

                  if (verbose == 2) {
                    message("Found confirmed sub-threshold ", if (outcome != 'custom') toupper(outcome) else 'outcome',
                            " worsening (visit no.", change_idx, ")")
                    message("Baseline at visit no.", bl_idx)
                  }
                }

      # NO confirmation:
      # ----------------
      else {
        search_idx <- change_idx + 1
        if (verbose == 2) {
          message("Change not confirmed: proceed with search")
        }
        if (!is.na(change_idx)) {
          unconf[[unconf_idx]] <- setNames(list(subjid,
                                                data_id[[date_col]][change_idx],
                                                data_id[[value_col]][change_idx],
                                                bl[[date_col]],
                                                bl[[value_col]],
                                                data_id[['closest_rel_before']][change_idx],
                                                data_id[['closest_rel_after']][change_idx]),
                                           c(subj_col, 'date', 'value', 'bl_date', 'bl_value',
                                             'closest_rel_before', 'closest_rel_after'))
          unconf_idx <- unconf_idx + 1
        }
      }

      }

      # Relapse-based rebaseline: if search_idx crossed a relapse, move baseline after it.
      # (unless search_idx is after the relapse but within its influence as per `relapse_assoc[0]`: could be a RAW)
        if (relapse_rebl && proceed && search_idx <= nvisits
            && any(data_id[[date_col]][bl_idx] < relapse_dates  # presence of a relapse between baseline...
                   & (relapse_dates + (if (event=='firstPIRA') 0 else relapse_assoc[1])
                      < data_id[[date_col]][search_idx]))  # ...and search index
            ) {

          # Move baseline just after `irel`-th relapse
          if (bl_idx <= nvisits && !is.na(irel)) {
            idx_tmp <- NA
            for (x in bl_idx:nvisits) {
              if (relapse_dates[irel] <= data_id[[date_col]][x]) {
                idx_tmp <- x
                break
              }
            }
            bl_idx <- idx_tmp
          }

          if (!is.na(bl_idx)) {

            # Move search index just after baseline
            search_idx <- bl_idx + 1

            if (verbose == 2) {
              message("[post-relapse rebaseline] Baseline moved to visit no.", bl_idx)
            }
          }

          # If no more rebaseline is possible, terminate search:
          if (proceed && (is.na(bl_idx) || bl_idx > nvisits - 1)) {
            proceed <- 0
            if (verbose == 2)
              message('[post-relapse rebaseline] Not enough visits after current baseline: end process')
          }
        }


        # In a first-event setting, stop search if specified event was already found:
        if (proceed && ((event == "first" && length(events) > 1) ||
                        (event == "firstCDI" && ("CDI" %in% event_type)) ||
                        (event == "firstCDW" && (("RAW" %in% event_type) || ("PIRA" %in% event_type) || ("CDW" %in% event_type))) ||
                        (event == "firstPIRA" && ("PIRA" %in% event_type)) ||
                        (event == "firstRAW" && ("RAW" %in% event_type)))
            ) {
          proceed <- 0
          if (verbose == 2) {
            message("\'", event, "\'", " already found: end process")
          }
        }

    } # end while (proceed)

    #################################################################

    # Possibly reduce to a subset of events (based on `event` argument)
    #####_ev_
    # subj_index <- as.numeric(row.names(results_df[results_df[subj_col] == subjid, ]))
    # if (length(event_type) > 1) {
    if (length(events) > 0) {
    #####_ev_

      #####_ev_
      # event_type <- event_type[2:length(event_type)]  # remove first empty event
      #####_ev_

      # Spot duplicate events
      # (can only occur if relapse_rebl is enabled - in that case, only keep last detected)
      uevents <- unique(event_index)
      ucounts <- table(event_index)
      duplicates <- uevents[ucounts > 1]
      diff <- length(event_index) - length(uevents) # keep track of no. duplicates
      for (ev in duplicates) {
        all_ind <- which(event_index == ev)
        event_index[all_ind[-length(all_ind)]] <- 0 # mark duplicate events with 0
      }

      event_order <- order(event_index)
      event_order <- event_order[(diff + 1):length(event_order)] # eliminate duplicates (those marked with 0)

      event_type <- event_type[event_order]

      if (startsWith(event, "first")) {
        impr_idx <- which(event_type == "CDI")[1]
        prog_idx <- which(event_type %in% c("CDW", "RAW", "PIRA"))[1]
        raw_idx <- which(event_type == "RAW")[1]
        pira_idx <- which(event_type == "PIRA")[1]

        if (event == "firstCDI") {
          first_events <- impr_idx
        } else if (event == "firstCDW") {
          first_events <- prog_idx
        } else if (event == "firstPIRA") {
          first_events <- pira_idx
        } else if (event == "firstRAW") {
          first_events <- raw_idx
        }

        if (event=='first') {
          first_events <- 1
        } else {
          first_events <- unique(na.omit(first_events))
        }

        event_order <- event_order[first_events]
      }
    } else {
      #####_ev_
      event_order <- character(0)
      #####_ev_
      # event_type <- c() # remove first empty event
      #####_ev_
      }
    #####_ev_
    events <- events[event_order]
    event_type <- event_type[event_order]

    # Build subject-level df
    if (length(event_type) > 0) {
      # 1. subject has events

      events_clean <- lapply(events, function(ev) {
        ev[setdiff(names(ev), "event_index")]
      })
      subject_df <- do.call(rbind.data.frame, c(events_clean, stringsAsFactors = FALSE))
      subject_df[[subj_col]] <- subjid
      subject_df$nevent <- seq_len(nrow(subject_df))
      subject_df$total_fu <- total_fu[subjid]

    } else if (include_stable) {
      # 2. subject has no events but is included

      ev <- make_empty_event()
      ev$time2event <- total_fu[subjid]
      subject_df <- as.data.frame(ev[setdiff(names(ev), "event_index")], stringsAsFactors = FALSE)
      subject_df[[subj_col]] <- subjid
      subject_df$nevent <- 0
      subject_df$total_fu <- total_fu[subjid]

    } else {
      # 3. subject has no events and is excluded

      subject_df <- NULL
    }

    # Add to global `subject_results` list
    subject_results[[subjid]] <- subject_df

    # Extract event counts
    CDI <- sum(subject_df$event_type == "CDI")
    CDW <- sum(subject_df$event_type %in% c("CDW", "RAW", "PIRA"))
    RAW <- sum(subject_df$event_type == "RAW")
    PIRA <- sum(subject_df$event_type == "PIRA")

    #####_ev_
    # # Build results for subjid with updated event_type
    # if (length(event_type) > 0) {
    #   # 1. subject has events
    #
    #   results_df <- results_df[-subj_index[(length(event_type) + 1):length(subj_index)], ]
    #   rownames(results_df) <- NULL # reset column names
    #   results_df[results_df[[subj_col]] == subjid, "event_type"] <- event_type
    #   results_df[results_df[[subj_col]] == subjid, "bl_date"] <- bl_date[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "bl_value"] <- bl_value[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "date"] <- edate[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "value"] <- evalue[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "last_delta_date"] <- last_delta_date[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "last_delta_value"] <- last_delta_value[event_order]
    #   results_df[results_df[[subj_col]] == subjid, 'total_fu'] <- total_fu[subjid]
    #   results_df[results_df[[subj_col]] == subjid, "time2event"] <- time2event[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "bl2event"] <- bl2event[event_order]
    #   for (m in conf_days) {
    #     # results_df[results_df[[subj_col]] == subjid, paste0("conf", m)] <- conf[[as.character(m)]][event_order]
    #     results_df[results_df[[subj_col]] == subjid, paste0("conf", m, "_date")] <- conf_date[[as.character(m)]][event_order]
    #     results_df[results_df[[subj_col]] == subjid, paste0("conf", m, "_value")] <- conf_value[[as.character(m)]][event_order]
    #     # results_df[results_df[[subj_col]] == subjid, paste0("PIRA_conf", m)] <- pira_conf[[as.character(m)]][event_order]
    #     results_df[results_df[[subj_col]] == subjid, paste0("PIRA_conf", m, "_date")] <- pira_conf_date[[as.character(m)]][event_order]
    #     results_df[results_df[[subj_col]] == subjid, paste0("PIRA_conf", m, "_value")] <- pira_conf_value[[as.character(m)]][event_order]
    #     }
    #   results_df[results_df[[subj_col]] == subjid, "sust_days"] <- sustd[event_order]
    #   results_df[results_df[[subj_col]] == subjid, "sust_last"] <- sustl[event_order]
    #
    # } else if (include_stable) {
    #   # 2. subject has no events but is included
    #
    #   results_df <- results_df[-subj_index[2:length(subj_index)], ]
    #   rownames(results_df) <- NULL # reset column names
    #   results_df[results_df[[subj_col]]==subjid, 'nevent'] <- 0
    #   results_df[results_df[[subj_col]]==subjid, 'total_fu'] <- total_fu[subjid]
    #   results_df[results_df[[subj_col]]==subjid, 'time2event'] <- total_fu[subjid]
    #   # results_df[results_df[[subj_col]] == subjid, 'date'] <- display_date(data_id[[date_col]][nvisits], global_start)
    #
    #   } else {
    #   # 3. subject has no events and is excluded
    #
    #   results_df <- results_df[-subj_index, ]
    #   rownames(results_df) <- NULL # reset column names
    # }
    #
    # # Extract event counts
    # CDI <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "CDI")
    # CDW <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] %in% c("CDW", "RAW", "PIRA"))
    # RAW <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "RAW")
    # PIRA <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "PIRA")
    #####_ev_

    # Update `summary` df with event counts + sequence
    summary[subjid, c('CDI', 'CDW', 'RAW', 'PIRA')] <- c(CDI, CDW, RAW, PIRA)
    summary[subjid, 'event_sequence'] <- paste(event_type, collapse=", ")

    if (verbose == 2) {
      message("Event sequence: ", ifelse(length(events) > 0,
                                paste(event_type, collapse=", "), "-"))
    }

  } # end for (subjid in all_subj)

  # Merge final data frame
  results_df <- do.call(rbind.data.frame, subject_results)
  rownames(results_df) <- NULL

  #################################################################

  if (verbose >= 1) {
    message(paste0("\n---\nOutcome: ", outcome, "\nConfirmation", ifelse(check_intermediate, " over: ", " at: "),
          paste(conf_days, collapse=", "), " days (-", conf_tol_days[1], " days, +",
          conf_tol_days[2], " days)\nBaseline: ", baseline,
          ifelse(baseline!='fixed' && sub_threshold_rebl!='none',
                 paste0(" (include sub-threshold ", sub_threshold_rebl, "s)"), ""),
          ifelse(relapse_rebl, ", and post-relapse re-baseline", ""),
          "\nBaseline skipped if: ", ifelse(relapse_to_bl[1]>0, ifelse(is.null(renddate_col),
                paste0("<", relapse_to_bl[1], " days from last relapse"),
                "within duration of a relapse"), ""),
              ifelse(relapse_to_bl[2]>0, paste0(ifelse(relapse_to_bl[1]>0, ", <", '<'),
                      relapse_to_bl[2], " days to next relapse"), ""),
          if (skip_local_extrema!='none') paste0(if (relapse_to_bl[1] == 0 && relapse_to_bl[2] == 0) "" else
                              ", or ", "local extremum") else "",
          if (relapse_to_bl[1]==0 && relapse_to_bl[2]==0 && skip_local_extrema=='none') "-" else "",
          "\nEvent skipped if: ", ifelse(relapse_to_event[1]>0, ifelse(is.null(renddate_col),
                paste0("<", relapse_to_event[1], " days from last relapse"),
                "within last relapse duration"), ""),
              ifelse(relapse_to_event[2]>0, paste0(ifelse(relapse_to_event[1]>0, ", <", '<'),
                    relapse_to_event[2], " days to next relapse"), ""),
          if (relapse_to_event[1]==0 && relapse_to_event[2]==0) "-" else "",
          "\nConfirmation visit skipped if: ", ifelse(relapse_to_conf[1]>0, ifelse(is.null(renddate_col),
                paste0("<", relapse_to_conf[1], " days from last relapse"),
                "within last relapse duration"), ""),
              ifelse(relapse_to_conf[2]>0, paste0(ifelse(relapse_to_conf[1]>0, ", <", '<'),
                    relapse_to_conf[2], " days to next relapse"), ""),
          if (relapse_to_conf[1] == 0 && relapse_to_conf[2] == 0) "-" else "",
          "\nEvents detected: ", event))
    message('\n*Please use `print(output)` to display full info on event detection criteria*')
    if (is.null(subjects) || length(subjects)>1) {
        message("\n---\nTotal subjects: ", nsub,
            "\n---\nSubjects with ",
            if (event=='firstPIRA') "PIRA: " else if (event=='firstRAW') "RAW: " else "CDW: ",
              sum(summary$CDW > 0), if (event %in% c('firstPIRA','firstRAW')) "" else paste0(" (PIRA: ", sum(summary$PIRA > 0),
            "; RAW: ", sum(summary$RAW > 0), ")"))
        if (!(event %in% c('firstCDW', 'firstPIRA', 'firstRAW'))) {
        message("Subjects with CDI: ", sum(summary$CDI > 0))
        }
        if (event == 'multiple') {
          message("---\nCDW events: ",
            sum(summary$CDW), " (PIRA: ", sum(summary$PIRA), "; RAW: ", sum(summary$RAW), ")")
          message("CDI events: ", sum(summary$CDI))
        }
      }

  }

  # Convert date columns to Date format (relative to global_start)
  date_cols <- names(results_df)[endsWith(names(results_df), "date")]
  results_df[date_cols] <- lapply(
    results_df[date_cols],
    num_to_date,
    start = global_start
  )

  # Split `event_type` into `event_type` and `CDW_type`
  results_df <- results_df %>% mutate(
    CDW_type = ifelse(event_type %in% c("PIRA", "RAW"), event_type, ifelse(event_type == "CDW", "undefined", "")),
    event_type = ifelse(event_type %in% c("CDW", "PIRA", "RAW"), "CDW", event_type)
  )

  columns <- names(results_df)
  # Reorder columns
  to_move <- c(subj_col, "nevent", "total_fu", "CDW_type")
  columns <- columns[!columns %in% to_move]
  columns <- c(subj_col, setdiff(columns, subj_col)) # move in front
  columns <- append(columns, "nevent", after = 1) # position 2
  columns <- append(columns, "CDW_type", after = 3) # position 4
  columns <- append(columns, "total_fu", after = 4) # position 5

  if (!include_dates) {
    columns <- columns[!endsWith(columns, "date")]
  }
  if (!include_values) {
    columns <- columns[!endsWith(columns, "value")]
  }

  scolumns <- names(summary)
  if (event=='firstPIRA') {
    scolumns <- c('PIRA')
  } else if (event=='firstRAW') {
    scolumns <- c('RAW')
    columns <- columns[!startsWith(columns, "PIRA")]
  } else if (event == 'firstCDW') {
    scolumns <- scolumns[scolumns!='CDI']
  } else if (event == 'firstCDI') {
    scolumns <- c('CDI')
    columns <- columns[!startsWith(columns, "PIRA")]
  }

  if (event %in% c('firstRAW', 'firstCDI')) {
    columns <- columns[!startsWith(columns, "PIRA")]  # remove PIRA confirmation columns from extended results
  }

  summary <- summary[scolumns]
  results_df <- results_df[columns]

  # Merge `unconfirmed` df
  unconfirmed <- do.call(rbind.data.frame, unconf)
  # Convert date columns to Date format (relative to global_start)
  date_cols <- names(unconfirmed)[endsWith(names(unconfirmed), "date")]
  unconfirmed[date_cols] <- lapply(
    unconfirmed[date_cols],
    num_to_date,
    start = global_start
  )

  for (w in warnings) {
      warning(w)
  }

  settings <- list(outcome=outcome, event=event, baseline=baseline, proceed_from=proceed_from,
                validconf_p=if (is.null(validconf_col)) 1 else mean(data[[validconf_col]]),
                validconf_col=validconf_col, skip_local_extrema=skip_local_extrema,
                conf_days=conf_days, conf_tol_days=conf_tol_days,
                require_sust_days=require_sust_days, check_intermediate=check_intermediate,
                relapse_to_bl=relapse_to_bl, relapse_to_event=relapse_to_event, relapse_to_conf=relapse_to_conf,
                relapse_assoc=relapse_assoc, relapse_indep=relapse_indep, renddate_col=renddate_col,
                sub_threshold_rebl=sub_threshold_rebl, bl_geq=bl_geq, relapse_rebl=relapse_rebl,
                impute_last_visit=impute_last_visit, delta_fun=delta_fun,
                worsening=worsening, bl_geq=bl_geq)

  output <- list(event_count=summary, results=results_df, settings=settings, unconfirmed=unconfirmed)
  class(output) <- 'MSprogOutput'

  return(output)
}



###############################################################################################



