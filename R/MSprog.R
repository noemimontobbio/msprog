
#' Assess multiple sclerosis disability course from longitudinal data.
#'
#' `MSprog()` detects and characterises the confirmed disability worsening (CDW)
#' or improvement events of an outcome measure (EDSS, NHPT, T25FW, or SDMT; or any custom outcome)
#' for one or more subjects, based on repeated assessments
#' through time (and on the dates of acute episodes, if any).
#' Several qualitative and quantitative options are given as arguments that can be set
#' by the user and reported as a complement to the results to ensure reproducibility.
#'
#' The events are detected sequentially by scanning the outcome values in chronological order.
#' Valid time windows for confirmation visits are determined by arguments
#' `conf_days`, `conf_tol_days`, `conf_unbounded_right`, `relapse_to_conf`.
#' CDW events are classified as relapse-associated or relapse-independent based on their relative timing
#' with respect to the relapses. Specifically, relapse-associated worsening (RAW) events are defined as
#' CDW events occurring within a specified interval (`relapse_assoc` argument) from a relapse;
#' the definition of progression independent of relapse activity (PIRA) is established by specifying relapse-free intervals
#' around the baseline, CDW event, and confirmation visits (`relapse_indep` argument).
#'
#'
#' @param data `data.frame` containing longitudinal data, including: subject ID, outcome value, date of visit.
#' @param subj_col Name of data column with subject ID.
#' @param value_col Name of data column with outcome value.
#' @param date_col Name of data column with date of visit.
#' @param outcome Specifies the outcome type. Must be one of the following:
#' \itemize{
#'  \item{`'edss'`}{ (Expanded Disability Status Scale);}
#'  \item{`'nhpt'`}{ (Nine-Hole Peg Test);}
#'  \item{`'t25fw'`}{ (Timed 25-Foot Walk);}
#'  \item{`'sdmt'`}{ (Symbol Digit Modalities Test);}
#'  \item{`NULL`}{ (only accepted when specifying a custom `delta_fun`)}
#'  }
#' @param relapse `data.frame` containing longitudinal data, including: subject ID and relapse date.
#' @param rsubj_col Name of subject ID column for relapse data, if different from outcome data.
#' @param rdate_col Name of onset date column for relapse data, if different from outcome data.
#' @param renddate_col Name of end date column for relapse data (if present).
#' @param subjects Subset of subjects (list of IDs). If none is specified, all subjects listed in data are included.
#' @param delta_fun Custom function specifying the minimum shift in the outcome measure that counts as a valid change from the provided reference value.
#' The function provided must take a numeric value (reference score) as input, and return a numeric value corresponding to the minimum shift from baseline, see example 3 below.
#' If none is specified (default), the user must provide a non-`NULL` value for the `outcome` argument (see above) in order to use
#' the built-in function [compute_delta()].
#' @param worsening The direction of worsening (`'increase'` if higher values correspond to worse disease course, `'decrease'` otherwise).<br />
#' This argument is only used when `outcome` is set to `NULL`. Otherwise, `worsening` is automatically set to
#' `'increase'` if `outcome` is set to `'edss'`, `'nhpt'`, `'t25fw'`,
#'  and to `'decrease'` if `outcome` is set to `'sdmt'`.
#' @param event Specifies which events to detect. Must be one of the following:
#' \itemize{
#' \item{`'firstCDW'`}{ (first confirmed disability worsening (CDW), default);}
#' \item{`'first'`}{ (only the very first confirmed event -- improvement or worsening);}
#' \item{`'firsteach'`}{ (first confirmed disability improvement and first CDW -- in chronological order);}
#' \item{`'firstCDWtype'`}{ (first CDW of each kind -- PIRA, RAW, and undefined, in chronological order);}
#' \item{`'firstPIRA'`}{ (first PIRA);}
#' \item{`'firstRAW'`}{ (first RAW);}
#' \item{`'multiple'`}{ (all events in chronological order).}
#' }
#' @param baseline Specifies the baseline scheme. Must be one of the following:
#' \itemize{
#' \item{`'fixed'`}{ (first valid outcome value, default);}
#' \item{`'roving_impr'`}{ (updated after every confirmed disability improvement to the visit determined by `proceed_from`;
#' suitable for a first-CDW setting to discard fluctuations around baseline);}
#' \item{`'roving'`}{ (updated after each improvement or worsening event to the visit determined by `proceed_from`;
#' suitable for a multiple-event setting -- i.e., when `event` is set to `'multiple'`,
#' `'firsteach'`, or `'firstCDWtype'` -- or when searching for a specific type of CDW
#' -- i.e., when `event` is set to `'firstPIRA'` or `'firstRAW'`).}
#' }
#' @param proceed_from After detecting a confirmed disability event, continue searching:
#' \itemize{
#' \item{}{from the next visit after the first qualifying confirmation visit if `proceed_from='firstconf'`;}
#'\item{}{from the next visit after the event if `proceed_from='event'`.}
#' }
#' If `baseline='roving'` or `baseline='roving_impr'`, when rebaselining after a confirmed disability event,
#' the baseline is moved to:
#' \itemize{
#' \item{}{the first qualifying confirmation visit if `proceed_from='firstconf'`;}
#'\item{}{the event visit if `proceed_from='event'`.}
#' }
#' @param sub_threshold_rebl This argument is only used if `baseline='roving'` or `baseline='roving_impr'`.
#' Must be one of the following:
#' \itemize{
#' \item{`'event'`:}{ any confirmed sub-threshold event (i.e. any \emph{confirmed} change in the outcome measure,
#' regardless of `delta_fun`) can potentially trigger a re-baseline;}
#' \item{`'improvement'`:}{ any confirmed sub-threshold improvement (i.e. any \emph{confirmed} improvement in the outcome measure,
#' regardless of `delta_fun`) can potentially trigger a re-baseline;}
#' \item{`'worsening'`:}{ any confirmed sub-threshold worsening (i.e. any \emph{confirmed} worsening in the outcome measure,
#' regardless of `delta_fun`) can potentially trigger a re-baseline;}
#' \item{`'none'`:}{ only use valid confirmed events (as per `delta_fun`) for rebaseline.}
#' }
#' @param bl_geq This argument is only used if the baseline is moved.
#' If `TRUE`, the new reference value must always be greater or equal than the previous one;
#' when it is not, the old reference value is assigned to it \[2\].
#' @param relapse_rebl If `TRUE`, re-baseline after every relapse.
#' @param skip_local_extrema If `TRUE`, the baseline cannot be placed at a local minimum or maximum.
#' A visit `i` is a local minimum point for `outcome` if `outcome(i+1)-outcome(i)>=delta_fun(outcome(i))`
#' and `outcome(i-1)-outcome(i)>=delta_fun(outcome(i))`. Local maxima are defined similarly.
#' @param validconf_col Name of data column specifying which visits can (`T`) or cannot (`F`) be used as confirmation visits.
#' The input data does not necessarily have to include such a column.
#' If `validconf_col=NULL`, all visits are potentially used as confirmation visits.
#' @param conf_days Period before confirmation (days). Can be a single value or list-like of any length if multiple windows are to be considered.
#' @param conf_tol_days Tolerance window for confirmation visit (days); can be an integer (same tolerance on left and right)
#' or list-like of length 2 (different tolerance on left and right).
#' In all cases, the right end of the interval is ignored if `conf_unbounded_right` is set to `TRUE`.
#' @param conf_unbounded_right If `TRUE`, confirmation window is unbounded on the right
#' (regardless of the right end indicated by `conf_tol_days`).
#' @param require_sust_days Minimum number of days over which a confirmed change must be sustained
#' (i.e., confirmed at \emph{all} visits occurring in the specified period) to be retained as an event.
#' Events sustained for the remainder of the follow-up period are always retained regardless of follow-up duration.
#' Setting `require_sust_days=Inf`, events are retained only when sustained for the remainder of the follow-up period.<br />
#' (Warning: if `check_intermediate` is set to `FALSE`, sustained change will be established based \emph{only on the end} of the specified period.)
#' @param check_intermediate If `TRUE` (default), events are confirmed \emph{over all intermediate visits}
#' up to the confirmation visit. <br />
#' If set to `FALSE` (not recommended in most cases, as it may discard meaningful fluctuations),
#' events will be confirmed \emph{only at} the specified confirmation visit
#' (and \emph{only at the end} of the period defined by `require_sust_days`, if any).
#' @param relapse_to_bl Minimum distance from a relapse (days) for a visit to be used as baseline.
#' Can be an integer (minimum distance from \emph{last} relapse) or list-like of length 2
#' (minimum distance from \emph{last} relapse, minimum distance from \emph{next} relapse).
#' Note that setting the distance to zero means keeping the baseline where it is regardless of surrounding relapses.
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse
#' is overwritten by the relapse duration, unless it was set to zero (in which case it stays 0).
#' If the designated baseline does not respect this constraint, the baseline is moved to the next available visit.
#' @param relapse_to_event Minimum distance from a relapse (days) for an event to be considered as such.
#' Can be an integer (minimum distance from \emph{last} relapse) or list-like of length 2
#' (minimum distance from \emph{last} relapse, minimum distance from \emph{next} relapse).
#' Note that setting the distance to zero means retaining the event regardless of surrounding relapses.
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse
#' is overwritten by the relapse duration, unless it was set to zero (in which case it stays 0).
#' @param relapse_to_conf Minimum distance from a relapse (days) for a visit to be a valid confirmation visit.
#' Can be an integer (minimum distance from \emph{last} relapse) or list-like of length 2
#' (minimum distance from \emph{last} relapse, minimum distance from \emph{next} relapse).
#' Note that setting the distance to zero means using any visit for confirmation regardless of surrounding relapses.
#' If relapse end dates are available (`renddate_col`), the minimum distance from last relapse
#' is overwritten by the relapse duration, unless it was set to zero (in which case it stays 0).
#' @param relapse_assoc Maximum distance from a relapse (days) for a CDW event to be classified as RAW.
#' Can be an integer (maximum distance from \emph{last} relapse) or list-like of length 2
#' (maximum distance from \emph{last} relapse, maximum distance from \emph{next} relapse).
#' If relapse end dates are available (`renddate_col`), the maximum distance from last relapse
#' is overwritten by the relapse duration.
#' @param relapse_indep Specifies relapse-free intervals for PIRA definition.
#' Must be given in the form produced by function [relapse_indep_from_bounds()] by calling
#' \cr`relapse_indep_from_bounds(b0, b1, e0, e1, c0, c1)`\cr
#' to specify the intervals around baseline (`b0` and `b1`),
#' event (`e0` and `e1`), and confirmation (`c0` and `c1`). For instance:
#' \itemize{
#' \item{No relapses within event-90dd->event+30dd and within confirmation-90dd->confirmation+30dd \[1\]:
#' \cr`relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)` (default);}
#' \item{No relapses between baseline and confirmation (high-specificity definition from \[1\]):
#' \cr`relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,NULL,NULL,0)`;}
#' \item{No relapses within baseline->event+30dd and within confirmation+-30dd \[2\]:
#' \cr`relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,30,30,30)`}
#' }
#' If relapse end dates are available (`renddate_col`), it is possible to define PIRA based on those
#' by setting `use_end_dates=T` in [relapse_indep_from_bounds()].
#' @param impute_last_visit Imputation probability for worsening events occurring at last visit (i.e. with no confirmation).
#' Unconfirmed worsening events occurring at the last visit are never imputed if `impute_last_visit=0`;
#' they are always imputed if `impute_last_visit=1`;
#' they are imputed with probability `p`, `0<p<1`, if `impute_last_visit=p`.
#' If a value `N>1` is passed, unconfirmed worsening events are imputed only if occurring within `N` days of follow-up
#' (e.g., in case of early discontinuation).
#' @param date_format Format of dates in the input data. If not specified, it will be inferred by function [as.Date()].
#' @param include_dates If `TRUE`, `output$results` will include the date of each event (`'date'` column)
#' and the date of the corresponding baseline (`'bldate'` column).
#' @param include_value If `TRUE`,  `output$results` will include the outcome value at each event (`'value'` column)
#' and at the corresponding baseline (`'blvalue'` column).
#' @param include_stable If `TRUE`, subjects with no confirmed events are included in `output$results`,
#' with `time2event` = total follow up.
#' @param verbose One of:
#' \itemize{
#'  \item{0}{ (print no info);}
#'  \item{1}{ (print concise info, default);}
#'  \item{2}{ (print extended info).}
#'  }
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
#'
#' @return An object of class `'MSprogOutput'` with the following attributes:
#' \itemize{
#' \item{`event_count`: }{a `data.frame` containing the event sequence detected for each subject, and the counts for each event type}
#' \item{`results`: }{a `data.frame` with extended info on each event for all subjects}
#' \item{`settings`: }{a list containing all the arguments used to compute the output.}
#' }
#'
#' @importFrom stats na.omit setNames complete.cases
#' @importFrom dplyr %>% group_by_at vars slice n mutate across
#' @export
#' @examples
#' # 1. EDSS course
#' output_edss <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output_edss$results) # extended info on each event for all subjects
#' print(output_edss$event_count) # summary of event sequence for each subject
#' # 2. SDMT course
#' output_sdmt <- MSprog(toydata_visits, 'id', 'SDMT', 'date', 'sdmt',
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output_sdmt$results) # extended info on each event for all subjects
#' print(output_sdmt$event_count) # summary of event sequence for each subject
#' # 3. SDMT course, with a custom delta function
#' my_sdmt_delta <- function(reference_value) {min(c(reference_value/5, 4))}
#' output_sdmt <- MSprog(toydata_visits, 'id', 'SDMT', 'date', NULL,
#'     delta_fun=my_sdmt_delta,
#'     relapse=toydata_relapses, conf_days=12*7, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output_sdmt$results) # extended info on each event for all subjects
#' print(output_sdmt$event_count) # summary of event sequence for each subject
MSprog <- function(data, subj_col, value_col, date_col, outcome,
                   relapse=NULL, rsubj_col=NULL, rdate_col=NULL, renddate_col=NULL,
                   subjects=NULL, delta_fun=NULL, worsening=NULL, event='firstCDW',
                   baseline='fixed', proceed_from='firstconf', sub_threshold_rebl='none',
                   bl_geq=F, relapse_rebl=F, skip_local_extrema=F,
                   validconf_col=NULL, conf_days=12*7, conf_tol_days=c(7,2*365.25), conf_unbounded_right=F, require_sust_days=0, check_intermediate=T,
                   relapse_to_bl=30, relapse_to_event=0, relapse_to_conf=30,
                   relapse_assoc=90, relapse_indep=NULL,
                   impute_last_visit=0, date_format=NULL, include_dates=F, include_value=F, include_stable=T, verbose=1
                   ) {

  # SETUP

  warnings <- list()

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


  if (is.null(outcome) ||
      !(tolower(outcome) %in% c('edss', 'nhpt', 't25fw', 'sdmt'))) {
    outcome <- 'outcome'
  } else {
    outcome <- tolower(outcome)
  }

  if (!event %in% c('firstCDW', 'first', 'firsteach', 'firstCDWtype',
                    'firstPIRA', 'firstRAW', 'multiple')) {
    stop('Invalid value for `event` argument. Valid values: \'firstCDW\', \'first\', \'firsteach\', \'firstCDWtype\', \'firstPIRA\', \'multiple\'.')
  }

  if (!baseline %in% c('fixed', 'roving_impr', 'roving')) {
    stop('Invalid value for `baseline` argument. Valid values: \'fixed\', \'roving_impr\', \'roving\'.')
  }

  if (!is.null(renddate_col)) {
    relapse_to_bl[1] <- ifelse(relapse_to_bl[1]==0, 0, Inf)
    relapse_to_event[1] <- ifelse(relapse_to_event[1]==0, 0, Inf)
    relapse_to_conf[1] <- ifelse(relapse_to_conf[1]==0, 0, Inf)
    relapse_assoc[1] <- Inf
  }

  # If no column names are specified for the relapse file, use the main ones
  if (is.null(rsubj_col)) {
    rsubj_col <- subj_col
  }
  if (is.null(rdate_col)) {
    rdate_col <- date_col
  }
  if (is.null(validconf_col)) {
    validconf_col <- 'validconf'
    data$validconf <- T
  } else {
    data[[validconf_col]] <- as.logical(data[[validconf_col]])
  }

  # Create empty relapse file if none is provided
  if (is.null(relapse)) {
    relapse <- data.frame(matrix(nrow=0, ncol=2))
    names(relapse) <- c(rsubj_col, rdate_col)
    renddate_col <- NULL
    relapse_rebl <- F
  }

  # Remove missing values from columns of interest
  data <- data[complete.cases(data[ , c(subj_col, value_col, date_col, validconf_col)]), ]
  if (is.null(renddate_col)) {
    relapse <- relapse[complete.cases(relapse[, c(rsubj_col, rdate_col)]), ]
  } else {
  relapse <- relapse[complete.cases(relapse[, c(rsubj_col, rdate_col, renddate_col)]), ]
  }

  # Convert dates to Date format
  if (is.null(date_format)) {
    data[[date_col]] <- as.Date(data[[date_col]])
    relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]])
    if (!is.null(renddate_col)) {
      relapse[[renddate_col]] <- as.Date(relapse[[renddate_col]])
    }
  } else {
  data[[date_col]] <- as.Date(data[[date_col]], format=date_format)
  relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]], format=date_format)
  if (!is.null(renddate_col)) {
    relapse[[renddate_col]] <- as.Date(relapse[[renddate_col]], format=date_format)
    }
  }

  # Convert dates to days from global minimum
  if (nrow(relapse)>0) {
    global_start <- min(min(data[[date_col]]), min(relapse[[rdate_col]]))
  } else {global_start <- min(data[[date_col]])}
  data[[date_col]] <- as.numeric(difftime(data[[date_col]], global_start), units='days')
  relapse[[rdate_col]] <- as.numeric(difftime(relapse[[rdate_col]], global_start), units='days')
  if (!is.null(renddate_col)) {
    relapse[[renddate_col]] <- as.numeric(difftime(relapse[[renddate_col]], global_start), units='days')
  }

  # Restrict to subset of subjects
  if (!is.null(subjects)) {
  data <- data[data[[subj_col]] %in% subjects,]
  relapse <- relapse[relapse[[rsubj_col]] %in% subjects,]
  }

  if (nrow(data)==0) {
    stop('Empty data. Did you pass an empty/invalid `subjects` argument?')
  }

  # Check if values are in correct range
  if (outcome!='outcome') {
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

  # # Set minimum value
  # if (is.null(min_value)) {
  #   min_value_ifany <- -Inf
  # } else {
  #   min_value_ifany <- min_value
  # }


  if (impute_last_visit<0) {
    stop('`impute_last_visit` must be nonnegative')
  } else if (impute_last_visit<=1) {
    # If impute_last_visit is a probability, set no limit to follow-up length (Inf)
    impute_max_fu <- Inf
  } else {
    # If impute_last_visit is a follow-up time, save the value and set probability to 1
    impute_max_fu <- impute_last_visit
    impute_last_visit <- 1
  }


  # Set direction of worsening
  if (outcome %in% c('edss', 'nhpt', 't25fw')) {
    worsening <- 'increase'
  } else if (outcome=='sdmt') {
    worsening <- 'decrease'
  } else if (is.null(worsening)) {
    stop('Either specify an outcome type, or specify the direction of worsening (\'increase\' or \'decrease\')')
  }

  # Define local is_event() function
  isevent_loc <- function(x, baseline, type='wors', st=F) {
    is_event(x, baseline, type=type, outcome=outcome, worsening=worsening,
             sub_threshold=st, delta_fun=delta_fun)
  }

  # Define a confirmation window for each value of conf_days
  if (conf_unbounded_right) {
    conf_tol_days[2] <- Inf
  }
  conf_window <- lapply(conf_days, function(t) {
    lower <- as.integer(t) - conf_tol_days[1]
      upper <- as.integer(t) + conf_tol_days[2]
    return(c(lower, upper))
  })

  # Define relapse-free intervals for PIRA definition
  if (is.null(relapse_indep)) {
    relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)
  }


  #################################################################
  # Assess disability course

  all_subj <- unique(data[[subj_col]])
  nsub <- length(all_subj)
  max_nevents <- round(max(table(data[[subj_col]]))/2)
  results_df <- data.frame(matrix(nrow=nsub*max_nevents, ncol=10+length(conf_days)*2+2)) #length(conf_days) + (length(conf_days)-1)
  allcol <- c(subj_col, 'nevent', 'event_type', 'bldate', 'blvalue', 'date', 'value', 'total_fu', 'time2event',
              'bl2event', paste0('conf', conf_days), paste0('PIRA_conf', conf_days), 'sust_days', 'sust_last')
  # if (length(conf_days)>1) {
  #   allcol <- c(allcol[1:(10+length(conf_days))],  paste0('PIRA_conf',
  #                     conf_days[2:length(conf_days)]), 'sust_days', 'sust_last')
  #   }
  colnames(results_df) <- allcol
  results_df[[subj_col]] <- rep(all_subj, each=max_nevents)
  results_df$nevent <- rep(1:max_nevents, times=nsub)

  summary <- data.frame(matrix(nrow=nsub, ncol=6))
  colnames(summary) <- c('event_sequence', 'improvement', 'CDW', 'RAW', 'PIRA', 'undefined_prog')
  rownames(summary) <- all_subj


  total_fu <- setNames(rep(0, nsub), all_subj)

  for (subjid in all_subj) {

    data_id <- data[data[[subj_col]] == subjid, ]

    # If more than one visit occur on the same day, only keep last
    ucounts <- table(data_id[, date_col])
    if (any(ucounts > 1)) {
      data_id <- data_id %>% group_by_at(vars(date_col)) %>% slice(n())
    }

    # Sort visits in chronological order
    order_tmp <- order(data_id[[date_col]])
    if (any(order_tmp != rownames(data_id))) {
      data_id <- data_id[order_tmp, ]
    }

    nvisits <- nrow(data_id)
    first_visit <- min(data_id[[date_col]])
    relapse_id <- relapse[relapse[[rsubj_col]] == subjid, ]
    relapse_id <- relapse_id[relapse_id[[rdate_col]] >= first_visit - relapse_to_bl[1], ] #as.difftime(relapse_to_bl, units="days") #_d_#
    relapse_dates <- relapse_id[[rdate_col]] #onset_dates
    nrel <- length(relapse_dates)

    total_fu[subjid] <- data_id[nvisits,][[date_col]] - data_id[1,][[date_col]]


    # Print info
    if (verbose == 2) {
      message("\nSubject #", subjid, ": ", nvisits, " visit", ifelse(nvisits == 1, "", "s"),
              ", ", nrel, " relapse", ifelse(nrel == 1, "", "s"))
      if (any(ucounts > 1)) {
        message("Found multiple visits in the same day: only keeping last.")
      }
      if (any(order_tmp != rownames(data_id))) {
        message("Visits not listed in chronological order: sorting them.")
      }
    }

    # Compute distance from relapses
    if (length(relapse_dates) > 0) {
      relapse_df <- data.frame(split(rep(relapse_dates, each=nrow(data_id)),
                                     rep(1:length(relapse_dates), each=nrow(data_id))))
      relapse_df$visit <- data_id[,][[date_col]]
      dist <- (relapse_df %>% mutate(across(1:length(relapse_dates),
                                ~ as.numeric(.x - visit))))[1:length(relapse_dates)]
      distm <- - dist
      distp <- dist
      distm[distm<0] <- Inf
      distp[distp<0] <- Inf
      data_id$closest_rel_before <- if (all(is.na(distm))) Inf else apply(distm, 1, min, na.rm=TRUE)
      if (!is.null(renddate_col)) {
        relapse_duration <- relapse_id[[renddate_col]] - relapse_id[[rdate_col]]
        which_rel <- if (all(is.na(distm))) Inf else apply(distm, 1, which.min)
        data_id$closest_rel_duration <- relapse_duration[which_rel]
        data_id$outside_influence <- data_id$closest_rel_duration < data_id$closest_rel_before
        data_id[data_id$outside_influence, 'closest_rel_before'] <- Inf
          }
      data_id$closest_rel_after <- if (all(is.na(distp))) Inf else apply(distp, 1, min, na.rm=TRUE)
    } else {
      data_id$closest_rel_before <- Inf
      data_id$closest_rel_after <- Inf
    }


    # Initialise results columns
    event_type <- ""
    event_index <- NULL
    bldate <- edate <- blvalue <- evalue <- time2event <- bl2event <- sustd <- sustl <- vector()
    conf <- pira_conf <- list()
    for (m in conf_days) {
      conf[[as.character(m)]] <- vector()
      pira_conf[[as.character(m)]] <- vector()}

    # Initialise variables
    bl_idx <- 1 # Baseline index
    search_idx <- 2 # Index of where we are in the search
    proceed <- 1 # "proceed with search" flag
    bl_last <- NULL # Previous baseline value
    irel <- 1 # Current relapse index
    if (nrel > 0) {
      for (r in 1:nrel) {
        if (relapse_dates[r] > data_id[bl_idx,][[date_col]]) {
          change_idx <- r
          break
        }
      }
    }

    if (verbose == 2) {
      message("Baseline at visit no.", bl_idx)
    }

    while (proceed) {

      # Set baseline (skip if within relapse influence)
      if (skip_local_extrema) {
        prec <- ifelse(bl_idx==1, data_id[bl_idx,][[value_col]], data_id[bl_idx-1,][[value_col]])
        subs <- ifelse(bl_idx==nvisits, data_id[bl_idx,][[value_col]], data_id[bl_idx+1,][[value_col]])
        vis <- data_id[bl_idx,][[value_col]]
        local_extr <- (isevent_loc(prec, baseline=vis, type='wors') && isevent_loc(subs, baseline=vis, type='wors')) || (
                        isevent_loc(prec, baseline=vis, type='impr') && isevent_loc(subs, baseline=vis, type='impr'))
      } else {
        local_extr <- F
      }
      while (proceed
             && (data_id[bl_idx,][['closest_rel_before']] < relapse_to_bl[1]
             || data_id[bl_idx,][['closest_rel_after']] < relapse_to_bl[2]
             || local_extr)) {
        if (verbose == 2) {
          message("Baseline (visit no.", bl_idx, ") is ",
                  ifelse(local_extr, "a local estremum: ", "within relapse influence: "),
                  "moved to visit no.", bl_idx + 1)
        }
        bl_idx <- bl_idx + 1
        search_idx <- search_idx + 1
        #
        if (skip_local_extrema) {
          prec <- ifelse(bl_idx==1, data_id[bl_idx,][[value_col]], data_id[bl_idx-1,][[value_col]])
          subs <- ifelse(bl_idx==nvisits, data_id[bl_idx,][[value_col]], data_id[bl_idx+1,][[value_col]])
          vis <- data_id[bl_idx,][[value_col]]
          local_extr <- (isevent_loc(prec, baseline=vis, type='wors') && isevent_loc(subs, baseline=vis, type='wors')) || (
            isevent_loc(prec, baseline=vis, type='impr') && isevent_loc(subs, baseline=vis, type='impr'))
        } else {
          local_extr <- F
        }
        #
        if (bl_idx > nvisits - 1) {
          proceed <- 0
          if (verbose == 2) {
            message("Not enough visits left: end process")
          }
        }
      }

      if (bl_idx > nvisits) {
        bl_idx <- nvisits
        proceed <- 0
        if (verbose == 2) {
          message("Not enough visits left: end process")
        }
      } else if (bl_geq && !is.null(bl_last) && bl_last > data_id[bl_idx,][[value_col]]) {
        # Kappos2020 (by Sean Yiu)
        data_id[bl_idx, value_col] <- bl_last
      }

      bl <- data_id[bl_idx, ]
      bl_last <- bl[[value_col]]

      # Event detection
      change_idx <- NA
      if (search_idx<=nvisits) {
      for (x in search_idx:nvisits) {
          if (isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='change',
                          st=sub_threshold_rebl!='none') &
              data_id[x,][['closest_rel_before']] >= relapse_to_event[1]
               && data_id[x,][['closest_rel_after']] >= relapse_to_event[2]) {
            change_idx <- x
            break
          }
      }
      }

      if (is.na(change_idx) || change_idx>nvisits) {
        proceed <- 0
        if (verbose == 2) {
          message("No ", outcome, " change in any subsequent visit: end process")
        }
      } else if (relapse_rebl && !is.null(irel) && nrel>irel
                 && change_idx + ifelse(event=='firstPIRA', 0, relapse_assoc) >= relapse_dates[irel]) {
        search_idx <- change_idx
      } else {
        if (change_idx==nvisits) {
          conf_idx <- list()
          conf_t <- list() #plv
          } else {
        conf_idx <- lapply(conf_window, function(t) {
          match_idx <- numeric(0)
          for (x in (change_idx + 1):nvisits) {
            if (data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] >= t[1]
                && data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] <= t[2]
                && data_id[x,][['closest_rel_before']] >= relapse_to_conf[1]
                && data_id[x,][['closest_rel_after']] >= relapse_to_conf[2]
                && data_id[x,][[validconf_col]]
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
        ####### #_conf_#
        }
        if (verbose == 2) {
          message(outcome, " change at visit no.", change_idx, " (",
                  global_start + as.difftime(data_id[change_idx,][[date_col]], units="days"),
                       "); potential confirmation visits available: no.", paste(conf_idx, collapse=", "))
        }


        # Confirmation
        # ============

        # CONFIRMED IMPROVEMENT:
        # --------------------
        if (isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='impr') # value improved (>delta) from baseline
            && length(conf_idx) > 0 # confirmation visits available
            && ifelse(check_intermediate,
            all(sapply((change_idx + 1):conf_idx[[1]], function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]],
                       type='impr'))),  # improvement is confirmed at (all visits up to) first valid date
            isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='impr'))  # improvement is confirmed at first valid date
            && !((event %in% c('firstCDW', 'firstCDWtype', 'firstPIRA', 'firstRAW')) && baseline=='fixed')
            ) {

          # First visit at which improvement is not sustained:
          if (conf_idx[[1]]==nvisits) {
            next_nonsust <- NA
            } else {
              next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                  bl[[value_col]], type='impr'))[1] + conf_idx[[1]]
          }
          # Discard potential confirmation visits if these occur out of sustained interval:
          if (!is.na(next_nonsust)) {
            conf_idx <- conf_idx[conf_idx < next_nonsust]
          }

          # The confirmed improvement can still be rejected if `require_sust_days>0`.
          # The `valid_impr` flag indicates whether the event can (1) or cannot (0) be retained:
          valid_impr <- 1
          if (require_sust_days>0) {
              if ((!check_intermediate)
                  && data_id[nvisits, date_col] - data_id[change_idx, date_col] >= require_sust_days # if follow-up lasts at least require_sust_days
                  ) {
                sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                      - data_id[change_idx,][[date_col]] >= require_sust_days)[1] + change_idx  # first visit occurring at least `require_sust_days` from event
              } else {
                sust_vis <- nvisits  # if follow-up lasts before `require_sust_days`, check at last visit
                }
            valid_impr <- ifelse(check_intermediate,
                is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]]
                          - data_id[change_idx, date_col]) >= require_sust_days,  # improvement sustained up to end of follow-up, or for `require_sust_days`
                isevent_loc(data_id[sust_vis,][[value_col]], bl[[value_col]], type='impr')  # improvement confirmed at `sust_vis` (last visit, or first visit after `require_sust_days`)
            )
          }

          # If the event is retained (as per `require_sust_days`), we store the info:
          if (valid_impr) {
            sust_idx <- ifelse(is.na(next_nonsust), nvisits, next_nonsust - 1)
            event_type <- c(event_type, "impr")
            event_index <- c(event_index, change_idx)
            bldate <- c(bldate, as.character(global_start + as.difftime(bl[[date_col]], units='days')))
            blvalue <- c(blvalue, bl[[value_col]])
            edate <- c(edate, as.character(global_start + as.difftime(data_id[change_idx,][[date_col]], units='days')))
            evalue <- c(evalue, data_id[change_idx,][[value_col]])
            bl2event <- c(bl2event, data_id[change_idx,][[date_col]] - bl[[date_col]])
            time2event <- c(time2event, data_id[change_idx,][[date_col]] - data_id[1,][[date_col]])
            for (cm in names(conf_t)) {
              confirmed_at <- intersect(conf_t[[cm]], conf_idx)
              if (length(confirmed_at)==0) {
                conf_t <- within(conf_t, rm(list=cm))
              }
              conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
              pira_conf[[cm]] <- c(pira_conf[[cm]], NA)}
            sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
            sustl <- c(sustl, as.integer(sust_idx == nvisits))
            # Print progress info
            if (verbose == 2) {
              message(outcome, " improvement (visit no.", change_idx, ", ",
                      global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                           ") confirmed at ", paste(names(conf_t), collapse=", "), " days, sustained up to visit no.", sust_idx,
                           " (", global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")")
            }
          } else {
            # If the event is NOT retained (as per `require_sust_days`), we proceed.
            if (verbose == 2) {
              message("Change confirmed but not sustained over ",
                      ifelse(require_sust_days<Inf, paste(">=", require_sust_days, "days"),
                             "entire follow-up"), ": proceed with search")
            }
          }

          # For each m in `conf_days`, only keep the earliest available confirmation visit:
          conf_idx <- unname(sapply(names(conf_t), function(cm) {
            min(intersect(conf_t[[cm]], conf_idx))
          }))


          if (baseline %in% c('roving', 'roving_impr')) { #_r_#
            # In a roving baseline setting, the baseline is moved after the confirmed event (even if it is not sustained):
            newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)
            bl_idx <- newref} else if (valid_impr) {
              newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)
            } else {
              # If the event is not retained (as per `require_sust_days`),
              # proceed with search starting from event, regardless of `proceed_from`:
              newref <- change_idx
            }

          # Next change (in any direction!)
          next_change <- NA
          if (newref<nvisits) {
            for (x in (newref + 1):nvisits) {
              if (isevent_loc(data_id[x,][[value_col]], data_id[bl_idx,][[value_col]],
                                                            type='change') # further change in any direction (from updated baseline)
                  # || !isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='impr') # or improvement not sustained (from original baseline)
                  ) {
                next_change <- x
                break
              }
            }
          }
          # # Next valid change (in any direction!) starting from *event*:
          # next_change_ev <- which(isevent_loc(data_id[(change_idx + 1):nvisits, value_col],
          #                                   data_id[bl_idx,][[value_col]], type='change'))[1] + change_idx

          # Move the search index.
          search_idx <- ifelse(is.na(next_change), nvisits + 1, next_change)

          if (verbose==2) {
          message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                  ifelse(search_idx > nvisits, "-", search_idx), " on")
          }

        }

      # Confirmed sub-threshold improvement: RE-BASELINE
      # ------------------------------------------------
      else if (length(conf_idx) > 0 # confirmation visits available
          && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='impr', st=T) # (sub-threshold) improvement from baseline
          && ifelse(check_intermediate,
          all(sapply((change_idx + 1):conf_idx[[1]], function(x)
            isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='impr', st=T))), # (sub-threshold) improvement is confirmed over (all visits up to) first valid date
            isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='impr', st=T)) # (sub-threshold) improvement is confirmed at first valid date
          && baseline %in% c('roving', 'roving_impr') #_r_#
          && sub_threshold_rebl %in% c('event', 'improvement')
              ) {
              newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)

              # Set new baseline after event:
              bl_idx <- newref
              # Move search index at next change from baseline:
              if (newref==nvisits) {
                next_change <- NA
              } else {
                next_change <- which(!isevent_loc(data_id[(newref + 1):nvisits, value_col],
                                      data_id[bl_idx,][[value_col]], type='impr', st=T))[1] + newref
              }
              search_idx <- ifelse(is.na(next_change), nvisits + 1, next_change)

              if (verbose == 2) {
                message("Confirmed sub-threshold ", outcome, " improvement (visit no.", change_idx, ")")
                message("Baseline at visit no.", bl_idx, ", searching for events from next change (visit no.",
                             ifelse(search_idx<=nvisits, search_idx, "-"), ") on")
              }
            }

      # CONFIRMED CDW:
      # --------------
      else if ( #data_id[change_idx,][[value_col]] >= min_value_ifany &&
         isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='wors')  # value worsened (>delta) from baseline

         && ((length(conf_idx) > 0 && # confirmation visits available
           ifelse(check_intermediate,
              all(sapply((change_idx + 1):conf_idx[[1]],
               function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='wors'))),  # worsenind is confirmed at (all visits up to) first valid date
              isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='wors') # worsening is confirmed at first valid date
           )
          # && all(sapply((change_idx + 1):conf_idx[[1]],
          #     function(x) data_id[x,][[value_col]] >= min_value_ifany)) # confirmation above min_value too
          ) || (data_id[change_idx,][[date_col]] - data_id[1,][[date_col]] <= impute_max_fu
                && rbinom(1,1,impute_last_visit)
                && change_idx == nvisits)
          )

         ) {

                 if (change_idx == nvisits) { # i.e., when imputing event at last visit
                   conf_idx <- c(nvisits)
                 }
                # First visit at which worsening is not sustained:
                if (conf_idx[[1]]==nvisits) {
                  next_nonsust <- NA
                  } else {
                    next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl[[value_col]],
                                          type='wors'))[1] + conf_idx[[1]] }
                # Discard potential confirmation visits if these occur out of sustained interval:
                if (!is.na(next_nonsust)) {
                  conf_idx <- conf_idx[conf_idx < next_nonsust] } # confirmed dates

                # The confirmed worsening can still be rejected if `require_sust_days>0`.
                # The `valid_prog` flag indicates whether the event can (1) or cannot (0) be retained:
                valid_prog <- 1
                if (require_sust_days>0) {
                  if ((!check_intermediate)
                      && data_id[nvisits, date_col] - data_id[change_idx, date_col] >= require_sust_days) { # follow-up lasts at least `require_sust_days`
                    sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                                      - data_id[change_idx,][[date_col]] >= require_sust_days)[1] + change_idx
                    } else {sust_vis <- nvisits}
                  valid_prog <- ifelse(check_intermediate,
                    is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]] -
                                data_id[change_idx,][[date_col]]) > require_sust_days, # worsening sustained up to end of follow-up, or for `require_sust_days`
                    isevent_loc(data_id[sust_vis,][[value_col]], bl[[value_col]], type='wors') # worsening confirmed at last visit, or first visit after `require_sust_days`
                  )
                }

                # If the event is retained (as per `require_sust_days`),
                # 1. we check if it's PIRA/RAW;
                # 2. we store the info.
                if (valid_prog) {

                  nev <- length(event_type)

                  sust_idx <- ifelse(is.na(next_nonsust), nvisits, next_nonsust - 1)

                  if (data_id[change_idx,][['closest_rel_before']] <= relapse_assoc[1]
                      || data_id[change_idx,][['closest_rel_after']] <= relapse_assoc[2]) { # event is relapse-associated
                    if (event=='firstPIRA' && baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for PIRA with a fixed baseline
                      next
                    }
                    event_type <- c(event_type, 'RAW')
                    event_index <- c(event_index, change_idx)
                  } else if (data_id[change_idx,][['closest_rel_before']] > relapse_assoc[1]
                             && data_id[change_idx,][['closest_rel_after']] > relapse_assoc[2]) { # event is not relapse-associated
                    if (event=='firstRAW' && baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for RAW with a fixed baseline
                      next
                    }

                    # The detected CDW is not RAW. Let's check if it's PIRA.

                    if (is.null(renddate_col) # either relapse end dates are not provided
                        || length(relapse_indep[['conf']]==2)) { # or they are provided but not used for PIRA (`use_end_dates=F` in `relapse_indep_from_bounds`)

                      # Compute intervals that must be relapse-free for PIRA definition
                    left <- right <- list() # left/right borders of relapse-free intervals
                    for (iic in 1:length(conf_idx)) {
                      left[[iic]] <- list() # left borders of relapse-free intervals for current confirmation visit
                      right[[iic]] <- list() # right borders of relapse-free intervals for current confirmation visit
                      ic <- conf_idx[[iic]]
                      for (point in c('bl', 'event', 'conf')) {
                        t <- ifelse(point == 'bl', bl[[date_col]],
                                    ifelse(point == 'event', data_id[change_idx,][[date_col]],
                                           data_id[ic,][[date_col]]))

                        if (!is.null(relapse_indep[[point]][[1]])) {
                          t0 <- t - relapse_indep[[point]][[1]]
                        } # for bl, a value for t0 is always set;
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

                      if (data_id[change_idx,][['closest_rel_before']] < Inf
                          || data_id[change_idx,][['closest_rel_after']] <= relapse_indep[['event']][[2]]) {
                        # (relapse-free interval around event not satisfied)
                        rel_inbetween <- rep(T, length(conf_idx))
                      } else {
                        # Check relapse-free interval around each confirmation visit:
                      rel_inbetween <- sapply(1:length(conf_idx), function(iic) {
                        data_id[iic,][['closest_rel_before']] < Inf || data_id[iic,][['closest_rel_after']] <= relapse_indep[['conf']][[2]]
                      })
                      }
                    }

                    # Store info:
                    pconf_idx <- conf_idx[!rel_inbetween] # PIRA confirmation visits (a subset of original confirmation visits)
                    if (length(pconf_idx) > 0) {
                      pconf_t <- conf_t
                      for (cm in names(conf_t)) {
                        confirmed_at <- intersect(pconf_t[[cm]], pconf_idx)
                        if (length(confirmed_at)==0) {
                          pconf_t <- within(pconf_t, rm(list=cm))
                        }
                        pira_conf[[cm]] <- c(pira_conf[[cm]], as.integer(length(confirmed_at)>0))
                      }
                      event_type <- c(event_type, 'PIRA')
                      event_index <- c(event_index, change_idx)
                    } else {
                      event_type <- c(event_type, 'CDW')
                      event_index <- c(event_index, change_idx)
                    }
                  }

                  # Store info:
                  if (event_type[length(event_type)] != 'PIRA') {
                    for (m in conf_days) {
                        pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], NA)}
                  }
                  bldate <- c(bldate, as.character(global_start + as.difftime(bl[[date_col]], units='days')))
                  blvalue <- c(blvalue, bl[[value_col]])
                  edate <- c(edate, as.character(global_start + as.difftime(data_id[change_idx,][[date_col]], units='days')))
                  evalue <- c(evalue, data_id[change_idx,][[value_col]])
                  bl2event <- c(bl2event, data_id[change_idx,][[date_col]] - bl[[date_col]])
                  time2event <- c(time2event, data_id[change_idx,][[date_col]] - data_id[1,][[date_col]])
                  for (cm in names(conf_t)) {
                    confirmed_at <- intersect(conf_t[[cm]], conf_idx)
                    if (length(confirmed_at)==0) {
                      conf_t <- within(conf_t, rm(list=cm))
                    }
                    conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
                  }
                  sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
                  sustl <- c(sustl, as.integer(sust_idx == nvisits))

                  # Print info
                  if (verbose == 2) {
                    message(outcome, " ", event_type[length(event_type)],
                                 " (visit no.", change_idx, ", ",
                                global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                            ifelse(length(conf_t)>0, paste0(") confirmed at ",
                            paste(ifelse(event_type[length(event_type)]=='PIRA', names(pconf_t), names(conf_t)), collapse=", "),
                                " days, sustained up to visit no.", sust_idx,
                                 " (", global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")"),
                            ") occurring at last assessment (no confirmation)")
                            )
                  }

                  } else {
                    # If the event is NOT retained (as per `require_sust_days`), we proceed.
                    for (cm in names(conf_t)) {
                      confirmed_at <- intersect(conf_t[[cm]], conf_idx)
                      if (length(confirmed_at)==0) {
                        conf_t <- within(conf_t, rm(list=cm))
                      }
                    }
                    if (verbose == 2) {
                      message("Change confirmed but not sustained over ",
                              ifelse(require_sust_days<Inf, paste(">=", require_sust_days, "days"),
                                     "entire follow-up"), ": proceed with search")
                    }
                  }


                if (length(conf_t)>0) {
                  # For each m in conf_days, only keep the earliest available confirmation visit:
                  conf_idx <- unname(sapply(names(conf_t), function(cm) {
                    min(intersect(conf_t[[cm]], conf_idx))
                  }))

                  if (baseline == 'roving') {
                    # In a roving baseline setting, the baseline is moved after the confirmed event (even if it is not sustained):
                    newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)
                    bl_idx <- newref
                    } else if ((event_type[length(event_type)]!='PIRA' && event=='firstPIRA')
                          || (event_type[length(event_type)]!='RAW' && event=='firstRAW')
                          || !valid_prog
                    ) {
                    # If the event is NOT retained (as per `require_sust_days`, or != required event type),
                    # proceed with search starting from event, regardless of `proceed_from`:
                    newref <- change_idx
                  } else {
                    # If baseline is static, only update `newref` (to be used for search index):
                    newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)
                  }

                  # Next valid change (in any direction!)
                  next_change <- NA
                  if (newref<nvisits) {
                    for (x in (newref + 1):nvisits) {
                      if (isevent_loc(data_id[x,][[value_col]],  data_id[bl_idx,][[value_col]],
                                         type='change') # further change in any direction (from updated baseline)
                          # || !isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='wors') # or worsening not sustained (from original baseline)
                          ) {
                        next_change <- x
                        break
                      }
                    }
                  }
                  # # Next valid change (in any direction!) starting from *event*:
                  # next_change_ev <- which(isevent_loc(data_id[(change_idx + 1):nvisits, value_col],
                  #                         data_id[bl_idx,][[value_col]], type='change'))[1] + change_idx

                  # Move the search index.
                  search_idx <- ifelse(is.na(next_change), nvisits + 1, next_change)

                } else { # worsening occurring at last visit (length(conf_t)==0)
                  search_idx <- change_idx + 1
                }


              if (verbose == 2) {
                message("Baseline at visit no.", bl_idx,
                        ", searching for events from next change (visit no.",
                        ifelse(search_idx > nvisits, "-", search_idx), ") on")
              }


          }

       # Confirmed sub-threshold worsening: RE-BASELINE
       # ------------------------------------------------
      else if (length(conf_idx) > 0
               && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='wors', st=T)
               && ifelse(check_intermediate,
                    all(sapply((change_idx + 1):conf_idx[[1]], function(x)
                      isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='wors', st=T))), # (sub-threshold) worsening confirmed over (all visits up to) first valid date
                    isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='wors', st=T))  # (sub-threshold) worsening confirmed at first valid date
               && baseline == 'roving'
               && sub_threshold_rebl %in% c('event', 'worsening')
               ) {
                  newref <- ifelse(proceed_from=='firstconf', conf_idx[[1]], change_idx)

                  # Set new baseline after the event:
                  bl_idx <- newref

                  # Move search index to next change:
                  if (newref==nvisits) {
                    next_change <- NA
                    } else {
                    next_change <- which(!isevent_loc(data_id[(newref + 1):nvisits, value_col],
                                          data_id[bl_idx,][[value_col]], type='wors', st=T))[1] + newref
                    }
                  search_idx <- ifelse(is.na(next_change), nvisits + 1, next_change)

                  if (verbose == 2) {
                    message("Confirmed sub-threshold", outcome, "worsening (visit no.", change_idx, ")")
                    message("Baseline at visit no.", bl_idx,
                            ", searching for events from next change (visit no.",
                            ifelse(search_idx > nvisits, "-", search_idx), ") on")
                  }
                }

      # NO confirmation:
      # ----------------
      else {
        search_idx <- change_idx + 1
        if (verbose == 2) {
          message("Change not confirmed: proceed with search")
        }
      }

      }

        # Relapse-based rebaseline: if search_idx crossed a relapse, move baseline after it.
        if (relapse_rebl && proceed && search_idx <= nvisits
            && any(data_id[bl_idx,][[date_col]] < relapse_dates  # presence of a relapse between baseline...
                   & (relapse_dates <= data_id[search_idx,][[date_col]]
                   + ifelse(event=='firstPIRA', 0, relapse_assoc)))  # ...and search index
            ) {
          # Move baseline just after `irel`-th relapse
          if (bl_idx<=nvisits) {
            for (x in bl_idx:nvisits) {
              if (relapse_dates[irel] <= data_id[x,][[date_col]]
                  ) {
                bl_idx <- x
                break
              }
            }
          } else {bl_idx <- NA}

          # Move search index just after baseline
          if (!is.na(bl_idx)) {
            search_idx <- bl_idx + 1
            if (verbose == 2) {
              message("[post-relapse rebaseline] Baseline moved to visit no.", bl_idx,
                      ", searching for events from visit no.",
                      ifelse(search_idx > nvisits, "-", search_idx), " on")
            }
          }
          # If no more rebaseline is possible, terminate search:
          if (proceed && (is.na(bl_idx) || bl_idx > nvisits - 1)) {
            proceed <- 0
            if (verbose == 2)
              message('[post-relapse rebaseline] Not enough visits after current baseline: end process')
          }
          # Update relapse index:
          irel <- irel + 1
        }


        # In a first-event setting, stop search if specified event was already found:
        if (proceed && ((event == "first" && length(event_type) > 1) ||
                        (event == "firsteach" && ("impr" %in% event_type) && ("CDW" %in% event_type)) ||
                        (event == "firstCDW" && (("RAW" %in% event_type) || ("PIRA" %in% event_type) || ("CDW" %in% event_type))) ||
                        (event == "firstCDWtype" && ("RAW" %in% event_type) && ("PIRA" %in% event_type) && ("CDW" %in% event_type)) ||
                        (event == "firstPIRA" && ("PIRA" %in% event_type)) ||
                        (event == "firstRAW" && ("RAW" %in% event_type)))
            ) {
                          proceed <- 0
                          if (verbose == 2) {
                            message("\'", event, "\'", " events already found: end process")
                          }
                        }

    } #while (proceed)

  #################################################################

  subj_index <- as.numeric(row.names(results_df[results_df[subj_col] == subjid, ]))

  if (length(event_type) > 1) {

    event_type <- event_type[2:length(event_type)]  # remove first empty event

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
    event_order <- event_order[(diff+1):length(event_order)] # eliminate duplicates (those marked with 0)

    event_type <- event_type[event_order]

    if (startsWith(event, "first")) {
      impr_idx <- which(event_type == "impr")[1]
      prog_idx <- which(event_type %in% c("CDW", "RAW", "PIRA"))[1]
      raw_idx <- which(event_type == "RAW")[1]
      pira_idx <- which(event_type == "PIRA")[1]
      undef_prog_idx <- which(event_type == "CDW")[1]

      if (event == "firsteach") {
        first_events <- c(impr_idx, prog_idx)
      } else if (event == "firstCDW") {
        first_events <- c(prog_idx)
      } else if (event == "firstCDWtype") {
        first_events <- c(raw_idx, pira_idx, undef_prog_idx)
      } else if (event == "firstPIRA") {
        first_events <- pira_idx
      } else if (event == "firstRAW") {
        first_events <- raw_idx
      }

      if (event=='first') {first_events <- 1} else {first_events <- unique(na.omit(first_events))}

      event_type <- event_type[first_events]
      event_order <- event_order[first_events]
    }

    if ((length(event_type)==0) && include_stable) {
      results_df <- results_df[-subj_index[2:length(subj_index)], ]
      rownames(results_df) <- NULL # reset column names
      results_df[results_df[[subj_col]]==subjid, 'nevent'] <- 0
      results_df[results_df[[subj_col]]==subjid, 'total_fu'] <- total_fu[subjid]
      results_df[results_df[[subj_col]]==subjid, 'time2event'] <- total_fu[subjid]
      results_df[results_df[[subj_col]] == subjid, 'date'] <- as.character(global_start + as.difftime(data_id[nvisits,][[date_col]], units='days'))
      results_df[results_df[[subj_col]]==subjid, 'event_type'] <- ''
    }
    else if (length(event_type)==0) {
      results_df <- results_df[-subj_index, ]
      rownames(results_df) <- NULL # reset column names
      }
    else {
    results_df <- results_df[-subj_index[(length(event_type) + 1):length(subj_index)], ]
    rownames(results_df) <- NULL # reset column names
    results_df[results_df[[subj_col]] == subjid, "event_type"] <- event_type
    results_df[results_df[[subj_col]] == subjid, "bldate"] <- bldate[event_order]
    results_df[results_df[[subj_col]] == subjid, "blvalue"] <- blvalue[event_order]
    results_df[results_df[[subj_col]] == subjid, "date"] <- edate[event_order]
    results_df[results_df[[subj_col]] == subjid, "value"] <- evalue[event_order]
    results_df[results_df[[subj_col]] == subjid, 'total_fu'] <- total_fu[subjid]
    results_df[results_df[[subj_col]] == subjid, "time2event"] <- time2event[event_order]
    results_df[results_df[[subj_col]] == subjid, "bl2event"] <- bl2event[event_order]
    for (m in conf_days) {
      results_df[results_df[[subj_col]] == subjid, paste0("conf", m)] <- conf[[as.character(m)]][event_order]
      }
    results_df[results_df[[subj_col]] == subjid, "sust_days"] <- sustd[event_order]
    results_df[results_df[[subj_col]] == subjid, "sust_last"] <- sustl[event_order]
    for (m in conf_days) {
      # if (m!=conf_days[1]) {
      results_df[results_df[[subj_col]] == subjid, paste0("PIRA_conf", m)] <- pira_conf[[as.character(m)]][event_order]}
      # }
    }
  } else if (include_stable) {
    results_df <- results_df[-subj_index[2:length(subj_index)], ]
    rownames(results_df) <- NULL # reset column names
    results_df[results_df[[subj_col]]==subjid, 'nevent'] <- 0
    results_df[results_df[[subj_col]]==subjid, 'total_fu'] <- total_fu[subjid]
    results_df[results_df[[subj_col]]==subjid, 'time2event'] <- total_fu[subjid]
    results_df[results_df[[subj_col]] == subjid, 'date'] <- as.character(global_start + as.difftime(data_id[nvisits,][[date_col]], units='days'))
    results_df[results_df[[subj_col]]==subjid, 'event_type'] <- ''
  } else {
    results_df <- results_df[-subj_index, ]
    rownames(results_df) <- NULL # reset column names
  }

  improvement <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "impr")
  CDW <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] %in% c("CDW", "RAW", "PIRA"))
  undefined_prog <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "CDW")
  RAW <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "RAW")
  PIRA <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "PIRA")

  summary[as.character(subjid), c('improvement', 'CDW', 'RAW', 'PIRA', 'undefined_prog'
          )] <- c(improvement, CDW, RAW, PIRA, undefined_prog)

  summary[as.character(subjid), 'event_sequence'] <- paste(event_type, collapse=", ")

  if (startsWith(event, "firstCDW")) {
    summary <- summary[, !colnames(summary) %in% "improvement"]
  }

  if (verbose == 2) {
    message("Event sequence: ", ifelse(event_type[1]!="",
                              paste(event_type, collapse=", "), "-"))
  }


  } #for (subjid in all_subj)

  #################################################################

    if (verbose >= 1) {
      message(paste0("\n---\nOutcome: ", outcome, "\nConfirmation", ifelse(check_intermediate, " over: ", " at: "),
            paste(conf_days, collapse=", "), " days (-", conf_tol_days[1], " days, +",
            ifelse(conf_unbounded_right, "Inf", conf_tol_days[2]), " days)\nBaseline: ", baseline,
            ifelse(baseline!='fixed' && sub_threshold_rebl!='none',
                   paste0(" (include sub-threshold ", sub_threshold_rebl, "s)"), ""),
            ifelse(relapse_rebl, ", and post-relapse re-baseline", ""),
            "\nBaseline skipped if: ", ifelse(relapse_to_bl[1]>0, ifelse(is.null(renddate_col),
                  paste0("<", relapse_to_bl[1], " days from last relapse"),
                  "within duration of a relapse"), ""),
                ifelse(relapse_to_bl[2]>0, paste0(ifelse(relapse_to_bl[1]>0, ", <", '<'),
                        relapse_to_bl[2], " days to next relapse"), ""),
            ifelse(skip_local_extrema, paste0(ifelse(relapse_to_bl[1]==0 && relapse_to_bl[2]==0, "",
                                ", or "), "local extremum"), ""),
            ifelse(relapse_to_bl[1]==0 && relapse_to_bl[2]==0 && !skip_local_extrema, '-', ''),
            "\nEvent skipped if: ", ifelse(relapse_to_event[1]>0, ifelse(is.null(renddate_col),
                  paste0("<", relapse_to_event[1], " days from last relapse"),
                  "within last relapse duration"), ""),
                ifelse(relapse_to_event[2]>0, paste0(ifelse(relapse_to_event[1]>0, ", <", '<'),
                      relapse_to_event[2], " days to next relapse"), ""),
            ifelse(relapse_to_event[1]==0 && relapse_to_event[2]==0, '-', ''),
            "\nConfirmation visit skipped if: ", ifelse(relapse_to_conf[1]>0, ifelse(is.null(renddate_col),
                  paste0("<", relapse_to_conf[1], " days from last relapse"),
                  "within last relapse duration"), ""),
                ifelse(relapse_to_conf[2]>0, paste0(ifelse(relapse_to_conf[1]>0, ", <", '<'),
                      relapse_to_conf[2], " days to next relapse"), ""),
            ifelse(relapse_to_conf[1]==0 && relapse_to_conf[2]==0, '-', ''),
            "\nEvents detected: ", event))
      message('\n*Please use `print(output)` to display full info on event detection criteria*')
      if (is.null(subjects) || length(subjects)>1) {
          message("\n---\nTotal subjects: ", nsub,
              "\n---\nSubjects with ",
              ifelse(event=='firstPIRA', "PIRA: ", ifelse(event=='firstRAW', "RAW: ", "disability worsening: ")),
                sum(summary$CDW > 0), ifelse(event %in% c('firstPIRA','firstRAW'), "", paste0(" (PIRA: ", sum(summary$PIRA > 0),
              "; RAW: ", sum(summary$RAW > 0), ")")))
          if (!(event %in% c('firstCDW', 'firstCDWtype', 'firstPIRA', 'firstRAW'))) {
          message("Subjects with disability improvement: ", sum(summary$improvement > 0))
          }
          if (event %in% c('multiple', 'firstCDWtype')) {
          message("---\nCDW events: ",
              sum(summary$CDW), " (PIRA: ", sum(summary$PIRA), "; RAW: ", sum(summary$RAW), ")")
          }
          if (event %in% c('multiple','firsteach')) {
            message("Improvement events: ", sum(summary$improvement))
          }
        }

      # if (!is.null(min_value)) {
      #   message("---\n*** NOTE: only CDW to ", outcome, ">=",
      #           min_value, " is considered ***\n")
      # }
    }

    columns <- names(results_df)
    if (!include_dates) {
      columns <- columns[!endsWith(columns, "date")]
    }
    if (!include_value) {
      columns <- columns[!endsWith(columns, "value")]
    }

    scolumns <- names(summary)
    if (event %in% c('firstCDW', 'firstCDWtype', 'firstPIRA', 'firstRAW')) {
      scolumns <- scolumns[scolumns!='improvement']
    }
    if (event=='firstPIRA') {
      scolumns <- c('PIRA')
    } else if (event=='firstRAW') {
      scolumns <- c('RAW')
      columns <- columns[!startsWith(columns, "PIRA")]
    }

    summary <- summary[scolumns]
    results_df <- results_df[columns]



    for (w in warnings) {
        warning(w)
    }


    settings <- list(outcome=outcome, event=event, baseline=baseline, proceed_from=proceed_from,
                  validconf_p=ifelse(is.null(validconf_col), 1, mean(data[[validconf_col]])),
                  skip_local_extrema=skip_local_extrema,
                  conf_days=conf_days, conf_tol_days=conf_tol_days, conf_unbounded_right=conf_unbounded_right,
                  require_sust_days=require_sust_days, check_intermediate=check_intermediate,
                  relapse_to_bl=relapse_to_bl, relapse_to_event=relapse_to_event, relapse_to_conf=relapse_to_conf,
                  relapse_assoc=relapse_assoc, relapse_indep=relapse_indep, renddate_col=renddate_col,
                  sub_threshold_rebl=sub_threshold_rebl, bl_geq=bl_geq, relapse_rebl=relapse_rebl, #min_value=min_value,
                  impute_last_visit=impute_last_visit, delta_fun=delta_fun)

    output <- list(event_count=summary, results=results_df, settings=settings)
    class(output) <- 'MSprogOutput'


  return(output)
}



###############################################################################################



