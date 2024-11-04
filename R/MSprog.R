
#' Compute multiple sclerosis disability progression from longitudinal data.
#'
#' `MSprog()` detects and characterises the worsening (or improvement) events of an outcome measure
#' (EDSS, NHPT, T25FW, or SDMT; or any custom outcome) for one or more subjects, based on repeated assessments
#' through time (and on the dates of acute episodes, if any).
#' Several qualitative and quantitative options are given as arguments that can be set
#' by the user and reported as a complement to the results to ensure reproducibility.
#'
#' The events are detected sequentially by scanning the outcome values in chronological order.
#' Valid time windows for confirmation visits are determined by arguments
#' `conf_weeks`, `conf_tol_days`, `conf_unbounded_right`, `relapse_to_conf`.
#' Progression events are classified as relapse-associated or relapse-independent based on their relative timing
#' with respect to the relapses. Specifically, relapse-associated worsening (RAW) events are defined as
#' confirmed progression events occurring within a specified interval (`relapse_assoc` argument) from a relapse;
#' the definition of progression independent of relapse activity (PIRA) is established by specifying relapse-free intervals
#' around the baseline, event, and confirmation visits (`relapse_indep` argument).
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
#' @param rdate_col Name of date column for relapse data, if different from outcome data.
#' @param subjects Subset of subjects (list of IDs). If none is specified, all subjects listed in data are included.
#' @param delta_fun Custom function specifying the minimum shift corresponding to a valid change from the provided reference value.
#' It must take a numeric value (reference) as input, and return a numeric value corresponding to the minimum shift from baseline.
#' If none is specified (default), function [compute_delta()] for the specified outcome is used.
#' @param worsening The direction of worsening (`'increase'` if higher values correspond to worse disease course, `'decrease'` otherwise).<br />
#' This argument is only used when `outcome` is set to `NULL`. Otherwise, `worsening` is automatically set to
#' `'increase'` if `outcome` is set to `'edss'`, `'nhpt'`, `'t25fw'`,
#'  and to `'decrease'` if `outcome` is set to `'sdmt'`.
#' @param event Specifies which events to detect. Must be one of the following:
#' \itemize{
#' \item{`'firstprog'`}{ (first progression, default);}
#' \item{`'first'`}{ (only the very first event - improvement or progression);}
#' \item{`'firsteach'`}{ (first improvement and first progression - in chronological order);}
#' \item{`'firstprogtype'`}{ (first progression of each kind - PIRA, RAW, and undefined, in chronological order);}
#' \item{`'firstPIRA'`}{ (first PIRA);}
#' \item{`'firstRAW'`}{ (first RAW);}
#' \item{`'multiple'`}{ (all events in chronological order).}
#' }
#' @param baseline Specifies the baseline scheme. Must be one of the following:
#' \itemize{
#' \item{`'fixed'`}{ (first valid outcome value, default);}
#' \item{`'roving_impr'`}{ (updated every time the value is lower than the previous measure and confirmed at the following visit;
#' suitable for a first-progression setting to discard fluctuations around baseline);}
#' \item{`'roving'``}{ (updated after each event to last valid confirmed outcome value;
#' suitable for a multiple-event setting - i.e., when `event` is set to `'multiple'`,
#' `'firsteach'`, or `'firstprogtype'` - or when searching for a specific type of progression
#' - i.e., when `event` is set to `'firstPIRA'` or `'firstRAW'`).}
#' }
#' @param sub_threshold If `TRUE` - and only if `baseline` is `'roving'` or `'roving_impr'` - move roving baseline
#' at any sub-threshold confirmed event (i.e. any confirmed change in outcome measure, regardless of `delta_fun`).
#' @param relapse_rebl If `TRUE`, re-baseline after every relapse to search for PIRA events.
#' @param validconf_col Name of data column specifying which visits can (`T`) or cannot (`F`) be used as confirmation visits.
#' The input data does not necessarily have to include such a column.
#' If `validconf_col=NULL`, all visits are potentially used as confirmation visits.
#' @param conf_weeks Period before confirmation (weeks).
#' @param conf_tol_days Tolerance window for confirmation visit (days); can be an integer (same tolerance on left and right)
#' or list-like of length 2 (different tolerance on left and right).
#' In all cases, the right end of the interval is ignored if `conf_unbounded_right` is set to `TRUE`.
#' @param conf_unbounded_right If `TRUE`, confirmation window is unbounded on the right.
#' @param require_sust_weeks Minimum number of weeks over which a confirmed change must be sustained
#' (i.e., confirmed at \emph{all} visits occurring in the specified period) to be retained as an event.
#' Events sustained for the entire follow-up are retained regardless of follow-up duration.
#' Setting `require_sust_weeks=Inf`, events are retained only when sustained for the entire follow-up duration.<br />
#' (Warning: if `check_intermediate` is set to `FALSE`, \emph{only the end} of the specified period will be checked for confirmation.)
#' @param check_intermediate If `TRUE` (default), events are confirmed \emph{over all intermediate visits}
#' up to the confirmation visit. <br />
#' If set to `FALSE` (not recommended in most cases, as it may discard meaningful fluctuations),
#' events will be confirmed \emph{only at} the specified confirmation visit
#' (and \emph{only at the end} of the period defined by `require_sust_weeks`, if any).
#' @param relapse_to_bl Minimum distance from last relapse (days) for a visit to be used as baseline
#' (otherwise the next available visit is used as baseline).
#' @param relapse_to_event Minimum distance from last relapse (days) for an event to be considered as such.
#' @param relapse_to_conf Minimum distance from last relapse (days) for a visit to be a valid confirmation visit.
#' @param relapse_assoc Maximum distance from last relapse (days) for a progression event to be considered as RAW.
#' @param relapse_indep Specifies relapse-free intervals for PIRA definition.
#' Must be given in the form produced by function [relapse_indep_from_bounds()] by calling
#' `relapse_indep_from_bounds(b0, b1, e0, e1, c0, c1)`
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
#' @param min_value Only include progression events where the outcome is >= value.
#' @param prog_last_visit If `TRUE`, include progressions occurring at last visit (i.e. with no confirmation).
#' If a numeric value N is passed, unconfirmed events are included only if occurring within N weeks of follow up
#' (e.g., in case of early discontinuation).
#' @param date_format Format of dates in the input data. If not specified, it will be inferred by function [as.Date()].
#' @param include_dates If `TRUE`, report dates of events.
#' @param include_value If `TRUE`, report value of outcome at event.
#' @param include_stable If `TRUE`, subjects with no confirmed events are included in extended output `data.frame`,
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
#' \item{`prog_settings`: }{a list containing all the arguments used to compute the output.}
#' }
#'
#' @importFrom stats na.omit setNames complete.cases
#' @importFrom dplyr %>% group_by_at vars slice n mutate across
#' @export
#' @examples
#' # EDSS progression
#' output_edss <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output_edss$results) # extended info on each event for all subjects
#' print(output_edss$event_count) # summary of event sequence for each subject
#' # SDMT progression
#' output_sdmt <- MSprog(toydata_visits, 'id', 'SDMT', 'date', 'sdmt',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(output_sdmt$results) # extended info on each event for all subjects
#' print(output_sdmt$event_count) # summary of event sequence for each subject
MSprog <- function(data, subj_col, value_col, date_col, outcome,
                   relapse=NULL, rsubj_col=NULL, rdate_col=NULL, subjects=NULL,
                   delta_fun=NULL, worsening=NULL, event='firstprog', baseline='fixed', sub_threshold=F, relapse_rebl=F,
                   validconf_col=NULL, conf_weeks=12, conf_tol_days=30, conf_unbounded_right=F, require_sust_weeks=0, check_intermediate=T,
                   relapse_to_bl=30, relapse_to_event=0, relapse_to_conf=30, relapse_assoc=90, relapse_indep=NULL,
                   min_value=NULL, prog_last_visit=F,
                   date_format=NULL, include_dates=F, include_value=F, include_stable=T, verbose=1
                   ) {

  # SETUP

  warnings <- list()

  # If conf_tol_days is a single value, duplicate it (equal left and right tolerance)
  if (length(conf_tol_days)==1) {
    conf_tol_days <- c(conf_tol_days, conf_tol_days)
  }


  if (is.null(outcome) ||
      !(tolower(outcome) %in% c('edss', 'nhpt', 't25fw', 'sdmt'))) {
    outcome <- 'outcome'
  } else {
    outcome <- tolower(outcome)
  }


  if (is.null(relapse) || event=='firstRAW') {
    relapse_rebl <- FALSE
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
  }

  # Remove missing values from columns of interest
  data <- data[complete.cases(data[ , c(subj_col, value_col, date_col, validconf_col)]), ]
  relapse <- relapse[complete.cases(relapse[ , c(rsubj_col, rdate_col)]), ]

  # Convert dates to datetime format
  if (is.null(date_format)) {
    data[[date_col]] <- as.Date(data[[date_col]])
    relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]])
  } else {
  data[[date_col]] <- as.Date(data[[date_col]], format=date_format)
  relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]], format=date_format)
  }

  # Convert dates to days from global minimum
  if (nrow(relapse)>0) {
    global_start <- min(min(data[[date_col]]), min(relapse[[rdate_col]]))
  } else {global_start <- min(data[[date_col]])}
  data[[date_col]] <- as.numeric(difftime(data[[date_col]], global_start), units='days')
  relapse[[rdate_col]] <- as.numeric(difftime(relapse[[rdate_col]], global_start), units='days')

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
    else if (outcome=='edss' & any(data[[value_col]]>10)) {
      stop('EDSS scores >10')
    }
    else if (outcome=='sdmt' & any(data[[value_col]]>110)) {
      stop('SDMT scores >110')
    }
    else if (outcome=='nhpt' & any(data[[value_col]]>300)) {
      warnings <- c(warnings, 'NHPT scores >300')
    }
    else if (outcome=='t25fw' & any(data[[value_col]]>180)) {
      warnings <- c(warnings, 'T25FW scores >180')
    }
  }

  # Set minimum value
  if (is.null(min_value)) {
    min_value_ifany <- -Inf
  } else {
    min_value_ifany <- min_value
  }

  # If prog_last_visit==T, set no limit to follow-up length (Inf)
  if (prog_last_visit==T) {
    prog_last_visit <- Inf
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
  isevent_loc <- function(x, baseline, type='prog', st=F) {
    is_event(x, baseline, type=type, outcome=outcome, worsening=worsening,
             sub_threshold=st, delta_fun=delta_fun)
  }

  # Define a confirmation window for each value of conf_weeks
  if (conf_unbounded_right) {
    conf_tol_days[2] <- Inf
  }
  conf_window <- lapply(conf_weeks, function(t) {
    lower <- as.integer(t * 7) - conf_tol_days[1]
    # if (conf_unbounded_right) {
    #   upper <- Inf
    # } else {
      upper <- as.integer(t * 7) + conf_tol_days[2]
    # }
    return(c(lower, upper))
  })

  # Define relapse-free intervals for PIRA definition
  if (is.null(relapse_indep)) {
    relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)
  }


  #################################################################
  # Assess progression

  all_subj <- unique(data[[subj_col]])
  nsub <- length(all_subj)
  max_nevents <- round(max(table(data[[subj_col]]))/2)
  results_df <- data.frame(matrix(nrow=nsub*max_nevents, ncol=10+length(conf_weeks)*2+2)) #length(conf_weeks) + (length(conf_weeks)-1)
  allcol <- c(subj_col, 'nevent', 'event_type', 'bldate', 'blvalue', 'date', 'value', 'total_fu', 'time2event',
              'bl2event', paste0('conf', conf_weeks), paste0('PIRA_conf', conf_weeks), 'sust_days', 'sust_last')
  # if (length(conf_weeks)>1) {
  #   allcol <- c(allcol[1:(10+length(conf_weeks))],  paste0('PIRA_conf',
  #                     conf_weeks[2:length(conf_weeks)]), 'sust_days', 'sust_last')
  #   }
  colnames(results_df) <- allcol
  results_df[[subj_col]] <- rep(all_subj, each=max_nevents)
  results_df$nevent <- rep(1:max_nevents, times=nsub)

  summary <- data.frame(matrix(nrow=nsub, ncol=6))
  colnames(summary) <- c('event_sequence', 'improvement', 'progression', 'RAW', 'PIRA', 'undefined_prog')
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
    relapse_id <- relapse_id[relapse_id[[rdate_col]] >= first_visit - relapse_to_bl, ] #as.difftime(relapse_to_bl, units="days") #_d_#
    relapse_dates <- relapse_id[[rdate_col]]
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
    for (m in conf_weeks) {
      conf[[as.character(m)]] <- vector()
      pira_conf[[as.character(m)]] <- vector()}

    bl_idx <- 1 # Baseline index
    search_idx <- 2 #Index of where we are in the search
    proceed <- 1
    phase <- 0 # if post-relapse re-baseline is enabled (relapse_rebl==True),
               # phase will become 1 when re-searching for PIRA events.


    while (proceed) {

      # Set baseline (skip if within relapse influence)
      while (proceed && data_id[bl_idx,][['closest_rel_before']] < relapse_to_bl) {
        if (verbose == 2) {
          message("Baseline (visit no.", bl_idx,
                       ") is within relapse influence: moved to visit no.", bl_idx + 1)
        }
        bl_idx <- bl_idx + 1
        search_idx <- search_idx + 1
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
      }

      bl <- data_id[bl_idx, ]

      # Event detection
      change_idx <- NA
      if (search_idx<=nvisits) {
      for (x in search_idx:nvisits) {
          if (isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='change', st=sub_threshold) &
              (data_id[x,][['closest_rel_before']] >= relapse_to_event)) {
            change_idx <- x
            break
          }
      }
      }

      if (is.na(change_idx) | change_idx>nvisits) {
        proceed <- 0
        if (verbose == 2) {
          message("No ", outcome, " change in any subsequent visit: end process")
        }
      } else {
        if (change_idx==nvisits) {
          conf_idx <- list()
          conf_t <- list() #plv
          } else {
        ####### #_conf_#
        # conf_idx <- lapply(conf_window, function(t) {
        #   match_idx <- NULL
        #   for (x in (change_idx + 1):nvisits) {
        #     if (data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] >= t[1] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]])
        #         data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] <= t[2] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]]) #_d_#
        #         data_id[x,][['closest_rel_before']] >= relapse_to_conf) {
        #       match_idx <- x
        #       break
        #     }
        #   }
        #   match_idx
        # })
        # conf_t <- list()
        # for (m in 1:length(conf_weeks)) {if (!is.null(conf_idx[[m]])) {conf_t <- c(conf_t, conf_weeks[m])}}
        # conf_idx <- Filter(Negate(is.null), conf_idx)
        ####### #_conf_#
        conf_idx <- lapply(conf_window, function(t) {
          match_idx <- numeric(0)
          for (x in (change_idx + 1):nvisits) {
            if (data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] >= t[1] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]])
                data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] <= t[2] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]]) #_d_#
                data_id[x,][['closest_rel_before']] >= relapse_to_conf &&
                data_id[x,][[validconf_col]]
                ) {
              match_idx <- append(match_idx, x)
            }
          }
          match_idx
        })
        conf_t <- list()
        for (i in seq_along(conf_weeks)) {
          conf_t[[as.character(conf_weeks[i])]] <- conf_idx[[i]]
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
        if (length(conf_idx) > 0 # confirmation visits available
            && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='impr') # value improved (>delta) from baseline
            && ifelse(check_intermediate,
            all(sapply((change_idx + 1):conf_idx[[1]], function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]],
                       type='impr'))),  # improvement is confirmed at (all visits up to) first valid date
            isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='impr'))  # improvement is confirmed at first valid date
            && phase == 0 # skip if re-checking for PIRA with post-relapse re-baseline
            && !((event %in% c('firstprog', 'firstprogtype', 'firstPIRA', 'firstRAW')) && baseline=='fixed')
            ) {
          if (conf_idx[[1]]==nvisits) {
            next_change <- NA
            } else {
            next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl[[value_col]],
                               type='impr'))[1] + conf_idx[[1]]
          }
          if (!is.na(next_change)) {
            conf_idx <- conf_idx[conf_idx < next_change]
          }
          #_conf_# conf_t <- conf_t[seq_along(conf_idx)]


          if (conf_idx[[1]]<nvisits) { #conf_idx[[length(conf_idx)]]
            next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                               bl[[value_col]], type='impr'))[1] + conf_idx[[1]] # improvement not sustained
            # next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[length(conf_idx)]] + 1):nvisits, value_col],
            #                                    bl[[value_col]], type='impr'))[1] + conf_idx[[1]] # improvement not sustained
          } else {next_nonsust <- NA}


          valid_impr <- 1
          if (require_sust_weeks>0) {
              if ((!check_intermediate)
                  && data_id[nvisits, date_col] - data_id[change_idx, date_col] >= require_sust_weeks * 7) { # follow-up lasts at least require_sust_weeks
                sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                      - data_id[change_idx,][[date_col]] >= require_sust_weeks * 7)[1] + change_idx
              } else {
                sust_vis <- nvisits
                }
            valid_impr <- ifelse(check_intermediate,
                is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]]
                                 - data_id[change_idx, date_col]) >= require_sust_weeks * 7, # improvement sustained up to end of follow-up, or for `require_sust_weeks` weeks
                isevent_loc(data_id[sust_vis,][[value_col]], bl[[value_col]], type='impr') # improvement confirmed at last visit, or first visit after `require_sust_weeks` weeks
            )
          }
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
              #### #_conf_#
              # conf[[as.character(m)]] <- c(conf[[as.character(m)]], as.integer(m %in% conf_t))
              #### #_conf_#
              confirmed_at <- intersect(conf_t[[cm]], conf_idx)
              if (length(confirmed_at)==0) {
                within(conf_t, rm(list=cm))
              }
              conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
              #### #_conf_#
              pira_conf[[cm]] <- c(pira_conf[[cm]], NA)}
            sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
            sustl <- c(sustl, as.integer(sust_idx == nvisits))


            # Print info
            if (verbose == 2) {
              message(outcome, " improvement (visit no.", change_idx, ", ",
                      global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                           ") confirmed at ", paste(names(conf_t), collapse=", "), " weeks, sustained up to visit no.", sust_idx,
                           " (", global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")")
            }
          } else {
            # Not sustained
            if (verbose == 2) {
              message("Change confirmed but not sustained over ",
                      ifelse(require_sust_weeks<Inf, paste(">=", require_sust_weeks, "weeks"),
                             "entire follow-up"), ": proceed with search")
            }
          }


          # For each m in conf_weeks, only keep the earliest available confirmation visit
          conf_idx <- unname(sapply(names(conf_t), function(cm) {
            min(intersect(conf_t[[cm]], conf_idx))
          })) #_conf_#

          # # next change from last confirmation
          # next_change <- NA
          # if (conf_idx[[length(conf_idx)]]<nvisits) {
          #   for (x in (conf_idx[[length(conf_idx)]] + 1):nvisits) {
          #     if ((data_id[x,][[value_col]] - bl[[value_col]]) > -delta(bl[[value_col]]) ||
          #         abs(data_id[x,][[value_col]] - data_id[conf_idx[[length(conf_idx)]],][[value_col]])
          #         >= delta(data_id[conf_idx[[length(conf_idx)]],][[value_col]])) {
          #       next_change <- x
          #       break
          #     }
          #   }
          # }
          # next change from first confirmation #_r_#
          next_change <- NA
          if (conf_idx[[1]]<nvisits) {
            for (x in (conf_idx[[1]] + 1):nvisits) {
              if (!isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='impr') # either improvement not sustained
                  || isevent_loc(data_id[x,][[value_col]], data_id[conf_idx[[1]],][[value_col]],
                                 type='change')) { # or further valid change from confirmation
                next_change <- x
                break
              }
            }
          }

          # Move the search index, and optionally the baseline
          if (baseline %in% c('roving', 'roving_impr')) { #_r_#
            bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1)
            search_idx <- bl_idx + 1
          } else if (valid_impr) {
            search_idx <- ifelse(is.na(next_change), nvisits, next_change)
          } else {
            search_idx <- change_idx + 1
          }

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
          && sub_threshold
          && phase == 0) { # skip if re-checking for PIRA after post-relapse re-baseline

        if (conf_idx[[1]]==nvisits) {
          next_change <- NA} else {
            next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                 bl[[value_col]], type='impr', st=T))[1] + conf_idx[[1]] }
        bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1) # set new baseline at last consecutive improved value
        search_idx <- bl_idx + 1 #next_change
        if (verbose == 2) {
          message("Confirmed sub-threshold ", outcome, " improvement (visit no.", change_idx, ")")
          message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                       ifelse(search_idx<=nvisits, search_idx, "-"), " on")
        }
      }

      # CONFIRMED PROGRESSION:
      # ---------------------
      else if (data_id[change_idx,][[value_col]] >= min_value_ifany
         && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='prog')  # value worsened (>delta) from baseline

         && ((length(conf_idx) > 0 && # confirmation visits available
           ifelse(check_intermediate,
              all(sapply((change_idx + 1):conf_idx[[1]],
               function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='prog'))),  # progression is confirmed at (all visits up to) first valid date
              isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='prog') # progression is confirmed at first valid date
           ) &&
          all(sapply((change_idx + 1):conf_idx[[1]],
              function(x) data_id[x,][[value_col]] >= min_value_ifany)) # confirmation above min_value too
          ) || (data_id[change_idx,][[date_col]] - data_id[1,][[date_col]] <= prog_last_visit*7 && change_idx == nvisits))

         ) {

                 if (change_idx == nvisits) {
                   conf_idx <- c(nvisits)
                 }
                if (conf_idx[[1]]==nvisits) {
                  next_change <- NA
                  } else {
                  next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl[[value_col]],
                                          type='prog'))[1] + conf_idx[[1]] }
                 if (!is.na(next_change)) {
                    conf_idx <- conf_idx[conf_idx < next_change] } # confirmed dates
                 #_conf_# conf_t <- conf_t[seq_along(conf_idx)]



                if (conf_idx[[1]]<nvisits) { #conf_idx[[length(conf_idx)]]
                  # next non-sustained value:
                  next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                                     bl[[value_col]], type='prog'))[1] + conf_idx[[1]]
                  # next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[length(conf_idx)]] + 1):nvisits, value_col],
                  #                                    bl[[value_col]], type='prog'))[1] + conf_idx[[1]]
                } else {
                  next_nonsust <- NA
                }


                valid_prog <- 1
                if (require_sust_weeks>0) {
                  if ((!check_intermediate)
                      && data_id[nvisits, date_col] - data_id[change_idx, date_col] >= require_sust_weeks * 7) { # follow-up lasts at least require_sust_weeks
                    sust_vis <- which(data_id[(change_idx + 1):nvisits, date_col]
                                      - data_id[change_idx,][[date_col]] >= require_sust_weeks * 7)[1] + change_idx
                    } else {sust_vis <- nvisits}
                  valid_prog <- ifelse(check_intermediate,
                    is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]] -
                                data_id[change_idx,][[date_col]]) > require_sust_weeks * 7, # progression sustained up to end of follow-up, or for `require_sust_weeks` weeks
                    isevent_loc(data_id[sust_vis,][[value_col]], bl[[value_col]], type='prog') # progression confirmed at last visit, or first visit after `require_sust_weeks` weeks
                  )
                }

                if (valid_prog) {

                  nev <- length(event_type)

                  sust_idx <- ifelse(is.na(next_nonsust), nvisits, next_nonsust - 1)

                  if (phase == 0 && data_id[change_idx,][['closest_rel_before']] <= relapse_assoc) { # event is relapse-associated
                    if (event=='firstPIRA' & baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for PIRA with a fixed baseline
                      next
                    }
                    event_type <- c(event_type, 'RAW')
                    event_index <- c(event_index, change_idx)
                  } else if (data_id[change_idx,][['closest_rel_before']] > relapse_assoc) { # event is not relapse-associated
                    if (event=='firstRAW' & baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for RAW with a fixed baseline
                      next
                    }

                    # rel_inbetween <- sapply(conf_idx,
                    #           function(ic) any(is_rel[date_dict[[as.character(bl_idx)]]:date_dict[[as.character(ic)]]]))


                    # Compute intervals that must be relapse-free for PIRA definition
                    left <- right <- list()
                    for (iic in 1:length(conf_idx)) {

                      left[[iic]] <- list()
                      right[[iic]] <- list()

                      ic <- conf_idx[[iic]]

                      for (point in c('bl', 'event', 'conf')) {
                        t <- ifelse(point == 'bl', bl[[date_col]],
                                    ifelse(point == 'event', data_id[change_idx,][[date_col]],
                                           data_id[ic,][[date_col]]))

                        if (!is.null(relapse_indep[[point]][[1]])) {
                          t0 <- t - relapse_indep[[point]][[1]]
                        }

                        if (!is.null(relapse_indep[[point]][[2]])) {
                          t1 <- t + relapse_indep[[point]][[2]]

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


                    pconf_idx <- conf_idx[!rel_inbetween]
                    if (length(pconf_idx) > 0) {
                      ######### #_conf_#
                      # if (length(pconf_idx) > 0
                      #   && data_id[pconf_idx[[length(pconf_idx)]], 'closest_rel_after'] < relapse_to_conf) {
                      #   pconf_idx <- pconf_idx[-length(pconf_idx)]
                      # }
                      # pconf_t <- conf_t[seq_along(pconf_idx)]
                      # if (length(pconf_idx) > 0) {
                      #   for (m in conf_weeks) {
                      #     pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], as.integer(m %in% pconf_t))}
                      ######### #_conf_#
                      pconf_t <- conf_t
                      for (cm in names(conf_t)) {
                        confirmed_at <- intersect(pconf_t[cm], pconf_idx)
                        if (length(confirmed_at)==0) {
                          within(pconf_t, rm(list=cm))
                        }
                        pira_conf[[cm]] <- c(pira_conf[[cm]], as.integer(length(confirmed_at)>0))
                      }
                      ######### #_conf_#
                      event_type <- c(event_type, 'PIRA')
                      event_index <- c(event_index, change_idx)
                    } else if (phase == 0) {
                      event_type <- c(event_type, 'prog')
                      event_index <- c(event_index, change_idx)
                    }
                  }

                  if (phase==0 & event_type[length(event_type)] != 'PIRA') {
                    for (m in conf_weeks) {
                        pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], NA)}
                  }

                  if (event_type[length(event_type)] == 'PIRA' || phase == 0) {
                    bldate <- c(bldate, as.character(global_start + as.difftime(bl[[date_col]], units='days')))
                    blvalue <- c(blvalue, bl[[value_col]])
                    edate <- c(edate, as.character(global_start + as.difftime(data_id[change_idx,][[date_col]], units='days')))
                    evalue <- c(evalue, data_id[change_idx,][[value_col]])
                    bl2event <- c(bl2event, data_id[change_idx,][[date_col]] - bl[[date_col]])
                    time2event <- c(time2event, data_id[change_idx,][[date_col]] - data_id[1,][[date_col]])
                    ######### #_conf_#
                    # for (m in conf_weeks) {
                    #   conf[[as.character(m)]] <- c(conf[[as.character(m)]], as.integer(m %in% conf_t))
                    # }
                    ######### #_conf_#
                    for (cm in names(conf_t)) {
                      confirmed_at <- intersect(conf_t[cm], conf_idx)
                      if (length(confirmed_at)==0) {
                        within(conf_t, rm(list=cm))
                      }
                      conf[[cm]] <- c(conf[[cm]], as.integer(length(confirmed_at)>0))
                    }
                    ######### #_conf_#
                    sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
                    sustl <- c(sustl, as.integer(sust_idx == nvisits))

                    # Print info
                    if (verbose == 2) {
                      message(outcome, " progression[", event_type[length(event_type)],
                                   "] (visit no.", change_idx, ", ",
                                  global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                              ifelse(length(conf_t)>0, paste0(") confirmed at ",
                              paste(ifelse(event_type[length(event_type)]=='PIRA', names(pconf_t), names(conf_t)), collapse=", "),
                                  " weeks, sustained up to visit no.", sust_idx,
                                   " (", global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")"),
                              ") occurring at last assessment (no confirmation)")
                              )
                    }
                  }

                  } else {
                    # Not sustained
                    if (verbose == 2) {
                      message("Change confirmed but not sustained over ",
                              ifelse(require_sust_weeks<Inf, paste(">=", require_sust_weeks, "weeks"),
                                     "entire follow-up"), ": proceed with search")
                    }
                  }



                if (length(conf_t)>0) {

                  # For each m in conf_weeks, only keep the earliest available confirmation visit
                  conf_idx <- unname(sapply(names(conf_t), function(cm) {
                    min(intersect(conf_t[[cm]], conf_idx))
                  })) #_conf_#


                  # # next event from last confirmation
                  # next_change <- NA
                  # if (conf_idx[[length(conf_idx)]]<nvisits) {
                  #   for (x in (conf_idx[[length(conf_idx)]] + 1):nvisits) {
                  #     if (data_id[x,][[value_col]] - bl[[value_col]] < delta(bl[[value_col]]) || # either not sustained
                  #         abs(data_id[x,][[value_col]] - data_id[conf_idx[[length(conf_idx)]],][[value_col]])
                  #         >= delta(data_id[conf_idx[[length(conf_idx)]],][[value_col]])) {  # or further change from *last* confirmation
                  #       next_change <- x
                  #       break
                  #     }
                  #   }
                  # }

                  #_r_#
                  # next valid change from first confirmation visit:
                  next_change <- NA
                  if (conf_idx[[1]]<nvisits) {
                    for (x in (conf_idx[[1]] + 1):nvisits) {
                      if (!isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='prog') # either not sustained
                          || isevent_loc(data_id[x,][[value_col]],  data_id[conf_idx[[1]],][[value_col]],
                                         type='change')) {  # or further change from *first* confirmation
                        next_change <- x
                        break
                      }
                    }
                  }
                  # next valid change from event:
                  next_change_ev <- which(isevent_loc(data_id[(change_idx + 1):nvisits, value_col],
                                                      bl[[value_col]], type='change'))[1] + change_idx

                }


              # Move the search index, and optionally the baseline
              if (length(conf_t) == 0 # progression occurring at last visit
                || (phase==1 && length(event_type)==nev)) { # ongoing relapse-based rebaseline, and the progression found is not PIRA
                search_idx <- change_idx + 1
              } else if (baseline == 'roving' && phase == 0) {
                bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1) # set new baseline at first confirmation time
                search_idx <- bl_idx + 1
              } else if (phase==0 && ((event_type[length(event_type)]!='PIRA' & event=='firstPIRA')
                                      || (event_type[length(event_type)]!='RAW' & event=='firstRAW')
                                      || !valid_prog)) {
                search_idx <- ifelse(is.na(next_change_ev), nvisits, next_change_ev) #_r_#
              } else {
                search_idx <- ifelse(is.na(next_change), nvisits+1, next_change)
              }

              if (verbose == 2 && phase == 0) {
                message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                        ifelse(search_idx > nvisits, "-", search_idx), " on")
              }


          }

       # Confirmed sub-threshold progression: RE-BASELINE
       # ------------------------------------------------
      else if (length(conf_idx) > 0
               && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='prog', st=T)
               && ifelse(check_intermediate,
                    all(sapply((change_idx + 1):conf_idx[[1]], function(x)
                      isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='prog', st=T))), # (sub-threshold) progression confirmed over (all visits up to) first valid date
                    isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='prog', st=T))  # (sub-threshold) progression confirmed at first valid date
               && baseline == 'roving'
               && sub_threshold && phase == 0) {
        if (conf_idx[[1]]==nvisits) {
          next_change <- NA
          } else {
          next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                              bl[[value_col]], type='prog', st=T))[1] + conf_idx[[1]] }
        bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1)
        search_idx <- bl_idx + 1
        if (verbose == 2) {
          message("Confirmed sub-threshold", outcome, "progression (visit no.", change_idx, ")")
          message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                  ifelse(search_idx > nvisits, "-", search_idx), " on")
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


        if (relapse_rebl && length(relapse_dates)>0 && phase == 0 && !proceed) {
          phase <- 1
          proceed <- 1
          bl_idx <- 1
          search_idx <- 2
          if (verbose == 2) {
            message("Completed search with fixed baseline, re-search for PIRA events with post-relapse rebaseline")
          }
        }

        if (proceed && ((event == "first" && length(event_type) > 1) ||
                        (event == "firsteach" && ("impr" %in% event_type) && ("prog" %in% event_type)) ||
                        (event == "firstprog" && (("RAW" %in% event_type) || ("PIRA" %in% event_type) || ("prog" %in% event_type))) ||
                        (event == "firstprogtype" && ("RAW" %in% event_type) && ("PIRA" %in% event_type) && ("prog" %in% event_type)) ||
                        (event == "firstPIRA" && ("PIRA" %in% event_type)) ||
                        (event == "firstRAW" && ("RAW" %in% event_type)))
            ) {
                          proceed <- 0
                          if (verbose == 2) {
                            message("\'", event, "\'", " events already found: end process")
                          }
                        }

        if (proceed && search_idx <= nvisits &&
            # any(is_rel[date_dict[[as.character(bl_idx)]]:date_dict[[as.character(search_idx)]]])
            any((data_id[bl_idx,][[date_col]]<=relapse_dates) & (relapse_dates<=data_id[search_idx,][[date_col]])) # if search_idx has been moved after another relapse
            && relapse_rebl && phase == 1) {
          if (bl_idx < nvisits) {
            bl_idx <- sapply(bl_idx, function (ib) {
            out <- NA
            for (x in (ib + 1):nvisits) { # visits after current baseline (or after last confirmed PIRA)
              if (#any(is_rel[date_dict[[as.character(ib)]]:date_dict[[as.character(x)]]])
                 any((data_id[ib,][[date_col]]<=relapse_dates) & (relapse_dates<=data_id[x,][[date_col]])) # after a relapse
                  & (data_id[x,][['closest_rel_before']] >= relapse_to_bl) # out of relapse influence
                  ){
                out <- x
                break
              }
            }
            out
            })
          } else {bl_idx <- NA}

          if (!is.na(bl_idx)) {
            search_idx <- bl_idx + 1
            if (verbose == 2) {
              message("[post-relapse rebaseline] Baseline at visit no.", bl_idx,
                          ", searching for events from visit no.",
                      ifelse(search_idx > nvisits, "-", search_idx), " on")
            }
          }
          if (proceed && (is.na(bl_idx) || bl_idx > nvisits - 1)) {
            proceed <- 0
            if (verbose == 2) {
              message("Not enough visits after current baseline: end process")
            }
          }
    } else if (proceed && search_idx <= nvisits && relapse_rebl && phase == 1 &&
               !any((data_id[bl_idx,][[date_col]]<=relapse_dates)
                    & (relapse_dates<=data_id[search_idx,][[date_col]])) # if search_idx is still
               && verbose==2) {
      message("[post-relapse rebaseline] Baseline at visit no.", bl_idx, ", searching for events from visit no.",
              ifelse(search_idx > nvisits, "-", search_idx), " on")
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
      prog_idx <- which(event_type %in% c("prog", "RAW", "PIRA"))[1]
      raw_idx <- which(event_type == "RAW")[1]
      pira_idx <- which(event_type == "PIRA")[1]
      undef_prog_idx <- which(event_type == "prog")[1]

      if (event == "firsteach") {
        first_events <- c(impr_idx, prog_idx)
      } else if (event == "firstprog") {
        first_events <- c(prog_idx)
      } else if (event == "firstprogtype") {
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

    if ((length(event_type)==0) & include_stable) {
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
    for (m in conf_weeks) {
      results_df[results_df[[subj_col]] == subjid, paste0("conf", m)] <- conf[[as.character(m)]][event_order]
      }
    results_df[results_df[[subj_col]] == subjid, "sust_days"] <- sustd[event_order]
    results_df[results_df[[subj_col]] == subjid, "sust_last"] <- sustl[event_order]
    for (m in conf_weeks) {
      # if (m!=conf_weeks[1]) {
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
  progression <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] %in% c("prog", "RAW", "PIRA"))
  undefined_prog <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "prog")
  RAW <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "RAW")
  PIRA <- sum(results_df[results_df[[subj_col]] == subjid, "event_type"] == "PIRA")

  summary[as.character(subjid), c('improvement', 'progression', 'RAW', 'PIRA', 'undefined_prog'
          )] <- c(improvement, progression, RAW, PIRA, undefined_prog)

  summary[as.character(subjid), 'event_sequence'] <- paste(event_type, collapse=", ")

  if (startsWith(event, "firstprog")) {
    summary <- summary[, !colnames(summary) %in% "improvement"]
  }

  if (verbose == 2) {
    message("Event sequence: ", ifelse(length(event_type) > 0,
                              paste(event_type, collapse=", "), "-"), sep="")
  }


  } #for (subjid in all_subj)

  #################################################################


    if (verbose >= 1) {
      message(paste0("\n---\nOutcome: ", outcome, "\nConfirmation", ifelse(check_intermediate, " over: ", " at: "),
            paste(conf_weeks, collapse=", "), " weeks (-", conf_tol_days[1], " days, +",
            ifelse(conf_unbounded_right, "Inf", conf_tol_days[2]), " days)\nBaseline: ", baseline,
            ifelse(sub_threshold, " (sub-threshold)", ""),
            ifelse(relapse_rebl, ", and post-relapse re-baseline", ""),
            "\nRelapse influence (baseline): ", relapse_to_bl, " days\nRelapse influence (event): ",
            relapse_to_event, " days\nRelapse influence (confirmation): ", relapse_to_conf, " days\nEvents detected: ", event))
      if (is.null(subjects) | length(subjects)>1) {
          message("\n---\nTotal subjects: ", nsub,
              "\n---\nProgressed subjects: ", sum(summary$progression > 0), " (PIRA: ", sum(summary$PIRA > 0),
              "; RAW: ", sum(summary$RAW > 0), ")")
          if (!(event %in% c('firstprog', 'firstprogtype', 'firstPIRA', 'firstRAW'))) {
          message("Improved subjects: ", sum(summary$improvement > 0))
          }
          if (event %in% c('multiple', 'firstprogtype')) {
          message("---\nProgression events: ",
              sum(summary$progression), " (PIRA: ", sum(summary$PIRA), "; RAW: ", sum(summary$RAW), ")")
          }
          if (event %in% c('multiple','firsteach')) {
            message("Improvement events: ", sum(summary$improvement))
          }
        }

      if (!is.null(min_value)) {
        message("---\n*** NOTE: only progressions to ", outcome, ">=",
                min_value, " are considered ***\n")
      }
    }

    columns <- names(results_df)
    if (!include_dates) {
      columns <- columns[!endsWith(columns, "date")]
    }
    if (!include_value) {
      columns <- columns[!endsWith(columns, "value")]
    }

    scolumns <- names(summary)
    if (event %in% c('firstprog', 'firstprogtype', 'firstPIRA', 'firstRAW')) {
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


    prog_settings <- list(outcome=outcome, event=event, baseline=baseline,
                  conf_weeks=conf_weeks, conf_tol_days=conf_tol_days, conf_unbounded_right=conf_unbounded_right,
                  require_sust_weeks=require_sust_weeks, check_intermediate=check_intermediate,
                  relapse_to_bl=relapse_to_bl, relapse_to_event=relapse_to_event, relapse_to_conf=relapse_to_conf,
                  relapse_assoc=relapse_assoc, relapse_indep=relapse_indep,
                  sub_threshold=sub_threshold, relapse_rebl=relapse_rebl, min_value=min_value,
                  prog_last_visit=prog_last_visit, delta_fun=delta_fun)

    output <- list(event_count=summary, results=results_df, prog_settings=prog_settings)
    class(output) <- 'MSprogOutput'


  return(output) #return(list(summary, results_df)) #
}



###############################################################################################



