
#' Compute multiple sclerosis progression from longitudinal data.
#'
#' `MSprog()` detects and characterises the progression (or improvement) events of an outcome measure
#' (EDSS, NHPT, T25FW, or SDMT) for one or more subjects, based on repeated assessments
#' through time and on the dates of acute episodes.
#' Several qualitative and quantitative options are given as arguments that can be set
#' by the user and reported as a complement to the results to ensure reproducibility.
#'
#' The events are detected sequentially by scanning the outcome values in chronological order.
#' Progression events are classified as relapse-associated or relapse-independent based on their relative timing
#' with respect to the relapses. Specifically, relapse-associated worsening (RAW) events
#' are defined as confirmed progression events occurring within the influence of a relapse,
#' while progression independent of relapse activity (PIRA) is established when the progression
#' event occurs out of relapse influence, and with no relapses between baseline and confirmation.
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
#' @param subjects Subset of subjects (list of IDs). If none is specified, all subjects listed in data are included.
#' @param date_format Format of dates in the input data. If not specified, it will be inferred by function [as.Date].
#' @param relapse `data.frame` containing longitudinal data, including: subject ID and relapse date.
#' @param rsubj_col Name of subject ID column for relapse data, if different from outcome data.
#' @param rdate_col Name of date column for relapse data, if different from outcome data.
#' @param delta_fun Custom function specifying the minimum shift corresponding to a valid change from the provided reference value.
#' It must take a numeric value (reference) as input, and return a numeric value corresponding to the minimum shift from baseline.
#' If none is specified (default), function [compute_delta()] for the specified outcome is used.
#' @param worsening The direction of worsening (`'increase'` if higher values correspond to worse disease course, `'decrease'` otherwise).
#' This argument is only used when `outcome` is set to `NULL`. Otherwise, `worsening` is automatically set to
#' `'increase'` if `outcome` is set to `'edss`, `'nhpt`, `'t25fw`,
#'  and to `'decrease'` if `outcome` is set to `'sdmt`.
#' @param conf_weeks Period before confirmation (weeks).
#' @param conf_tol_days Tolerance window for confirmation visit (days); can be an integer (same tolerance on left and right)
#' or list-like of length 2 (different tolerance on left and right).
#' In all cases, the right end of the interval is ignored if `conf_unbounded_right` is set to `TRUE`.
#' @param conf_unbounded_right If `TRUE`, confirmation window is unbounded on the right.
#' @param require_sust_weeks Minimum number of weeks for which a confirmed change must be sustained to be retained as an event.
#' @param relapse_to_bl Minimum distance from last relapse (days) for a visit to be used as baseline
#' (otherwise the next available visit is used as baseline).
#' @param relapse_to_event Minimum distance from last relapse (days) for an event to be considered as such.
#' @param relapse_to_conf Minimum distance from last relapse (days) for a visit to be a valid confirmation visit.
#' @param relapse_assoc Maximum distance from last relapse (days) for a progression event to be considered as RAW.
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
#' @param relapse_indep Specifies relapse-free intervals for PIRA definition.
#' Must be given in the form produced by function [relapse_indep_from_bounds()] by calling
#' `relapse_indep_from_bounds(b0, b1, e0, e1, c0, c1)`
#' to specify the intervals around baseline (`b0` and `b1`),
#' event (`e0` and `e1`), and confirmation (`c0` and `c1`). For instance:
#' \itemize{
#' \item{\[Muller JAMA Neurol 2023\]}{ No relapses within event-90dd->event+30dd and within confirmation-90dd->confirmation+30dd:
#' \cr`relapse_indep <- relapse_indep_from_bounds(0,0,90,30,90,30)` (default);}
#' \item{\[Muller JAMA Neurol 2023\](high-specificity definition)}{ No relapses between baseline and confirmation:
#' \cr\`relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,NULL,NULL,0)`;}
#' \item{\[Kappos JAMA Neurol 2020\]}{ No relapses within baseline->event+30dd and within confirmation+-30dd:
#' \cr`relapse_indep <- relapse_indep_from_bounds(0,NULL,NULL,30,30,30)`}
#' }
#' @param sub_threshold If `TRUE` - and only if `baseline` is `'roving'` or `'roving_impr'` - move roving baseline
#' at any sub-threshold confirmed event (i.e. any confirmed change in outcome measure, regardless of `delta_fun`).
#' @param relapse_rebl If `TRUE`, re-baseline after every relapse to search for PIRA events.
#' @param min_value Only consider progressions events where the outcome is >= value.
#' @param prog_last_visit If `TRUE`, include progressions occurring at last visit (i.e. with no confirmation).
#' If a numeric value N is passed, unconfirmed events are included only if occurring within N weeks of follow up
#' (e.g., in case of early discontinuation).
#' @param include_dates If `TRUE`, report dates of events.
#' @param include_value If `TRUE`, report value of outcome at event.
#' @param include_stable If `TRUE`, subjects with no events are included in extended output `data.frame`,
#' with `time2event` = total follow up.
#' @param verbose One of:
#' \itemize{
#'  \item{0}{ (print no info);}
#'  \item{1}{ (print concise info, default);}
#'  \item{2}{ (print extended info).}
#'  }
#' @param devtest_conf (to be removed soon) Temporary test - for developer's use only.
#'
#' @return An object of class `'MSprogOutput'`.
#' @importFrom stats na.omit setNames complete.cases
#' @importFrom dplyr %>% group_by_at vars slice n mutate across
#' @export
#' @examples
#' # EDSS progression
#' output_edss <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(results(output_edss)) # extended info on each event for all subjects
#' print(event_count(output_edss)) # summary of event sequence for each subject
#' # SDMT progression
#' output_sdmt <- MSprog(toydata_visits, 'id', 'SDMT', 'date', 'sdmt',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=1)
#' print(results(output_sdmt)) # extended info on each event for all subjects
#' print(event_count(output_sdmt)) # summary of event sequence for each subject
MSprog <- function(data, subj_col, value_col, date_col, outcome, subjects=NULL, date_format=NULL,
                   relapse=NULL, rsubj_col=NULL, rdate_col=NULL, delta_fun=NULL, worsening=NULL,
                   conf_weeks=12, conf_tol_days=30, conf_unbounded_right=FALSE, require_sust_weeks=0,
                   relapse_to_bl=30, relapse_to_event=0, relapse_to_conf=30, relapse_assoc=90,
                   event='firstprog', baseline='fixed', relapse_indep=NULL, sub_threshold=FALSE, relapse_rebl=FALSE,
                   min_value=NULL, prog_last_visit=FALSE, include_dates=FALSE, include_value=FALSE,
                   include_stable=TRUE, verbose=1,
                   devtest_conf=FALSE # developer tests
                   ) {

  # SETUP

  warnings <- list()

  if (length(conf_tol_days)==1) {
    conf_tol_days <- c(conf_tol_days, conf_tol_days)
  }

  if (is.null(outcome) ||
      !(tolower(outcome) %in% c('edss', 'nhpt', 't25fw', 'sdmt'))) {
    outcome <- 'outcome'
  } else {
    outcome <- tolower(outcome)
  }


  if (is.null(relapse)) {
    relapse_rebl <- FALSE
  }

  if (is.null(rsubj_col)) {
    rsubj_col <- subj_col
  }
  if (is.null(rdate_col)) {
    rdate_col <- date_col
  }

  if (is.null(relapse)) {
    relapse <- data.frame(matrix(nrow=0, ncol=2))
    names(relapse) <- c(rsubj_col, rdate_col)
  }


  # Remove missing values from columns of interest
  data <- data[complete.cases(data[ , c(subj_col, value_col, date_col)]), ]
  relapse <- relapse[complete.cases(relapse[ , c(rsubj_col, rdate_col)]), ]

  # Convert dates to datetime format
  if (is.null(date_format)) {
    data[[date_col]] <- as.Date(data[[date_col]])
    relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]])
  } else {
  data[[date_col]] <- as.Date(data[[date_col]], format=date_format)
  relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]], format=date_format)
  }
  # Convert dates to days from minimum
  if (nrow(relapse)>0) {
    global_start <- min(min(data[[date_col]]), min(relapse[[rdate_col]]))
  } else {global_start <- min(data[[date_col]])}

  data[[date_col]] <- as.numeric(difftime(data[[date_col]], global_start), units='days')
  relapse[[rdate_col]] <- as.numeric(difftime(relapse[[rdate_col]], global_start), units='days')

  if (!is.null(subjects)) {
  data <- data[data[[subj_col]] %in% subjects,]
  relapse <- relapse[relapse[[rsubj_col]] %in% subjects,]
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

  if (is.null(min_value)) {
    min_value_ifany <- min(data[[value_col]]) - 1
  } else {
    min_value_ifany <- min_value
  }

  if (prog_last_visit==T) {
    prog_last_visit <- Inf
  }


  # # Define progression delta
  # if (is.null(delta_fun) & outcome=='outcome') {
  #   stop('Either specify a valid outcome type (`outcome` argument) or provide a custom `delta_fun`')
  # } else {
  #   delta <- function(value) {
  #     if (is.null(delta_fun)) {
  #       return(compute_delta(value, outcome))
  #     } else {
  #       return(delta_fun(value))
  #     }
  #   }
  # }
  if (outcome %in% c('edss', 'nhpt', 't25fw')) {
    worsening <- 'increase'
  } else if (outcome=='sdmt') {
    worsening <- 'decrease'
  } else if (is.null(worsening)) {
    stop('Either specify an outcome type, or specify the direction of worsening (\'increase\' or \'decrease\')')
  }

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

    # all_dates <- unique(c(data_id[[date_col]], relapse_dates))
    # sorted_ind <- order(all_dates)
    # all_dates <- all_dates[sorted_ind]
    # is_rel <- all_dates %in% relapse_dates
    # date_dict <- setNames(1:length(all_dates), sorted_ind)

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
      data_id$closest_rel_minus <- if (all(is.na(distm))) Inf else apply(distm, 1, min, na.rm=TRUE)
      data_id$closest_rel_plus <- if (all(is.na(distp))) Inf else apply(distp, 1, min, na.rm=TRUE)
    } else {
      data_id$closest_rel_minus <- Inf
      data_id$closest_rel_plus <- Inf
    }


    event_type <- ""
    event_index <- NULL
    bldate <- edate <- blvalue <- evalue <- time2event <- bl2event <- sustd <- sustl <- vector()
    conf <- pira_conf <- list()
    for (m in conf_weeks) {
      conf[[as.character(m)]] <- vector()
      # if (m!=conf_weeks[1]) {
        pira_conf[[as.character(m)]] <- vector()}
    # }

    bl_idx <- 1
    search_idx <- 2
    proceed <- 1
    phase <- 0


    while (proceed) {

      # Set baseline (skip if within relapse influence)
      while (proceed && data_id[bl_idx,][['closest_rel_minus']] < relapse_to_bl) {
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

      # if (bl_idx > nvisits - 1) {
      #   break
      # }
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
              (data_id[x,][['closest_rel_minus']] >= relapse_to_event)) {
            change_idx <- x
            break
          }
      }
      }
      #
      # if (search_idx<=nvisits) {
      # change_idx1 <- match(TRUE, data_id[search_idx:nvisits, value_col] != bl[[value_col]])
      # if (!is.na(change_idx1)) {
      #   change_idx1 <- search_idx + change_idx1 - 1
      # } } else {change_idx1 <- NA}
      #
      # if ((is.na(change_idx) & !is.na(change_idx1)) ||
      #     (is.na(change_idx1) & !is.na(change_idx)) ||
      #     (!is.na(change_idx1) & !is.na(change_idx) & change_idx!=change_idx1)) {stop('uffa')}
      #


      if (is.na(change_idx) | change_idx>nvisits) {
        proceed <- 0
        if (verbose == 2) {
          message("No ", outcome, " change in any subsequent visit: end process")
        }
      } else {
        if (change_idx==nvisits) {
          conf_idx=list()
          conf_t <- list() #plv
          } else {
        conf_idx <- lapply(conf_window, function(t) {
          match_idx <- NULL
          for (x in (change_idx + 1):nvisits) {
            if (data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] >= t[1] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]])
                data_id[x,][[date_col]] - data_id[change_idx,][[date_col]] <= t[2] && #difftime(data_id[x,][[date_col]], data_id[change_idx,][[date_col]]) #_d_#
                data_id[x,][['closest_rel_minus']] >= relapse_to_conf) {
              match_idx <- x
              break
            }
          }
          match_idx
        })

        conf_t <- list()
        for (m in 1:length(conf_weeks)) {if (!is.null(conf_idx[[m]])) {conf_t <- c(conf_t, conf_weeks[m])}}
        conf_idx <- Filter(Negate(is.null), conf_idx)
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
            && isevent_loc(data_id[change_idx,][[value_col]], bl[[value_col]], type='impr') # value decreased (>delta) from baseline
            && all(sapply((change_idx + 1):conf_idx[[1]], function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]],
                       type='impr')))  # decrease is confirmed at all visits between event and confirmation visit
            && phase == 0 # skip if re-checking for PIRA after post-relapse re-baseline
            && !((event %in% c('firstprog', 'firstprogtype', 'firstPIRA', 'firstRAW')) && baseline=='fixed')
            ) {
          if (conf_idx[[1]]==nvisits) {next_change <- NA} else {
          next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl[[value_col]],
                               type='impr'))[1] + conf_idx[[1]]
          }
          if (!is.na(next_change)) {
            conf_idx <- conf_idx[conf_idx < next_change]
          }
          conf_t <- conf_t[seq_along(conf_idx)]

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
          #   next_nonsust <- which(data_id[(conf_idx[[length(conf_idx)]] + 1):nvisits, value_col]
          #                         - bl[[value_col]] > -delta(bl[[value_col]]))[1] + conf_idx[[length(conf_idx)]]
          # } else {next_nonsust <- NA}

          # next change from first confirmation #_r_#
          next_change <- NA
          if (conf_idx[[1]]<nvisits) {
            for (x in (conf_idx[[1]] + 1):nvisits) {
              if (!isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='impr') # either decrease not sustained
                  || isevent_loc(data_id[x,][[value_col]], data_id[conf_idx[[1]],][[value_col]],
                  type='change')) { # or further valid change from confirmation
                next_change <- x
                break
              }
            }
            next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                  bl[[value_col]], type='impr'))[1] + conf_idx[[1]] # decrease not sustained
          } else {next_nonsust <- NA}


          valid_impr <- 1
          if (require_sust_weeks) {
            valid_impr <- is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]]
                                 - data_id[change_idx, date_col]) > require_sust_weeks * 7
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
            for (m in conf_weeks) {
              conf[[as.character(m)]] <- c(conf[[as.character(m)]], as.integer(m %in% conf_t))
              # if (m!=conf_weeks[1]) {
                pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], NA)}
            # }
            sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
            sustl <- c(sustl, as.integer(sust_idx == nvisits))

            if (baseline %in% c('roving', 'roving_impr')) { #_r_#
              bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1)
              search_idx <- bl_idx + 1
            } else {
              search_idx <- ifelse(is.na(next_change), nvisits, next_change)
            }

            if (verbose == 2) {
              message(outcome, " improvement (visit no.", change_idx, ", ",
                      global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                           ") confirmed at ", paste(conf_t, collapse=", "), " weeks, sustained up to visit no.", sust_idx,
                           " (",
                      global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")")
              message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                           ifelse(search_idx > nvisits, "-", search_idx), " on")
            }
          } else {
            search_idx <- change_idx + 1
            if (verbose == 2) {
              message("Change confirmed but not sustained for >=", require_sust_weeks,
                      " weeks: proceed with search")
            }
          }
        }

      # Confirmed sub-threshold improvement: RE-BASELINE
      # ------------------------------------------------
      else if (length(conf_idx) > 0 && # confirmation visits available
          data_id[change_idx,][[value_col]] < bl[[value_col]] && # value decreased from baseline
          all(sapply((change_idx + 1):conf_idx[[1]], function(x) data_id[x,][[value_col]] < bl[[value_col]])) &&  # decrease is confirmed
          baseline %in% c('roving', 'roving_impr') #_r_#
          && sub_threshold &&
          phase == 0) { # skip if re-checking for PIRA after post-relapse re-baseline

        if (conf_idx[[1]]==nvisits) {
          next_change <- NA} else {
            next_change <- which(isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                 bl[[value_col]], type='change'))[1] + conf_idx[[1]] }
        bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1) # set new baseline at last consecutive decreased value
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
           ifelse(devtest_conf,
              isevent_loc(data_id[conf_idx[[1]],][[value_col]], bl[[value_col]], type='prog'), # [to remove after testing] increase is confirmed at first valid date
              all(sapply((change_idx + 1):conf_idx[[1]],
               function(x) isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='prog')))  # increase is confirmed at (all visits up to) first valid date
           ) &&
          all(sapply((change_idx + 1):conf_idx[[1]],
              function(x) data_id[x,][[value_col]] >= min_value_ifany)) # confirmation above min_value too
          ) || (data_id[change_idx,][[date_col]] - data_id[1,][[date_col]] <= prog_last_visit*7 && change_idx == nvisits))

         ) {

                 if (change_idx == nvisits) {
                   conf_idx <- c(nvisits)
                 }
                if (conf_idx[[1]]==nvisits) {
                  next_change <- NA} else {
                 next_change <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col], bl[[value_col]],
                                          type='prog'))[1] + conf_idx[[1]] }
                 if (!is.na(next_change)) {
                    conf_idx <- conf_idx[conf_idx < next_change] } # confirmed dates
                 conf_t <- conf_t[seq_along(conf_idx)]


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
                 #  next_nonsust <- which(data_id[(conf_idx[[length(conf_idx)]] + 1):nvisits, value_col]
                 #                   - bl[[value_col]] < delta(bl[[value_col]]))[1] + conf_idx[[length(conf_idx)]]
                 # } else {next_nonsust <- NA}

                 #_r_#
                 # next...
                 next_change <- NA
                 if (conf_idx[[1]]<nvisits) {
                   # ...valid change from first confirmation visit:
                   for (x in (conf_idx[[1]] + 1):nvisits) {
                     if (!isevent_loc(data_id[x,][[value_col]], bl[[value_col]], type='prog') # either not sustained
                         || isevent_loc(data_id[x,][[value_col]],  data_id[conf_idx[[1]],][[value_col]],
                         type='change')) {  # or further change from *first* confirmation
                       next_change <- x
                       break
                     }
                   }
                   # ...non-sustained value:
                   next_nonsust <- which(!isevent_loc(data_id[(conf_idx[[1]] + 1):nvisits, value_col],
                                         bl[[value_col]], type='prog'))[1] + conf_idx[[1]]
                 } else {
                   next_nonsust <- NA
                 }
                 # ...valid change from event:
                 next_change_ev <- which(isevent_loc(data_id[(change_idx + 1):nvisits, value_col],
                                             bl[[value_col]], type='change'))[1] + change_idx


                valid_prog <- 1
                if (require_sust_weeks) {
                  valid_prog <- ifelse(devtest_conf,
                    isevent_loc(data_id[nvisits,][[value_col]], bl[[value_col]], type='prog'), # [to remove after testing] progression confirmed at last visit
                    is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]] -
                                data_id[change_idx,][[date_col]]) > require_sust_weeks * 7 # progression sustained up to end of follow-up, or for `require_sust_weeks` weeks
                  )
                }

                if (valid_prog) {

                  nev <- length(event_type)

                  sust_idx <- ifelse(is.na(next_nonsust), nvisits, next_nonsust - 1)

                  if (phase == 0 && data_id[change_idx,][['closest_rel_minus']] <= relapse_assoc) { # event is relapse-associated
                    if (event=='firstPIRA' & baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for PIRA with a fixed baseline
                      next
                    }
                    event_type <- c(event_type, 'RAW')
                    event_index <- c(event_index, change_idx)
                  } else if (data_id[change_idx,][['closest_rel_minus']] > relapse_assoc) { # event is not relapse-associated
                    if (event=='firstRAW' & baseline=='fixed') {
                      search_idx <- change_idx + 1 # skip this event if only searching for RAW with a fixed baseline
                      next
                    }

                    # rel_inbetween <- sapply(conf_idx,
                    #           function(ic) any(is_rel[date_dict[[as.character(bl_idx)]]:date_dict[[as.character(ic)]]]))


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

                    rel_inbetween <- sapply(1:length(conf_idx), function(iic) {
                      any(sapply(1:length(left[[iic]]), function(j) {
                        any((left[[iic]][j] <= relapse_dates) & (relapse_dates <= right[[iic]][j]))
                      }))
                    })

                    # if (pira_def==0) {
                    #   rel_inbetween <- sapply(conf_idx,
                    #           function(ic) ifelse(length(relapse_dates)>0,
                    #                               any((data_id[bl_idx,][[date_col]]<=relapse_dates)
                    #                               & (relapse_dates<=data_id[ic,][[date_col]])), FALSE))
                    # } else if (pira_def==1) {
                    #   rel_inbetween <- sapply(conf_idx,
                    #           function(ic) ifelse(length(relapse_dates)>0, any(
                    #             ((data_id[bl_idx,][[date_col]]<=relapse_dates) & (relapse_dates<=data_id[change_idx,][[date_col]]+rel_infl))
                    #             | ((data_id[ic,][[date_col]]-rel_infl<=relapse_dates) & (relapse_dates<=data_id[ic,][[date_col]]+rel_infl))),
                    #             FALSE))
                    # }

                    pconf_idx <- conf_idx[!rel_inbetween]
                    # if (any(rel_inbetween)) {
                    #   if (min(which(rel_inbetween))>1) {
                    #   pconf_idx <- conf_idx[1:(min(which(rel_inbetween)) - 1)] } else {pconf_idx <- list()}
                    # } else {pconf_idx <- conf_idx}

                    if (length(pconf_idx) > 0
                      && data_id[pconf_idx[[length(pconf_idx)]], 'closest_rel_plus'] < relapse_to_conf) {
                      pconf_idx <- pconf_idx[-length(pconf_idx)]
                    }
                    pconf_t <- conf_t[seq_along(pconf_idx)]

                    if (length(pconf_idx) > 0) {
                      for (m in conf_weeks) {
                        # if (m!=conf_weeks[1]) {
                        pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], as.integer(m %in% pconf_t))}
                      # }
                      event_type <- c(event_type, 'PIRA')
                      event_index <- c(event_index, change_idx)
                    } else if (phase == 0) {
                      event_type <- c(event_type, 'prog')
                      event_index <- c(event_index, change_idx)
                    }
                  }

                  if (phase==0 & event_type[length(event_type)] != 'PIRA') {
                    for (m in conf_weeks) {
                      # if (m!=conf_weeks[1]) {
                        pira_conf[[as.character(m)]] <- c(pira_conf[[as.character(m)]], NA)}
                    # }
                  }

                  if (event_type[length(event_type)] == 'PIRA' || phase == 0) {
                    bldate <- c(bldate, as.character(global_start + as.difftime(bl[[date_col]], units='days')))
                    blvalue <- c(blvalue, bl[[value_col]])
                    edate <- c(edate, as.character(global_start + as.difftime(data_id[change_idx,][[date_col]], units='days')))
                    evalue <- c(evalue, data_id[change_idx,][[value_col]])
                    bl2event <- c(bl2event, data_id[change_idx,][[date_col]] - bl[[date_col]])
                    time2event <- c(time2event, data_id[change_idx,][[date_col]] - data_id[1,][[date_col]])
                    for (m in conf_weeks) {
                      conf[[as.character(m)]] <- c(conf[[as.character(m)]], as.integer(m %in% conf_t))
                    }
                    sustd <- c(sustd, data_id[sust_idx,][[date_col]] - data_id[change_idx,][[date_col]])
                    sustl <- c(sustl, as.integer(sust_idx == nvisits))

                    if (verbose == 2) {
                      message(outcome, " progression[", event_type[length(event_type)],
                                   "] (visit no.", change_idx, ", ",
                                  global_start + as.difftime(data_id[change_idx,][[date_col]], units='days'),
                                   ") confirmed at ", paste(conf_t, collapse=", "), " weeks, sustained up to visit no.", sust_idx,
                                   " (", global_start + as.difftime(data_id[sust_idx,][[date_col]], units='days'), ")")
                    }
                  }

                    if ((baseline == 'roving' && phase == 0)
                        ) {
                      bl_idx <- ifelse(is.na(next_change), nvisits, next_change - 1) # set new baseline at first confirmation time
                      search_idx <- bl_idx + 1
                    } else if (phase==0 && ((event_type[length(event_type)]!='PIRA' & event=='firstPIRA') ||
                               (event_type[length(event_type)]!='RAW' & event=='firstRAW'))) {
                      search_idx <- ifelse(is.na(next_change_ev), nvisits, next_change_ev) #_r_#
                    } else if (phase==1 && length(event_type)==nev) {
                      search_idx <- change_idx + 1
                    }
                      else {
                      search_idx <- ifelse(is.na(next_change), nvisits+1, next_change)
                      }

                    if (verbose == 2 && phase == 0) {
                      message("Baseline at visit no.", bl_idx, ", searching for events from visit no.",
                                   ifelse(search_idx > nvisits, "-", search_idx), " on")
                    }
                  } else {
                    search_idx <- change_idx + 1 # skip the change and look for other patterns after it
                    if (verbose == 2) {
                      message("Change confirmed but not sustained for >=", require_sust_weeks,
                                   " weeks: proceed with search")
                    }
                  }
                }

       # Confirmed sub-threshold progression: RE-BASELINE
       # ------------------------------------------------
      else if (length(conf_idx) > 0
               && data_id[change_idx,][[value_col]] > bl[[value_col]]
               && all(sapply((change_idx + 1):conf_idx[[1]], function(x) data_id[x,][[value_col]] > bl[[value_col]]))
               && baseline == 'roving'
               && sub_threshold && phase == 0) {
        if (conf_idx[[1]]==nvisits) {
          next_change <- NA} else {
        next_change <- which(data_id[(conf_idx[[1]] + 1):nvisits, value_col]
                             <= bl[[value_col]])[1] + conf_idx[[1]] }
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
                  & (data_id[x,][['closest_rel_minus']] >= relapse_to_bl) # out of relapse influence
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
      message(paste("\n---\nOutcome: ", outcome, "\nConfirmation at: ",
            paste(conf_weeks, collapse=", "), "mm (-", conf_tol_days[1], "dd, +",
            ifelse(conf_unbounded_right, "inf", conf_tol_days[2]), "dd)\nBaseline: ", baseline,
            ifelse(sub_threshold, " (sub-threshold)", ""),
            ifelse(relapse_rebl, " (and post-relapse re-baseline)", ""),
            "\nRelapse influence (baseline): ", relapse_to_bl, "dd\nRelapse influence (event): ",
            relapse_to_event, "dd\nRelapse influence (confirmation): ", relapse_to_conf, "dd\nEvents detected: ", event))
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


    prog_settings <- list(outcome=outcome, conf_weeks=conf_weeks, conf_tol_days=conf_tol_days,
                          conf_unbounded_right=conf_unbounded_right, require_sust_weeks=require_sust_weeks,
                          relapse_to_bl=relapse_to_bl, relapse_to_event=relapse_to_event, relapse_to_conf=relapse_to_conf,
                          relapse_assoc=relapse_assoc, event=event, baseline=baseline, relapse_indep=relapse_indep,
                          sub_threshold=sub_threshold, relapse_rebl=relapse_rebl, min_value=min_value,
                          prog_last_visit=prog_last_visit, compute_delta=compute_delta, delta_fun=delta_fun,
                          bl_value=min(data[value_col]))

    output <- list(summary=summary, results_df=results_df, prog_settings=prog_settings)
    class(output) <- 'MSprogOutput'


  return(output) #return(list(summary, results_df)) #
}



###############################################################################################



