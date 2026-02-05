
#' Time to disability milestone.
#'
#' `value_milestone()` scans the visits in chronological order to detect the first
#' outcome value exceeding a specified disability milestone (e.g., EDSS>=6), *with confirmation*.
#' Note: "exceeding" means either value>=milestone or value<=milestone, depending on the
#' outcome measure (see arguments `outcome` and `worsening`).
#'
#' An event is only retained if **confirmed**, i.e., if all values *up to* the
#' confirmation visit exceed the milestone.
#' Valid time windows for confirmation visits are determined by arguments
#' `conf_days`, `conf_tol_days`, `relapse_to_conf`.
#'
#' @param data a `data.frame` containing longitudinal data, including: subject ID, outcome value, date of visit.
#' @param milestone Disability milestone (outcome value to check data against).
#' @param subj_col Name of data column with subject ID.
#' @param value_col Name of data column with outcome value.
#' @param date_col Name of data column with date of visit.
#' @param outcome Specifies the outcome type. Must be one of the following:
#' \itemize{
#'  \item{`'edss'` (Expanded Disability Status Scale);}
#'  \item{`'nhpt'` (Nine-Hole Peg Test);}
#'  \item{`'t25fw'` (Timed 25-Foot Walk);}
#'  \item{`'sdmt'` (Symbol Digit Modalities Test);}
#'  \item{`NULL` (only accepted when specifying argument `worsening`)}
#'  }
#' @param worsening The direction of worsening (`'increase'` if higher values correspond to worse disease course, `'decrease'` otherwise).<br />
#' The given value is only used when `outcome` is set to `NULL`. Otherwise, `worsening` is automatically set to
#' `'increase'` if `outcome` is set to `'edss'`, `'nhpt'`, `'t25fw'`,
#'  and to `'decrease'` if `outcome` is set to `'sdmt'`.
#' @param relapse `data.frame` containing longitudinal data, including: subject ID and relapse date.
#' @param rsubj_col Name of subject column for relapse data, if different from outcome data.
#' @param rdate_col Name of date column for relapse data, if different from outcome data.
#' @param validconf_col Name of data column specifying which visits can (`T`) or cannot (`F`) be used as confirmation visits.
#' The input data does not necessarily have to include such a column.
#' If `validconf_col=NULL`, all visits are potentially used as confirmation visits.
#' @param conf_days Period before confirmation (days).
#' @param conf_tol_days Tolerance window for confirmation visit (days); can be an integer (same tolerance on left and right)
#' or list-like of length 2 (different tolerance on left and right).
#' The right end of the interval can be set to `Inf` (confirmation window unbounded on the right
#' -- e.g., "confirmed over 12 \emph{or more} weeks").
#' @param require_sust_days Minimum number of days over which the milestone must be sustained
#' (i.e., confirmed at \emph{all} visits occurring in the specified period).
#' If the milestone is sustained for the remainder of the follow-up period, it is considered reached regardless of follow-up duration.
#' If `require_sust_days=Inf`, values are retained only when sustained for the remainder of the follow-up period.
#' @param relapse_to_event Minimum distance from a relapse (days) for the milestone to be considered reached.
#' Can be an integer (minimum distance from \emph{last} relapse) or list-like of length 2
#' (minimum distance from \emph{last} relapse, minimum distance from \emph{next} relapse).
#' Note that setting the distance to zero means retaining the event regardless of surrounding relapses.
#' @param relapse_to_conf Minimum distance from a relapse (days) for a visit to be a valid confirmation visit.
#' Can be an integer (minimum distance from \emph{last} relapse) or list-like of length 2
#' (minimum distance from \emph{last} relapse, minimum distance from \emph{next} relapse).
#' Note that setting the distance to zero means using any visit for confirmation regardless of surrounding relapses.
#' @param impute_last_visit Imputation probability when the milestone is reached at the last visit (i.e. with no confirmation).
#' Unconfirmed values exceeding the milestone at the last visit are never imputed if `impute_last_visit=0`;
#' they are always imputed if `impute_last_visit=1`;
#' they are imputed with probability `p`, `0<p<1`, if `impute_last_visit=p`.
#' If a value `N>1` is passed, unconfirmed values exceeding the milestone are imputed only if occurring within `N` days of follow-up
#' (e.g., in case of early discontinuation).
#' @param date_format Format of dates in the input data. Can be:
#' \itemize{
#' \item `'day'` if dates are given as "days from start" (the starting point can be different for each subject
#' -- e.g., days from randomisation in a clinical trial); negative values are accepted.
#' \item Standard format for dates (e.g., \code{"\%d-\%m-\%Y"}; see [strptime()] docs for correct syntax).
#' }
#' If not specified, function [as.Date()] will try to infer it automatically.
#' @param verbose, One of:
#' \itemize{
#'  \item{0}{ (print no info);}
#'  \item{1}{ (print concise info, default);}
#'  \item{2}{ (print extended info).}
#'  }
#' @return A `data.frame` containing the following columns:
#' \itemize{
#' \item{`date_col`: }{the date of first reaching or exceeding the milestone (or last date of follow-up if milestone is not reached);}
#' \item{`value_col`: }{the first value  reaching orexceeding the milestone, if present, otherwise no value;}
#' \item{`'time2event'`: }{the time taken to reach or exceed the milestone (or total follow-up length if milestone is not reached);}
#' \item{`'observed'`: }{whether the milestone was reached (1) or not (0).}
#' }
#' @importFrom stats complete.cases
#' @importFrom dplyr %>% group_by_at vars slice n mutate across
#' @export

value_milestone <- function(data, milestone, subj_col, value_col, date_col, outcome,
                            worsening=NULL, relapse=NULL, rsubj_col=NULL, rdate_col=NULL,
                            validconf_col=NULL, conf_days=24*7, conf_tol_days=c(7, 2*365.25), require_sust_days=0,
                            relapse_to_event=0, relapse_to_conf=30, impute_last_visit=0, date_format=NULL,
                            verbose=0) {

  ###########################
  # CHECKS ON ARGUMENT VALUES

  # If conf_tol_days is a single value, duplicate it (equal left and right tolerance)
  if (length(conf_tol_days)==1) {
    conf_tol_days <- c(conf_tol_days, conf_tol_days)
  }

  # If relapse_to_event is a single value, set right bound to zero
  if (length(relapse_to_event)==1) {
    relapse_to_event <- c(relapse_to_event, 0)
  }
  # If relapse_to_conf is a single value, set right bound to zero
  if (length(relapse_to_conf)==1) {
    relapse_to_conf <- c(relapse_to_conf, 0)
  }

  if (is.null(outcome) ||
      !(tolower(outcome) %in% c('edss', 'nhpt', 't25fw', 'sdmt'))) {
    outcome <- 'outcome'
  } else {
    outcome <- tolower(outcome)
  }

  # end of checks
  ###########################

  # If no column names are specified for the relapse file, use the main ones
  if (is.null(rsubj_col)) {
    rsubj_col <- subj_col
  }
  if (is.null(rdate_col)) {
    rdate_col <- date_col
  }

  # Create empty relapse file if none is provided
  if (is.null(relapse)) {
    relapse <- data.frame(matrix(nrow=0, ncol=2))
    names(relapse) <- c(rsubj_col, rdate_col)
  }

  # If no `validconf_col` is specified, create a dummy one
  if (is.null(validconf_col)) {
    validconf_col <- 'validconf'
    data$validconf <- T
  } else {
    data[[validconf_col]] <- as.logical(data[[validconf_col]])
  }

  # Remove missing values from columns of interest
  data <- data[complete.cases(data[ , c(subj_col, value_col, date_col, validconf_col)]), ]
  relapse <- relapse[complete.cases(relapse[, c(rsubj_col, rdate_col)]), ]

  # Convert dates to Date format
  if (is.null(date_format)) {
    tryCatch({
      data[[date_col]] <- as.Date(data[[date_col]])
      relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]])
    }, error=function(e) {
      message("Failed to infer format for date columns; please provide correct format via `date_format` argument.")
      NULL
    }
    )
  } else if (date_format == 'day') {
    tryCatch({
      data[[date_col]] <- as.numeric(data[[date_col]])
      relapse[[rdate_col]] <- as.numeric(relapse[[rdate_col]])
    }, error=function(e) {
      message("Failed to intepret date columns as numeric (number of days, as per `date_format='day'`)")
      NULL
    }
    )
  } else {
    tryCatch({
      data[[date_col]] <- as.Date(data[[date_col]], format=date_format)
      relapse[[rdate_col]] <- as.Date(relapse[[rdate_col]], format=date_format)
    }, error=function(e) {
      message("Failed to intepret date columns as \"", date_format, "\"; please provide correct format via `date_format` argument.")
      NULL
    }
    )
  }

  # Local function to display dates/days
  display_date <- function(day, start, to_print=T) {
    ifelse(!is.null(date_format) && date_format == 'day',
           ifelse(to_print, paste("day", day), day),
           as.character(start + as.difftime(day, units="days")))
  }

  # Convert dates to days from global minimum
  if (is.null(date_format) || date_format != 'day') {
    if (nrow(relapse)>0) {
      global_start <- min(min(data[[date_col]]), min(relapse[[rdate_col]]))
    } else {global_start <- min(data[[date_col]])}
    data[[date_col]] <- as.numeric(difftime(data[[date_col]], global_start), units='days')
    relapse[[rdate_col]] <- as.numeric(difftime(relapse[[rdate_col]], global_start), units='days')
  } else {
    global_start <- NULL
  }

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
  } else if (is.null(worsening) | !(worsening %in% c('increase', 'decrease'))) {
    stop('Either specify an outcome type, or specify the direction of worsening (\'increase\' or \'decrease\')')
  }

  # Define a confirmation window for each value of conf_days
  conf_window <- lapply(conf_days, function(t) {
    lower <- as.integer(t) - conf_tol_days[1]
    upper <- as.integer(t) + conf_tol_days[2]
    return(c(lower, upper))
  })

  #################################################################
  # Assess time to milestone

  # Make subject ID a character for safer indexing
  data[[subj_col]] <- as.character(data[[subj_col]])

  all_subj <- unique(data[[subj_col]])
  nsub <- length(all_subj)

  # Initialise results data.frame
  results <- data.frame(matrix(ncol = 4, nrow = nsub))
  colnames(results) <- c(date_col, value_col, 'time2event', 'observed')
  rownames(results) <- all_subj
  if (!is.null(date_format) && date_format == 'day') {
    results[[date_col]] <- NaN  # numeric
  } else {
    results[[date_col]] <- as.Date(NA)  # Date
  }
  results[[value_col]] <- NaN
  results$time2event <- NaN
  results$observed <- 0

  for (subjid in all_subj) {

    data_id <- data[data[[subj_col]] == subjid, ]

    # If more than one visit occur on the same day, only keep last
    ucounts <- table(data_id[, date_col])
    if (any(ucounts > 1)) {
      data_id <- data_id %>% group_by_at(vars(date_col)) %>% slice(n())
    }

    # Sort visits in chronological order
    order_tmp <- order(data_id[[date_col]])
    if (any(order_tmp != seq_len(nrow(data_id)))) {
      data_id <- data_id[order_tmp, ]
    }

    nvisits <- nrow(data_id)
    first_visit <- min(data_id[[date_col]])
    relapse_id <- relapse[relapse[[rsubj_col]] == subjid, ]
    relapse_id <- relapse_id[relapse_id[[rdate_col]] >= first_visit - relapse_to_event[1], ]
    relapse_dates <- relapse_id[[rdate_col]]
    nrel <- length(relapse_dates)

    # Print info
    if (verbose == 2) {
      message("\nSubject #", subjid, ": ", nvisits, " visit", ifelse(nvisits == 1, "", "s"),
              ", ", nrel, " relapse", ifelse(nrel == 1, "", "s"))
      if (any(ucounts > 1)) {
        message("Found multiple visits on the same day: only keeping last.")
      }
      if (any(order_tmp != seq_len(nrow(data_id)))) {
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

    proceed <- TRUE
    search_idx <- 1 # Index of where we are in the search
    while (proceed) {

      milestone_idx <- NA
      if (search_idx <= nvisits) {
          for (x in (search_idx:nvisits)) {
            if (ifelse(worsening=='increase',
                       data_id[x, value_col] >= milestone,
                       data_id[x, value_col] <= milestone) # first value reaching milestone
                && data_id[x, 'closest_rel_before'] >= relapse_to_event[1]
                && data_id[x, 'closest_rel_after'] >= relapse_to_event[2])
              {
              milestone_idx <- x
              break
            }
          }
        }

      if (is.na(milestone_idx)) {
        results[subjid, date_col] <- display_date(data_id[nvisits,][[date_col]], global_start) # end of FU
        results[subjid, 'time2event'] <- data_id[nvisits, date_col] - data_id[1, date_col] # FU length
        proceed <- FALSE
        if (verbose == 2) {
          message("No value",  ifelse(worsening=='increase', '>=', '<='), milestone, " in any visit: end process\n")
        }

      } else {
        if (milestone_idx==nvisits) {
          conf_idx <- list()
        } else {
          conf_idx <- lapply(conf_window, function(t) {
            match_idx <- numeric(0)
            for (x in (milestone_idx + 1):nvisits) {
              if (data_id[x,][[date_col]] - data_id[milestone_idx,][[date_col]] >= t[1]
                  && data_id[x,][[date_col]] - data_id[milestone_idx,][[date_col]] <= t[2]  # date in confirmation range
                  && data_id[x,][['closest_rel_before']] >= relapse_to_conf[1]  # occurring out of influence of last relapse
                  && data_id[x,][['closest_rel_after']] >= relapse_to_conf[2]  # occurring out of influence of next relapse
                  && data_id[x,][[validconf_col]]  # can be used as confirmation
              ) {
                match_idx <- append(match_idx, x)
              }
            }
            match_idx
          })
          conf_idx <- unique(unlist(conf_idx))
        }
        if (verbose == 2) {
          message("Found value", ifelse(worsening=='increase', '>=', '<='), milestone,
                  " at visit no.", milestone_idx, " (",
                  display_date(data_id[milestone_idx,][[date_col]], global_start, to_print=T),
                  "); potential confirmation visits available: ", ifelse(length(conf_idx)>0,
                                        paste0("no. ", paste(conf_idx, collapse=", ")), "none"))
        }

        if (
          (length(conf_idx) > 0  # confirmation visits available
            && ifelse(worsening=='increase',
                      all(data_id[(milestone_idx + 1):conf_idx[[1]], value_col] >= milestone),
                      all(data_id[(milestone_idx + 1):conf_idx[[1]], value_col] <= milestone))   # milestone is confirmed at (all visits up to) first valid date
            ) || (milestone_idx == nvisits  # milestone reached at last visit
                  && data_id[milestone_idx,][[date_col]] - data_id[1,][[date_col]] <= impute_max_fu  # visit below follow-up threshold
                  && rbinom(1, 1, impute_last_visit)  # impute with probability `impute_last_visit`
                  )
            ) {

          if (milestone_idx == nvisits) {  # i.e., when imputing event at last visit
            conf_idx <- c(nvisits)
          }

          # First visit at which milestone is not sustained:
          if (conf_idx[[1]]==nvisits) {
            next_nonsust <- NA
          } else {
            next_nonsust <- which(ifelse(worsening=='increase',
                                         data_id[(conf_idx[[1]] + 1):nvisits, value_col] < milestone,
                                         data_id[(conf_idx[[1]] + 1):nvisits, value_col] > milestone)
            )[1] + conf_idx[[1]]
          }

          # The confirmed milestone can still be rejected if `require_sust_days>0`.
          # The `valid` flag indicates whether the event can (1) or cannot (0) be retained:
          valid <- 1
          if (require_sust_days > 0) {
            valid <- is.na(next_nonsust) || (data_id[next_nonsust,][[date_col]] -
                                data_id[milestone_idx,][[date_col]]) >= require_sust_days # sustained up to end of follow-up, or for `require_sust_days`
          }

          if (valid) {
          results[subjid, date_col] <- display_date(data_id[milestone_idx,][[date_col]], global_start) # date of reaching the milestone
          results[subjid, value_col] <- data_id[milestone_idx, value_col] # first value >= milestone
          results[subjid, 'time2event'] <- data_id[milestone_idx, date_col] - data_id[1, date_col] # time to reach the milestone
          results[subjid, 'observed'] <- 1 # whether milestone was reached
          proceed <- FALSE
          if (verbose == 2) message(ifelse(milestone_idx == nvisits, "Imputed", "Confirmed"), " value",
                                    ifelse(worsening=='increase', '>=', '<='), milestone,
                                    ifelse(milestone_idx == nvisits, " (last visit", paste0(" (visit no.", milestone_idx, ", ",
                                    display_date(data_id[milestone_idx,][[date_col]], global_start, to_print=T))),
                                    "): end process\n")
          } else {
            # (not sustained)
            search_idx <- next_nonsust + 1
            if (verbose == 2) {
              message("Value", ifelse(worsening=='increase', '>=', '<='),
                      milestone, " confirmed but not sustained over ",
                      ifelse(require_sust_days<Inf, paste(">=", require_sust_days, "days"),
                             "remainder of follow-up"), ": proceed with search")
            }
          }

        } else {
          # (not confirmed)
          if (milestone_idx == nvisits) {
            next_change <- nvisits
          } else {
            next_change <- which(ifelse(worsening=='increase',
                                         data_id[(milestone_idx + 1):nvisits, value_col] < milestone,
                                         data_id[(milestone_idx + 1):nvisits, value_col] > milestone)
            )[1] + milestone_idx
          }
          search_idx <- ifelse(is.na(next_change), nvisits, next_change + 1)
          if (verbose == 2) {
            message("Value", ifelse(worsening=='increase', '>=', '<='),
                              milestone, " not confirmed: proceed with search")
            }
        }
      }
    }  # END while (proceed)

  }  # END for (subjid in all_subj)

  if (verbose >= 1) {
    message(paste0("\n---\nOutcome: ", outcome, "\nConfirmation over: ",
           paste(conf_days, collapse=", "), " days (-", conf_tol_days[1], " days, +",
           conf_tol_days[2], " days)",
           "\nEvent skipped if: ", ifelse(relapse_to_event[1]>0,
                                  paste0("<", relapse_to_event[1], " days from last relapse"), ""),
           ifelse(relapse_to_event[2]>0, paste0(ifelse(relapse_to_event[1]>0, ", <", '<'),
                                                relapse_to_event[2], " days to next relapse"), ""),
           ifelse(relapse_to_event[1]==0 && relapse_to_event[2]==0, '-', ''),
           "\nConfirmation visit skipped if: ", ifelse(relapse_to_conf[1]>0, paste0("<", relapse_to_conf[1], " days from last relapse"), ""),
           ifelse(relapse_to_conf[2]>0, paste0(ifelse(relapse_to_conf[1]>0, ", <", '<'),
                                               relapse_to_conf[2], " days to next relapse"), ""),
           ifelse(relapse_to_conf[1]==0 && relapse_to_conf[2]==0, '-', '')
           ))
    message("\n---\nTotal subjects: ", nsub, "\n",
            sum(results[['observed']]), " reached the milestone ", outcome, "=", milestone, ".")

  }

  return(results)
}
