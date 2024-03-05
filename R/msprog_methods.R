
#' Event count from object.
#'
#' @param object An object for which an event count is desired.
#'
#' @return The form of the value returned by `event_count` depends on the class of its argument.
#' See the documentation of the particular methods for details of what is produced by that method.
#' @export
#' @keywords internal
event_count <- function (object) {
  UseMethod("event_count")
}



#' Event count for disability progression results.
#'
#' `event_count` method for class `'MSprogOutput'`.
#'
#' @param object An object of class `'MSprogOutput'` (result of a call to [MSprog()]).
#'
#' @return A `data.frame` object containing the sequence of events for each subject,
#' as well as the event count separated by event type
#' (improvement, progression, RAW, PIRA, undefined progression).
#' @export
#' @examples
#' # EDSS progression
#' output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=2)
#' print(event_count(output)) # event sequence and count for each subject
event_count.MSprogOutput <- function(object) {
  object$summary
}


###############################################################################################


#' Extended result report from object.
#'
#' @param object An object for which an extended result report is desired.
#'
#' @return The form of the value returned by `results` depends on the class of its argument.
#' See the documentation of the particular methods for details of what is produced by that method.
#' @export
#' @keywords internal
results <- function (object) {
  UseMethod("results")
}



#' Extended disability progression results.
#'
#' `results` method for class `'MSprogOutput'`.
#'
#'
#' @param object An object of class `'MSprogOutput'` (result of a call to [MSprog()]).
#'
#' @return A `data.frame` object containing an extended report of all events
#' detected by function [MSprog()] for each subject.
#' @export
#' @examples
#' # EDSS progression
#' output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=2)
#' print(results(output)) # extended event info for each subject
results.MSprogOutput <- function(object) {
  object$results_df
}


###############################################################################################


#' Textual description of criteria used to obtain object.
#'
#' @param object An object for which a textual description of criteria is desired.
#'
#' @return The form of the value returned by `criteria_text` depends on the class of its argument.
#' See the documentation of the particular methods for details of what is produced by that method.
#' @export
#' @keywords internal
criteria_text <- function (object) {
  UseMethod("criteria_text")
}



#' Textual description of criteria used to compute disability progression.
#'
#' `criteria_text` method for class `'MSprogOutput'`.
#'
#' The method prints out a short paragraph describing the set of criteria used to
#' obtain the output.
#'
#' @param object An object of class `'MSprogOutput'` (result of a call to [MSprog()]).
#'
#' @export
#' @examples
#' # EDSS progression
#' output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_weeks=12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=2)
#' criteria_text(output) # textual description of parameters used to obtain output
criteria_text.MSprogOutput <- function(object) {

  s <- object$prog_settings
  outcome <- ifelse(s$outcome=='outcome', 'outcome', toupper(s$outcome))
  delta <- function(value) {
    if (is.null(s$delta_fun)) {
      return(s$compute_delta(value, s$outcome))
    } else {
      return(s$delta_fun(value))
    }
  }

  if (startsWith(s$event, 'firstprog')) {
    event_text <- paste0('the first ', outcome, ' progression event')
  } else if (s$event=='first') {
    event_text <- paste0('the first ', outcome, ' progression or improvement event')
  } else if (s$event=='firsteach') {
    event_text <- paste0('the first ', outcome, ' progression and the first ',
                      outcome, ' improvement event (in chronological order) -')
  } else if (s$event=='firstprogtype') {
    event_text <- paste0('the first ', outcome,
          ' progression event of each kind - PIRA, RAW, and undefined (in chronological order) -')
  } else if (s$event=='firstPIRA') {
    event_text <- paste0('the first ', outcome,' PIRA event')
  } else if (s$event=='firstRAW') {
    event_text <- paste0('the first ', outcome, ' RAW event')
  } else if (s$event=='multiple') {
    event_text <- paste0('all ', outcome, ' changes (in chronological order)')
  }

  pira_text <- ''
  for (point in c('bl', 'event', 'conf')) {
    if (!(is.null(s$relapse_indep[[point]][[1]]) & is.null(s$relapse_indep[[point]][[2]]))
        & !((!is.null(s$relapse_indep[[point]][[1]]) && (s$relapse_indep[[point]][[1]]==0))
          & (!is.null(s$relapse_indep[[point]][[2]]) && (s$relapse_indep[[point]][[2]]==0)))) {
    pp <- ifelse(point=='bl', 'baseline', ifelse(point=='event', 'the event', 'confirmation'))
    pira_text <- paste0(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[1]]), paste0('from ',
        ifelse(s$relapse_indep[[point]][[1]]>0, paste0(s$relapse_indep[[point]][[1]], ' days before '), ''),
                                 pp), ''))
    pira_text <-  paste0(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[2]]), paste0(' to ',
        ifelse(s$relapse_indep[[point]][[2]]>0, paste0(s$relapse_indep[[point]][[2]], ' days after '), ''),
                                  pp, ', or '), ''))
  }
  }
  pira_text <- gsub('.{5}$', '', pira_text)

  text <- paste0(
    'For each subject, we detected ', event_text, ' confirmed over ', paste0(s$conf_weeks, collapse=" or "),
        ' weeks', ifelse(s$conf_unbounded_right, ' or more', ''),
    ifelse(s$conf_tol_days[1]>0, paste0(', with a tolerance of ', s$conf_tol_days[1],
        ifelse(s$conf_tol_days[1]==s$conf_tol_days[2], ' days on both sides', paste0(' days on the left',
               ifelse(s$conf_unbounded_right, '', paste0(' and ', s$conf_tol_days[2], ' on the right'))))),
              ifelse(s$conf_unbounded_right, '', paste0(', with a tolerance of ',
                                              s$conf_tol_days[2], ' days on the right'))), '. ',
    ifelse(s$relapse_to_conf>0, paste0('A visit could only be used as confirmation if occurring at least ',
                        s$relapse_to_conf, ' days from a relapse. '), ''),
    ifelse(s$baseline=='fixed',
           paste0('The baseline was kept fixed at the first visit',
                  ifelse(s$relapse_to_bl>0, paste0(' occurring at least ', s$relapse_to_bl, ' days from a relapse. '), '. ')),
           paste0('A roving baseline scheme was applied where the reference value was ',
                  ifelse(s$baseline=='roving_impr',
                         'updated every time the value was lower than the previous measure and confirmed. ',
                         'updated after each confirmed progression or improvement event. '),
                  'The new reference value was set as the ', outcome, ' value at the first available confirmation visit. ',
                  ifelse(s$sub_threshold,
                         paste0('Rebaseline was also triggered by (confirmed) \"sub-threshold\" events, i.e., when the shift in the ', outcome,
                                ' value was too small to define a valid event. ' # (e.g., a confirmed change from
                                #outcome, '=', s$bl_value, ' to ', outcome, '=', s$bl_value + round(delta(s$bl_value)/2), '). '
                         ),
                         ''), ifelse(s$relapse_to_bl>0, paste0('Whenever the baseline fell within ', s$relapse_to_bl,
                                                               ' days from a relapse, it was moved to the next available visit. '), ''))
    ),
    ifelse(s$prog_last_visit>0, paste0('Progressions ',
              ifelse(s$prog_last_visit<Inf, paste0('of patients terminating follow-up before week ',
                                                   s$prog_last_visit, ' '), ''),
                              'were included if occurring at the last available visit without confirmation. '), ''),
    ifelse(s$require_sust_weeks>0, paste0('Events were only retained if sustained',
                ifelse(s$require_sust_weeks<Inf, paste0(' either over ', s$require_sust_weeks, ' weeks, or'), ''),
                ' until the end of follow-up. '), ''),
    ifelse(s$relapse_to_event>0, paste0('Events occurring within ', s$relapse_to_event,
                                     ' days from a relapse were discarded. '), ''),
    ifelse(!is.null(s$min_value), paste0('Only progressions to ', outcome,
                                      ' values of at least ', s$min_value,
                                     ' were retained. '), ''),
    ifelse(s$event!='firstPIRA', paste0('A confirmed ', outcome, ' progression event was labelled as RAW if occurring within ',
    s$relapse_assoc, ' days from a relapse. '), ''),
    ifelse(s$event!='firstRAW', paste0('A confirmed ', outcome,
           ' progression event was labelled as PIRA if no relapses occurred in the interval ',
           pira_text, '. '), ''),
    ifelse(s$relapse_rebl, 'A further search for PIRA events was performed by resetting the baseline to the first valid visit after each relapse. ',
           '')
    )

  cat(text)

}


###############################################################################################




