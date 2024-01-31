
#' Event count from object.
#'
#' @param object An object for which an event count is desired.
#'
#' @return The form of the value returned by `event_count` depends on the class of its argument.
#' See the documentation of the particular methods for details of what is produced by that method.
#' @export
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
  object$results
}


###############################################################################################


#' Textual description of criteria used to obtain object.
#'
#' @param object An object for which a textual description of criteria is desired.
#'
#' @return The form of the value returned by `criteria_text` depends on the class of its argument.
#' See the documentation of the particular methods for details of what is produced by that method.
#' @export
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

  if (startsWith(s$event, 'firstprog')) {
    event_text <- paste('the first ', s$outcome, ' progression event', sep='')
  } else if (s$event=='first') {
    event_text <- paste('the first ', s$outcome, ' progression or improvement event', sep='')
  } else if (s$event=='firsteach') {
    event_text <- paste('the first ', s$outcome, ' progression and the first ',
                      s$outcome, ' improvement event (in chronological order) -', sep='')
  } else if (s$event=='firstprogtype') {
    event_text <- paste('the first ', s$outcome,
          ' progression event of each kind - PIRA, RAW, and undefined (in chronological order) -', sep='')
  } else if (s$event=='firstPIRA') {
    event_text <- paste('the first ', s$outcome,' PIRA event', sep='')
  } else if (s$event=='firstRAW') {
    event_text <- paste('the first ', s$outcome, ' RAW event', sep='')
  } else if (s$event=='multiple') {
    event_text <- paste('all ', s$outcome, ' changes (in chronological order)', sep='')
  }

  pira_text <- ''
  for (point in c('bl', 'event', 'conf')) {
    if (!(is.null(s$relapse_indep[[point]][[1]]) & is.null(s$relapse_indep[[point]][[2]]))
        & !((!is.null(s$relapse_indep[[point]][[1]]) && (s$relapse_indep[[point]][[1]]==0))
          & (!is.null(s$relapse_indep[[point]][[2]]) && (s$relapse_indep[[point]][[2]]==0)))) {
    pp <- ifelse(point=='bl', 'baseline', ifelse(point=='event', 'the event', 'confirmation'))
    pira_text <- paste(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[1]]), paste('from ',
        ifelse(s$relapse_indep[[point]][[1]]>0, paste(s$relapse_indep[[point]][[1]], ' days before ', sep=''), ''),
                                 pp, sep=''), ''), sep='')
    pira_text <-  paste(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[2]]), paste(' to ',
        ifelse(s$relapse_indep[[point]][[2]]>0, paste(s$relapse_indep[[point]][[2]], ' days after ', sep=''), ''),
                                  pp, ', or ', sep=''), ''), sep='')
  }
  }
  pira_text <- gsub('.{5}$', '', pira_text)

  text <- paste(
    'For each subject, we detected ', event_text, ' confirmed at ', paste(s$conf_weeks, collapse=" or "),
        ' weeks', ifelse(s$conf_unbounded_right, ' or more', ''),
        ifelse(s$conf_tol_days[1]>0, paste(', with a tolerance of ', s$conf_tol_days[1],
        ifelse(s$conf_tol_days[1]==s$conf_tol_days[2], ' days on both sides. ', paste(' days on the left',
               ifelse(s$conf_unbounded_right, '', paste(' and ', s$conf_tol_days[2], ' on the right', sep='')),
                                                                                      sep='')), sep=''),
              ifelse(s$conf_unbounded_right, '', paste(', with a tolerance of ',
                                              s$conf_tol_days[2], ' days on the right', sep=''))), '. ',
    ifelse(s$relapse_to_conf>0, paste('A visit could only be used as confirmation if occurring at least ',
                        s$relapse_to_conf, ' days from a relapse. ', sep=''), ''),
    ifelse(s$prog_last_visit, 'Progressions occurring at the last available visit were included. ', ''),
    ifelse(s$require_sust_weeks>0, paste('Events were only retained if sustained for either ',
                s$require_sust_weeks, ' weeks, or until the end of follow-up. ', sep=''), ''),
    ifelse(s$relapse_to_event>0, paste('Events occurring within ', s$relapse_to_event,
                                     ' days from a relapse were discarded. ', sep=''), ''),
    ifelse(!is.null(s$min_value), paste('Only progressions to ', s$outcome,
                                      ' values of at least ', s$min_value,
                                     ' were retained. ', sep=''), ''),
    ifelse(s$event!='firstPIRA', paste('A confirmed ', s$outcome, ' progression event was labelled as RAW if occurring within ',
    s$relapse_assoc, ' days from a relapse. ', sep=''), ''),
    ifelse(s$event!='firstRAW', paste('A confirmed ', s$outcome,
           ' progression event was labelled as PIRA if no relapses occurred in the interval ',
           pira_text, '. ', sep=''), ''),
    sep='')
  text <- paste(text,
    ifelse(s$baseline=='fixed',
      paste('The baseline was kept fixed at the first visit',
          ifelse(s$relapse_to_bl>0, paste(' occurring at least ', s$relapse_to_bl, ' days from a relapse. ', sep=''), '. '), sep=''),
      paste('A roving baseline scheme was applied where the reference value was ',
          ifelse(s$baseline=='roving_impr',
                 'updated every time the value was lower than the previous measure and confirmed. ',
                 'updated after each confirmed progression or improvement event. '),
          'The new reference value was set as the ', s$outcome, ' value at the confirmation visit. ',
          ifelse(s$sub_threshold,
             paste('The baseline was also moved when the (confirmed) shift in the ', s$outcome,
                 ' value was too small to define a valid event (e.g., a confirmed change from ',
                 s$outcome, '=', s$bl_value, ' to ', s$outcome, '=', s$bl_value + s$delta(s$bl_value)/2, '. ', sep=''),
             ''), ifelse(s$relapse_to_bl>0, paste('Whenever the baseline fell within ', s$relapse_to_bl,
                              ' days from a relapse, it was moved to the next available visit. ', sep=''), ''),
        sep='')
    ),
    ifelse(s$relapse_rebl, 'A further search for PIRA events was performed by resetting the baseline to the virst valid visit after each relapse. ',
           ''),
  sep='')

  cat(text)

}


###############################################################################################




