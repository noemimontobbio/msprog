

#' Textual description of criteria used to assess disability course.
#'
#' `print` method for class `'MSprogOutput'`.
#'
#' The method prints out a short paragraph describing the set of criteria used to
#' obtain the output.
#'
#' @param x An object of class `'MSprogOutput'` (result of a call to [MSprog()]).
#' @param ... Optional arguments for `print` methods. They are ignored in this function.
#'
#' @export
#' @examples
#' output <- MSprog(toydata_visits, 'id', 'EDSS', 'date', 'edss',
#'     relapse=toydata_relapses, conf_days=7*12, conf_tol_days=30,
#'     event='multiple', baseline='roving', verbose=2)
#' print(output) # textual description of parameters used to obtain output
print.MSprogOutput <- function(x, ...) {

  s <- x$settings
  outcome <- ifelse(s$outcome=='outcome', 'outcome', toupper(s$outcome))
  delta <- function(value) {
    if (is.null(s$delta_fun)) {
      return(s$compute_delta(value, s$outcome))
    } else {
      return(s$delta_fun(value))
    }
  }

  if (startsWith(s$event, 'firstCDW')) {
    event_text <- paste0('the first ', outcome, ' worsening event')
  } else if (s$event=='first') {
    event_text <- paste0('the first ', outcome, ' worsening or improvement event')
  } else if (s$event=='firsteach') {
    event_text <- paste0('the first ', outcome, ' worsening and the first ',
                         outcome, ' improvement event (in chronological order) -')
  } else if (s$event=='firstCDWtype') {
    event_text <- paste0('the first ', outcome,
                         ' worsening event of each kind - PIRA, RAW, and undefined (in chronological order) -')
  } else if (s$event=='firstPIRA') {
    event_text <- paste0('the first ', outcome,' PIRA event')
  } else if (s$event=='firstRAW') {
    event_text <- paste0('the first ', outcome, ' RAW event')
  } else if (s$event=='multiple') {
    event_text <- paste0('all ', outcome, ' changes (in chronological order)')
  }

  if (length(s$relapse_indep[['event']])==2) {
  pira_text <- ''
  for (point in c('bl', 'event', 'conf')) {
    if (!(is.null(s$relapse_indep[[point]][[1]]) & is.null(s$relapse_indep[[point]][[2]]))
        & !((!is.null(s$relapse_indep[[point]][[1]]) && (s$relapse_indep[[point]][[1]]==0))
            & (!is.null(s$relapse_indep[[point]][[2]]) && (s$relapse_indep[[point]][[2]]==0)))) {
      pp <- ifelse(point=='bl', 'baseline', ifelse(point=='event', 'the event', 'confirmation'))
      pira_text <- paste0(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[1]]), paste0('from ',
                  ifelse(s$relapse_indep[[point]][[1]]>0, paste0(s$relapse_indep[[point]][[1]],
                                                                 ' days before '), ''), pp), ''))
      pira_text <-  paste0(pira_text, ifelse(!is.null(s$relapse_indep[[point]][[2]]), paste0(' to ',
                     ifelse(s$relapse_indep[[point]][[2]]>0, paste0(s$relapse_indep[[point]][[2]],
                                                               ' days after '), ''), pp, ', or '), ''))
    }
  }
  pira_text <- gsub('.{5}$', '', pira_text)
  }

  text <- paste0(
    'For each subject, we detected ', event_text, ' confirmed', ifelse(s$check_intermediate, ' over ', ' at '),
    paste0(s$conf_days, collapse=" or "),
    ' days', ifelse(s$conf_unbounded_right, ' or more', ''),
    ifelse(s$conf_tol_days[1]>0, paste0(' (with a tolerance of ', s$conf_tol_days[1],
                                        ifelse(s$conf_tol_days[1]==s$conf_tol_days[2], ' days on both sides)',
                                            paste0(' days on the left', ifelse(s$conf_unbounded_right || s$conf_tol_days[2]==0, ')',
                                                                        paste0(' and ', s$conf_tol_days[2], ' days on the right)'))))),
           ifelse(s$conf_unbounded_right || s$conf_tol_days[2]==0, '', paste0(' (with a tolerance of ',
                                                                              s$conf_tol_days[2], ' days on the right)'))),
    ifelse(s$check_intermediate, '. ', ', ignoring all intermediate visits. '),
    ifelse(s$validconf_p<1,
           'Visits [insert condition implemented by `validconf_col`] were not used for event confirmation. ', ''),
    ifelse(s$relapse_to_conf[1]>0, paste0('A visit could not be used as confirmation if occurring within ',
                ifelse(is.null(s$renddate_col), paste0(s$relapse_to_conf[1], ' days after the onset of a relapse'),
                                                 'a relapse (onset-to-end)')), ''),
    ifelse(s$relapse_to_conf[2]>0, paste0(ifelse(s$relapse_to_conf[1]>0, ', or within ',
              'A visit could not be used as confirmation if occurring within '), paste0(s$relapse_to_conf[2],
                                          ' days before the onset of a relapse')), ''),
    ifelse(s$relapse_to_conf[1]>0 | s$relapse_to_conf[2]>0, '. ', ''),
    ifelse(s$baseline=='fixed',
           paste0('The baseline was kept fixed at the first visit',
                  ifelse(s$relapse_to_bl[1]>0, paste0(', unless occurring within ', ifelse(is.null(s$renddate_col),
                      paste0(s$relapse_to_bl[1], ' days after the onset of a relapse'), 'a relapse (onset-to-end)'))),
                  ifelse(s$relapse_to_bl[2]>0, paste0(ifelse(s$relapse_to_bl[1]>0, ', or within ',
                        ', unless occurring within '), paste0(s$relapse_to_bl[2], ' days before the onset of a relapse')), ''),
            ifelse(s$relapse_to_bl[1]>0 | s$relapse_to_bl[2]>0,
                   ' (in which case it was moved to the next available assessment out of relapse influence). ', '. ')),
           paste0('A roving baseline scheme was applied where the reference value was ',
                  ifelse(s$baseline=='roving_impr',
                         'updated after each confirmed improvement event. ',
                         'updated after each confirmed worsening or improvement event. '),
                  'The new baseline was set at ', ifelse(s$proceed_from=='event',
                                                         'the event. ',
                                                         'the first available confirmation visit. '),
                  ifelse(s$sub_threshold_rebl!='none',
                         paste0('Rebaseline was also triggered by confirmed \"sub-threshold\" ', s$sub_threshold_rebl,
                                ', i.e., when the shift in the ', outcome,
                                ' value was too small to define a valid event. ' # (e.g., a confirmed change from
                                #outcome, '=', s$bl_value, ' to ', outcome, '=', s$bl_value + round(delta(s$bl_value)/2), '). '
                         ), ''),
                  ifelse(s$relapse_to_bl[1]>0, paste0('Whenever the current baseline fell within ',
                              ifelse(is.null(s$renddate_col), paste0(s$relapse_to_bl[1], ' days after the onset of a relapse, '),
                                'a relapse (onset-to-end)')),  ''),
                  ifelse(s$relapse_to_bl[2]>0, paste0(ifelse(s$relapse_to_bl[1]>0, 'or within ',
                            'Whenever the current baseline fell within '), s$relapse_to_bl[2],
                                                          ' days before the onset of a relapse'), ''),
                  ifelse(s$relapse_to_bl[1]>0 | s$relapse_to_bl[2]>0, ', it was moved to the next available visit. ', '')
              ) #end[paste0(roving)]
        ), #end[ifelse(baseline=='fixed)]
    ifelse(s$relapse_rebl, paste0('The onset of each relapse', ifelse(s$baseline=='fixed', '', ' also'),
            ' prompted a re-baseline to the next available visit',
            ifelse(s$relapse_to_bl[1]>0 | s$relapse_to_bl[2]>0, ' out of relapse influence. ', '. ')), ''),
    ifelse(s$skip_local_extrema & (s$relapse_to_bl[1]>0 || s$relapse_to_bl[2]>0 || s$relapse_rebl || s$baseline!='fixed'),
           'Local minima or maxima were skipped when updating the baseline. ', ''),
    ifelse(s$impute_last_visit>0, paste0('CDWs ', ifelse(s$impute_last_visit<Inf & s$impute_last_visit>=1,
                          paste0('of patients terminating follow-up before day ', s$impute_last_visit, ' '), ''),
                                       'were included if occurring at the last available visit without confirmation',
                          ifelse(s$impute_last_visit<1, paste0(' with a probability of ', s$impute_last_visit), ''), '. '), ''),
    ifelse(s$require_sust_days>0, paste0('Events were only retained if sustained',
                                          ifelse(s$require_sust_days<Inf, paste0(' either over ', s$require_sust_days, ' days, or'), ''),
                                          ' until the end of follow-up. '), ''),
    ifelse(s$relapse_to_event[1]>0, paste0('Events occurring within ', ifelse(is.null(s$renddate_col),
                    paste0(s$relapse_to_event[1], ' days after the onset of a relapse'), 'a relapse (onset-to-end)')), ''),
    ifelse(s$relapse_to_event[2]>0, paste0(ifelse(s$relapse_to_event[1]>0, ', or within ', 'Events occurring within '),
                      paste0(s$relapse_to_event[2], ' days before the onset of a relapse')), ''),
    ifelse(s$relapse_to_event[1]>0 | s$relapse_to_event[2]>0, ' were discarded. ', ''),
    # ifelse(!is.null(s$min_value), paste0('Only CDWs to ', outcome,
    #                                      ' values of at least ', s$min_value,
    #                                      ' were retained. '), ''),
    ifelse(s$event!='firstPIRA', paste0('A confirmed ', outcome, ' worsening event was labelled as RAW if occurring within ',
            ifelse(is.null(s$renddate_col), paste0(s$relapse_assoc[1], ' days after the onset of a relapse'),
                        'a relapse (between onset and end)'),
            ifelse(s$relapse_assoc[2]>0, paste0(', or within ', s$relapse_assoc[2], ' days before the onset of a relapse'), ''),
                 '. '), ''),
    ifelse(s$event!='firstRAW', paste0('A confirmed ', outcome, ' worsening event was labelled as PIRA if ',
                          ifelse(length(s$relapse_indep[['event']])==2, paste0('no relapses started in the interval ', pira_text, '. '),
                                 paste0('it did not occur within a relapse (onset to end)', ifelse(s$relapse_indep[['event']]>0,
                                        paste0(', or less than ', s$relapse_indep[['event']], ' days before a relapse')),
                                        ', and the confirmation did not occur within a relapse', ifelse(s$relapse_indep[['conf']]>0,
                                        paste0(', or less than ', s$relapse_indep[['conf']], ' days before a relapse')), '. '))), '')
  )

  cat(text)

}


