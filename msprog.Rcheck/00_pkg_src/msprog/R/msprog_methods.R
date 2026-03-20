
#' Textual description of criteria used to assess disability course.
#'
#' `print` method for class `"MSprogOutput"`.
#'
#' The method prints out (1) the package version, (2) a full list of function arguments,
#' and (3) a short paragraph describing the full set of criteria used to
#' obtain the output.
#'
#' @param x An object of class `"MSprogOutput"` (result of a call to [MSprog()]).
#' @param web Adapt for printing in web app.
#' @param ... Optional arguments for `print` methods. They are ignored in this function.
#'
#' @export
#' @examples
#' output <- MSprog(toydata_visits, "id", "EDSS", "date", "edss",
#'     relapse=toydata_relapses, conf_days=7*12, conf_tol_days=30,
#'     event="multiple", baseline="roving", verbose=2)
#' print(output) # textual description of parameters used to obtain output
print.MSprogOutput <- function(x, web=FALSE, ...) {

  cat('---\nmsprog version:', as.character(utils::packageVersion("msprog")), '\n---')

  s <- x$settings
  outcome <- if (s$outcome=="custom") "outcome" else toupper(s$outcome)

  keep <- setdiff(names(s), c("validconf_p", "delta_fun"))
  cat("\nMSprog() arguments:\n", paste0(paste(keep, s[keep], sep="="), collapse=", "),
      ",\ndelta_fun=", as.character(s["delta_fun"]), sep="")

  # %%%%%%%%%%%%%
  # EVENT
  # %%%%%%%%%%%%%

  if (s$event == "firstCDW") {
    event_text <- paste0("the first ", outcome, " CDW event")
  } else if (s$event=="first") {
    event_text <- paste0("the first ", outcome, " CDW or CDI event")
  } else if (s$event == "firstCDI") {
    event_text <- paste0("the first ", outcome, " CDI event")
  } else if (s$event=="firstPIRA") {
    event_text <- paste0("the first ", outcome, " PIRA event")
  } else if (s$event=="firstRAW") {
    event_text <- paste0("the first ", outcome, " RAW event")
  } else if (s$event=="multiple") {
    event_text <- paste0("all confirmed ", outcome, " changes (in chronological order)")
  }

  # %%%%%%%%%%%%%
  # CONFIRMATION
  # %%%%%%%%%%%%%

  # Conf tolerance text
  no_tolerance <- s$conf_tol_days[1]==0 & s$conf_tol_days[2] %in% c(0, Inf)
  tol <- c()
  if (s$conf_tol_days[1]>0) {tol <- c(tol, paste0("a lower tolerance of ", s$conf_tol_days[1], " days"))}
  if (!(s$conf_tol_days[2] %in% c(0, Inf))) {tol <- c(tol, paste0("an upper tolerance of ", s$conf_tol_days[2], " days"))}
  # Relapse-related conf text
  rel_constraint <- s$relapse_to_conf[1]>0 | s$relapse_to_conf[2]>0
  relintervals <- c()
  if (s$relapse_to_conf[1]>0 & !is.null(s$renddate_col)) {
    relintervals <- c(relintervals, "a relapse (onset-to-end)")
    } else if (s$relapse_to_conf[1]>0) {
    relintervals <- c(relintervals, paste0(s$relapse_to_conf[1], " days after"))
    }
  if (s$relapse_to_conf[2]>0) {relintervals <- c(relintervals, paste0(s$relapse_to_conf[2], " days before"))}
  # Full conf text
  conf_text <- paste0("We detected ", event_text, " confirmed",
                      if (s$check_intermediate) " over " else " at ",
                      paste0(paste0(s$conf_days, if (s$conf_tol_days[2]==Inf) " or more" else ""),
                             collapse=", or "), " days",
                      # Tolerance
                      if (no_tolerance) "" else " (with ",
                      paste0(tol, collapse=" and "),
                      if (no_tolerance) "" else ")",
                      # Intermediate visits
                      if (s$check_intermediate) ". " else ", ignoring all intermediate visits. ",
                      # Valid confirmation
                      if (s$validconf_p < 1)
                            "Visits [insert condition implemented by `validconf_col`] were not used for event confirmation. "
                      else "",
                      # Relapses
                      if (rel_constraint) "A visit could not be used as confirmation if occurring within "
                      else "",
                      paste0(relintervals, collapse=" or "),
                      if (rel_constraint) {if (is.null(s$renddate_col) | length(relintervals) > 1)
                        " the onset of a relapse. "
                        else ". "}
                      else ""
                      )

  # %%%%%%%%%%%%%
  # BASELINE
  # %%%%%%%%%%%%%

  rel_constraint <- s$relapse_to_bl[1]>0 | s$relapse_to_bl[2]>0
  relintervals <- c()
  if (s$relapse_to_bl[1]>0 & !is.null(s$renddate_col)) {
    relintervals <- c(relintervals, "a relapse (onset-to-end)")
  } else if (s$relapse_to_bl[1]>0) {
    relintervals <- c(relintervals, paste0(s$relapse_to_bl[1], " days after"))
    }
  if (s$relapse_to_bl[2]>0) {relintervals <- c(relintervals, paste0(s$relapse_to_bl[2], " days before"))}
  if (s$baseline=="fixed") {
    baseline_text <- paste0("The baseline was kept fixed at the first visit",
                            if (rel_constraint) ", unless this occurred within " else "",
                            paste0(relintervals, collapse=" or "),
                            if (rel_constraint & (is.null(s$renddate_col) | length(relintervals) > 1)) " the onset of a relapse" else "",
                            if (rel_constraint) " - in which case the baseline was moved to the next eligible visit. " else ". "
                            )
  } else {
    baseline_text <- paste0("A roving baseline scheme was applied where the reference value was updated after each confirmed",
                            # Event triggering rebaseline
                            if (s$baseline=="roving_impr") " improvement event. "
                            else if (s$baseline=="roving_wors") " worsening event. "
                            else " worsening or improvement event. ",
                            # Proceed from
                            "The new baseline was set at ",
                            if (s$proceed_from=="event") "the event"
                            else "the first eligible confirmation visit for the event",
                            " that triggered the re-baseline. ",
                            # Sub-threshold
                            if (s$sub_threshold_rebl!="none")
                                   paste0("Rebaseline was also triggered by confirmed \"sub-threshold\" ", s$sub_threshold_rebl,
                                          ", i.e., when the shift in the ", outcome,
                                          " value was below the clinically meaningful threshold. ")
                            else "",
                            # Relapses
                            if (rel_constraint) "Whenever the current baseline fell within " else "",
                            paste0(relintervals, collapse=" or "),
                            if (rel_constraint & (is.null(s$renddate_col) | length(relintervals) > 1))
                              " the onset of a relapse"
                            else "",
                            if (rel_constraint) ", it was moved to the next eligible visit. " else ""
    )
  }

  baseline_text <- paste0(baseline_text,
                          # Relapse-based re-baseline
                          if (s$relapse_rebl) paste0("The onset of each relapse", ifelse(s$baseline=="fixed", "", " also"),
                                " prompted a re-baseline to the next eligible visit",
                                if (rel_constraint) " (out of relapse influence). " else ". ")
                          else "",
                          # New baseline >= previous baseline
                          if (s$bl_geq & (rel_constraint || s$relapse_rebl || s$baseline!="fixed"))
                                 "The re-baselined disability assessment could not be less than the original baseline value. "
                          else "",
                          # Skip local extrema
                          if (s$skip_local_extrema!="none" & (rel_constraint || s$relapse_rebl || s$baseline!="fixed"))
                                 paste0(ifelse(s$skip_local_extrema=="all", "Local", "Clinically meaningful local"),
                                        " minima or maxima were skipped when updating the baseline. ")
                          else ""
                          )

  # %%%%%%%%%%%%%
  # OTHER OPTIONS
  # %%%%%%%%%%%%%

  imputation_text <- if (s$impute_last_visit>0)
    paste0("In case of ", outcome, " worsening at the last visit, CDW was imputed ",
           if (s$impute_last_visit<Inf & s$impute_last_visit>=1)
             paste0("for patients terminating follow-up before day ", s$impute_last_visit)
           else "",
           if (s$impute_last_visit < 1)
             paste0(" with a probability of ", s$impute_last_visit)
           else "",
           ". ") else ""

  sustained_text <- if (s$require_sust_days>0)
    paste0("Confirmed events were only retained if sustained",
           if (s$require_sust_days < Inf)
             paste0(" either over ", s$require_sust_days, " days, or")
           else "",
           " until the end of follow-up. ") else ""

  rel_constraint <- s$relapse_to_event[1]>0 | s$relapse_to_event[2]>0
  relintervals <- c()
  if (s$relapse_to_event[1] > 0 & !is.null(s$renddate_col)) {
    relintervals <- c(relintervals, "a relapse (onset-to-end)")
  } else if (s$relapse_to_event[1]>0) {
    relintervals <- c(relintervals, paste0(s$relapse_to_event[1], " days after"))
    }
  if (s$relapse_to_event[2] > 0) {relintervals <- c(relintervals, paste0(s$relapse_to_event[2], " days before"))}
  rel_to_event_text <- paste0(if (rel_constraint) "Events occurring within " else "",
                              paste0(relintervals, collapse=" or "),
                              if (rel_constraint & (is.null(s$renddate_col) | length(relintervals) > 1)) " the onset of a relapse " else "",
                              if (rel_constraint) "were discarded. " else ""
                              )


  # %%%%%%%%%%%%%
  # RAW & PIRA
  # %%%%%%%%%%%%%

  # RAW
  raw_text <- paste0("A confirmed ", outcome,
                     " worsening event was labelled as relapse-associated worsening (RAW) if occurring within ",
                     if (is.null(s$renddate_col))
                       paste0(s$relapse_assoc[1], " days after the onset of a relapse")
                     else "a relapse (between onset and end)",
                     if (s$relapse_assoc[2] > 0) paste0(", or within ", s$relapse_assoc[2], " days before the onset of a relapse") else "",
                     ". ")

  # PIRA
  if (length(s$relapse_indep[["event"]]) == 2) {
    # "prec" event
    prec <- if (s$relapse_indep[["prec_type"]] == "baseline") "the baseline"
            else if (s$relapse_indep[["prec_type"]] == "last") "the last visit preceding the event"
            else paste0("the last pre-worsening visit")
    # PIRA definition
    pira_def <- ""
    for (point in c("prec", "event", "conf")) {
      if (!(is.null(s$relapse_indep[[point]][[1]]) & is.null(s$relapse_indep[[point]][[2]]))
          & !((!is.null(s$relapse_indep[[point]][[1]]) && (s$relapse_indep[[point]][[1]]==0))
              & (!is.null(s$relapse_indep[[point]][[2]]) && (s$relapse_indep[[point]][[2]]==0)))) {
        pp <- if (point == "prec") prec else if (point == "event") "the event" else "confirmation"
        pira_def <- paste0(pira_def,
                           if (!is.null(s$relapse_indep[[point]][[1]]))
                             paste0("from ", if (s$relapse_indep[[point]][[1]]>0) paste0(s$relapse_indep[[point]][[1]], " days before ") else "", pp)
                           else "")
        pira_def <-  paste0(pira_def,
                            if (!is.null(s$relapse_indep[[point]][[2]]))
                              paste0(" to ",
                                     if (s$relapse_indep[[point]][[2]] > 0)
                                       paste0(s$relapse_indep[[point]][[2]], " days after ")
                                     else "",
                                     pp, ", or ")
                            else "")
      }
    }
    pira_def <- gsub(".{5}$", "", pira_def)
  }
  # Full PIRA text
  pira_text <- paste0("A confirmed ", outcome,
                      " worsening event was labelled as progression independent of relapse activity (PIRA) if ",
                      if (length(s$relapse_indep[["event"]]) == 2)
                        paste0("no relapses started in the interval ", pira_def, ". ")
                      else paste0("it did not occur within a relapse (onset to end)",
                                  if (s$relapse_indep[["event"]] > 0)
                                    paste0(", or less than ", s$relapse_indep[["event"]], " days before a relapse")
                                  else "",
                                  ", and the confirmation did not occur within a relapse",
                                  if (s$relapse_indep[["conf"]] > 0)
                                    paste0(", or less than ", s$relapse_indep[["conf"]], " days before a relapse")
                                  else "",
                                  ". ")
                      )


  # %%%%%%%%%%%%%
  # FULL TEXT
  # %%%%%%%%%%%%%

  text <- paste0(conf_text, baseline_text,
                 imputation_text, sustained_text, rel_to_event_text,
                 if (s$event!="firstPIRA") raw_text else "",
                 if (s$event!="firstRAW") pira_text else "")

  cat("\n\nTextual description of applied criteria:\n", text, sep="")
  if (s$outcome=="outcome") {
    cat("\n---\nDirection of worsening: ", s$worsening)
  }
  cat("\n---\nClinically meaningful threshold for", outcome, "change (delta function):",
      if (is.null(s$delta_fun)) paste("default for", outcome,
            ifelse(web, '(see "Outcome definition" section)', "(check by typing ?compute_delta)."))
      else "user-specified (`delta_fun` argument above).")

}


