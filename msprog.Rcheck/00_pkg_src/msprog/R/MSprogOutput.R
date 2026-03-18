#' Objects of class "MSprogOutput"
#'
#' Objects of class `"MSprogOutput"` are returned by [MSprog()]. They contain
#' the results of event detection and associated metadata.
#'
#' @section Structure:
#' Objects of class `"MSprogOutput"` are lists with the following elements:
#'
#' \describe{
#' \item{`event_count`}{A data frame containing the event sequence detected for each subject, and the counts for each event type}
#' \item{`results`}{A data frame with extended info on each event for all subjects}
#' \item{`settings`}{A list containing all the arguments used to compute the output}
#' \item{`unconfirmed`}{A data frame with info on unconfirmed events (initial change from baseline, but no confirmation) for all subjects.}
#' }
#'
#' @section `event_count` data frame:
#' \describe{
#'   \item{event_sequence}{Sequence of detected events (if any), in chronological order.}
#'   \item{CDI}{Count of confirmed disability improvement (CDI) events.}
#'   \item{CDW}{Count of confirmed disability worsening (CDW) events.}
#'   \item{RAW}{Count of relapse-associated worsening (RAW) events.}
#'   \item{PIRA}{Count of progression independent of relapse activity (PIRA) events.}
#' }
#' Note: depending on computation settings, some of the above columns may not be
#' included in the data frame (e.g., no CDI column if `event='firstCDW'` in [MSprog()]).
#'
#' @section `results` data frame:
#' \describe{
#'   \item{`<subj_col>`}{Subject ID.}
#'   \item{nevent}{Cumulative count of events for the subject.}
#'   \item{event_type}{Type of event (CDI or CDW).}
#'   \item{CDW_type}{Type of CDW (PIRA, RAW, undefined).}
#'   \item{total_fu}{Total length of follow-up period in days.}
#'   \item{time2event}{Number of days from start of follow-up to event onset.}
#'   \item{bl2event}{Number of days from baseline to event onset.}
#'   \item{date}{Date of event onset.}
#'   \item{value}{Outcome value at event onset.}
#'   \item{bl_date}{Date of baseline for the event.}
#'   \item{bl_value}{Outcome value at baseline.}
#'   \item{last_delta_date}{Date of last visit before event onset at a clinically meaningful score distance from it.}
#'   \item{last_delta_value}{Last outcome value before event onset at a clinically meaningful score distance from it.}
#'   \item{conf`<conf_days>`_date}{Date of `<conf_days>`-day confirmation visit for the event.}
#'   \item{conf`<conf_days>`_value}{Outcome value at `<conf_days>`-day confirmation visit.}
#'   \item{PIRA_conf`<conf_days>`_date}{For PIRA events, date of `<conf_days>`-day confirmation visit
#'   (could be different from CDW confirmation visit if the PIRA definition includes confirmation-related constraints on relapses).}
#'   \item{PIRA_conf`<conf_days>`_value}{For PIRA events, outcome value at `<conf_days>`-day confirmation visit
#'   (could be different from CDW confirmation visit if the PIRA definition includes confirmation-related constraints on relapses).}
#'   \item{sust_days}{Number of days for which the event was sustained.}
#'   \item{sust_last}{Whether the event was sustained up to the end of follow-up.}
#' }
#' Note: some of the columns may not be included, depending on computation settings
#' (e.g., no date columns if `include_dates=F` in [MSprog()]).
#'
#' @section `unconfirmed` data frame:
#' \describe{
#'   \item{`<subj_col>`}{Subject ID.}
#'   \item{date}{Date of initial outcome change.}
#'   \item{value}{Outcome value at initial outcome change.}
#'   \item{bl_date}{Date of baseline.}
#'   \item{bl_value}{Outcome value at baseline.}
#'   \item{closest_rel_before}{Distance in days from the last relapse before the outcome change (if any).}
#'   \item{closest_rel_after}{Distance in days from the next relapse after the outcome change (if any).}
#'  }
#'  Note: in first-event scenarios (e.g., `event='firstCDW'`), the event search stops as soon as
#'  the first confirmed event is found -- in this case, the `unconfirmed` data frame only reports
#'  unconfirmed outcome changes occurring *before* detecting a confirmed event.
#'
#' @name MSprogOutput
#' @docType class
#' @keywords internal
NULL
