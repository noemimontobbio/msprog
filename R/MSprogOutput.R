#' Objects of class "MSprogOutput"
#'
#' Objects of class `"MSprogOutput"` are returned by [MSprog()]. They contain
#' the results of event detection and associated metadata.
#'
#' @section Structure:
#' Objects of class `"MSprogOutput"` are lists with the following elements:
#'
#' \describe{
#' \item{`event_count`}{a data frame containing the event sequence detected for each subject, and the counts for each event type}
#' \item{`results`}{a data frame with extended info on each event for all subjects}
#' \item{`settings`}{a list containing all the arguments used to compute the output}
#' \item{`unconfirmed`}{a data frame with info on unconfirmed events (initial change from baseline, but no confirmation) for all subjects.}
#' }
#'
#' @section `event_count` data frame:
#' \describe{
#'   \item{event_sequence}{Sequence of detected events (if any), in chronological order.}
#'   \item{CDI}{Count of confirmed disability improvement (CDI) events.}
#'   \item{CDW}{Count of confirmed disability improvement (CDW) events.}
#'   \item{RAW}{Count of confirmed disability improvement (RAW) events.}
#'   \item{PIRA}{Count of confirmed disability improvement (PIRA) events.}
#'   \item{undef_CDW}{Count of confirmed disability improvement (undef_CDW) events.}
#' }
#' Note: depending on computation settings, some of the above columns may not be
#' included in the data frame
#' (e.g., no CDI column if `event='firstCDW'` in [MSprog()]).
#'
#' @section `results` data frame:
#' \describe{
#'   \item{`<subj_col>`}{Subject ID.}
#'   \item{nevent}{Cumulative count of events for the subject.}
#'   \item{date}{Date of event onset.}
#'   \item{value}{Outcome value at event onset.}
#'   \item{bl_date}{Date of baseline for the event.}
#'   \item{bl_value}{Outcome value at baseline.}
#'   \item{last_delta_date}{Date of last visit before event onset at a clinically meaningful score distance from it.}
#'   \item{last_delta_value}{Last outcome value before event onset at a clinically meaningful score distance from it.}
#'   \item{conf`<conf_days>`}{Whether the event was confirmed at `<conf_days>` days.}
#'   \item{conf`<conf_days>`_date}{Date of `<conf_days>`-day confirmation visit for the event.}
#'   \item{conf`<conf_days>`_value}{Outcome value at `<conf_days>`-day confirmation visit.}
#'   \item{PIRA_conf`<conf_days>`}{Whether the event was confirmed \emph{as a PIRA event} at `<conf_days>` days.}
#'   \item{PIRA_conf`<conf_days>`_date}{Date of (potential) `<conf_days>`-day confirmation visit for PIRA event.}
#'   \item{PIRA_conf`<conf_days>`_value}{Outcome value at (potential) `<conf_days>`-day confirmation visit for PIRA.}
#'   \item{total_fu}{Total length of follow-up period in days.}
#'   \item{time2event}{Number of days from start of follow-up to event onset.}
#'   \item{bl2event}{Number of days from baseline to event onset.}
#'   \item{sust_days}{Number of days for which the event was sustained.}
#'   \item{sust_last}{Whether the event was sustained up to the end of follow-up.}
#' }
#' Note: some of the columns may not be included, depending on computation settings
#' (e.g., no date columns if `include_dates=F` in [MSprog()]).
#'
#' @name MSprogOutput
#' @docType class
#' @keywords internal
NULL
