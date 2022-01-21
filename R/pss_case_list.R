#' Create Project SafeSchools Case List from Tracking Log
#'
#' @param tracking_log A data frame created from the pss_tracking_log function.
#'
#' @return A data frame for the PSS Case List
#' @export
#'
#' @examples
#' cases <- redcap_read(redcap_uri = url, "token"=token, events = redcap_events)$data
#' tracking_log <- pss_tracking_log(cases)
#' case_list <- pss_case_list(tracking_log)
pss_case_list <- function(tracking_log, report_date = Sys.Date()){
  ##### Construct Case List #####
  case_list <- tracking_log

  case_list <- case_list %>%
    #filter out records where participant was ineligible or declined
    filter(!(pot_participant_status %in% c("Decline", "Ineligible") | exitstatus %in% c(2, 3, 5)))

  case_list <- case_list %>%
    #filter out child idi's for teachers
    filter(!(role_final == "Teacher/Staff" & redcap_event_name %in% c("baseline_idi_child_consent","baseline_idi_child", "followup_idi_child")))

  case_list <- case_list %>%
    #filter out idi's for parents who are not selected for idis
    filter(!(role_final == "Parent/Caregiver" & idi_selected != 1 & redcap_event_name %in% c("baseline_idi_consent", "baseline_idi", "baseline_idi_child_consent","baseline_idi_child","followup_idi", "followup_idi_child")))

  case_list <- case_list %>%
    #filter out idi's for teachers/staff who are not selected for idis
    filter(!(role_final == "Teacher/Staff" & idi_selected != 1 & redcap_event_name %in% c("baseline_idi_consent", "baseline_idi", "followup_idi")))

  #remove any further idi's or idi consents if adult idi consent is not completed
  case_list <- case_list %>%
    filter(!(!(consent_idi %in% "1") & redcap_event_name %in% c("baseline_idi_consent", "baseline_idi", "followup_idi", "baseline_idi_child_consent","baseline_idi_child", "followup_idi_child")))

  declined_idi <- length(which(case_list$pot_participant_status_idi == "Decline" & case_list$redcap_event_name == "initial_contact_fo"))

  case_list <- case_list %>%
    #filter out idi's for participants who have decline participation in IDI
    filter(!(pot_participant_status_idi %in% c("Decline", "Excluded") & redcap_event_name %in% c("baseline_idi_consent", "baseline_idi", "followup_idi"))) %>%
    filter(!(consent_idi %in% 0 & redcap_event_name %in% c("baseline_idi_consent", "baseline_idi", "followup_idi"))) %>%
    filter(!(pot_participant_status_child %in% c("Decline", "Ineligible") & redcap_event_name %in% c("baseline_idi_child_consent", "baseline_idi_child", "followup_idi_child"))) %>%
    filter(!((parent_perm %in% 0 | youth_assent %in% 0) & redcap_event_name %in% c("baseline_idi_child_consent", "baseline_idi_child", "followup_idi_child")))

  case_list <- case_list %>%
    #filter out events beyond initial contact when initial contact is not complete
    filter(!(redcap_event_name != "initial_contact_fo" & recruitment_complete == 0 & event_complete != 1))

  #recode missing role final to unknown
  case_list$role_final[which(is.na(case_list$role_final))] <- "Role Unknown"

  #remove all events if exit status notes as  lost to follow-up, withdrew from study after baseline, declined before baseline was done or other
  case_list <- case_list %>%
    filter(!(exitstatus %in% c(2,3,5,6)))

  #find the record ids for participants that are currently doing hybrid or online learning. These will get marked on the case lists.
  virtual_idis <- case_list %>%
    filter(redcap_event_name == "initial_contact_fo",
           (parent_schooltypescurr == "Online/virtual learning (only learning online)" | parent_schooltypescurr == "Hybrid (split between learning online and in-person)" | teacherrole == "Online/virtual learning (only teaching/working online)" | parent_schooltypescurr == "Hybrid (split between teaching/working online and in-person)"))
  virtual_idis <- virtual_idis$record_id

  #get the current case_list (remove events that are not completed or not due yet, create case_list_flag and follow_up_break variables)
  case_list <- case_list %>% filter(
    !is.na(event_due_date),
    event_complete == 0,
    event_window_start <= report_date | is.na(event_window_start)
  ) %>% mutate(case_list_flag = case_when(
    redcap_event_name == "initial_contact_fo" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "baseline" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "consent" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "baseline_idi_consent" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "baseline_idi_child_consent" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "baseline_idi" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "baseline_idi_child" & event_due_date + 7 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "wave2" & event_due_date + 28 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "followup_idi" & event_due_date + 28 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "followup_idi_child" & event_due_date + 28 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "wave3" & event_due_date + 28 <= floor_date(report_date, unit="week", week_start = 4) ~ 1,
    redcap_event_name == "wave4" & event_due_date + 28 <= floor_date(report_date, unit="week", week_start = 4) ~ 1),
    site_call_flag = ifelse(event_window_end < floor_date(report_date, unit="week", week_start = 4), 1, 0),
    followup_break = case_when(
      redcap_event_name == "initial_contact_fo" & followup_break___0 == 1 ~ 1,
      redcap_event_name == "consent" & followup_break___1 == 1 ~ 1,
      redcap_event_name == "baseline" & followup_break___2 == 1 ~ 1,
      redcap_event_name == "baseline_idi_consent" & followup_break___3 == 1 ~ 1,
      redcap_event_name == "baseline_idi_child_consent" & followup_break___4 == 1 ~ 1,
      redcap_event_name == "baseline_idi" & followup_break___5 == 1 ~ 1,
      redcap_event_name == "baseline_idi_child" & followup_break___6 == 1 ~ 1,
      redcap_event_name == "wave2" & followup_break___7 == 1 ~ 1,
      redcap_event_name == "followup_idi" & followup_break___8 == 1 ~ 1,
      redcap_event_name == "followup_idi_child" & followup_break___9 == 1 ~ 1,
      redcap_event_name == "wave3" & followup_break___10 == 1 ~ 1,
      redcap_event_name == "wave4" & followup_break___11 == 1 ~ 1,
      TRUE ~ 0),
  ) %>%
    select(record_id, role_final, redcap_event_name, data_access_group, event_complete, event_due_date, event_window_start, event_window_end, besttime, notes, case_list_flag, site_call_flag, followup_break)

  ### final changes to case list formatting
  #add * for virtual records
  case_list$record_id[which(case_list$record_id %in% virtual_idis)] <- paste0(case_list$record_id[which(case_list$record_id %in% virtual_idis)], "*")
  #change date formatting to M-D-Y
  case_list <- case_list %>% mutate(event_due_date = format(event_due_date, "%m-%d-%Y"),
                                    event_window_end = format(event_window_end, "%m-%d-%Y"),
                                    event_window_start = format(event_window_start, "%m-%d-%Y"))

  return(case_list)
}
