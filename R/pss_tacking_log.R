#' Create PSS Tracking Log From REDCap Data
#'
#' @param cases A data frame with output from redcap_read function for PSS
#'
#' @return A cleaned tracking log data frame.
#' @export
#'
#' @examples
#' cases <- redcap_read(redcap_uri = url, "token"=token, events = redcap_events)$data
#' tracking_log <- pss_tracking_log(cases)
#'
pss_tracking_log <- function(cases, week_start_day = 5) {
  redcap_events <- c("initial_contact_fo_arm_1", "consent_arm_1", "baseline_arm_1","baseline_idi_consent_arm_1", "baseline_idi_arm_1","baseline_idi_child_consent_arm_1", "baseline_idi_child_arm_1", "wave2_arm_1", "followup_idi_arm_1","followup_idi_child_arm_1", "wave3_arm_1", "wave4_arm_1")

  ############# Clean Data #############
  #define variables for sets of REDCap variables that we need for the case list
  cases_info_vars <- c("record_id", "redcap_event_name", "iden_date_mdy", "contact_dob", "contact_currentage", "eligible", "role_final", "school_comb", "besttimetoreach___1", "besttimetoreach___2", "besttimetoreach___3", "besttimetoreach___4", "besttimetoreach___5","randomization_child_num", "pot_participant_status", "pot_participant_status_idi","pot_participant_status_child", "exitstatus", "idi_selected", "randomization_child_age", "followup_break___0", "followup_break___1", "followup_break___2", "followup_break___3", "followup_break___4", "followup_break___5", "followup_break___6", "followup_break___7", "followup_break___8", "followup_break___9", "followup_break___10", "followup_break___11")
  consent_vars <- c("consent", "consent_date", "informed_consent_complete","consent_idi", "consent_idi_date", "idi_informed_consent_complete", "parent_perm", "parent_perm_date", "idi_parental_permission_complete", "youth_assent", "youth_assent_date", "idi_youth_assent_complete")
  idi_vars <- c("idi_date", "idi_date_child")
  survey_vars <- c("date_baseline_demo", "date_wave_completed", "survey_admin_name", "date", "survey_admin_date1", "survey_admin_complete1", "survey_admin_date2", "survey_admin_complete2", "survey_admin_date3", "survey_admin_complete3", "survey_admin_date4", "survey_admin_complete4")
  demo_vars <- c('age_yrs', 'race_ethn_race___1', 'race_ethn_race___2', 'race_ethn_race___3', 'race_ethn_race___4', 'race_ethn_race___5', 'race_ethn_race___6', 'race_ethn_race___15', 'race_ethn_race___99', 'race_ethn_orig_other', 'race_ethn_native', 'tribe___1', 'tribe___2', 'tribe___3', 'tribe___4', 'tribe___5', 'tribe___6', 'tribe___7', 'tribe_specify', 'tribe_num', 'tribe_most', 'tribe_most_other', 'tribe_most_text', 'tribe_str', 'race_ethn_hispanic', 'race_ethn_hispanic_detail___1', 'race_ethn_hispanic_detail___2', 'race_ethn_hispanic_detail___3', 'race_ethn_hispanic_detail___4', 'race_ethn_hispanic_other', 'bio_sex_birth', 'gender_identity_term___1', 'gender_identity_term___0', 'gender_identity_term___2', 'gender_identity_term___3', 'gender_identity_term___4', 'gender_identity_term___5', 'gender_identity_term___6', 'gender_identity_term___7', 'gender_identity_term___8', 'gender_identity_term___96', 'gender_identity_term___99', 'pregnancy_status', 'language_spoken___1', 'language_spoken___2', 'language_spoken___3', 'language_spoken___4', 'language_spoken___5', 'language_spoken___6', 'language_spoken___7', 'language_spoken_other', 'maritalstatus', 'maritalstatus_other', 'edu_years_of_school', 'bio_sex_birth_child', 'gender_identity_term_child', 'child_race___1', 'child_race___2', 'child_race___3', 'child_race___4', 'child_race___5', 'child_race___15', 'child_race___99', 'user_demo', 'demographics_skip', 'demographics_skip_conf', 'demographics_complete')
  school_enroll_vars <- c('teacherrole_yn', 'teacher_currentschool_type', 'teacherrole', 'teacherrole_desc', 'teacherrole_currsatis', 'teacherrole_hist___0', 'teacherrole_hist___1', 'teacherrole_hist___3', 'teacherrole_hist___4', 'teacherrole_desc_hist', 'teacherrole_pastsatis', 'parent_currentschool_yn', 'parent_currentschool_type', 'parent_schooltypescurr', 'parent_schooltypescurr_desc', 'parent_school_currsatis', 'parent_schooltypes___0', 'parent_schooltypes___1', 'parent_schooltypes___3', 'parent_schooltypes___4', 'parent_schooltypes___5', 'parent_schooltypes_desc', 'parent_schoolpastsatis', 'answered_promoted_2021', 'answered_promoted_2022', 'answered_promoted_2023', 'child_promoted_year', 'child_promoted', 'child_promoted_add_work', 'repeat_grade_year', 'child_promoted_repeat', 'child_acad_satisfaction', 'user_role', 'school_enrollment_and_promotion_skip', 'school_enrollment_and_promotion_skip_conf', 'school_enrollment_and_promotion_complete')
  access_service_vars <- c('services_recd_yn', 'services_recd___1', 'services_recd___2', 'services_recd___3', 'services_recd___4', 'services_recd___5', 'services_recd___6', 'services_recd___7', 'services_recd___8', 'services_recd___9', 'services_recd___10', 'services_recd___88', 'services_recd_other', 'services_no_access_yn', 'services_no_access___1', 'services_no_access___2', 'services_no_access___3', 'services_no_access___4', 'services_no_access___5', 'services_no_access___6', 'services_no_access___7', 'services_no_access___8', 'services_no_access___9', 'services_no_access___10', 'services_no_access___88', 'services_no_access_other', 'services_difficulty')
  housing_employ_vars <- c('jobloss_covid19', 'current_employment_status___1', 'current_employment_status___2', 'current_employment_status___3', 'current_employment_status___4', 'current_employment_status___5', 'current_employment_status___6', 'current_employment_status___7', 'current_employment_status___8', 'current_employment_status___96', 'current_employment_status___98', 'current_employment_status___99', 'cur_employ_stat_specify', 'employed_ew', 'employed_healthcare___1', 'employed_healthcare___2', 'employed_healthcare___11', 'employed_healthcare___12', 'employed_healthcare___3', 'employed_healthcare___4', 'employed_healthcare___5', 'employed_healthcare___6', 'employed_healthcare___7', 'employed_healthcare___8', 'employed_healthcare___9', 'employed_healthcare___10', 'employed_healthcare___0', 'employed_education___1', 'employed_education___2', 'employed_education___3', 'employed_education___4', 'employed_education___5', 'employed_education___6', 'employed_education___7', 'employed_education___8', 'employed_education_other', 'highrisk', 'jobtype', 'jobtype_other', 'work_wash', 'work_closecont', 'work_ppe', 'hi_ihs', 'hi_coverage_type___0', 'hi_coverage_type___3', 'hi_coverage_type___1', 'hi_coverage_type___2', 'hi_coverage_type___98', 'hi_coverage_type___99', 'hi_loss_covid', 'transportation', 'transportation_other', 'money', 'household_homeless', 'household_congregate', 'household_other', 'placeslived_pastyr', 'monthslived_home', 'stable_housing', 'numppl_home', 'havechildren', 'livewchildren', 'caretakers___1', 'caretakers___2', 'caretakers___3', 'caretakers___4', 'caretakers___5', 'caretakers___6', 'caretakers___7', 'caretakers___8', 'caretakers___9', 'caretakers___10', 'caretakers___11', 'caretakers___12', 'caretakers___13', 'caretakers___14', 'caretakers___15', 'caretakers___16', 'caretakers___17', 'caretakers_other', 'household_famgen___1', 'household_famgen___2', 'household_famgen___3', 'household_famgen___4', 'household_famgen___5', 'household_famgen___6', 'household_famgen___7', 'household_famgen___8', 'household_famgen___9', 'household_famgen___10', 'household_famgen___11', 'household_famgen___12', 'household_famgen___13', 'household_famgen___14', 'household_famgen___15', 'household_famgen_other', 'over65', 'numrooms', 'high_risk', 'household_highrisk', 'highrisk_condition___1', 'highrisk_condition___2', 'highrisk_condition___3', 'highrisk_condition___4', 'highrisk_condition___5', 'highrisk_condition___6', 'highrisk_condition___7', 'highrisk_condition___8', 'highrisk_condition___9', 'highrisk_condition___10', 'highrisk_condition___11', 'highrisk_conditionother', 'high_risk_child', 'covid_pandemic_challenges_healthcare', 'covid_pandemic_challenges_abode', 'covid_pandemic_challenges_food', 'covid_pandemic_challenges_water', 'covid_pandemic_challenges_medications', 'covid_pandemic_challenges_transportation', 'self_reported_health_status_assessment', 'self_reported_disability')
  # #select only the needed variables from the cases df
  # tracking_log <- cases %>% select(all_of(cases_info_vars), all_of(consent_vars), all_of(idi_vars), all_of(survey_vars), all_of(demo_vars), all_of(school_enroll_vars), all_of(housing_employ_vars), all_of(access_service_vars))
  tracking_log <- cases

  #duplicate IDI events so you can split them up into idi consent, idi, idi child consent, idi child
  baseline_idis <- tracking_log %>% slice(rep(which(redcap_event_name == "baseline_idi_arm_1"), each = 3)) #create four copies of each baseline IDI record
  baseline_idis$redcap_event_name <- rep(c("baseline_idi_consent_arm_1", "baseline_idi_child_consent_arm_1", "baseline_idi_child_arm_1"), length.out = nrow(baseline_idis)) #change redcap event names
  tracking_log <- rbind(tracking_log, baseline_idis)

  followup_idis <- tracking_log %>% slice(rep(which(redcap_event_name == "followup_idi_arm_1"), each = 1))
  followup_idis$redcap_event_name <- rep(c("followup_idi_child_arm_1"), length.out = nrow(followup_idis))
  tracking_log <- rbind(tracking_log, followup_idis)

  #create rows for any missing redcap events
  tracking_log <- tracking_log %>% complete(record_id, redcap_event_name = redcap_events)

  #set important variables as factors
  tracking_log$redcap_event_name <- factor(tracking_log$redcap_event_name, levels = redcap_events)
  tracking_log$pot_participant_status <- factor(tracking_log$pot_participant_status, levels = c(1, 5, 2, 3, 4), labels = c("Undecided", "Waiting for Consent", "Consent", "Decline", "Ineligible"))
  tracking_log$role_final <- factor(tracking_log$role_final, levels = c(1, 2, 3), labels = c("Parent/Caregiver", "Teacher/Staff", "Role Unknown"))

  #create data_access_group variable
  tracking_log <- tracking_log %>% mutate(data_access_group = case_when(
    substr(record_id, 1, 4) == "5255" ~ "wmat",
    substr(record_id, 1, 4) == "5256" ~ "shiprock",
    substr(record_id, 1, 4) == "5257" ~ "tuba_city",
    substr(record_id, 1, 4) == "5258" ~ "chinle",
  ),
  tribe = case_when(
    substr(record_id, 1, 4) %in% c("5255") ~ "WMAT",
    substr(record_id, 1, 4) %in% c("5256", "5257", "5258") ~ "Navajo"
  ))

  #add in a date for any initial contact forms that are missing a date; arrange by DAG, then redcap_event_name, create previous_record_date variable
  tracking_log <- tracking_log %>% arrange(redcap_event_name, record_id) %>%
    group_by(data_access_group, redcap_event_name) %>%
    mutate(previous_record_date = lag(iden_date_mdy)) %>%
    ungroup()
  #set the date for the initial contact form to the previous initial contact form date within the DAG if missing
  tracking_log$iden_date_mdy[which(is.na(tracking_log$iden_date_mdy))] <- tracking_log$previous_record_date[which(is.na(tracking_log$iden_date_mdy))]

  #combine best time to reach variable
  tracking_log <- tracking_log %>%
    mutate(besttime1 = recode(besttimetoreach___1, "1" = "7 - 9am", "0" = ""),
           besttime2 = recode(besttimetoreach___2, "1" = "9 - 12pm", "0" = ""),
           besttime3 = recode(besttimetoreach___3, "1" = "12 - 3pm", "0" = ""),
           besttime4 = recode(besttimetoreach___4, "1" = "3 - 6pm", "0" = ""),
           besttime5 = recode(besttimetoreach___5, "1" = "6 - 8pm", "0" = ""),
           besttime = paste(besttime1, besttime2, besttime3, besttime4, besttime5, sep = ";"),
           besttime = gsub("NA", "", besttime),
           besttime = gsub("^;|;$", "", besttime),
           besttime = gsub("^;|;$", "", besttime),
           besttime = gsub("^;|;$", "", besttime),
           besttime = gsub("^;|;$", "", besttime),
           besttime = gsub("^;|;$", "", besttime),
           besttime = gsub(";", "; ", besttime),
           besttime = gsub(" ;", "", besttime)
    )

  tracking_log$besttime[which(tracking_log$besttime =="" )] <- NA

  #create a variable for if the recruitment (eligibility screen, role, and randomization for parents) is done
  tracking_log <- tracking_log %>% mutate(recruitment_complete = ifelse(
    eligible == 1 & !is.na(role_final) & (role_final == "Teacher/Staff" | (role_final == "Parent/Caregiver" & !is.na(randomization_child_num))), 1, 0
  )) #sets 1 for recruitment complete; 0 to all other records
  tracking_log$recruitment_complete[which(tracking_log$redcap_event_name != "initial_contact_fo_arm_1")] <- NA #change 0 to NA for any non-initial contact form events; this is so that you can fill them later based on carrying forward the initial contact recruitment complete

  #remove missing data_access_group records (test records)
  tracking_log <- filter(tracking_log, !is.na(data_access_group) | record_id == "5256-18")
  #remove child idis for 5257-1; was selected for IDI by staff but child is not eligible
  tracking_log <- filter(tracking_log, !(record_id == "5257-1" & redcap_event_name %in% c("baseline_idi_child_consent_arm_1", "baseline_idi_child_arm_1", "followup_idi_child_arm_1")))

  #create variables for finding completed baselines that are not marked as complete yet
  parent_baseline_forms <- c('demographics', 'housing_employment_and_insurance', 'school_enrollment_and_promotion', 'communications', 'food_insecurity', 'access_to_services', 'adult_distress', 'alcohol_and_tobacco', 'covid_testing', 'covid_prevention_strategies', 'vaccines_and_masks', 'virtual_and_inperson_learning', 'culture_routines', 'tribal_identification', 'communal_mastery', 'promis_anxiety', 'center_for_epidemiological_studies_depression_revi', 'problems_from_stressful_experiences', 'selfharm', 'adverse_childhood_experiences', 'rosenberg_selfesteem', 'trait_hope', 'alcohol_and_tobacco_child', 'covid_testing_child', 'vaccines_and_masks_child', 'strengths_and_difficulties_questionnaire', 'child_anxiety', 'child_and_adolescent_trauma_screen', 'child_selfharm', 'child_and_youth_resilience_measure', 'child_selfefficacy', 'education_perceptions', 'medical_history')
  teacher_baseline_forms <- c('demographics', 'housing_employment_and_insurance', 'school_enrollment_and_promotion', 'food_insecurity', 'access_to_services', 'adult_distress', 'alcohol_and_tobacco', 'covid_testing', 'covid_prevention_strategies', 'vaccines_and_masks', 'virtual_and_inperson_learning', 'medical_history')
  parent_forms_complete_vars <- paste0(parent_baseline_forms, "_complete")
  teacher_forms_complete_vars <- paste0(teacher_baseline_forms, "_complete")

  ############ build tracking log #################
  tracking_log <- tracking_log %>%
    arrange(record_id, redcap_event_name) %>%
    group_by(record_id) %>%
    #carry forward important variables
    fill(
      iden_date_mdy:idi_youth_assent_complete,
      date_baseline_demo,
      besttime,
      recruitment_complete,
      all_of(demo_vars),
      .direction = "downup"
    ) %>%
    fill(
      all_of(school_enroll_vars),
      all_of(access_service_vars),
      .direction = "up"
    ) %>%
    ungroup() %>%
    #rename initial contact and baseline dates
    mutate(initial_contact_date = iden_date_mdy,
           baseline_date = date_baseline_demo) %>%
    mutate(
      #remove _arm_1 from event names for easier coding
      redcap_event_name = gsub("_arm_1", "", redcap_event_name),
      #count number of parent & teacher forms completed
      parent_forms_completed = rowSums(ifelse(across(all_of(parent_forms_complete_vars)) == 2,1,0)),
      teacher_forms_completed = rowSums(ifelse(across(all_of(teacher_forms_complete_vars)) == 2,1,0)),
      #create event_complete variable
      event_complete = case_when(
        redcap_event_name == "initial_contact_fo" & recruitment_complete == 1 ~ 1,
        redcap_event_name == "consent" & informed_consent_complete == 2 ~ 1,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & (survey_admin_complete1 == 1 | survey_admin_complete2 == 1 | survey_admin_complete3 == 1 | survey_admin_complete4 == 1) ~ 1,
        redcap_event_name %in% c("baseline") & ((role_final == "Parent/Caregiver" & parent_forms_completed == 33) | (role_final == "Teacher/Staff" & teacher_forms_completed == 12)) ~ 1,
        redcap_event_name %in% c("wave2", "wave4") & ((role_final == "Parent/Caregiver" & parent_forms_completed == 28) | (role_final == "Teacher/Staff" & teacher_forms_completed == 8)) ~ 1,
        redcap_event_name %in% c("wave3") & ((role_final == "Parent/Caregiver" & parent_forms_completed == 29) | (role_final == "Teacher/Staff" & teacher_forms_completed == 9)) ~ 1,
        redcap_event_name == "baseline_idi_consent" & !is.na(consent_idi) & !is.na(consent_idi_date) ~ 1,
        redcap_event_name == "baseline_idi" & !is.na(idi_date) ~ 1,
        redcap_event_name == "baseline_idi_child_consent" & !is.na(parent_perm) & !is.na(parent_perm_date) & !is.na(youth_assent) & !is.na(youth_assent_date) ~ 1,
        redcap_event_name == "baseline_idi_child" & !is.na(idi_date_child) ~ 1,
        redcap_event_name == "followup_idi" & !is.na(idi_date) ~ 1,
        redcap_event_name == "followup_idi_child" & !is.na(idi_date_child) ~ 1,
        TRUE ~ 0),
      initial_contact_date = as.Date(initial_contact_date),
      consent_date = as.Date(consent_date),
      survey_admin_date1 = as.Date(survey_admin_date1),
      survey_admin_date2 = as.Date(survey_admin_date2),
      survey_admin_date3 = as.Date(survey_admin_date3),
      survey_admin_date4 = as.Date(survey_admin_date4),
      consent_idi_date = as.Date(consent_idi_date),
      youth_assent_date = as.Date(youth_assent_date),
      idi_date = as.Date(idi_date),
      idi_date_child = as.Date(idi_date_child),
      event_complete_date = case_when(
        redcap_event_name == "initial_contact_fo" & recruitment_complete == 1 ~ initial_contact_date,
        redcap_event_name == "consent" & !is.na(consent) & !is.na(consent_date) ~ consent_date,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & survey_admin_complete1 == 1 ~ survey_admin_date1,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & survey_admin_complete2 == 1 ~ survey_admin_date2,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & survey_admin_complete3 == 1 ~ survey_admin_date3,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & survey_admin_complete4 == 1 ~ survey_admin_date4,
        redcap_event_name %in% c("baseline") & event_complete == 1 ~ baseline_date,
        redcap_event_name %in% c("wave2", "wave3", "wave4") & event_complete == 1 ~ date_wave_completed,
        redcap_event_name == "baseline_idi_consent" & !is.na(consent_idi) & !is.na(consent_idi_date) ~ consent_idi_date,
        redcap_event_name == "baseline_idi" & !is.na(idi_date) ~ idi_date,
        redcap_event_name == "baseline_idi_child_consent" & !is.na(parent_perm) & !is.na(parent_perm_date) & !is.na(youth_assent) & !is.na(youth_assent_date) ~ youth_assent_date,
        redcap_event_name == "baseline_idi_child" & !is.na(idi_date_child) ~ idi_date_child,
        redcap_event_name == "followup_idi" & !is.na(idi_date) ~ idi_date,
        redcap_event_name == "followup_idi_child" & !is.na(idi_date_child) ~ idi_date_child
      ),
      week = floor_date(event_complete_date, unit = "week", week_start = week_start_day),
      month = floor_date(event_complete_date, unit = "month"),
      event_complete_week = floor_date(event_complete_date, unit = "week", week_start = week_start_day),
      event_complete_month = month(event_complete_date),
      event_complete_yr_month = format(event_complete_date, "%Y-%m"),
      event_complete_qtr = quarter(event_complete_date),
      event_complete_year = year(event_complete_date),
      event_complete_yr_qtr = paste0(event_complete_year, ", Q", event_complete_qtr),
      notes = "")

  #add in due dates & windows
  tracking_log <- tracking_log %>%
    mutate(
      event_due_date = case_when(
        redcap_event_name == 'initial_contact_fo' ~ initial_contact_date,
        redcap_event_name == 'consent' ~ initial_contact_date,
        redcap_event_name == 'baseline' ~ initial_contact_date,
        redcap_event_name == 'baseline_idi_consent' ~ baseline_date,
        redcap_event_name == 'baseline_idi' ~ baseline_date,
        redcap_event_name == 'baseline_idi_child_consent' ~ baseline_date,
        redcap_event_name == 'baseline_idi_child' ~ baseline_date,
        redcap_event_name == 'wave2' ~ baseline_date + 365*.5,
        redcap_event_name == 'followup_idi' ~ baseline_date + 365*.5,
        redcap_event_name == 'followup_idi_child' ~ baseline_date + 365*.5,
        redcap_event_name == 'wave3' ~ baseline_date + 365,
        redcap_event_name == 'wave4' ~ baseline_date + 365 *1.5
      ),
      event_window_start = case_when(
        redcap_event_name %in% c('wave2', 'followup_idi', 'followup_idi_child', 'wave3', 'wave4') ~ event_due_date - 14,
        redcap_event_name %in% c('baseline','consent', 'baseline_idi', 'baseline_idi_child', 'baseline_idi_consent', 'baseline_idi_child_consent') ~ event_due_date
      ),
      event_window_end = case_when(
        redcap_event_name %in% c('wave2', 'followup_idi', 'followup_idi_child', 'wave3', 'wave4') ~ event_due_date + 28,
        redcap_event_name %in% c('baseline', 'baseline_idi', 'baseline_idi_child', 'baseline_idi_consent', 'baseline_idi_child_consent') ~ event_due_date + 28,
        redcap_event_name == 'consent' ~ event_due_date + 14
      ),
      days_after_window_start = Sys.Date() - event_window_start
    )

  #add list of when each event is complete to each event so you can reference across events
  event_complete_df <- tracking_log %>% select(record_id, redcap_event_name, event_complete, event_complete_date) %>% pivot_wider(names_from = redcap_event_name, values_from = c(event_complete, event_complete_date))

  tracking_log <- merge(tracking_log, event_complete_df, by = "record_id", all.x = TRUE)

  #labels, factors, and values for demographic variables
  #combine parent school type and teacher school type
  tracking_log <- tracking_log %>% mutate(school_type = case_when(
    role_final == "Teacher/Staff" ~ teacherrole,
    role_final == "Parent/Caregiver" ~ parent_schooltypescurr
  ))

  tracking_log <- tracking_log %>% mutate(age_cat_child = case_when(
    randomization_child_age >= 8 & randomization_child_age <= 12 ~ "8-12",
    randomization_child_age >= 13 & randomization_child_age <= 16 ~ "13-16"
  ))
  #remove randomization child age for teachers (exports as 0 from REDCap)
  tracking_log$randomization_child_age[which(tracking_log$role_final  == "Teacher/Staff")] <- NA

  #remove index child's sex for teachers (these were already all NA so I'm unsure why I did this)
  tracking_log$bio_sex_birth_child[which(tracking_log$role_final  == "Teacher/Staff")] <- NA

  #tracking_log$redcap_data_access_group = factor(tracking_log$redcap_data_access_group,levels=c("chinle","shiprock","tuba_city","wmat"))
  tracking_log$race_ethn_race___1 = factor(tracking_log$race_ethn_race___1,levels=c("1"))
  tracking_log$race_ethn_race___2 = factor(tracking_log$race_ethn_race___2,levels=c("1"))
  tracking_log$race_ethn_race___3 = factor(tracking_log$race_ethn_race___3,levels=c("1"))
  tracking_log$race_ethn_race___4 = factor(tracking_log$race_ethn_race___4,levels=c("1"))
  tracking_log$race_ethn_race___5 = factor(tracking_log$race_ethn_race___5,levels=c("1"))
  tracking_log$race_ethn_race___6 = factor(tracking_log$race_ethn_race___6,levels=c("1"))
  tracking_log$race_ethn_race___15 = factor(tracking_log$race_ethn_race___15,levels=c("1"))
  tracking_log$race_ethn_hispanic = factor(tracking_log$race_ethn_hispanic,levels=c("1"))
  tracking_log$tribe___1 = factor(tracking_log$tribe___1,levels=c("1"))
  tracking_log$tribe___2 = factor(tracking_log$tribe___2,levels=c("1"))
  tracking_log$tribe___3 = factor(tracking_log$tribe___3,levels=c("1"))
  tracking_log$tribe___4 = factor(tracking_log$tribe___4,levels=c("1"))
  tracking_log$tribe___5 = factor(tracking_log$tribe___5,levels=c("1"))
  tracking_log$tribe___6 = factor(tracking_log$tribe___6,levels=c("1"))
  tracking_log$tribe___7 = factor(tracking_log$tribe___7,levels=c("1"))
  tracking_log$bio_sex_birth = factor(tracking_log$bio_sex_birth, levels=c("1", "0"))
  tracking_log$gender_identity_term___0 = factor(tracking_log$gender_identity_term___0,levels=c("1"))
  tracking_log$gender_identity_term___1 = factor(tracking_log$gender_identity_term___1,levels=c("1"))
  tracking_log$gender_identity_term___2 = factor(tracking_log$gender_identity_term___2,levels=c("1"))
  tracking_log$gender_identity_term___3 = factor(tracking_log$gender_identity_term___3,levels=c("1"))
  tracking_log$gender_identity_term___4 = factor(tracking_log$gender_identity_term___4,levels=c("1"))
  tracking_log$gender_identity_term___5 = factor(tracking_log$gender_identity_term___5,levels=c("1"))
  tracking_log$gender_identity_term___6 = factor(tracking_log$gender_identity_term___6,levels=c("1"))
  tracking_log$gender_identity_term___7 = factor(tracking_log$gender_identity_term___7,levels=c("1"))
  tracking_log$gender_identity_term___8 = factor(tracking_log$gender_identity_term___8,levels=c("1"))
  tracking_log$gender_identity_term___96 = factor(tracking_log$gender_identity_term___96,levels=c("1"))
  tracking_log$language_spoken___1 = factor(tracking_log$language_spoken___1,levels=c("1"))
  tracking_log$language_spoken___2 = factor(tracking_log$language_spoken___2,levels=c("1"))
  tracking_log$language_spoken___3 = factor(tracking_log$language_spoken___3,levels=c("1"))
  tracking_log$language_spoken___4 = factor(tracking_log$language_spoken___4,levels=c("1"))
  tracking_log$language_spoken___5 = factor(tracking_log$language_spoken___5,levels=c("1"))
  tracking_log$language_spoken___6 = factor(tracking_log$language_spoken___6,levels=c("1"))
  tracking_log$language_spoken___7 = factor(tracking_log$language_spoken___7,levels=c("1"))
  tracking_log$maritalstatus = factor(tracking_log$maritalstatus,levels=c("1","2","3","4","5","6","7"))
  tracking_log$pregnancy_status = factor(tracking_log$pregnancy_status,levels=c("1"))
  tracking_log$edu_years_of_school = factor(tracking_log$edu_years_of_school,levels=c("0","1","2","3","4","5","6","7","98","99"))
  tracking_log$bio_sex_birth_child = factor(tracking_log$bio_sex_birth_child,levels=c("0","1","2"))
  tracking_log$child_race___1 = factor(tracking_log$child_race___1,levels=c("1"))
  tracking_log$child_race___2 = factor(tracking_log$child_race___2,levels=c("1"))
  tracking_log$child_race___3 = factor(tracking_log$child_race___3,levels=c("1"))
  tracking_log$child_race___4 = factor(tracking_log$child_race___4,levels=c("1"))
  tracking_log$child_race___5 = factor(tracking_log$child_race___5,levels=c("1"))
  tracking_log$child_race___15 = factor(tracking_log$child_race___15,levels=c("1"))
  tracking_log$parent_currentschool_yn = factor(tracking_log$parent_currentschool_yn,levels=c("1","0"))
  tracking_log$parent_currentschool_type = factor(tracking_log$parent_currentschool_type,levels=c("0","1","4"))
  tracking_log$parent_schooltypescurr = factor(tracking_log$parent_schooltypescurr,levels=c("0","1","3","4"))
  tracking_log$teacherrole = factor(tracking_log$teacherrole,levels=c("0","1","3","4"))
  tracking_log$school_type = factor(tracking_log$school_type,levels=c("0","1","3","4"))
  tracking_log$age_cat_child = factor(tracking_log$age_cat_child, levels=c("8-12", "13-16"))
  tracking_log$pot_participant_status_idi = factor(tracking_log$pot_participant_status_idi, levels=c(1,2,3,4,5), labels = c("Undecided", "Waiting for Consent", "Consent", "Decline", "Excluded"))
  tracking_log$pot_participant_status_child = factor(tracking_log$pot_participant_status_child, levels=c(1,2,3,4), labels = c("Undecided", "Consent", "Decline", "Ineligible"))

  #levels(tracking_log$redcap_data_access_group)=c("Chinle","Shiprock","Tuba City","WMAT")
  levels(tracking_log$race_ethn_race___1)=c("American Indian or Alaskan Native")
  levels(tracking_log$race_ethn_race___2)=c("Black or African American")
  levels(tracking_log$race_ethn_race___3)=c("Asian")
  levels(tracking_log$race_ethn_race___4)=c("Native Hawaiian or Other Pacific Islander")
  levels(tracking_log$race_ethn_race___5)=c("White")
  levels(tracking_log$race_ethn_race___6)=c("Other Indigenous Ancestry")
  levels(tracking_log$race_ethn_race___15)=c("Some other race")
  levels(tracking_log$race_ethn_hispanic)=c("Hispanic, Latino, or Spanish origin")
  levels(tracking_log$tribe___1)=c("Navajo Nation")
  levels(tracking_log$tribe___2)=c("San Carlos Apache")
  levels(tracking_log$tribe___3)=c("White Mountain Apache")
  levels(tracking_log$tribe___4)=c("Ute")
  levels(tracking_log$tribe___5)=c("Hopi")
  levels(tracking_log$tribe___6)=c("Other Indigenous Ancestry or Lineage")
  levels(tracking_log$tribe___7)=c("Other")
  levels(tracking_log$bio_sex_birth)=c("Female", "Male")
  levels(tracking_log$gender_identity_term___0)=c("Man")
  levels(tracking_log$gender_identity_term___1)=c("Woman")
  levels(tracking_log$gender_identity_term___2)=c("Non-binay")
  levels(tracking_log$gender_identity_term___3)=c("Transgender man/Female-to-male (FTM)")
  levels(tracking_log$gender_identity_term___4)=c("Transgender woman/Male-to-female (MTF)")
  levels(tracking_log$gender_identity_term___5)=c("Gender non-binary/Genderqueer/Gender nonconforming")
  levels(tracking_log$gender_identity_term___6)=c("Agender")
  levels(tracking_log$gender_identity_term___7)=c("Bigender")
  levels(tracking_log$gender_identity_term___8)=c("Two Spirited")
  levels(tracking_log$gender_identity_term___96)=c("None of these describe me")
  levels(tracking_log$language_spoken___1)=c("Navajo")
  levels(tracking_log$language_spoken___2)=c("Apache")
  levels(tracking_log$language_spoken___3)=c("Hopi")
  levels(tracking_log$language_spoken___4)=c("Ute")
  levels(tracking_log$language_spoken___5)=c("English")
  levels(tracking_log$language_spoken___6)=c("Other Native American Language")
  levels(tracking_log$language_spoken___7)=c("Other")
  levels(tracking_log$pregnancy_status)=c("Pregnant")
  levels(tracking_log$maritalstatus)=c("Single","Not married, but living with partner","Legally married","Separated","Divorced","Widowed","Other")
  levels(tracking_log$edu_years_of_school)=c("Have never gone to school","5th grade or less","6th to 8th grade","9th to 12th grade, no diploma","High school graduate or GED completed","Some college level/ Technical / Vocational degree","Bachelors degree","Other advanced degree (Masters, Doctoral degree)","Prefer not to answer","Dont know")
  levels(tracking_log$bio_sex_birth_child)=c("Male","Female","Prefer not to answer")
  levels(tracking_log$child_race___1)=c("American Indian or Alaskan Native")
  levels(tracking_log$child_race___2)=c("Black or African American")
  levels(tracking_log$child_race___3)=c("Asian")
  levels(tracking_log$child_race___4)=c("Native Hawaiian or Other Pacific Islander")
  levels(tracking_log$child_race___5)=c("White")
  levels(tracking_log$child_race___15)=c("Some other race")
  levels(tracking_log$parent_currentschool_yn)=c("Yes","No")
  levels(tracking_log$parent_currentschool_type)=c("Regular school year","Summer school","Other")
  levels(tracking_log$parent_schooltypescurr)=c("Online/virtual learning (only learning online)","In-person learning (only learning in-person)","Hybrid (split between learning online and in-person)","Other")
  levels(tracking_log$teacherrole)=c("Online/virtual learning (only teaching/working online)","In-person learning (only teaching/working in-person)","Hybrid (split between teaching/working online and in-person)","Other")
  levels(tracking_log$school_type)=c("Online/virtual learning","In-person learning","Hybrid","Other")


  label(tracking_log$record_id)="Record ID"
  label(tracking_log$redcap_event_name)="Event Name"
  label(tracking_log$data_access_group)="Data Access Group"
  label(tracking_log$age_yrs)="Age at Baseline"
  label(tracking_log$race_ethn_race___1)="Race"
  label(tracking_log$race_ethn_race___2)="Race"
  label(tracking_log$race_ethn_race___3)="Race"
  label(tracking_log$race_ethn_race___4)="Race"
  label(tracking_log$race_ethn_race___5)="Race"
  label(tracking_log$race_ethn_race___6)="Race"
  label(tracking_log$race_ethn_race___15)="Race"
  label(tracking_log$tribe___1)="Tribal Affiliation"
  label(tracking_log$tribe___2)="Tribal Affiliation"
  label(tracking_log$tribe___3)="Tribal Affiliation"
  label(tracking_log$tribe___4)="Tribal Affiliation"
  label(tracking_log$tribe___5)="Tribal Affiliation"
  label(tracking_log$tribe___6)="Tribal Affiliation"
  label(tracking_log$tribe___7)="Tribal Affiliation"
  label(tracking_log$race_ethn_hispanic)="Ethnicity"
  #label(tracking_log$gender_identity_term)="What terms best express how you describe your gender identity?"
  #label(tracking_log$language_spoken)="What languages do you speak?"
  label(tracking_log$bio_sex_birth)="Sex"
  label(tracking_log$gender_identity_term___0)="Gender"
  label(tracking_log$gender_identity_term___1)="Gender"
  label(tracking_log$gender_identity_term___2)="Gender"
  label(tracking_log$gender_identity_term___3)="Gender"
  label(tracking_log$gender_identity_term___4)="Gender"
  label(tracking_log$gender_identity_term___5)="Gender"
  label(tracking_log$gender_identity_term___6)="Gender"
  label(tracking_log$gender_identity_term___7)="Gender"
  label(tracking_log$gender_identity_term___8)="Gender"
  label(tracking_log$gender_identity_term___96)="Gender"
  label(tracking_log$language_spoken___1)="Languages Spoken"
  label(tracking_log$language_spoken___2)="Languages Spoken"
  label(tracking_log$language_spoken___3)="Languages Spoken"
  label(tracking_log$language_spoken___4)="Languages Spoken"
  label(tracking_log$language_spoken___5)="Languages Spoken"
  label(tracking_log$language_spoken___6)="Languages Spoken"
  label(tracking_log$language_spoken___7)="Languages Spoken"
  label(tracking_log$maritalstatus)="Marital Status"
  label(tracking_log$pregnancy_status)="Pregnancy Status"
  label(tracking_log$edu_years_of_school)="Education"
  label(tracking_log$bio_sex_birth_child)="Index Child's Sex"
  label(tracking_log$child_race___1)="Index Child's Race"
  label(tracking_log$child_race___2)="Index Child's Race"
  label(tracking_log$child_race___3)="Index Child's Race"
  label(tracking_log$child_race___4)="Index Child's Race"
  label(tracking_log$child_race___5)="Index Child's Race"
  label(tracking_log$child_race___15)="Index Child's Race"
  label(tracking_log$gender_identity_term_child)="How would your child [initial_contact_fo_arm_1][randomization_child_name] best describe their gender identity?"
  #label(tracking_log$child_race)="What race is your child [initial_contact_fo_arm_1][randomization_child_name]?"
  label(tracking_log$role_final)="Participant Type"
  #label(tracking_log$parent_currentschool_yn)="Is your child currently attending school?"
  label(tracking_log$teacherrole)="Current Schooling Type"
  label(tracking_log$parent_schooltypescurr)="Current Schooling Type"
  label(tracking_log$school_type)="Current Schooling Type"
  label(tracking_log$age_cat_child)="Index Child's Age (yrs)"
  #label(tracking_log$parent_schooltypescurr)="What type of learning does your child currently attend?"
  #label(tracking_log$parent_schooltypes)="Since March 2020, what types of learning did your child do?"
  tracking_log$services_recd_yn <- factor(tracking_log$services_recd_yn, levels = c(0,1), labels = c("No", "Yes"))
  label(tracking_log$services_recd_yn) <- "Received Services in Last Six Months"
  tracking_log$services_recd___1 <- factor(tracking_log$services_recd___1, levels = c(1), labels = c("Healthcare"))
  tracking_log$services_recd___2 <- factor(tracking_log$services_recd___2, levels = c(1), labels = c("Mental Health"))
  tracking_log$services_recd___3 <- factor(tracking_log$services_recd___3, levels = c(1), labels = c("Substance Abuse"))
  tracking_log$services_recd___4 <- factor(tracking_log$services_recd___4, levels = c(1), labels = c("Educational Services"))
  tracking_log$services_recd___5 <- factor(tracking_log$services_recd___5, levels = c(1), labels = c("Social Services"))
  tracking_log$services_recd___6 <- factor(tracking_log$services_recd___6, levels = c(1), labels = c("TANF/DES"))
  tracking_log$services_recd___7 <- factor(tracking_log$services_recd___7, levels = c(1), labels = c("Housing"))
  tracking_log$services_recd___8 <- factor(tracking_log$services_recd___8, levels = c(1), labels = c("WIC"))
  tracking_log$services_recd___9 <- factor(tracking_log$services_recd___9, levels = c(1), labels = c("Food or Nutrition Program"))
  tracking_log$services_recd___10 <- factor(tracking_log$services_recd___10, levels = c(1), labels = c("Parenting Education/Service Program"))
  tracking_log$services_recd___88 <- factor(tracking_log$services_recd___88, levels = c(1), labels = c("Other"))
  label(tracking_log$services_recd___1) <- "Services Received"
  label(tracking_log$services_recd___2) <- "Services Received"
  label(tracking_log$services_recd___3) <- "Services Received"
  label(tracking_log$services_recd___4) <- "Services Received"
  label(tracking_log$services_recd___5) <- "Services Received"
  label(tracking_log$services_recd___6) <- "Services Received"
  label(tracking_log$services_recd___7) <- "Services Received"
  label(tracking_log$services_recd___8) <- "Services Received"
  label(tracking_log$services_recd___9) <- "Services Received"
  label(tracking_log$services_recd___10) <- "Services Received"
  label(tracking_log$services_recd___88) <- "Services Received"
  tracking_log$money <- factor(tracking_log$money, levels = c(1,2,3,4,5), labels = c("More than enough money left",
                                                                                     "Some money left over",
                                                                                     "Just enough to make ends meet",
                                                                                     "Almost enough to make ends meet",
                                                                                     "Not enough to make ends meet"))
  label(tracking_log$money) <- "At end of month, household ended up with:"
  label(tracking_log$randomization_child_age) <- "Index Child's Age"


  return(tracking_log)
}
