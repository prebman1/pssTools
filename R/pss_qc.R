#' Create QC List
#'
#' @param cases A data frame with output from redcap_read function for PSS.
#' @param logs A data frame with all logging events from REDCap.
#' @param data_dictionary A data frame with the current data dictionary to look for missingness.
#' @param qc_rules A data frame with the project specific qc_rules.
#'
#' @return A cleaned data frame of current QC Issues.
#' @export
#'
#' @examples
#' cases <- redcap_read(redcap_uri = url, "token"=token, events = redcap_events)$data
#' qc <- pss_qc(cases)
pss_qc <- function(cases, logs, data_dictionary, qc_rules) {
  #clean data_dictionary file
  data_dictionary <- data_dictionary[,c(1,2,4,12,18)]
  data_dictionary <- data_dictionary[-c(which(data_dictionary$Variable...Field.Name == "survey_admin_notes")),]
  colnames(data_dictionary) <- c("var", "form", "field_type", "branching", "annotation")
  #manually fix data dictionary form for idi_tracking so you can examine missingness for adult and child idi separately
  data_dictionary$form[data_dictionary$form == "idi_tracking" & grepl("child", data_dictionary$var)] <- "idi_tracking_child"

  #################################### Clean REDCap Data #########################################
  #create new df to maintain cases (original REDCap data) if needed later
  qc_cases <- cases
  #Set Event name as a factor
  qc_cases$redcap_event_name <- factor(qc_cases$redcap_event_name, levels = redcap_events, labels = gsub("_arm_1", "", redcap_events))

  #create a variable for redcap events w/o 'arm_1' at end
  redcap_events <- gsub("_arm_1", "", redcap_events)

  #duplicate baseline_idi_consent into baseline_idi_child_consent, baseline_idi into baseline_idi_child, and followup_id into followup_idi_child
  baseline_idi <- qc_cases %>% filter(redcap_event_name == "baseline_idi")
  baseline_idi_copies <- qc_cases[0,]
  if(nrow(baseline_idi)) {
    for(i in 1:nrow(baseline_idi)) {
      new_row_num <- nrow(baseline_idi_copies) + 1
      baseline_idi_copies[new_row_num,] <- baseline_idi[i,]
      baseline_idi_copies[new_row_num,"redcap_event_name"] <- "baseline_idi_consent"
      new_row_num <- nrow(baseline_idi_copies) + 1
      baseline_idi_copies[new_row_num,] <- baseline_idi[i,]
      baseline_idi_copies[new_row_num,"redcap_event_name"] <- "baseline_idi_child"
      new_row_num <- nrow(baseline_idi_copies) + 1
      baseline_idi_copies[new_row_num,] <- baseline_idi[i,]
      baseline_idi_copies[new_row_num,"redcap_event_name"] <- "baseline_idi_child_consent"
    }
  }
  qc_cases <- rbind(qc_cases, baseline_idi_copies)

  followup_idi <- qc_cases %>% filter(redcap_event_name == "followup_idi")
  followup_idi_copies <- qc_cases[0,]
  if(nrow(followup_idi) > 0){
    for(i in 1:nrow(followup_idi)) {
      new_row_num <- nrow(followup_idi_copies) + 1
      followup_idi_copies[new_row_num,] <- followup_idi[i,]
      followup_idi_copies[new_row_num,"redcap_event_name"] <- "followup_idi_child"
    }
  }
  qc_cases <- rbind(qc_cases, followup_idi_copies)

  #Carry forward initial contact data, consent, IDI, and baseline data into other time points
  #This allows for branching logic from other events to reference values on these events without having to reference a separate row
  # child_idi_forms <- c("idi_parental_permission", "idi_youth_assent", "idi_tracking")
  # child_idi_vars <- data_dictionary$var[data_dictionary$form %in% child_idi_forms & !(data_dictionary$field_type %in% c("descriptive", "checkbox"))]
  # child_idi_checkbox_vars <- data_dictionary$var[data_dictionary$form %in% child_idi_forms & data_dictionary$field_type == "checkbox"]


  qc_cases <- qc_cases %>%
    arrange(record_id, redcap_event_name) %>%
    group_by(record_id) %>%
    fill(iden_date_mdy:initial_contact_form_complete, .direction = "down") %>%
    fill(date_baseline_demo:user_demo, .direction = "downup") %>%
    ungroup()

  #create recruitment complete and event complete variables
  qc_cases <- qc_cases %>%
    mutate(
      recruitment_complete = case_when(
        eligible == 1 & !is.na(role_final) & (role_final == 2 | (role_final == 1 & !is.na(randomization_child_num))) ~ 1
      ),
      event_complete = case_when( #you need to update this b/c idi's and idi consent aren't set up right
        redcap_event_name == "initial_contact_fo" & recruitment_complete == 1 ~ 1,
        redcap_event_name == "consent" & informed_consent_complete == 2 ~ 1,
        redcap_event_name %in% c("baseline", "wave2", "wave3", "wave4") & (survey_admin_complete1 == 1 | survey_admin_complete2 == 1 | survey_admin_complete3 == 1 | survey_admin_complete4 == 1) ~ 1,
        redcap_event_name == "baseline_idi_consent" & idi_informed_consent_complete == 2 ~ 1,
        redcap_event_name == "baseline_idi" & !is.na(idi_date) ~ 1,
        redcap_event_name == "baseline_idi_child_consent" & idi_parental_permission_complete == 2 & idi_youth_assent_complete == 2 ~ 1,
        redcap_event_name == "baseline_idi_child" & !is.na(idi_date_child) ~ 1,
        redcap_event_name == "followup_idi" & !is.na(idi_date) ~ 1,
        redcap_event_name == "followup_idi_child" & !is.na(idi_date_child) ~ 1,
        TRUE ~ 0),
    )


  #################################### Missing Data ####################################

  write_r_branch_logic <- function(x){
    x <- tolower(x) #change all text to lower for ease of replacement
    x <- gsub("\\[initial_contact_fo_arm_1\\]", "",x) #remove any references to initial contact form; this will be carried forward onto row
    x <- gsub("\\[baseline_arm_1\\]", "",x) #remove any references to initial contact form; this will be carried forward onto row
    x <- gsub("event-name", "redcap_event_name", x) #change reference to redcap event name to column name in export
    x <- gsub('"', "'",x) #change all double quotes to single quotes for consistency
    x <- gsub("!=", "<>", x) #change uses of != to <> in redcap dictionary for easier subing later
    x <- gsub(" and ", " & ", x) #change and to &
    x <- gsub(" or ", " | ", x) #change or to |
    x <- gsub("\\[", "form_completed$", x) #change the way fields are referenced to r formatting (df name$)
    x <- gsub("\\]", '\\[r\\]', x) #remove last ] from field reference and add[r] to reference row in for loop
    n <- str_count(x, "\\(\\d{1,2}\\)") #count the number of times a checkbox field is referenced
    if(n > 0){ #if there are checkboxes fields, reformat the reference to redcap output format varname___#
      for (i in 1:n) {
        x <- sub("\\(\\d{1,2}\\)", paste0("___",str_extract(str_extract(x, "\\(\\d{1,2}\\)"), "\\d{1,2}")), x)
      }
    }
    x <- gsub("(?<![\\>\\<])=", "%in%", x, perl = TRUE) #change redcap = to R ==; ignore <= or >=
    n_blank_checks <- str_count(x, "[a-z\\$_\\d\\[\\]]+\\s*<>\\s*''")
    if(n_blank_checks > 0){ #if there are blank value checks , reformat the reference to redcap output format (is.na())
      for (i in 1:n_blank_checks) {
        na_exp <- str_extract(x, "[a-z\\$_\\d\\[\\]]+\\s*<>\\s*''") #get string for REDCap blank value check
        na_exp <- sub("\\s*<>\\s*''", "", na_exp) #remove spaces and REDCap blank values check
        x <- sub("[a-z\\$_\\d\\[\\]]+\\s*<>\\s*''", paste0("!is.na(", na_exp,")"), x, perl = T) #substitute a is.na() format for blank value checks
      }
    }
    x <- gsub("<>", "%notin%", x) #change any remaining not equals to to R format
    ### need to add second & !is.na() for <, <=, >, >= values
    n_ls_gr_than <- str_count(x, "[\\>\\<]=?")
    if(n_ls_gr_than > 0){ #if there are <= or >=, reformat the reference to redcap output format <(is.na())
      for (i in 1:n_ls_gr_than) {
        ls_gr_than_var_name <- str_extract(str_extract(x, "[a-z\\$_\\d\\[\\]]+\\s*[\\>\\<]=?"), "[a-z\\$_\\d\\[\\]]+")
        ls_gr_than_exp <- str_extract(x, "[a-z\\$_\\d\\[\\]]+\\s*[\\>\\<]=?\\s*'?[\\d]+'?")
        x <- sub("[a-z\\$_\\d\\[\\]]+\\s*[\\>\\<]=?\\s*'?[\\d]+'?", paste0("(", ls_gr_than_exp, " & !is.na(", ls_gr_than_var_name, "))"), x, perl = T)
      }
    }
    #correct less than or greater than w/ quotes around numbers
    n_ls_gr_than_quotes <- str_count(x, "[\\>\\<]=? *'\\d*'")
    if(n_ls_gr_than_quotes > 0){
      for (i in 1:n_ls_gr_than_quotes) {
        ls_gr_than_exp_no_quotes <- str_extract(x, "[\\>\\<]=? *'\\d*'")
        ls_gr_than_exp_no_quotes <- gsub("'", '', ls_gr_than_exp_no_quotes)
        x <- sub("[\\>\\<]=? *'\\d*'", ls_gr_than_exp_no_quotes, x, perl = T)
      }
    }

    #x <- paste0("(", x, ")") #add parenthesis around string
    return(x)
  }


  #remove any variables that you don't want to check anything for
  branching <- data_dictionary
  branching$branching_r <- sapply(branching$branching, write_r_branch_logic)
  #manually fix branching for online consent signature electronic (set as hidden on electronic survey)
  branching$branching_r[which(branching$var %in% c("consent_staff_sign", "consent_staff_name"))] <- "form_completed$reg_surveymode_staff[r]%in%'2'"
  branching <- branching %>% filter(branching_r != '')
  #get a list of all vars but don't include initial contact form (too many unique patterns)
  #forms determines which forms are checked for missing values
  forms <- unique(data_dictionary$form)[-c(1:6,40:48)]
  forms <- c("informed_consent", "idi_informed_consent", "idi_parental_permission", "idi_youth_assent","survey_administration_info")
  fields_not_to_check <- c()
  forms_complete_vars <- paste0(forms, "_complete")

  form_mapping <- read.csv("instrument_mapping_20211105.csv", stringsAsFactors = FALSE)
  form_mapping$unique_event_name <- gsub("_arm_1", "", form_mapping$unique_event_name)

  #manually adjust form mapping b/c we've conceptually split parent and child idi's, even though they are the same event in REDCap
  form_mapping$unique_event_name[form_mapping$form == "idi_informed_consent"] <- "baseline_idi_consent"
  form_mapping$unique_event_name[form_mapping$form %in% c("idi_parental_permission", "idi_youth_assent")] <- "baseline_idi_child_consent"
  form_mapping[nrow(form_mapping) +1, ] <- c("1", "baseline_idi_child", "idi_tracking_child")
  form_mapping[nrow(form_mapping) +1, ] <- c("1", "followup_idi_child", "idi_tracking_child")

  qc_errors <- data.frame(date = as.Date(character()), record_id = character(), event = character(), form = character(), variables = character(), description = character(), status = character())
  # validation_checks <- data.frame(event = character(), form = character(), var = character(), condition = character())
  # validation_checks[1,] <- c("initial_contact_fo","initial_contact_fo", "contact_dob", "is.na(qc_cases$contact_dob)")
  # qc_cases[eval(rlang::parse_expr(validation_checks[1,2])), "record_id"]

  ##### Missing Data for all forms past initial_contact_form
  missingness <- data.frame(form = character(), var = character(), num_expected = numeric(), num_missing = numeric())
  for(f in 1:length(forms)){
    events <- form_mapping[which(form_mapping$form == forms[f]),"unique_event_name"] #select the events where the form is used
    #form_completed <- qc_cases[which(qc_cases[which(colnames(qc_cases) == forms_complete_vars[f])] == 2),]
    form_completed <- qc_cases %>% filter(redcap_event_name %in% events & event_complete == 1) #find the records where the event that uses the form is completed
    if(nrow(form_completed) > 0){
      form_name <- forms[f]
      current_form_vars <- data_dictionary %>% #select all variables that are for the current form
        filter(form == forms[f]) %>%
        filter(field_type %notin% c("descriptive", "calc", "checkbox")) %>%
        filter(!grepl("@HIDDEN(?!\\-)", annotation, perl = T)) %>%#removed @hidden but not @hidden-pdf/@hidden-survey
        filter(!grepl("@CALCTEXT", annotation, perl = T))
      current_form_vars <- current_form_vars$var

      for(i in current_form_vars) { #cycle through each variable for the current form
        num_expected <- 0
        num_missing <- 0
        for(r in 1:nrow(form_completed)) {
          is_missing <- is.na(form_completed[,which(colnames(form_completed) == i)])[r]
          branching_logic_true <- ifelse(i %in% branching$var, eval(rlang::parse_expr(branching[which(branching$var == i),"branching_r"])), TRUE)
          if(branching_logic_true) {
            num_expected <- num_expected + 1
          }
          if(is_missing & branching_logic_true) { #if the variable is missing and i
            qc_errors[nrow(qc_errors) + 1,] <- c(format(Sys.Date(), "%Y-%m-%d"), form_completed$record_id[r], form_completed$redcap_event_name[r], form_name, i, "Missing Data", "New")
            num_missing <- num_missing + 1
          }
        }
        missingness[(nrow(missingness)+1),] <- c(form_name, i, num_expected, num_missing)
      }
    }
  }

  missingness$num_missing <- as.numeric(missingness$num_missing)
  missingness$num_expected <- as.numeric(missingness$num_expected)

  missingness$perc_miss[which(missingness$num_expected != 0)] <- missingness$num_missing[which(missingness$num_expected != 0)] / missingness$num_expected[which(missingness$num_expected != 0)]
  missingness$perc_miss[which(is.na(missingness$perc_miss))] <- 0

  write.csv(missingness, file = paste0("Missingness ", format(Sys.Date(), '%Y%m%d'), ".csv"))

  #missingness <- missingness[which(missingness$perc_miss > .05),]



  part_miss_forms <- forms[which(!(forms %in% c("informed_consent", "idi_informed_consent", "idi_parental_permission", "idi_youth_assent", "idi_tracking")))]
  participant_missingness <- data.frame(matrix(nrow = 0, ncol = 3 + length(part_miss_forms)))
  colnames(participant_missingness) <- c("record_id","role_final", "redcap_event_name", part_miss_forms)
  part_miss_form_vars <- forms_complete_vars[which(!(forms %in% c("informed_consent", "idi_informed_consent", "idi_parental_permission", "idi_youth_assent", "idi_tracking")))]

  for(c in 1:nrow(qc_cases)){
    current_case <- qc_cases[c,] #select 1 record at time
    current_row_part_miss <- nrow(participant_missingness) + 1
    for(f in 1:length(part_miss_forms)){ #run through each form
      if(current_case[part_miss_form_vars[f]] %in% 2){ #if the form is complete
        current_form_vars <- data_dictionary %>% #select variables for the current form
          filter(form == part_miss_forms[f]) %>%
          filter(field_type %notin% c("descriptive", "calc", "checkbox")) %>%
          filter(!grepl("@HIDDEN(?!\\-)", annotation, perl = T)) %>%#removed @hidden but not @hidden-pdf/@hidden-survey
          filter(!grepl("@CALCTEXT", annotation, perl = T))
        current_form_vars <- current_form_vars$var
        num_expected <- 0 #set up number of expected and missing for current form
        num_missing <- 0
        form_completed <- current_case #setting form completed as current case so I can use the branching logic
        r <- 1
        for(v in current_form_vars) { #run through each variable and check if it should be answered and if so, if it's missing
          is_missing <- is.na(current_case[,which(colnames(current_case) == v)])[1]
          branching_logic_true <- ifelse(v %in% branching$var, eval(rlang::parse_expr(branching[which(branching$var == v),"branching_r"])), TRUE)
          #print(paste0("Variable: ", v, "Is missing: ", is_missing, "; branching_logic_true: ", branching_logic_true))
          if(branching_logic_true) {
            num_expected <- num_expected + 1
          }
          if(is_missing & branching_logic_true) { #if the variable is missing and i
            num_missing <- num_missing + 1
          }
        }
        perc_missing <- ifelse(num_expected > 1, num_missing / num_expected, NA)
        participant_missingness[current_row_part_miss, "record_id"] <- current_case$record_id
        participant_missingness[current_row_part_miss, "redcap_event_name"] <- current_case$redcap_event_name
        participant_missingness[current_row_part_miss, "role_final"] <- current_case$role_final
        participant_missingness[current_row_part_miss,which(colnames(participant_missingness) == part_miss_forms[f])] <- perc_missing
      }
    }
  }
  participant_missingness$redcap_event_name <- factor(participant_missingness$redcap_event_name, levels = 1:12, labels = gsub("_arm_1", "", redcap_events))
  participant_missingness$role_final <- factor(participant_missingness$role_final, levels = 1:2, labels = c("Parent/Caregiver", "Teacher/Staff"))

  participant_missingness_long <- pivot_longer(participant_missingness, cols = all_of(part_miss_forms), values_to = "perc_miss")
  participant_missingness_long <- participant_missingness_long[which(participant_missingness_long$perc_miss > .05),]

  output_time <- format(Sys.time(), '%Y%m%d %I%M')
  rmarkdown::render("Missingness.Rmd", output_file = paste0("Missingness - ", output_time))


  ##### Missing data for select initial_contact_fo vars
  initial_cont_miss_vars <- c("iden_date_mdy", "contact_dob", "reg_phone", "teacher_school_location",
                              "teacher_school_other", "parent_school_location", "parent_school_other",
                              "numchild_b", "randomization_child_num", "dob_child", "contact_decline_other",
                              "contact_ineligible_other", "withdrew_reason", "exitstatus_other", "name_child1",
                              "age_child1", "name_child2", "age_child2", "name_child3", "age_child3", "name_child4",
                              "age_child4", "name_child5", "age_child5", "name_child6", "age_child6")
  form_completed <- qc_cases %>% filter(redcap_event_name == "initial_contact_fo" & (!is.na(caregiver) | !is.na(school_employee) | !is.na(adult)))
  initial_cont_data_dict <- data_dictionary %>% filter(var %in% initial_cont_miss_vars)
  for(i in initial_cont_miss_vars) {
    for(r in 1:nrow(form_completed)) {
      is_missing <- is.na(form_completed[,which(colnames(form_completed) == i)])[r]
      branching_logic_true <- ifelse(i %in% branching$var, eval(rlang::parse_expr(branching[which(branching$var == i),"branching_r"])), TRUE)
      if(is_missing & branching_logic_true) { #if the variable is missing and branching logic is true
        qc_errors[nrow(qc_errors) + 1,] <- c(format(Sys.Date(), "%Y-%m-%d"), form_completed$record_id[r], 1, "Initial Contact Form", i, "Missing Data", "New")
      }
    }
  }

  # ############################### Specific Validation Rules #############################
  #
  # for(i in 1:nrow(qc_rules)){
  #   curr_qc_check <- qc_cases %>% filter(redcap_event_name == qc_rules$event[i])
  #
  #   for(r in 1:nrow(curr_qc_check)){
  #     qc_condition <- eval(rlang::parse_expr(qc_rules$r_rule[i]))
  #     if(qc_condition %in% TRUE) {
  #       qc_errors[nrow(qc_errors) + 1,] <- c(format(Sys.Date(), "%Y-%m-%d"), curr_qc_check$record_id[r], curr_qc_check$redcap_event_name[r], qc_rules$form[i], qc_rules$variables[i], qc_rules$description[i], "New")
  #     }
  #   }
  #
  # }


  # ############################### Field History Checks ###############################
  # #checks to ensure specific fields have only been modified by specific people
  #
  # #selected for idi field
  # idi_selected_issues <- logs[which(grepl("idi_selected =", logs$List.of.Data.Changes.OR.Fields.Exported)),]
  # idi_selected_issues$record_id <- str_extract(idi_selected_issues$Action, "\\d{4}-\\d*")
  # idi_selected_issues$idi_selected_value <- str_extract(idi_selected_issues$List.of.Data.Changes.OR.Fields.Exported, "idi_selected = '[01]*'")
  # idi_selected_issues <- idi_selected_issues %>% arrange(record_id, Time...Date)
  # idi_selected_issues <- idi_selected_issues %>% mutate(prev_record_id = lag(record_id))
  #
  # list_to_remove <- c()
  # for(i in 1:nrow(idi_selected_issues)){
  #   #remove_cond <- (idi_selected_issues$record_id[i] == idi_selected_issues$prev_record_id[i] & idi_selected_issues$idi_selected_value[i] == "idi_selected = ''") | (idi_selected_issues$record_id[i] == idi_selected_issues$prev_record_id[i] & idi_selected_issues$Username[i] %in% c("prebman1@jh.edu", "aingall4@jh.edu"))
  #   remove_cond <- (idi_selected_issues$record_id[i] == idi_selected_issues$prev_record_id[i] & idi_selected_issues$idi_selected_value[i] == "idi_selected = ''")
  #   if(is.na(remove_cond)) {remove_cond <- FALSE}
  #   if(remove_cond){
  #     list_to_remove <- c(list_to_remove, i, i-1)
  #   }
  # }
  # idi_selected_issues <- idi_selected_issues[-list_to_remove,]
  # idi_selected_issues <- idi_selected_issues[which(idi_selected_issues$Username %notin% c("prebman1@jh.edu","aingall4@jh.edu")),]
  # idi_selected_issues <- idi_selected_issues %>% mutate(date = format(Sys.Date(), "%Y-%m-%d"), event = 1, form = "initial_contact_form", variables = "idi_selected", description = paste0(Username, " set ", idi_selected_value, " on ", Time...Date), status = "New") %>%
  #   select(date, record_id, event, form, variables, description, status)
  #
  # qc_errors <- rbind(qc_errors, idi_selected_issues)
  #
  # #randomization override field
  # rand_override_issues <- logs[which(grepl("randomization_override =", logs$List.of.Data.Changes.OR.Fields.Exported)),]
  # rand_override_issues$record_id <- str_extract(rand_override_issues$Action, "\\d{4}-\\d*")
  # rand_override_issues$rand_override_value <- str_extract(rand_override_issues$List.of.Data.Changes.OR.Fields.Exported, "randomization_override = '[01]*'")
  # rand_override_issues <- rand_override_issues %>% arrange(record_id, Time...Date)
  # rand_override_issues <- rand_override_issues %>% mutate(prev_record_id = lag(record_id))
  #
  # list_to_remove <- c()
  # for(i in 1:nrow(rand_override_issues)){
  #   #remove_cond <- (rand_override_issues$record_id[i] == rand_override_issues$prev_record_id[i] & rand_override_issues$rand_override_value[i] == "idi_selected = ''") | (rand_override_issues$record_id[i] == rand_override_issues$prev_record_id[i] & rand_override_issues$Username[i] %in% c("prebman1@jh.edu", "aingall4@jh.edu"))
  #   remove_cond <- (rand_override_issues$record_id[i] == rand_override_issues$prev_record_id[i] & rand_override_issues$rand_override_value[i] == "randomization_override = ''")
  #   if(is.na(remove_cond)) {remove_cond <- FALSE}
  #   if(remove_cond){
  #     list_to_remove <- c(list_to_remove, i, i-1)
  #   }
  # }
  # if(!is.null(list_to_remove)) rand_override_issues <- rand_override_issues[-list_to_remove,]
  # rand_override_issues <- rand_override_issues[which(rand_override_issues$Username %notin% c("prebman1@jh.edu","aingall4@jh.edu")),]
  # rand_override_issues <- rand_override_issues %>% mutate(date = format(Sys.Date(), "%Y-%m-%d"), event = 1, form = "initial_contact_form", variables = "randomization_override", description = paste0(Username, " set ", rand_override_value, " on ", Time...Date), status = "New") %>%
  #   select(date, record_id, event, form, variables, description, status)
  #
  # qc_errors <- rbind(qc_errors, rand_override_issues)
  #
  # #surveys that are complete but not marked as complete in REDCap
  # #get a list of all vars but don't include initial contact form (too many unique patterns)
  # #forms determines which forms are checked for missing values
  # forms <- unique(data_dictionary$form)
  # parent_baseline_forms <- forms[8:40]
  # teacher_baseline_forms <- forms[c(8:10,12:19, 40)]
  # parent_forms_complete_vars <- paste0(parent_baseline_forms, "_complete")
  # teacher_forms_complete_vars <- paste0(teacher_baseline_forms, "_complete")
  #
  # baselines <- cases %>% group_by(record_id) %>%
  #   fill(role_final, .direction = "downup") %>%
  #   ungroup() %>%
  #   filter(redcap_event_name == "baseline_arm_1") %>%
  #   mutate(
  #     event_complete = case_when(
  #       survey_admin_complete1 == 1 | survey_admin_complete2 == 1 | survey_admin_complete3 == 1 | survey_admin_complete4 == 1 ~ 1,
  #       TRUE ~ 0))
  # incomplete_baselines <- baselines %>% filter(event_complete == 0)
  #
  # incomplete_baselines$parent_forms_completed <- rowSums(ifelse(incomplete_baselines[,parent_forms_complete_vars] == 2, 1, 0))
  # incomplete_baselines$teacher_forms_completed <- rowSums(ifelse(incomplete_baselines[,teacher_forms_complete_vars] == 2, 1, 0))
  #
  # completed_not_marked <- incomplete_baselines %>%
  #   filter((role_final == 1 & parent_forms_completed == 33) | (role_final == 2 & teacher_forms_completed == 12)) %>%
  #   select(record_id)
  #
  # completed_not_marked <- completed_not_marked %>%
  #   mutate(date = format(Sys.Date(), "%Y-%m-%d"),
  #          event = 3, form = "survey_administration_info",
  #          variables = "survey_completed",
  #          description = "This event appears to be completed in REDCap, but has not been marked as completed on the survey admin info form",
  #          status = "New") %>%
  #   select(date, record_id, event, form, variables, description, status)
  #
  # qc_errors <- rbind(qc_errors, completed_not_marked)
  #
  # ###############################  Final QC Errors DF ###############################
  # qc_errors$event <- recode(qc_errors$event, '1' = levels(qc_cases$redcap_event_name)[1],
  #                           '2' = levels(qc_cases$redcap_event_name)[2],
  #                           '3' = levels(qc_cases$redcap_event_name)[3],
  #                           '4' = levels(qc_cases$redcap_event_name)[4],
  #                           '5' = levels(qc_cases$redcap_event_name)[5],
  #                           '6' = levels(qc_cases$redcap_event_name)[6],
  #                           '7' = levels(qc_cases$redcap_event_name)[7],
  #                           '8' = levels(qc_cases$redcap_event_name)[8],
  #                           '9' = levels(qc_cases$redcap_event_name)[9],
  #                           '10' = levels(qc_cases$redcap_event_name)[10],
  #                           '11' = levels(qc_cases$redcap_event_name)[11],
  #                           '12' = levels(qc_cases$redcap_event_name)[12])
  #
  # qc_errors <- qc_errors %>% mutate(data_access_group = case_when(
  #   substr(record_id, 1, 4) == "5255" ~ "wmat",
  #   substr(record_id, 1, 4) == "5256" ~ "shiprock",
  #   substr(record_id, 1, 4) == "5257" ~ "tuba_city",
  #   substr(record_id, 1, 4) == "5258" ~ "chinle"),
  #   event_id = case_when(
  #     event == "initial_contact_fo" ~ "41033",
  #     event == "consent" ~ "41788",
  #     event == "baseline" ~ "40326",
  #     event == "baseline_idi_consent" ~ "42175",
  #     event == "baseline_idi" ~ "42175",
  #     event == "baseline_idi_child_consent" ~ "42175",
  #     event == "baseline_idi_child" ~ "42175",
  #     event == "wave2" ~ "40679",
  #     event == "followup_idi" ~ "42176",
  #     event == "wave3" ~ "40680",
  #     event == "wave4" ~ "40681"),
  #   redcap_form_name = case_when(
  #     form == "Initial Contact Form" ~ "initial_contact_form",
  #     TRUE ~ form
  #   ),
  #   url = paste0("HYPERLINK(\"","https://mrprcbcw.hosts.jhmi.edu/redcap/redcap_v10.6.28/DataEntry/index.php?pid=4306&arm=1&id=", record_id, "&event_id=", event_id, "&page=", redcap_form_name, "\", \"", "Click to view form", "\")")
  # ) %>% select(date, data_access_group, record_id, event, form, variables, description, status, url) %>%
  #   arrange(data_access_group, record_id, event, form, date)
  #
  # class(qc_errors$url) <- "formula"
  #
  # #remove any participants who declined or were ineligible
  # qc_cases_inelg_decl <- which(qc_cases$redcap_event_name == "initial_contact_fo" & qc_cases$pot_participant_status %in% c(3,4))
  # qc_cases_inelg_decl <- qc_cases$record_id[qc_cases_inelg_decl]
  # qc_errors <- qc_errors %>% filter(!(record_id %in% qc_cases_inelg_decl))


  # options(gargle_oauth_cache = "Authorization Credentials",
  #         gargle_oauth_email = TRUE)
  #
  # sheets <- sheet_names("https://docs.google.com/spreadsheets/d/1UrjMWWrgS1n2osDYxLDfFv2Xf_bmXXg69k5ubAXRB5g/edit#gid=0")
  # qc_errors_list <- list()
  # for(sheet in sheets){
  #   qc_errors_list[[(length(qc_errors_list)+1)]] <- read_sheet("https://docs.google.com/spreadsheets/d/1UrjMWWrgS1n2osDYxLDfFv2Xf_bmXXg69k5ubAXRB5g/edit#gid=0", sheet = sheet)
  # }
  # #add any missing columns to each dataframe
  # colnames <- c()
  # for(i in 1:length(qc_errors_list)){
  #   colnames <- c(colnames, colnames(qc_errors_list[[i]]))
  # }
  # colnames <- unique(colnames)
  # for(i in 1:length(qc_errors_list)){
  #   for(j in 1:length(colnames)){
  #     if(colnames[j] %notin% colnames(qc_errors_list[[i]])){
  #       qc_errors_list[[i]][ncol(qc_errors_list[[i]]) + 1] <- NA
  #       colnames(qc_errors_list[[i]])[ncol(qc_errors_list[[i]])] <- colnames[j]
  #       print(colnames[j])
  #     }
  #   }
  # }
  # #merge qc errors sheets together
  # qc_errors_current <- do.call(rbind, qc_errors_list)
  # # qc_errors_current <- qc_errors_current %>% rename(record_id = "Record ID",
  # #                                                   event = Event,
  # #                                                   form = Form,
  # #                                                   variables = Variables,
  # #                                                   description = Description,
  # #                                                   status = Status) %>%
  # qc_errors_current <- qc_errors_current %>% mutate(matching_column = paste0(record_id, event, form, variables, description))
  # qc_errors_current$matching_column <- str_replace(qc_errors_current$matching_column, "initial_contacto_form", "initial_contact_form") #this is to fix an error on previous QC reports
  #
  #
  # qc_errors <- qc_errors %>% mutate(matching_column = paste0(record_id, event, form, variables, description))
  # qc_errors_to_remove <- qc_errors_current %>% filter(status == "Remove Flag")
  # qc_errors$status[which(qc_errors$matching_column %in% qc_errors_current$matching_column)] <- "From Previous Week"
  # qc_errors_keep <- qc_errors %>%
  #   filter(matching_column %notin% qc_errors_to_remove$matching_column) %>%
  #   select(!matching_column) %>%
  #   arrange(record_id)

  return(qc_errors)
}
