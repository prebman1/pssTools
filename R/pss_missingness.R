#' Get Missingness Report
#'
#' @param cases A data frame with output from redcap_read function for PSS.
#' @param data_dictionary A data frame with the current data dictionary to look for missingness.
#' @param form_mapping A data frame that has all the form mapping done.
#' @param missingness_type Either "participant" or "item". Determines whether the data frame returned should be for missingness by each participant or by each item.
#' @param write_files Either true or false.
#'
#' @return A data frame of item level missinging
#' @export
#'
#' @examples
#' cases <- redcap_read(redcap_uri = url, "token"=token, events = redcap_events)$data
#' qc <- pss_missingness(cases)
pss_missingness <- function(cases, data_dictionary, form_mapping, missingness_type = "item", write_files = FALSE){

  #define notin function
  `%notin%` <- Negate(`%in%`)

  #remove seconds from log timestamp
  logs$timestamp <- format(logs$timestamp, "%Y-%m-%d %H:%M")

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
  redcap_events <- c("initial_contact_fo_arm_1", "consent_arm_1", "baseline_arm_1","baseline_idi_consent_arm_1", "baseline_idi_arm_1","baseline_idi_child_consent_arm_1", "baseline_idi_child_arm_1", "wave2_arm_1", "followup_idi_arm_1","followup_idi_child_arm_1", "wave3_arm_1", "wave4_arm_1")
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
  #forms <- c("informed_consent", "idi_informed_consent", "idi_parental_permission", "idi_youth_assent","survey_administration_info")
  fields_not_to_check <- c()
  forms_complete_vars <- paste0(forms, "_complete")

  form_mapping$unique_event_name <- gsub("_arm_1", "", form_mapping$unique_event_name)

  #manually adjust form mapping b/c we've conceptually split parent and child idi's, even though they are the same event in REDCap
  form_mapping$unique_event_name[form_mapping$form == "idi_informed_consent"] <- "baseline_idi_consent"
  form_mapping$unique_event_name[form_mapping$form %in% c("idi_parental_permission", "idi_youth_assent")] <- "baseline_idi_child_consent"
  form_mapping[nrow(form_mapping) +1, ] <- c("1", "baseline_idi_child", "idi_tracking_child")
  form_mapping[nrow(form_mapping) +1, ] <- c("1", "followup_idi_child", "idi_tracking_child")

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
            #qc_errors[nrow(qc_errors) + 1,] <- c(format(Sys.Date(), "%Y-%m-%d"), form_completed$record_id[r], form_completed$redcap_event_name[r], form_name, i, "Missing Data", "New")
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

  if(write_files == TRUE){
    write.csv(missingness, file = paste0("Missingness ", format(Sys.Date(), '%Y%m%d'), ".csv"))
  }
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
  if(write_files == TRUE){
    rmarkdown::render("Missingness.Rmd", output_file = paste0("Missingness - ", output_time))
  }

  if(missingness_type == "item") {
    return(missingness)
  }

  if(missingness_type == "participant") {
    return(participant_missingness)
  }

}
