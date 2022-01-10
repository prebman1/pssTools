#' Combine Current QC Errors with Historical Google Sheets Records.
#'
#' @param qc_errors QC errors df from pss_qc function
#' @param gs_url URL for Google Sheets
#'
#' @return
#' @export
#'
#' @examples
#' qc_errors <- pss_qc(cases, logs, data_dictionary, qc_rules)
#' options(gargle_oauth_cache = "Authorization Credentials", gargle_oauth_email = TRUE)
#' gs_url <- "https://docs.google.com/spreadsheets/d/1UrjMWWrgS1n2osDYxLDfFv2Xf_bmXXg69k5ubAXRB5g/edit#gid=0"
#' qc_errors_final <- pss_qc_ggl_shts(qc_errors, gs_url)
pss_qc_ggl_shts <- function(qc_errors, gs_url) {
  #define notin function
  `%notin%` <- Negate(`%in%`)

  sheets <- sheet_names(gs_url)
  qc_errors_list <- list()
  for(sheet in sheets){
    qc_errors_list[[(length(qc_errors_list)+1)]] <- read_sheet(gs_url, sheet = sheet)
  }
  #add any missing columns to each dataframe
  colnames <- c()
  for(i in 1:length(qc_errors_list)){
    colnames <- c(colnames, colnames(qc_errors_list[[i]]))
  }
  colnames <- unique(colnames)
  for(i in 1:length(qc_errors_list)){
    for(j in 1:length(colnames)){
      if(colnames[j] %notin% colnames(qc_errors_list[[i]])){
        qc_errors_list[[i]][ncol(qc_errors_list[[i]]) + 1] <- NA
        colnames(qc_errors_list[[i]])[ncol(qc_errors_list[[i]])] <- colnames[j]
        print(colnames[j])
      }
    }
  }
  #merge qc errors sheets together
  qc_errors_current <- do.call(rbind, qc_errors_list)
  # qc_errors_current <- qc_errors_current %>% rename(record_id = "Record ID",
  #                                                   event = Event,
  #                                                   form = Form,
  #                                                   variables = Variables,
  #                                                   description = Description,
  #                                                   status = Status) %>%
  qc_errors_current <- qc_errors_current %>% mutate(matching_column = paste0(record_id, event, form, variables, description))
  qc_errors_current$matching_column <- str_replace(qc_errors_current$matching_column, "initial_contacto_form", "initial_contact_form") #this is to fix an error on previous QC reports


  qc_errors <- qc_errors %>% mutate(matching_column = paste0(record_id, event, form, variables, description))
  qc_errors_to_remove <- qc_errors_current %>% filter(status == "Remove Flag")
  qc_errors$status[which(qc_errors$matching_column %in% qc_errors_current$matching_column)] <- "From Previous Week"
  qc_errors_keep <- qc_errors %>%
    filter(matching_column %notin% qc_errors_to_remove$matching_column) %>%
    select(!matching_column) %>%
    arrange(record_id)
}
