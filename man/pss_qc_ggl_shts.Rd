% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pss_qc_ggl_shts.R
\name{pss_qc_ggl_shts}
\alias{pss_qc_ggl_shts}
\title{Combine Current QC Errors with Historical Google Sheets Records.}
\usage{
pss_qc_ggl_shts(qc_errors, gs_url)
}
\arguments{
\item{qc_errors}{QC errors df from pss_qc function}

\item{gs_url}{URL for Google Sheets}
}
\value{

}
\description{
Combine Current QC Errors with Historical Google Sheets Records.
}
\examples{
qc_errors <- pss_qc(cases, logs, data_dictionary, qc_rules)
options(gargle_oauth_cache = "Authorization Credentials", gargle_oauth_email = TRUE)
gs_url <- "https://docs.google.com/spreadsheets/d/1UrjMWWrgS1n2osDYxLDfFv2Xf_bmXXg69k5ubAXRB5g/edit#gid=0"
qc_errors_final <- pss_qc_ggl_shts(qc_errors, gs_url)
}
