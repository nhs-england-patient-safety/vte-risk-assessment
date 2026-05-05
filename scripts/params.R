# Input the date of the report
time_period <- "VTE-Q4-2025-26"

# If FALSE then UDAL code is not run. Stored data used instead. 
# Use TRUE when creating final report
use_udal <- TRUE

# API toggle when boundaries have changed but we need old mapping files
# E.g Q4 25/26 when ICB boundaries changed in April 2026, need to use pre 
# April 2026 boundaries for the Q4 report. use_udal needs to be TRUE
# TRUE = API is used. FALSE = saved datalake file used
use_api <- TRUE

# Saves a copy of ods_provider_hierarchies files in datalake
# run this before any mapping changes happen such as ICB boundaries
# use_udal needs to be true
copy_ods_files <- FALSE

# toggle whether to create draft SUS emails
draft_emails_sus <- FALSE

# toggle whether to create draft non submission emails
draft_emails_non_submitters <- FALSE

# How many quarters to go back for charts
previous_quarters <- 20

# Readable time periods used in run.R outside of quarto
time_period_html <- paste0(
  "VTE-Risk-Assessment-",
  (substring(time_period, 8, 14)),
  "-Quarter-", (substring(time_period, 6, 6)),
  ".html"
)
time_period_folder <- paste0(
  substring(time_period, 8, 14),
  " Q",
  substring(time_period, 6, 6)
)
