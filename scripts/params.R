# Developer mode (if TRUE then UDAL code is not run
# Use FALSE when creating final report)
developer_mode <- TRUE

# toggle whether to create draft SUS emails
draft_emails_sus <- FALSE

# toggle whether to create draft non submission emails
draft_emails_non_submitters <- FALSE

# Input the date of the report
time_period <- "VTE-Q4-2025-26"

# API toggle when boundaries have changed but we need old mapping files
# E.g Q4 25/26 when ICB boundaries changed in April 2026, need to use pre 
# April 2026 boundaries for the Q4 report. 
# developer_mode needs to be FALSE
# TRUE = API is used. FALSE = saved datalake file used
api_toggle <- FALSE

# Saves a copy of ods_provider_hierarchies files in datalake
# developer_mode needs to be FALSE
copy_ods_files <- FALSE

# How many quarters to go back for charts
previous_quarters <- 20

# Decide what percentage change to flag in validation
signficant_change <- 40

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
