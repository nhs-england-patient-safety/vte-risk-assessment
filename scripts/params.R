# Developer mode (if TRUE then UDAL code is not run
# Use FALSE when creating final report)
developer_mode <- TRUE

# toggle whether to create draft SUS emails
draft_emails_sus <- FALSE

# toggle whether to create draft non submission emails
draft_emails_non_submitters <- FALSE

# Input the date of the report
time_period <- "VTE-Q2-2025-26"

# release tag of the published code
release_tag <- "2025-26_Q3_v3"

release_url <- paste0(
  "[GitHub]",
  "(https://github.com/nhs-england-patient-safety/vte-risk-assessment/releases/tag/",
  release_tag,
  ")"
  )

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
