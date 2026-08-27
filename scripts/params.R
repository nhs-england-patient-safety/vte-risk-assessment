# Input the date of the report
time_period <- "VTE-Q1-2026-27"

# Choose mode: publish, publish_legacy_mapping, local
# publish
#   Uses UDAL + API (normal publishing)
# publish_legacy_mapping
#   Uses UDAL only, API not used.
#   Used where boundaries have changed and historic mappings are required
#   (e.g. Q4 2025/26 using pre‑April 2026 ICB boundaries)
# local
#   No UDAL, no API. Uses saved file copies (developer / offline use)
mode <- "publish"

# Saves a copy of ods_provider_hierarchies files to the datalake.
# Run this before any mapping changes (e.g. ICB boundary changes).
# Requires mode == "publish"
copy_ods_files <- FALSE

# toggle draft emails
draft_emails_sus <- FALSE
draft_emails_non_submitters <- FALSE
draft_emails_dq <- FALSE

# release tag of the published code
release_tag <- "2026-27_Q1_v1"

release_url <- paste0(
  "[GitHub]",
  "(https://github.com/nhs-england-patient-safety/vte-risk-assessment/releases/tag/",
  release_tag,
  ")"
  )

# How many quarters to go back for charts
previous_quarters <- 20

# error handling
if (copy_ods_files && mode != "publish") {
  stop("copy_ods_files = TRUE requires mode == 'publish'")
}

