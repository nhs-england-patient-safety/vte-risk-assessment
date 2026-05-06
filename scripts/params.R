# Input the date of the report
time_period <- "VTE-Q4-2025-26"

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

# error handling
if (copy_ods_files && mode != "publish") {
  stop("copy_ods_files = TRUE requires mode == 'publish'")
}

