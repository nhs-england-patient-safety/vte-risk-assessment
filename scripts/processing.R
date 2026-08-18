# //////////////////////////////////////////////////////////////////////////////
#
##  Cleansing and mapping data  ----
#
# //////////////////////////////////////////////////////////////////////////////

# format and cleanse dataframe
seft_cleansed <- seft_udal |>
  clean_names() |>
  # Some previous years dummy entries added org_name as NA so filtering here
  filter(!is.na(org_name)) |>
  mutate(
    fy_start = as.numeric(substr(period, 8, 11)),
    fy_end = fy_start + 1,
    month = as.numeric(str_replace_all(month, "Month ", "")),
    quarter = as.numeric(substr(period, 6, 6)),
    month_corrected = case_when(
      quarter == 1 ~ month + 3,
      quarter == 2 ~ month + 6,
      quarter == 3 ~ month + 9,
      quarter == 4 ~ month
    ),
    year = ifelse(quarter == 4, fy_end, fy_start),
    date = lubridate::make_date(
      year = year,
      month = month_corrected,
      day = 1
    ),
    period_readable = paste0(substring(period, 5, 6), "\n", 
                             substring(period, 10, 11), "/", 
                             substring(period, 13, 14)),
    fy = paste0(fy_start, "/", fy_end)
  )

# api mapping table
mapping <- mapping_table |>
  select(organisation_code = api_org_code, 
         organisation_name = api_org_name, 
         org_type)

# tac table
mapping_tac <- trust_accounts_consolidation_table |>
  select(provider_code = org_nhs_code, provider_name = org_name, sector)

# pull out submitters with 0 admissions
zero_admissions_list <- seft_cleansed |> 
  group_by(period, org_name, org_code) |>
  summarise(
    total_admissions = sum(total_admissions, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(total_admissions == 0) 

# Joining org type to data and bringing in up to date organisation names
df_mapped <- seft_cleansed |>
  left_join(mapping, join_by(org_code == organisation_code)) |>
  select(!org_name) |>
  rename(org_name = organisation_name) |>
  relocate(org_name, .after = org_code)

# excluded data that we have manually excluded from the data_quality file
excluded_data_manual <- data_quality_input |>
  filter(excluded_data == "excluded") |>
  select(period, org_code)

# excluded providers who submitted zero admissions
excluded_data_zero_admissions <- zero_admissions_list |> 
  select(period, org_code)

# excluded data
excluded_data <- bind_rows(excluded_data_manual,excluded_data_zero_admissions) |> 
  distinct()

# filtering out excluded data
df_joined <- df_mapped |>
  anti_join(excluded_data, join_by(period == period, org_code == org_code))


# //////////////////////////////////////////////////////////////////////////////
#
##  Helper files to aid with manual input on reports  ----
#
# //////////////////////////////////////////////////////////////////////////////


# excluded data percentages for manual input into data quality section
df_mapped_grouped <- df_mapped |>
  group_by(period, org_code) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = round(100 * x / n, digits = 0)) |>
  ungroup() |>
  select(period, org_code, percentage)

data_quality_percentages <- data_quality_input |>
  select(!percentage) |>
  left_join(df_mapped_grouped, 
            join_by(period == period, org_code == org_code)) |>
  select(period, type, org_code, excluded_data, 
         excluded_reason, percentage, text)

dq_up <- reslib$get_item("VTE/vte-risk-assessment/input-data/")
dq_up$save_dataframe(data_quality_percentages, "data_quality.csv")


# number of returns and overall percentage for manual input into revisions
number_of_returns <- df_joined |>
  group_by(period) |>
  summarize(returns = n_distinct(org_code)) |>
  select(period, returns)

england_percent_revisions <- df_joined |>
  group_by(period) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = 100 * x / n) |>
  select(period, percentage)

revisions_returns_percentages <- revisions_input |>
  select(period, text) |>
  left_join(number_of_returns, join_by(period == period)) |>
  left_join(england_percent_revisions, join_by(period == period)) |>
  select(period, percentage, returns, text)

rev_up <- reslib$get_item("VTE/vte-risk-assessment/input-data/")
rev_up$save_dataframe(revisions_returns_percentages, "revisions.csv")

# //////////////////////////////////////////////////////////////////////////////
#
##  ICB mapping  ----
#
# //////////////////////////////////////////////////////////////////////////////

# get ICB names 
icb_mapping <- mapping_table |>
  select(organisation_code = api_org_code, 
         icb_code = api_icb_code, 
         icb_name = api_icb_name)

# creating master geospatial mapping file
geospatial_mapping <- df_joined |>
  # add ICB
  left_join(icb_mapping, join_by(org_code == organisation_code)) |>
  # rename regions
  mutate(parent_name = str_remove(parent_name, " COMMISSIONING REGION"),
         parent_name = str_to_title(parent_name),
         parent_name = str_replace_all(parent_name, "\\bAnd\\b|\\bOf\\b", str_to_lower))


# //////////////////////////////////////////////////////////////////////////////
#
##  Commentary time periods  ----
#
# //////////////////////////////////////////////////////////////////////////////


# Creating an ordered table of each collection we have by quarter
collections_quarter <- df_joined |>
  group_by(period) |>
  mutate(min_date = min(date), 
         max_date = max(date),
         time_period_readable = paste0(substring(period, 5, 6), " ", 
                               substring(period, 8, 11), "/", 
                               substring(period, 13, 14))) |>
  select(period, min_date, max_date,time_period_readable) |>
  arrange(max_date) |>
  distinct() |>
  rowid_to_column("index")

# index value of our selected period so we can use this to get previous periods
time_period_index_quarter <- 
  collections_quarter$index[collections_quarter$period == time_period]

# index value of previous time period for QA emails
previous_period_index_qa <- time_period_index_quarter -  1

# previous quarter time period readable for QA emails
prev_q_time_period_readable <- 
  collections_quarter$time_period_readable[collections_quarter$index ==
                                             previous_period_index_qa]

# index value of previous time period for time series chart
previous_period_index_quarter <- 
  case_when(time_period_index_quarter - previous_quarters < 1 ~ 1,
  .default = time_period_index_quarter - previous_quarters
)

# start and end periods
start_period_quarter <- 
  collections_quarter$min_date[collections_quarter$index == 
                                 previous_period_index_quarter]
end_period_quarter <- 
  collections_quarter$max_date[collections_quarter$index == 
                                 time_period_index_quarter]
