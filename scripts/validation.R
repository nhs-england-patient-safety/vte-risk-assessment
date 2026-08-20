library(openxlsx)

# //////////////////////////////////////////////////////////////////////////////
#
#  Importing and transforming data  ----
#
# //////////////////////////////////////////////////////////////////////////////

# ods_provider_hierarchies table to compare types across UDAL, API and TAC
mapping_codes <- ods_provider_hierarchies |>
  clean_names() |>
  select(organisation_code, nhse_organisation_type, ods_organisation_type)

# Non submitter lists downloaded manually from SDCS so should have
# ALL organisations on SDCS expecting to submit
sdcs_list <- reslib$load_dataframe(
  "VTE/vte-risk-assessment/sdcs-data/sdcs_submitter_list.csv", 
  show_col_types = FALSE
) |>
  clean_names() |>
  left_join(mapping_codes, join_by(org_code == organisation_code)) |>
  left_join(mapping_tac, join_by(org_code == provider_code)) |>
  left_join(mapping_table, join_by(org_code == api_org_code)) |> 
  select(
    org_code, 
    organisation_name, 
    udal_nhse_org_type = nhse_organisation_type,
    udal_ods_org_type = ods_organisation_type,
    api_org_type = api_role_name,
    tac_sector = sector
  ) |>
  arrange(udal_nhse_org_type,api_org_type,tac_sector) |>
  distinct()

sdcs_email <- reslib$load_dataframe(
  "VTE/vte-risk-assessment/sdcs-data/sdcs_submitter_list.csv", 
  show_col_types = FALSE
) |>
  clean_names() |>
  # join mapping table for orgtype on this master list and update organisation
  group_by(org_code) |>
  mutate(email = paste0(contact_email, collapse = "; ")) |>
  select(org_code, email) |>
  distinct()

# notes for this period (note these files need manually downloading)
current_notes <- notes |>
  filter(
    status == "Complete",
    reporting_period == time_period
  ) |>
  select(organisation_code, notes) |>
  distinct()

# excluded data (taken from our manual list in sharepoint)
excluded <- data_quality_input |>
  filter(period == time_period, excluded_data == "excluded") |>
  left_join(df_mapped, join_by(org_code == org_code, period == period)) |>
  left_join(current_notes, join_by(org_code == organisation_code)) |>
  select(
    org_code,
    org_name,
    org_type,
    month,
    number_of_vte_assessed_admissions,
    total_admissions,
    percentage_of_admitted_patients_risk_assessed_for_vte,
    notes,
    excluded_reason,
    text
  )


# //////////////////////////////////////////////////////////////////////////////
#
#  Validation  ----
#
# //////////////////////////////////////////////////////////////////////////////

# //////////////////////////////////////////////////////////////////////////////
#
##  UDAL ODS provider hierarchies vs API  ----
#
# //////////////////////////////////////////////////////////////////////////////

udal_vs_api <- ods_provider_hierarchies |> 
  clean_names() |> 
  left_join(ods_sites, join_by(organisation_code == Site_Code)) |>
  left_join(successor_orgs |> filter(Is_Latest == 1), 
            join_by(organisation_code == Organisation_Code)) |>
  left_join(mapping_table, join_by(organisation_code == api_org_code)) |> 
  clean_names() |> 
  filter(!is.na(api_org_name)) |> 
  mutate(
    name_test = organisation_name == api_org_name,
    role_test = case_when(
      # if both ods AND api role names are missing
      # test is true as both cols are empty
      is.na(ods_organisation_type) & is.na(api_role_name) ~ TRUE,
      # if both ods AND api role names are identical
      # test is true as both cols have the same string
      ods_organisation_type == api_role_name ~ TRUE,
      # if either ods OR api role names are missing
      # test is false because one the cols is empty and the other has a value
      is.na(ods_organisation_type) | is.na(api_role_name) ~ FALSE,
      .default = FALSE
    ),
    effective_to_test = case_when(
      is.na(effective_to) & is.na(api_effective_to) ~ TRUE,
      is.na(effective_to) | is.na(api_effective_to) ~ FALSE,
      effective_to == api_effective_to ~ TRUE,
      .default = FALSE
    ),
    icb_test = case_when(
      is.na(stp_name) & is.na(api_icb_name) ~ TRUE,
      is.na(stp_name) | is.na(api_icb_name) ~ FALSE,
      stp_name == api_icb_name ~ TRUE,
      .default = FALSE
    ),
    region_test = case_when(
      is.na(region_name) & is.na(api_region_name) ~ TRUE,
      is.na(region_name) | is.na(api_region_name) ~ FALSE,
      region_name == api_region_name ~ TRUE,
      .default = FALSE
    ),
    operated_by_test = case_when(
      is.na(trust_name) & is.na(api_operated_by_name) ~ TRUE,
      is.na(trust_name) | is.na(api_operated_by_name) ~ FALSE,
      trust_name == api_operated_by_name ~ TRUE,
      .default = FALSE
    ),
    successor_test = case_when(
      is.na(successor_organisation_code) & is.na(api_successor_code) ~ TRUE,
      is.na(successor_organisation_code) | is.na(api_successor_code) ~ FALSE,
      successor_organisation_code == api_successor_code ~ TRUE,
      .default = FALSE
    )
  ) |> 
  select(organisation_code,
         organisation_name, api_org_name, name_test,
         ods_organisation_type, api_role_name, role_test,
         effective_to, api_effective_to, effective_to_test,
         stp_name, api_icb_name, icb_test,
         region_name,api_region_name, region_test,
         trust_name, api_operated_by_name, operated_by_test,
         successor_organisation_code, api_successor_code, successor_test)




# //////////////////////////////////////////////////////////////////////////////
#
##  SDCS organisation names  ----
#
# //////////////////////////////////////////////////////////////////////////////


# inactive and succeeded organisations
inactive_and_successor_orgs <- reslib$load_dataframe(
  "VTE/vte-risk-assessment/sdcs-data/sdcs_submitter_list.csv", 
  show_col_types = FALSE
) |>
  clean_names() |>
  select(org_code, organisation_name) |>
  distinct() |>
  left_join(mapping_table, join_by(org_code == api_org_code)) |>
  select(
    org_code,
    api_org_name,
    api_legal_end,
    api_operational_end,
    api_successor_code,
    api_successor_name,
    api_effective_to
  ) |>
  filter(!is.na(api_effective_to))

# List of organisation names that need updating
org_name_updates_required <- reslib$load_dataframe(
  "VTE/vte-risk-assessment/sdcs-data/sdcs_submitter_list.csv", 
  show_col_types = FALSE
) |>
  clean_names() |>
  select(org_code, sdcs_org_name = organisation_name) |>
  distinct() |>
  left_join(mapping |>  rename(ods_org_name = organisation_name), 
            join_by(org_code == organisation_code)) |>
  select(!org_type) |>
  mutate(check = (sdcs_org_name == ods_org_name)) |>
  filter(check == FALSE) |>
  select(org_code:ods_org_name)


# //////////////////////////////////////////////////////////////////////////////
#
##  Check for NAs/nulls and non assigned org types ----
#
# //////////////////////////////////////////////////////////////////////////////


# check for NAs / nulls
validation_na <- df_mapped |>
  filter(if_any(everything(), is.na))

# check for non assigned org types
validation_join <- df_mapped |>
  filter(is.na(org_type))

# number of denominator >1
num_over_den <- df_mapped |>
  filter(period == time_period) |>
  mutate(num_over_den_flag = if_else(number_of_vte_assessed_admissions / 
                                       total_admissions > 1, "TRUE", NA)) |>
  select(org_code, num_over_den_flag) |>
  filter(!is.na(num_over_den_flag)) |>
  distinct()

# //////////////////////////////////////////////////////////////////////////////
#
##  Not submitted  ----
#
# //////////////////////////////////////////////////////////////////////////////

submitted_in_period <- df_mapped |>
  filter(period == time_period) |>
  select(org_code) |>
  distinct()


time_period_deadline <- case_when(
  substring(time_period, 6, 6) == 
    1 ~ paste0("31/07/", substring(time_period, 8, 11)),
  substring(time_period, 6, 6) == 
    2 ~ paste0("31/10/", substring(time_period, 8, 11)),
  substring(time_period, 6, 6) == 
    3 ~ paste0("31/01/", substring(time_period, 8, 9), 
               substring(time_period, 13, 14)),
  substring(time_period, 6, 6) == 
    4 ~ paste0("30/04/", substring(time_period, 8, 9), 
               substring(time_period, 13, 14))
)

not_submitted <- sdcs_list |>
  mutate(period = time_period) |>
  anti_join(submitted_in_period, join_by(org_code == org_code)) |>
  anti_join(excluded_data, join_by(period == period, org_code == org_code)) |>
  left_join(current_notes, join_by(org_code == organisation_code)) |>
  left_join(sdcs_email, join_by(org_code == org_code)) |>
  select(!period) |>
  filter(org_code != "X24" & org_code != "X26" & org_code != "T1520") |>
  mutate(export_email = paste0(
    "Dear colleague,\n\n",
    "The VTE risk assessment collection for ",
    organisation_name,
    " (",
    org_code,
    ") for ",
    time_period_readable,
    " was due for submission on ",
    time_period_deadline,
    ". When we checked our records, we noticed your submission was still outstanding. ",
    "Could you inform us of any issues which may have prevented submission of this data?\n\n",
    "If you are no longer the relevant contact for this data collection, please advise and we will update our contact list.\n\n",
    "Thank you,\n",
    "Patient safety team"
  ))


# //////////////////////////////////////////////////////////////////////////////
#
##  Data excluded  ----
#
# //////////////////////////////////////////////////////////////////////////////





# //////////////////////////////////////////////////////////////////////////////
#
##  Admissions submitted compared with SUS admissions data  ----
#
# //////////////////////////////////////////////////////////////////////////////

# highlighting changes between submitted and SUS data
sus_admissions <- df_mapped |>
  select(period, org_code, month, date, total_admissions) |>
  left_join(mapping, join_by(org_code == organisation_code)) |>
  left_join(sdcs_email, join_by(org_code == org_code)) |>
  left_join(admissions, 
            join_by(org_code == provider_code, date == date)) |>
  # calculating difference in admissions
  mutate(sus_change = 100 * (total_admissions_sus - total_admissions) /
      ((total_admissions_sus + total_admissions) / 2)) |> 
  filter(period == time_period) |>
  select(!date) |> 
  arrange(org_code,month) |> 
  # reformatting to 1 row per org
  pivot_wider(
    names_from = month,
    values_from = c(
      total_admissions,
      total_admissions_sus,
      sus_change
    )
  ) |>
  mutate(
    total_admissions = (total_admissions_1 + total_admissions_2 + 
                          total_admissions_3),
    total_admissions_sus = (total_admissions_sus_1 + total_admissions_sus_2 + 
                              total_admissions_sus_3),
    sus_total_change = 100 * (total_admissions_sus - total_admissions) /
        ((total_admissions_sus + total_admissions) / 2),
    abs_sus_total_change = abs(sus_total_change),
    export_email = case_when(
      abs_sus_total_change > 10 ~ paste0(
        "Dear colleague,\n\n",
        "For the VTE risk assessment collection for ",
        organisation_name, " (", org_code, ") for ", time_period_readable, 
        ", we noticed a difference in the number of admitted patients compared with our records from the Secondary Uses Service (SUS).\n\n",
        "Self-reported total admissions for the reporting quarter: ", total_admissions, "\n",
        "Total spells in SUS for the reporting quarter where age is 16 and over: ", total_admissions_sus, "\n\n",
        "Please could you review?\n\n",
        "A reminder that this data collection is a census of patients, and it is not appropriate to use sampling methodologies to produce estimates. Cohorted patients should still be included in the numerator and denominator.\n\n",
        "Thank you,\n",
        "Patient safety team"
      )
    )
  ) |>
  # adding notes
  left_join(current_notes, join_by(org_code == organisation_code)) |>
  select(!period) |> 
  arrange(desc(abs_sus_total_change))

# no sus admissions
no_sus_admissions <- sus_admissions |>
  filter(is.na(total_admissions_sus)) |>
  select(org_code:total_admissions_sus_3)

# //////////////////////////////////////////////////////////////////////////////
#
##  Non acute trusts  ----
#
# //////////////////////////////////////////////////////////////////////////////


non_acute_providers <- df_mapped |>
  clean_names() |>
  filter(period == time_period) |>
  select(org_code, org_name) |>
  distinct() |>
  left_join(mapping_codes, join_by(org_code == organisation_code)) |>
  left_join(mapping_tac, join_by(org_code == provider_code)) |>
  select(org_code, org_name, nhse_organisation_type, 
         ods_organisation_type, sector) |>
  filter(!str_detect(nhse_organisation_type, "(?i)independent")) |>
  filter(!str_detect(nhse_organisation_type, "(?i)acute") | 
           !str_detect(sector, "(?i)acute")) |>
  arrange(sector)




# //////////////////////////////////////////////////////////////////////////////
#
##  SEFT UDAL validation ----
#
# //////////////////////////////////////////////////////////////////////////////

# get UDAL process time for files and join onto UDAL table
seft_udal_time <- seft_udal |>
  mutate(udal_file_id = as.integer(udal_file_id)) |>
  left_join(udal_process_time, join_by(udal_file_id == udal_file_id)) |>
  filter(period == time_period)

# //////////////////////////////////////////////////////////////////////////////
#
##  TAC tables and ODS provider hierarchies validation ----
#
# //////////////////////////////////////////////////////////////////////////////


# ods_provider_hierarchies with just org code and effective to columns
org_codes_with_effective_to <- ods_provider_hierarchies |>
  clean_names() |>
  select(organisation_code, effective_to)

# checking acute tusts missing in SDCS from tac tables
acute_trusts_missing_from_tac <- mapping_tac |>
  # join SDCS list to the tac table
  left_join(sdcs_list, join_by(provider_code == org_code)) |>
  # join the manual list of excluded providers collecting since Q1 24/25
  left_join(manual_excluded_providers, join_by(provider_code == org_code)) |>
  # join on the effective to column from ods_provider_heirearchies from UDAL
  left_join(org_codes_with_effective_to, 
            join_by(provider_code == organisation_code)) |>
  # filter out ambulance, community and MH sectors
  filter(
    sector != "Ambulance", sector != "Community",
    sector != "Mental Health",
    # filters out all valid org names in SDCS table leaving only missing trusts
    is.na(organisation_name)
  ) |>
  select(provider_code, provider_name, sector, reason, effective_to) |>
  arrange(effective_to)

# checking acute tusts missing in SDCS from ods_provider_hierarchies
acute_trusts_missing_from_ods <- ods_provider_hierarchies |>
  clean_names() |>
  # join SDCS list to the ods_provider_hierarchies table
  left_join(sdcs_list, join_by(organisation_code == org_code)) |>
  # join the manual list of excluded providers collecting since Q1 24/25
  left_join(manual_excluded_providers, 
            join_by(organisation_code == org_code)) |>
  # filter for only acute trusts in nhse_organisation_type column
  filter(
    nhse_organisation_type == "ACUTE TRUST",
    # filters out valid org names in our SDCS table leaving only missing trusts
    is.na(organisation_name.y)
  ) |>
  select(organisation_code, organisation_name.y, nhse_organisation_type, 
         reason, effective_to) |>
  arrange(effective_to)


# //////////////////////////////////////////////////////////////////////////////
#
##  Flagging DQ issues across quarters ----
#
# //////////////////////////////////////////////////////////////////////////////

# Summarise all three months in the quarter and highlight differences
flags <- df_mapped |> 
  filter(org_code != "X26") |> 
  left_join(admissions, 
            join_by(org_code == provider_code, date == date)) |>
  group_by(period,org_code, org_name, org_type, fy, quarter) |> 
  summarise(vte_admissions = sum(number_of_vte_assessed_admissions, na.rm = TRUE),
            total_admissions = sum(total_admissions, na.rm = TRUE),
            total_admissions_sus = sum(total_admissions_sus, na.rm = TRUE),
            percentage = vte_admissions/total_admissions,
            .groups = "drop") |> 
  group_by(org_code) |>
  arrange(org_code, fy, quarter, .by_group = TRUE) |> 
  mutate(vte_admissions_prev_q = lag(vte_admissions),
         total_admissions_prev_q = lag(total_admissions),
         total_admissions_sus_prev_q = lag(total_admissions_sus),
         percentage_prev_q = lag(percentage),
         vte_admissions_percent_change = if_else(!is.na(lag(vte_admissions)),
                                      s_percent_change(vte_admissions,
                                                       lag(vte_admissions)),
                                      NA),
         abs_vte_admissions_percent_change = abs(vte_admissions_percent_change),
         total_admissions_percent_change = if_else(!is.na(lag(total_admissions)),
                                                 s_percent_change(total_admissions,
                                                                  lag(total_admissions)),
                                                 NA),
         abs_total_admissions_percent_change = abs(total_admissions_percent_change),
         total_admissions_sus_percent_change = if_else(!is.na(lag(total_admissions_sus)),
                                                   s_percent_change(total_admissions_sus,
                                                                    lag(total_admissions_sus)),
                                                   NA),
         abs_total_admissions_sus_percent_change = abs(total_admissions_sus_percent_change),
         percentage_percent_change = if_else(!is.na(lag(percentage)),
                                                 s_percent_change(percentage,
                                                                  lag(percentage)),
                                                 NA),
         abs_percentage_percent_change = abs(percentage_percent_change)) |> 
  ungroup() |> 
  select(period:quarter,
         vte_admissions_prev_q,
         vte_admissions,
         vte_admissions_percent_change,
         abs_vte_admissions_percent_change,
         total_admissions_prev_q,
         total_admissions,
         total_admissions_percent_change,
         abs_total_admissions_percent_change,
         total_admissions_sus_prev_q,
         total_admissions_sus,
         total_admissions_sus_percent_change,
         abs_total_admissions_sus_percent_change,
         percentage_prev_q,
         percentage,
         percentage_percent_change,
         abs_percentage_percent_change) |> 
  left_join(sdcs_email, join_by(org_code == org_code)) |> 
  relocate(email, .after = org_type)

# all years of data for validation spreadsheet
flags_all_years <- flags |> 
  select(-fy, 
         -quarter, 
         -vte_admissions_prev_q, 
         -total_admissions_prev_q, 
         -total_admissions_sus_prev_q,
         -percentage_prev_q)

# vte assessed changes between current and previous quarter
flags_prev_quarter_comparison <- flags |> 
  filter(period == time_period) |> 
  mutate(
    vte_admissions_bullet = case_when(
    abs_vte_admissions_percent_change > 1000 ~ paste0(
      "-   a change in VTE risk assessed admissions from ",
      vte_admissions_prev_q, " in ", prev_q_time_period_readable, " to ",
      vte_admissions, " in ", time_period_readable,".\n"
    ),
    TRUE ~ ""
  ),
  total_admissions_bullet = case_when(
    abs_total_admissions_percent_change > 9.9 ~ paste0(
      "-   a change in total admissions from ",
      total_admissions_prev_q, " in ", prev_q_time_period_readable, " to ",
      total_admissions, " in ", time_period_readable,".\n"
    ),
    TRUE ~ ""
  ),
  percentage_bullet = case_when(
    abs_percentage_percent_change > 20 ~ paste0(
      "-   a change in percentage of admitted patients risk assessed for VTE from ",
      round(percentage_prev_q * 100, 1), "% in ", prev_q_time_period_readable, " to ",
      round(percentage * 100, 1), "% in ", time_period_readable,".\n"
    ),
    TRUE ~ ""
  ),
  export_email = case_when(vte_admissions_bullet != "" | 
                             total_admissions_bullet != "" |
                             percentage_bullet != "" ~
    paste0(
      "Dear colleague,\n\n",
      "For the VTE risk assessment collection for ",
      org_name, " (", org_code, ") for ", time_period_readable, 
      " we have identified: \n\n",
      vte_admissions_bullet,
      total_admissions_bullet,
      percentage_bullet,"\n",
      "We would be grateful if you could review your submissions and confirm their accuracy. If any issues are identified please let us know so we can discuss potential resubmission.\n\n",
      "Thank you,\n",
      "Patient safety team"
    ),
    TRUE ~ ""
  )
  )

# //////////////////////////////////////////////////////////////////////////////
#
##  Automated emails ----
#
# //////////////////////////////////////////////////////////////////////////////

if (draft_emails_sus == TRUE) {
  sus_email_drafts <- sus_admissions |> 
    filter(org_code == "RWG") |> 
    select(org_code, organisation_name, email, export_email) |> 
    rowwise() |>
    mutate(email_address = list(strsplit(email, ";\\s")[[1]]),
           drafts = list(
             outlook$create_email(to = email_address,
                                  cc = "patientsafety.analysis@nhs.net",
                                  subject = paste0(time_period_readable,
                                                   " VTE Submissions Query - ",
                                                   organisation_name," (",
                                                   org_code,")"),
                                  body = paste(export_email))),
           drafts = list(drafts$update(
             from = list(emailAddress = list(
               address = "patientsafety.analysis@nhs.net")))))
}


if (draft_emails_non_submitters == TRUE) {
  non_submitters_email_drafts <- not_submitted |> 
    select(org_code, organisation_name, email, export_email) |> 
    rowwise() |>
    mutate(email_address = list(strsplit(email, ";\\s")[[1]]),
           drafts = list(
             outlook$create_email(to = email_address,
                                  cc = "patientsafety.analysis@nhs.net",
                                  subject = paste0(time_period_readable,
                                                   " VTE Submissions Query - ",
                                                   organisation_name," (",
                                                   org_code,")"),
                                  body = paste(export_email))),
           drafts = list(drafts$update(
             from = list(emailAddress = list(
               address = "patientsafety.analysis@nhs.net")))))
}

if (draft_emails_dq == TRUE) {
  dq_email_drafts <- flags_prev_quarter_comparison |> 
    filter(org_type == "NHS acute care provider",
           export_email != "",
           org_code == "RRF") |> 
    select(org_code, org_name, email, export_email) |> 
    rowwise() |>
    mutate(email_address = list(strsplit(email, ";\\s")[[1]]),
           drafts = list(
             outlook$create_email(to = email_address,
                                  cc = "patientsafety.analysis@nhs.net",
                                  subject = paste0(time_period_readable,
                                                   " VTE Submissions Query - ",
                                                   org_name," (",
                                                   org_code,")"),
                                  body = paste(export_email))),
           drafts = list(drafts$update(
             from = list(emailAddress = list(
               address = "patientsafety.analysis@nhs.net")))))
}


# No VTE risk assessed admissions in at least one month 
zero_vte_admissions <- df_mapped |> 
  filter(period == time_period,
         number_of_vte_assessed_admissions == 0) |> 
  select(c(org_code, org_name, number_of_vte_assessed_admissions, 
           total_admissions, percentage_of_admitted_patients_risk_assessed_for_vte,
           quarter, month_corrected, year))

