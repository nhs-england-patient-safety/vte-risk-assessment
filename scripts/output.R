# //////////////////////////////////////////////////////////////////////////////
#
#  Create output folders  ----
#
# //////////////////////////////////////////////////////////////////////////////

dir.create(file.path(here("output", time_period_folder)), showWarnings = FALSE)
dir.create(file.path(here("output", time_period_folder, "figures")), 
           showWarnings = FALSE)
dir.create(file.path(here("output", time_period_folder, "reference")), 
           showWarnings = FALSE)


# //////////////////////////////////////////////////////////////////////////////
#
#  Preparing and creating csv ----
#
# //////////////////////////////////////////////////////////////////////////////
# ods_sites and icb_mapping 
api_icb_ods <- api_mapping_table |> 
  select(api_org_code, api_icb_code, api_icb_name,
         api_operated_by_code, api_operated_by_name) |> 
  rename_with(~ str_remove(.x, "api_"))

# Removing and renaming columns, filtering for time period
published_csv <- df_joined |>
  filter(period == time_period) |>
  left_join(api_icb_ods, join_by(org_code == org_code)) |>
  rename(operated_by = operated_by_name) |>
  rename(region = parent_name) |>
  mutate("operational_standard_met" = 
           ifelse(percentage_of_admitted_patients_risk_assessed_for_vte >= 0.95, 
                  "yes", "no")) |>
  select(
    org_code,
    org_name,
    icb_name,
    region,
    operated_by,
    org_type,
    number_of_vte_assessed_admissions:
      percentage_of_admitted_patients_risk_assessed_for_vte,
    operational_standard_met,
    date
  ) |>
  arrange(org_code, date) |> 
  # Round percentage of admitted patients risk assessed to 4 decimal place.
  mutate(percentage_of_admitted_patients_risk_assessed_for_vte = 
           round(percentage_of_admitted_patients_risk_assessed_for_vte, 4))

write_csv(published_csv, here("output",time_period_folder,time_period_csv))

# //////////////////////////////////////////////////////////////////////////////
#
#  Creating validation excel  ----
#
# //////////////////////////////////////////////////////////////////////////////

# creating validation excel
names <- list(
  "no_submissions" = not_submitted, 
  "excluded_data" = excluded,
  "sus_admissions" = sus_admissions,
  "no_sus_admissions" = no_sus_admissions,
  "flags_all_years" = flags,
  "flags_vte_assessed" = flags_vte_assessed,
  "flags_total_admissions" = flags_total_admissions,
  "flags_risk_assessed" = flags_risk_assessed,
  "zero_vte_admissions" = zero_vte_admissions,
  "org_name_updates" = org_name_updates_required,
  "inactive_and_succeeded" = inactive_and_successor_orgs,
  "num_over_den" = num_over_den,
  "na" = validation_na,
  "no_assigned_type" = validation_join,
  "non_acute_trusts" = non_acute_providers,
  "seft_udal_time" = seft_udal_time,
  "ods_vs_api_vs_tac" = sdcs_list,
  "acute_trusts_missing_from_tac" = acute_trusts_missing_from_tac,
  "acute_trusts_missing_from_ods" = acute_trusts_missing_from_ods
)
write.xlsx(names, file = here("output",time_period_folder,time_period_xlsx))


# //////////////////////////////////////////////////////////////////////////////
#
#  Reference files  ----
#
# //////////////////////////////////////////////////////////////////////////////

write_csv(df_joined, 
          here("output",time_period_folder,"reference/all_seft_data.csv"))
write_csv(notes, here("output",time_period_folder,"reference/notes.csv"))
write_csv(data_quality_input, 
          here("output",time_period_folder,"reference/data_quality.csv"))
write_csv(revisions_input, 
          here("output",time_period_folder,"reference/revisions.csv"))
write_csv(trust_accounts_consolidation_table, 
          here("output",time_period_folder,
               "reference/trust_accounts_consolidation_table.csv"))
write_csv(sdcs_list, 
          here("output",time_period_folder,"reference/sdcs_list.csv"))
write_csv(ods_provider_hierarchies, 
          here("output",time_period_folder,
               "reference/ods_provider_hierarchies.csv"))

# //////////////////////////////////////////////////////////////////////////////
#
#  Upload files  ----
#
# //////////////////////////////////////////////////////////////////////////////

upload_file(time_period_csv)
upload_file(time_period_xlsx)
upload_ref_file("all_seft_data.csv")
upload_ref_file("data_quality.csv")
upload_ref_file("notes.csv")
upload_ref_file("revisions.csv")
upload_ref_file("trust_accounts_consolidation_table.csv")
upload_ref_file("sdcs_list.csv")
upload_ref_file("ods_provider_hierarchies.csv")
upload_figures("figures")