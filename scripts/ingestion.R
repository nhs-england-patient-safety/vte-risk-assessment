library(sf)
library(knitr)
library(janitor)

# //////////////////////////////////////////////////////////////////////////////
#
##  Sharepoint (SDCS, notes, input files ----
#
# //////////////////////////////////////////////////////////////////////////////

# read in file that is updated manually with reasons providers are excluded
manual_excluded_providers <- reslib$load_dataframe(
  "VTE/vte-risk-assessment/input-data/excluded_providers.csv", 
  show_col_types = FALSE
)

# read in sdcs note files (manually uploaded)
dr_sdcs <- reslib$get_item(
  "VTE/vte-risk-assessment/sdcs-data/submission_notes/",)
sdcs_file_list <- dr_sdcs$list_files()

notes <- lapply(sdcs_file_list$name, function(x) {
  reslib$load_dataframe(paste0(
    "VTE/vte-risk-assessment/sdcs-data/submission_notes/", x),
    show_col_types = FALSE)
}) |>
  bind_rows() |>
  clean_names()

# read data_quality file (manually updated)
data_quality_input <- 
  reslib$load_dataframe("VTE/vte-risk-assessment/input-data/data_quality.csv",
                        show_col_types = FALSE)

# read revisions file (manually updated)
revisions_input <- 
  reslib$load_dataframe("VTE/vte-risk-assessment/input-data/revisions.csv",
                        show_col_types = FALSE)


# //////////////////////////////////////////////////////////////////////////////
#
##  UDAL  ----
#
# //////////////////////////////////////////////////////////////////////////////


if (mode == "publish" | mode == "publish_legacy_mapping") {
  message("retrieving admissions data from UDAL")
  admissions <- 
    tbl(con_udal, I("Reporting_MESH_APC.APCS_Core_Monthly_Snapshot")) |>
    filter(Der_Activity_Month >= 202404, 
           Age_At_Start_of_Spell_SUS >= 16,
           Age_At_Start_of_Spell_SUS < 120) |>
    group_by(Der_Provider_Code,Der_Activity_Month) |> 
    summarise(total_admissions = n()) |> 
    ungroup() |> 
    select(Der_Provider_Code, Der_Activity_Month, total_admissions) |>
    collect() |>
    clean_names()
  
  message("retrieving ods_provider_hierarchies data from UDAL")
  ods_provider_hierarchies <- 
    tbl(con_udal, I("Reporting_UKHD_ODS.Provider_Hierarchies")) |> 
    collect()
  
  message("retrieving ods_sites data from UDAL")
  ods_sites <- 
    tbl(con_udal, I("Reporting_UKHD_ODS.Provider_Site")) |> 
    collect()
  
  message("retrieving successor data from UDAL")
  successor_orgs <- 
    tbl(con_udal, I("UKHD_ODS.Successors_For_Orgs_Of_Current_Interest_SCD")) |> 
    collect()
  
  message("retrieving seft data from UDAL")
  seft_udal <- tbl(con_udal, I("SEFT_VTE.VTE_Data")) |>
    collect() |>
    clean_names()
  
  message("retrieving UDAL process time from UDAL")
  udal_process_time <- 
    tbl(con_udal, I("Internal_Definitions.UDAL_IngestFileLog")) |>
    filter(Dataset == "VTE", Entity == "VTE_Data") |>
    select(UDALFileID, ProcessDate) |>
    collect() |>
    clean_names()
  
  # test your sql query
  # show_query(tbl(con_udal, 
  # I('UKHF_Trust_Accounts_Consolidation.Provider_List1_1')) |> 
  # filter(Sector == "Acute") )
  
  # close connection
  dbDisconnect(con_udal)
}


# //////////////////////////////////////////////////////////////////////////////
#
#  Upload copies to sharepoint  ----
#
# //////////////////////////////////////////////////////////////////////////////

if (mode == "publish" | mode == "publish_legacy_mapping") {
  # Uploading UDAL files to datalake for quicker access during development
  datalake_upload(ods_provider_hierarchies, stored_files_folder, "object")
  datalake_upload(ods_sites, stored_files_folder, "object")
  datalake_upload(successor_orgs, stored_files_folder, "object")
  datalake_upload(admissions, stored_files_folder, "object")
  datalake_upload(seft_udal, stored_files_folder, "object")
  datalake_upload(udal_process_time, stored_files_folder, "object")
}

# //////////////////////////////////////////////////////////////////////////////
#
#  Ingest UDAL and API files from datalake  ----
#
# //////////////////////////////////////////////////////////////////////////////

if (mode == "local") {
  ods_provider_hierarchies <- datalake_download("ods_provider_hierarchies", 
                                                stored_files_folder)
  ods_sites <- datalake_download("ods_sites", stored_files_folder)
  successor_orgs <- datalake_download("successor_orgs", stored_files_folder)
  admissions <- datalake_download("admissions", stored_files_folder)
  seft_udal <- datalake_download("seft_udal", stored_files_folder)
  udal_process_time <- datalake_download("udal_process_time",
                                         stored_files_folder)
  mapping_table <- datalake_download("mapping_table", stored_files_folder)
}


# //////////////////////////////////////////////////////////////////////////////
#
##  Download from Data lake ----
#
# //////////////////////////////////////////////////////////////////////////////

# Trust Accounts Consolidation Table (TAC)
trust_accounts_consolidation_table <- latest_file(tac_folder)


# //////////////////////////////////////////////////////////////////////////////
#
##  Upload ods_provider_hierarchies to Data Lake  ----
#
# //////////////////////////////////////////////////////////////////////////////

# storing copy of ods_provider_hierarchies from UDAL
if (copy_ods_files == TRUE) {
  datalake_upload(ods_provider_hierarchies,ods_provider_folder, "date")
}


# //////////////////////////////////////////////////////////////////////////////
#
##  FHIR API query  ----
#
# //////////////////////////////////////////////////////////////////////////////

if (mode == "publish") {
  
  # distinct org codes for api query
  # org codes from SDCS
  org_codes_sdcs <- reslib$load_dataframe(
    "VTE/vte-risk-assessment/sdcs-data/sdcs_submitter_list.csv", 
    show_col_types = FALSE
  )  |> 
    select(org_code = "Org Code")
  
  # org codes from submitters
  org_codes_seft <- seft_udal |> 
    select(org_code)
  
  # distinct org codes
  distinct_org_codes <- bind_rows(org_codes_sdcs,org_codes_seft) |> 
    distinct() |> 
    arrange(org_code) |> 
    pull(org_code)
  
  # creating api mapping table
  mapping_table <- map_dfr(distinct_org_codes, run_api_calls) |> 
    rename(api_org_code = code,
           api_org_name = display, 
           api_role_code = primaryRole, 
           api_legal_end = legEndDate,
           api_operational_end = opEndDate,
           api_operationally_active = status,
           api_icb_code = ICB,
           api_region_code = NHSER,
           api_operated_by_code = RE6,
           api_successor_code = successor) |> 
    left_join(role_lookup, join_by(api_role_code == api_role_code)) |>
    left_join(icb_lookup, join_by(api_icb_code == api_icb_code)) |>
    left_join(region_lookup, join_by(api_region_code == api_region_code)) |>
    mutate(
      api_legal_end = ymd(api_legal_end),         
      api_operational_end = ymd(api_operational_end), 
      api_effective_to = pmin(api_legal_end, api_operational_end, na.rm = TRUE),
    )
  
  # upload to datalake
  datalake_upload(mapping_table, stored_files_folder, "object")
}

# //////////////////////////////////////////////////////////////////////////////
#
##  Using UDAL instead of API ----
#
# //////////////////////////////////////////////////////////////////////////////

# Using UDAL instead of API when boundaries change 
if (mode == "publish_legacy_mapping") {
  
  # Provider Hierarchies table from datalake (stored from step above)
  latest_ods_provider_hierarchies_table <- latest_file(ods_provider_folder)
  
  # ODS sites from UDAL for operated by names
  ods_sites_cleaned <- ods_sites |> clean_names() |> 
    select(site_code, site_name, trust_code, trust_name)
  
  # Successor file from UDAL for successors
  successor_orgs_cleaned <- successor_orgs |>  clean_names() |> 
    left_join(latest_ods_provider_hierarchies_table, 
              join_by(successor_organisation_code == organisation_code)) |> 
    distinct(organisation_code, .keep_all = TRUE) |>
    select(organisation_code,
           api_successor_code = successor_organisation_code,
           api_successor_name = organisation_name) 
  
  # Bring together UDAL files and rename to match API
  mapping_table <- latest_ods_provider_hierarchies_table |> 
    left_join(ods_sites_cleaned, join_by(organisation_code == site_code)) |> 
    left_join(successor_orgs_cleaned, join_by(organisation_code == organisation_code)) |> 
    mutate(org_type = case_when(
      ods_organisation_type == "INDEPENDENT SECTOR H/C PROVIDER SITE" ~ 
        "Independent provider",
      ods_organisation_type == "INDEPENDENT SECTOR HEALTHCARE PROVIDER" ~ 
        "Independent provider",
      ods_organisation_type == "NHS TRUST" ~ "NHS acute care provider",
      .default = NA)
    ) |> 
    select(api_org_code = organisation_code,
           api_org_name = organisation_name,
           api_legal_end = effective_to,
           api_operational_end = effective_to,
           api_icb_code = stp_code,
           api_icb_name = stp_name,
           api_region_code = region_code,
           api_region_name = region_name,
           api_operated_by_code = trust_code,
           api_operated_by_name = trust_name,
           api_role_name = ods_organisation_type,
           api_effective_to = effective_to,
           api_successor_code,
           api_successor_name,
           org_type)
}

# //////////////////////////////////////////////////////////////////////////////
#
##  Geojson files from open geography portal  ----
#
# //////////////////////////////////////////////////////////////////////////////

message("retrieving ICB boundaries from geoportal")
if (last_day_of_quarter <= as.Date("2026-03-31")) {
  nhs_icb <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
} else {
  nhs_icb <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2026_Boundaries_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
}





