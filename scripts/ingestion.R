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


if (developer_mode == FALSE) {
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
##  Data lake (TAC table) ----
#
# //////////////////////////////////////////////////////////////////////////////


# list all files
files <- list_storage_files(cont, tac_folder, recursive = FALSE, info = "all")

# get latest file
latest_file <- files |>
  mutate(
    date_str = str_extract(name, "\\d{4}_\\d{2}_\\d{2}"),
    file_date = ymd(str_replace_all(date_str, "_", "-"))
  ) |>
  filter(!is.na(file_date)) |>
  arrange(desc(file_date)) |>
  slice(1) |> 
  pull(name)

# reading in TAC table
trust_accounts_consolidation_table <- 
  read_csv(storage_download(cont, latest_file, dest = NULL))


# //////////////////////////////////////////////////////////////////////////////
#
##  FHIR API query  ----
#
# //////////////////////////////////////////////////////////////////////////////


if (developer_mode == FALSE) {
  
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
  api_mapping_table <- map_dfr(distinct_org_codes, run_api_calls) |> 
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
}


# //////////////////////////////////////////////////////////////////////////////
#
##  Geojson files from open geography portal  ----
#
# //////////////////////////////////////////////////////////////////////////////

message("retrieving ICB boundaries from geoportal")
nhs_icb <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFE/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# //////////////////////////////////////////////////////////////////////////////
#
#  Upload copies to datalake  ----
#
# //////////////////////////////////////////////////////////////////////////////
if (developer_mode == FALSE) {
  # Uploading UDAL files to datalake for quicker access during development
  datalake_upload(ods_provider_hierarchies, stored_files_folder)
  datalake_upload(ods_sites, stored_files_folder)
  datalake_upload(successor_orgs, stored_files_folder)
  datalake_upload(admissions, stored_files_folder)
  datalake_upload(seft_udal, stored_files_folder)
  datalake_upload(udal_process_time, stored_files_folder)
  datalake_upload(api_mapping_table, stored_files_folder)
}

# //////////////////////////////////////////////////////////////////////////////
#
#  Ingest from datalake  ----
#
# //////////////////////////////////////////////////////////////////////////////
if (developer_mode == TRUE){
  ods_provider_hierarchies <- datalake_download("ods_provider_hierarchies", 
                                                stored_files_folder)
  ods_sites <- datalake_download("ods_sites", stored_files_folder)
  successor_orgs <- datalake_download("successor_orgs", stored_files_folder)
  admissions <- datalake_download("admissions", stored_files_folder)
  seft_udal <- datalake_download("seft_udal", stored_files_folder)
  udal_process_time <- datalake_download("udal_process_time",
                                         stored_files_folder)
  api_mapping_table <- datalake_download("api_mapping_table",
                                         stored_files_folder)
}