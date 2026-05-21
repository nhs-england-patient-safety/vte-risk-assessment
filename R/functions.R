library(Microsoft365R)
library(here)
library(httr2)
library(AzureStor)
library(AzureAuth)
library(AzureKeyVault)
library(httpuv)

# figure saving
save_figure <- function(n, p, w, h) {
  ggsave(
    filename = paste0(time_period_fig, "_fig_", n, "_", substitute(p), ".png"),
    path = here("output",time_period_folder, "figures"),
    plot = p,
    width = w,
    height = h,
    units = "px",
    dpi = 192,
    bg = "white",
    create.dir = TRUE
  )
}

# bullet points
bullet_points <- function(dq_type) {
 # this function filters the data quality notes to pull text needed in commentary 
  data_quality_input |>
    filter(!is.na(text)) |>
    mutate(text = paste0("-   ", text)) |>
    filter(period == time_period, type == dq_type) |>
    pull(text) |>
    paste0(collapse = "\n")
}

# data lake token
# Authenticating and accessing data lake using MFA (repeated as fails first time)
# clear cache
# AzureAuth::clean_token_directory()
# AzureGraph::delete_graph_login(tenant="eae2146b-01ed-4b70-8a27-caa5804ab9ca")
token <- tryCatch(
  {
    token <- get_azure_token(
      resource = "https://storage.azure.com/",
      tenant = "eae2146b-01ed-4b70-8a27-caa5804ab9ca", 
      app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46" 
    ) 
  }, error = function(e) {
    message("Trying to authenticate access to data lake again")
    token <- get_azure_token(
      resource = "https://storage.azure.com/",
      tenant = "eae2146b-01ed-4b70-8a27-caa5804ab9ca",
      app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
    )}
)

# get latest file from datalake (that is dated before end of selected quarter)
datalake_latest_file <- function(folder) {
  latest_file <- list_storage_files(cont, folder, recursive = FALSE, info = "all") |> 
    mutate(
      date_str = str_extract(name, "\\d{4}_\\d{2}_\\d{2}"),
      file_date = ymd(str_replace_all(date_str, "_", "-"))
    ) |>
    filter(!is.na(file_date)) |>
    arrange(desc(file_date)) |>
    # filter only for files created before end of selected quarter
    filter(file_date <= last_day_of_quarter) |> 
    slice(1) |> 
    pull(name)
  
  table <- read_csv(storage_download(cont, latest_file, dest = NULL)) |> 
    clean_names()
  return(table)
}

# download specified file from datalake
datalake_download <- function(file_name, folder) {
  url_name <- paste0(folder, "/",
                     as.character(file_name),
                     ".csv")
  
  table <- read_csv(storage_download(cont, url_name, dest = NULL))
  return(table)
}

# upload to data lake
datalake_upload <- function(df, folder, type = c("date", "object")) {
  type <- match.arg(type)
  
  # file name
  name <- switch(
    type,
    date = gsub("-", "_", as.character(Sys.Date())),
    object = as.character(substitute(df))
  )
  url_name <- paste0(folder, "/", name, ".csv")
  
  # upload 
  r_con <- rawConnection(raw(), "wb")
  write_csv(df, r_con)
  raw_data <- rawConnectionValue(r_con)
  storage_upload(
    cont,
    src = rawConnection(raw_data, "rb"),
    dest = url_name
  )
  close(r_con)
}

# outlook variable for draft emails
outlook <- get_business_outlook(tenant = "nhs")

# sharepoint variables
site_url <- Sys.getenv("sharepoint_url")
site <- get_sharepoint_site(site_url = site_url, tenant = "nhs")
reslib <- site$get_drive("Restricted Library")

# uploading files
upload_file <- function(file) {
  vte_outputs <- reslib$get_item("VTE/Outputs/")
  try(vte_outputs$create_folder(time_period_folder), silent = TRUE)
  vte_outputs_q <- reslib$get_item(paste0("VTE/Outputs/", time_period_folder))
  vte_outputs_q$upload(
    src = here("output",time_period_folder, file),
    dest = file
  )
}

# uploading reference files used to support creation of report
upload_ref_file <- function(file) {
  vte_ref <- reslib$get_item(paste0("VTE/Outputs/",time_period_folder))
  try(vte_ref$create_folder("reference"), silent = TRUE)
  vte_ref_u <- reslib$get_item(paste0("VTE/Outputs/", time_period_folder,"/reference"))
  vte_ref_u$upload(
    src = here("output",time_period_folder,"reference", file),
    dest = file
  )
}

# uploading figures which are used to insert into wordpress when publishing
upload_figures <- function(figure) {
  vte_figures<- reslib$get_item("VTE/Outputs/")
  try(vte_figures$create_folder(time_period_folder), silent = TRUE)
  vte_figures_u <- reslib$get_item(paste0("VTE/Outputs/", time_period_folder))
  vte_figures_u$upload(
    src  = here("output", time_period_folder, figure),
    dest = figure
  )
}

# Symmetrical Percentage Change
s_percent_change <- function(new, old){
  round(((2 * (new - old)) / (new + old)) * 100, 0)
}

# api query
# get api key
api_key <- Sys.getenv("ods_key")

# pull value from api output df
pull_value <- function(df, key_name) {
  val <- df |> filter(key == key_name) |> pull(value)
  if (length(val) == 0) return(NA)  
  val[1]  
}

# api call function to retrieve name from a provided org_code
get_ods_org_name <- function(org_code) {
    request <- request(paste0("https://api.service.nhs.uk/organisation-data-terminology-api/fhir/Organization/", 
                 org_code,"?&_elements=name")) |>
    req_headers(apikey = api_key) |>
    req_perform() |> 
    resp_body_json()
    
    api_org_name <- pluck(request,"name",.default = NA)
    api_org_name <- tibble(api_org_name)
    
    return(api_org_name)
}


# api call function to retrieve mapping codes from a provided org_code
get_ods_mapping_codes <- function(lookup,org_code,properties) {
  
  # lookup <- "CodeSystem/$lookup?system=https://fhir.nhs.uk/Id/ods-organization-code&code="
  # org_code <- "RW6"
  # properties <- "&property=ICB&property=NHSER&property=RE6&property=primaryRole&property=successor&property=legEndDate&property=opEndDate&property=status"

  request <- request(paste0("https://api.service.nhs.uk/organisation-data-terminology-api/fhir/",
                 lookup,org_code,properties)) |>
    req_headers(apikey = api_key) |>
    req_perform() |> 
    resp_body_json()
  
    # pulls out the key and value for each property/non property
    json_to_objects <- request$parameter |>
    map_df(function(p) {
      if (p$name != "property") {
        tibble(
          key = p$name,
          value = coalesce(p$valueCode, p$valueString)
        )
      } else {
        tibble(
          key = p$part[[1]]$valueCode,
          value = coalesce(
            p$part[[2]]$valueCode,
            p$part[[2]]$valueDateTime,
            p$part[[2]]$valueString,
            p$part[[2]]$valueCoding$code
          )
        )
      }
    })
  
  # convert to wide format for easier manipulation
    json_to_object_wide <- json_to_objects %>%
      pivot_wider(
        names_from = key,
        values_from = value,
        values_fn = list
      ) %>%
      select(-name) %>%
      {
        # if successor exists & if multiple then split into comma separated
        if ("successor" %in% names(.)) {
          mutate(., successor = map_chr(successor, ~ paste(.x, collapse = ", ")))
        } else {
          .
        }
      } %>%
      # convert all columns to characters
      mutate(across(everything(), as.character))

  return(json_to_object_wide)
}

# api call for lookup tables
api_call_lookup <- function(url) {
  request(url) |>
  req_headers(apikey = api_key) |>
  req_perform() |> 
  resp_body_json()
}

# safe api calls
safe_api_call <- function(fun, ..., default = NA) {
  tryCatch(
    fun(...),
    error = function(e) default,
    warning = function(w) default
  )
}

# retrieving lookup tables for primary role, ICB and region to reduce api calls
# api calls
api_call_all_roles <- api_call_lookup("https://api.service.nhs.uk/organisation-data-terminology-api/fhir/ValueSet/$expand?url=https://digital.nhs.uk/services/organisation-data-service/CodeSystem/ODSOrganisationRole/vs&property=primaryRole")
api_call_all_icbs <- api_call_lookup("https://api.service.nhs.uk/organisation-data-terminology-api/fhir/Organization?activeRoleCode=RO318&_elements=id,name&_count=100")
api_call_all_regions <- api_call_lookup("https://api.service.nhs.uk/organisation-data-terminology-api/fhir/Organization?activeRoleCode=RO209&_elements=id,name&_count=100")

# lookup tables
role_lookup <- api_call_all_roles$expansion$contains |>
  map_df(~data.frame(
    api_role_code = .x$code,
    api_role_name = .x$display,
    stringsAsFactors = FALSE
  )) |> 
  mutate(org_type = case_when(
      api_role_name == "INDEPENDENT SECTOR H/C PROVIDER SITE" ~ 
        "Independent provider",
      api_role_name == "INDEPENDENT SECTOR HEALTHCARE PROVIDER" ~ 
        "Independent provider",
      api_role_name == "NHS TRUST" ~ "NHS acute care provider",
      .default = NA
    )
  )
icb_lookup <- api_call_all_icbs$entry |>
  map_df(~data.frame(
    api_icb_code = .x$resource$id,
    api_icb_name = .x$resource$name,
    stringsAsFactors = FALSE
  ))
region_lookup <- api_call_all_regions$entry |>
  map_df(~data.frame(
    api_region_code = .x$resource$id,
    api_region_name = .x$resource$name,
    stringsAsFactors = FALSE
  )) |> 
  mutate(
    api_region_name = gsub(" COMMISSIONING REGION", "", api_region_name),
  )

# api call function
run_api_calls <- function(org_code) {
  # api call to get all mapping codes
  api_ods_mapping_codes <- safe_api_call(get_ods_mapping_codes,"CodeSystem/$lookup?system=https://fhir.nhs.uk/Id/ods-organization-code&code=",org_code,"&property=ICB&property=NHSER&property=RE6&property=primaryRole&property=successor&property=legEndDate&property=opEndDate&property=status")
  # api call to get operated by name (only run if RE6 exists)
  api_operated_by_name <- if ("RE6" %in% names(api_ods_mapping_codes)) {
    safe_api_call(
      get_ods_org_name,
      api_ods_mapping_codes$RE6
    ) |>
      rename(api_operated_by_name = api_org_name)
  } else {
    tibble(api_operated_by_name = NA)
  }
  # api call to get successor name(s)  (only run if successor exists)
  api_successor_name <- if ("successor" %in% names(api_ods_mapping_codes)) {
    successor_codes <- api_ods_mapping_codes |> 
      pull(successor) |> 
      str_split(",\\s*") |> 
      unlist() |> 
      str_trim()
    # take multiple successor codes and return comma separated names
    test <- map_dfr(successor_codes, get_ods_org_name) |> 
      summarize(api_successor_name = str_c(api_org_name, collapse = ", "))
  } else {
    tibble(api_successor_name = NA)
  }

  message(paste0("retrieving ODS information for ",org_code))
  
  # output table
  output_table <- bind_cols(api_ods_mapping_codes,
                            api_operated_by_name,
                            api_successor_name)
  
  return(output_table)

}
