library(DBI)
library(odbc)

# sharepoint is in functions as required by Run.R script to upload

# data lake token is in functions to allow pop up to appear when running run.R
dl_endp <- 
  storage_endpoint("https://udalstdataanalysisprod.dfs.core.windows.net", 
                   token = token)
cont <- storage_container(dl_endp, "analytics-projects")
tac_folder <- "PatientSafety/PatientSafety/trust_accounts_consolidation_(TAC)"
ods_provider_folder <- "PatientSafety/PatientSafety/ods_provider_hierarchies"

# UDAL
if (use_udal == TRUE) {
  con_udal <- dbConnect(
    drv = odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    server = Sys.getenv("udal_server"),
    database = Sys.getenv("udal_database"),
    UID = Sys.getenv("udal_uid"),
    authentication = "ActiveDirectoryInteractive"
  )
}





