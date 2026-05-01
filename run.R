library(quarto)
source("scripts/params.R")
source("R/functions.R")

# creating html
quarto_render("scripts/quarterly_report.qmd",
  output_file = time_period_html,
  metadata = list(
    format = list("html" = list("embed-resources" = TRUE))
  )
)

fs::file_move(here("scripts",time_period_html),
  here("output",time_period_folder,time_period_html)
)

# uploading html
upload_file(time_period_html)
