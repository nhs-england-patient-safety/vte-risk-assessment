library(tidyverse)

#  Readable time periods
time_period_readable <- paste0(substring(time_period, 5, 6), " ", 
                               substring(time_period, 8, 11), "/", 
                               substring(time_period, 13, 14))
time_period_csv <- paste0("VTE-Risk-Assessment-", 
                          (substring(time_period, 8, 14)), 
                          "-Quarter-", (substring(time_period, 6, 6)), ".csv")
time_period_xlsx <- paste0("VTE-Risk-Assessment-", 
                           (substring(time_period, 8, 14)), 
                           "-Quarter-", (substring(time_period, 6, 6)), 
                           "-validation.xlsx")
time_period_fig <- paste0(substring(time_period, 10, 11), "_", 
                          substring(time_period, 13, 14), "_q", 
                          substring(time_period, 6, 6))

time_period_title <- case_when(
  substring(time_period, 6, 6) == 
    1 ~ paste0("Quarter ", substring(time_period, 6, 6), " ", 
               substring(time_period, 8, 11), "/", 
               substring(time_period, 13, 14), 
               " (April to June ", substring(time_period, 8, 11), ")"),
  substring(time_period, 6, 6) == 
    2 ~ paste0("Quarter ", substring(time_period, 6, 6), " ", 
               substring(time_period, 8, 11), "/", 
               substring(time_period, 13, 14), " (July to September ", 
               substring(time_period, 8, 11), ")"),
  substring(time_period, 6, 6) == 
    3 ~ paste0("Quarter ", substring(time_period, 6, 6), " ", 
               substring(time_period, 8, 11), "/", 
               substring(time_period, 13, 14), " (October to December ", 
               substring(time_period, 8, 11), ")"),
  substring(time_period, 6, 6) == 
    4 ~ paste0("Quarter ", substring(time_period, 6, 6), " ", 
               substring(time_period, 8, 11), "/", 
               substring(time_period, 13, 14), " (January to March ", 
               substring(time_period, 8, 9), 
               substring(time_period, 13, 14), ")")
)

quarter <- as.numeric(substring(time_period, 6, 6))
quarter_end_month <- c(6, 9, 12, 3)[quarter]
time_period_paragraph <- time_period_title |> 
  str_replace("Quarter", str_to_lower) |> 
  str_replace("quarter [0-9]", str_glue("quarter {quarter} (Q{quarter})"))

year <- ifelse(quarter == 4, as.numeric(substring(time_period, 8, 11)) + 1,
               as.numeric(substring(time_period, 8, 11))
)
month_1_q <- ifelse(quarter == 4, 1, 3 * quarter + 1)
month_1_readable <- as.character(format(make_date(year, month_1_q, 1), "%B %Y"))
month_2_readable <- as.character(format(make_date(year, month_1_q, 1) + 
                                          months(1), "%B %Y"))
month_3_readable <- as.character(format(make_date(year, month_1_q, 1) + 
                                          months(2), "%B %Y"))
last_day_of_quarter <- make_date(year, quarter_end_month, 1) |> 
  ceiling_date("month") - days(1)