# //////////////////////////////////////////////////////////////////////////////
#
##  Key findings ----
#
# //////////////////////////////////////////////////////////////////////////////


# % of patients admitted to NHS acute providers received a VTE risk assessment
figure2a <- df_joined |>
  filter(period == time_period, org_type == "NHS acute care provider") |>
  group_by() |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n)

figure2a$percentage <- percent(figure2a$percentage, accuracy = 1)
percentage_acute <- figure2a |>
  pull(percentage) |>
  as.character()


# % of patients admitted to independent providers received a VTE risk assessment
figure3a <- df_joined |>
  filter(period == time_period, org_type == "Independent provider") |>
  group_by() |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n)

figure3a$percentage <- percent(figure3a$percentage, accuracy = 1)
percentage_independent <- figure3a |>
  pull(percentage) |>
  as.character()


# Calculating which regions met the 95% target
figure4a <- table2 |>
  clean_names() |>
  select(nhs_region, all_providers)

# Converting back from percentages to double
figure4a$all_providers <- parse_number(figure4a$all_providers)

## Used this to test the formula was working
# figure4a <- data.frame(nhs_region=c('East of England', 'London', 'Midlands',
# 'North East and Yorkshire', 'North West','South East','South West'),
#                       all_providers=c(0, 00, 00, 00,00,00,00))

# Last trust needs to be separated by an "and" instead of ","
# Split out into two strings, first X and last X

figure4_met <- figure4a |>
  filter(all_providers >= 95) |>
  select(nhs_region) |>
  ungroup()

# No. of trusts that met standard and then calculating 1 less
met_standard <- nrow(figure4_met)
met_standard_minus <- abs(met_standard - 1)

figure4_not_met <- figure4a |>
  filter(all_providers < 95) |>
  select(nhs_region) |>
  ungroup()

# Number of trusts that did NOT meet standard and then calculating 1 less
not_met_standard <- nrow(figure4_not_met)
not_met_standard_minus <- abs(not_met_standard - 1)

# Selecting n-1 met standard
figure4c <- do.call(paste, c(figure4_met |> slice(1:met_standard_minus),
                             sep = ",", collapse = ", "
))

# Selecting last met standard
figure4d <- do.call(paste, c(figure4_met |> slice(met_standard),
                             sep = ",", collapse = ", "
))

# Selecting if there is only 1 that met standard
figure4e <- do.call(paste, c(figure4_met |> slice(1),
                             sep = ",", collapse = ", "
))

# Selecting n-1 NOT met standard
figure4f <- do.call(paste, c(figure4_not_met |> slice(1:not_met_standard_minus),
                             sep = ",", collapse = ", "
))

# Selecting last NOT met standard
figure4g <- do.call(paste, c(figure4_not_met |> slice(not_met_standard),
                             sep = ",", collapse = ", "
))

# Selecting if there is only 1 that NOT met standard
figure4h <- do.call(paste, c(figure4_not_met |> slice(1),
                             sep = ",", collapse = ", "
))

# This cycles through different scenarios. It assumes there are only 7 regions
# (note pre 2019 it was 5 regions)
figure4i <- case_when(
  met_standard == 0 ~ paste0(
    "no regions achieved the 95% NHS Standard Contract 
                             operational standard in ",
    time_period_readable, "."
  ),
  met_standard == 1 ~ paste0(
    "1 region (", figure4e, ") achieved the 95% NHS 
                             Standard Contract operational standard in ",
    time_period_readable, ". 6 regions (", figure4f,
    " and ", figure4g,
    ") did not meet the operational standard."
  ),
  met_standard == 6 ~ paste0(
    "6 regions (", figure4c, " and ", figure4d,
    ") achieved the 95% NHS Standard Contract 
                             operational standard in ",
    time_period_readable, ". 1 region (", figure4h, ") 
                             did not meet the operational standard."
  ),
  met_standard == 7 ~ paste0(
    "all regions achieved the 95% NHS Standard Contract 
                             operational standard in ",
    time_period_readable, "."
  ),
  .default = paste0(
    met_standard, " regions (", figure4c, " and ", figure4d,
    ") achieved the 95% NHS Standard Contract operational 
                    standard in ", time_period_readable,
    ". ", not_met_standard, " regions (", figure4f, " and ",
    figure4g,
    ") did not meet the operational standard."
  )
)


# As this only works for 7 regions, case when to not use it pre 2019
regions_achieved <- case_when(as.double(substring(time_period, 8, 11)) >= 2019 ~
                                figure4i, .default = "")
regions_achieved_paragraph <- paste(toupper(substr(regions_achieved, 1, 1)),
                                    substr(
                                      regions_achieved,
                                      2, nchar(regions_achieved)
                                    ),
                                    sep = ""
)




# //////////////////////////////////////////////////////////////////////////////
#
##  % risk assessed for VTE ----
#
# //////////////////////////////////////////////////////////////////////////////


# Calculating total admissions, total VTE risk assessed and overall percentage
figure6a <- df_joined |>
  filter(period == time_period) |>
  group_by() |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n)

total_admitted <- as.character(round(figure6a |> pull("n") / 1000000, 
                                     digits = 1))
total_vte_assessed <- as.character(round(figure6a |> pull("x") / 1000000, 
                                         digits = 1))
total_percent <- as.character(round(figure6a |> pull("percentage") * 100, 
                                    digits = 0))


# Calculating percentage of VTE risk assessments by org type
figure9a <- df_joined |>
  filter(period == time_period) |>
  group_by(org_type) |>
  summarise(x = sum(number_of_vte_assessed_admissions)) |>
  mutate(y = x / (sum(x)))

proportion_acute <- 
  as.character(round(figure9a |> 
                       filter(org_type == "NHS acute care provider") |> 
                       pull("y") * 100, digits = 0))
proportion_independent <- 
  as.character(round(figure9a |> 
                       filter(org_type == "Independent provider") |> 
                       pull("y") * 100, digits = 0))


# Percentages for each region

region_percentages_func <- function(x) {
  as.character(round(
    100 * table2b |>
      filter(parent_name == x) |>
      pull("percentage"),
    digits = 0
  ))
}

region_e <- region_percentages_func("EAST OF ENGLAND COMMISSIONING REGION")
region_ldn <- region_percentages_func("LONDON COMMISSIONING REGION")
region_mid <- region_percentages_func("MIDLANDS COMMISSIONING REGION")
region_ne_y <- region_percentages_func("NORTH EAST AND YORKSHIRE COMMISSIONING REGION")
region_nw <- region_percentages_func("NORTH WEST COMMISSIONING REGION")
region_se <- region_percentages_func("SOUTH EAST COMMISSIONING REGION")
region_sw <- region_percentages_func("SOUTH WEST COMMISSIONING REGION")


# //////////////////////////////////////////////////////////////////////////////
#
## Number of data returns  ----
#
# //////////////////////////////////////////////////////////////////////////////


# Calculating all distinct trusts
distinct_trusts <- df_joined |>
  filter(period == time_period) |>
  summarize(distinct = n_distinct(org_code))
distinct_trusts_str <- as.character(distinct_trusts)

# Calculating acute distinct trusts
distinct_acute <- df_joined |>
  filter(period == time_period, org_type == "NHS acute care provider") |>
  summarize(distinct = n_distinct(org_code))
distinct_acute_str <- as.character(distinct_acute)

# Calculating independent distinct trusts
distinct_independent <- df_joined |>
  filter(period == time_period, org_type == "Independent provider") |>
  summarize(distinct = n_distinct(org_code))
distinct_independent_str <- as.character(distinct_independent)
