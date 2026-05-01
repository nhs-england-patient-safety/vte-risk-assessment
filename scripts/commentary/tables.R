library(scales)

# //////////////////////////////////////////////////////////////////////////////
#
## % risk assessed in England ----
#
# //////////////////////////////////////////////////////////////////////////////

# Grouping by org type, month and calculating percentage
table1a <- df_joined |>
  filter(period == time_period) |>
  group_by(org_type, month) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n, month = as.character(month)) |>
  select(org_type, month, percentage)

# Grouping by org type for all time and calculating percentage
table1b <- df_joined |>
  filter(period == time_period) |>
  group_by(org_type) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n, month = time_period_readable) |>
  select(org_type, month, percentage)

# Grouping by month for all orgs and calculating percentage
table1c <- df_joined |>
  filter(period == time_period) |>
  group_by(month) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(
    percentage = x / n, month = as.character(month),
    org_type = "All providers of NHS-funded acute care"
  ) |>
  select(org_type, month, percentage)

# Calculating percentage for all orgs and all months
table1d <- df_joined |>
  filter(period == time_period) |>
  group_by() |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(
    percentage = x / n, month = time_period_readable,
    org_type = "All providers of NHS-funded acute care"
  ) |>
  select(org_type, month, percentage)

# Bringing all the table together and formatting percentage
table1e <- bind_rows(table1a, table1b, table1c, table1d)
table1e$percentage <- percent(table1e$percentage, accuracy = 0.1)

# Pivoting data to match publication and formatting
table1 <- table1e |>
  mutate(month = case_when(
    month == "1" ~ month_1_readable,
    month == "2" ~ month_2_readable,
    month == "3" ~ month_3_readable,
    .default =  month
  )) |>
  pivot_wider(
    names_from = month,
    values_from = percentage
  ) |>
  mutate(org_type = case_when(
    org_type == "NHS acute care provider" ~ "NHS acute care providers",
    org_type == "Independent provider" ~ "Independent sector providers",
    .default =  org_type
  )) |>
  rename(" " = org_type)


table_percentage_england_month <- table1 |>
  ungroup() |>
  slice(3) |>
  select(!5)


table_percentage_org_type_month <- table1 |>
  ungroup() |>
  rename(label = 1) |>
  slice(1, 2) |>
  select(!5) |>
  arrange(desc(label)) |>
  rename(" " = 1)


# //////////////////////////////////////////////////////////////////////////////
#
##  % risk assessed by region ----
#
# //////////////////////////////////////////////////////////////////////////////

# Built table by combining two tables, split by org_type and by all providers
# First table - Grouping by region and org type and calculating percentage
table2a <- df_joined |>
  filter(period == time_period) |>
  group_by(parent_name, org_type) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n) |>
  select(parent_name, org_type, percentage)

# Second table - Grouping by region only and calculating percentage
table2b <- df_joined |>
  filter(period == time_period) |>
  group_by(parent_name) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n, org_type = "All providers") |>
  arrange(parent_name) |>
  select(parent_name, org_type, percentage)

# Bringing both tables together
table2c <- bind_rows(table2a, table2b)
table2c$percentage <- percent(table2c$percentage, accuracy = 0.1)

# Pivoting data to match publication and formatting
table2 <- table2c |>
  pivot_wider(
    names_from = org_type,
    values_from = percentage
  ) |>
  mutate(parent_name = case_when(
    parent_name == "EAST OF ENGLAND COMMISSIONING REGION" ~ "East of England",
    parent_name == "LONDON COMMISSIONING REGION" ~ "London",
    parent_name == "MIDLANDS COMMISSIONING REGION" ~ "Midlands",
    parent_name == "NORTH EAST AND YORKSHIRE COMMISSIONING REGION" ~
      "North East and Yorkshire",
    parent_name == "NORTH WEST COMMISSIONING REGION" ~ "North West",
    parent_name == "SOUTH EAST COMMISSIONING REGION" ~ "South East",
    parent_name == "SOUTH WEST COMMISSIONING REGION" ~ "South West",
    parent_name == "NORTH OF ENGLAND COMMISSIONING REGION" ~ "North of England",
    parent_name == "MIDLANDS AND EAST OF ENGLAND COMMISSIONING REGION" ~
      "Midlands and East of England",
    .default = parent_name
  )) |>
  rename(
    "NHS region" = parent_name,
    "NHS acute care providers" = "NHS acute care provider",
    "Independent sector providers" = "Independent provider"
  ) |>
  select(
    "NHS region", "All providers",
    "NHS acute care providers",
    "Independent sector providers"
  )


table_region_england <- table2 |>
  select("NHS region", "All providers")

table_region_org <- table2 |>
  select(!"All providers")

# //////////////////////////////////////////////////////////////////////////////
#
##  Top 5 and bottom 5 performing acute trusts ----
#
# //////////////////////////////////////////////////////////////////////////////

acute_providers_table <- df_joined |>
  filter(period == time_period, org_type == "NHS acute care provider") |>
  group_by(org_name, org_code) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n) |>
  ungroup() |>
  mutate("Provider" = gsub(
    "And",
    "and",
    gsub(
      "Nhs",
      "NHS",
      str_to_title(org_name, locale = "en")
    )
  )) |>
  arrange("Percentage risk assessed for VTE") |>
  rename(Code = org_code) |>
  select(Code, Provider, x, n, percentage)

bottom_5_acute <- acute_providers_table |>
  arrange(percentage) |>
  slice(1:5) |>
  mutate(x = number(x, big.mark = ","), n = number(n, big.mark = ",")) |>
  rename(
    "Risk assessed for VTE" = x,
    "Total admissions" = n,
    "Percentage risk assessed for VTE" = percentage
  )

bottom_5_acute$"Percentage risk assessed for VTE" <-
  percent(bottom_5_acute$"Percentage risk assessed for VTE", accuracy = 0.1)

top_5_acute <- acute_providers_table |>
  arrange(desc(percentage)) |>
  slice(1:5) |>
  mutate(x = number(x, big.mark = ","), n = number(n, big.mark = ",")) |>
  rename(
    "Risk assessed for VTE" = x,
    "Total admissions" = n,
    "Percentage risk assessed for VTE" = percentage
  )

top_5_acute$"Percentage risk assessed for VTE" <-
  percent(top_5_acute$"Percentage risk assessed for VTE", accuracy = 0.1)
