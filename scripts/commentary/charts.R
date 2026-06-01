library(FunnelPlotR)
library(ggrepel)


# //////////////////////////////////////////////////////////////////////////////
#
## VTE % by month all providers  ----
#
# //////////////////////////////////////////////////////////////////////////////

table_month_england <- table1 |>
  pivot_longer(
    cols = 2:5,
    names_to = "date",
    values_to = "values"
  ) |>
  rename("org_type" = 1) |>
  mutate(date = factor(date, date)) |>
  filter(org_type == "All providers of NHS-funded acute care") |>
  filter(date != time_period_readable)

table_month_england$values <- 
  as.numeric(sub("%", "", table_month_england$values))

vte_by_month_all_providers <- 
  ggplot(table_month_england, 
         aes(fill = org_type, x = date, y = values / 100, group = 1)) +
  # all providers blue line
  geom_point(size = 3, na.rm = TRUE, show.legend = FALSE) + 
  geom_line(aes(linetype = "All providers of NHS-funded acute care"), colour = "#12436D", linewidth = 1.5) +
  geom_text(aes(label = paste0(round(values, digits = 1), "%")), 
            color = "black", vjust = -1.2) +
  # operational standard line
  geom_hline(
    aes(
      yintercept = 0.95,
      linetype = "95% operational standard"
    ),
    colour = "#F46A25",
    linewidth = 1.2
  ) +
  # manual legend
  scale_linetype_manual(
    name = NULL,
    values = c(
      "All providers of NHS-funded acute care" = "solid",
      "95% operational standard" = "dashed"
    )
  ) +
  # reverse the legend order
  guides(linetype = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0.8, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank()
  )

save_figure(1, vte_by_month_all_providers, 1574, 960)


# //////////////////////////////////////////////////////////////////////////////
#
##  VTE % by month by org type   ----
#
# //////////////////////////////////////////////////////////////////////////////

table_month_org_type <- table1 |>
  pivot_longer(
    cols = 2:5,
    names_to = "date",
    values_to = "values"
  ) |>
  rename("org_type" = 1) |>
  filter(org_type != "All providers of NHS-funded acute care") |>
  filter(date != time_period_readable) |>
  mutate(date = factor(date, date)) |>
  ungroup()


table_month_org_type$values <- 
  as.numeric(sub("%", "", table_month_org_type$values))

vte_by_month_by_org_type <- 
  ggplot(table_month_org_type, aes(x = date, y = values / 100, 
                                   colour = org_type, group = org_type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#28A197", "#801650")) +
  geom_text_repel(aes(label = paste0(round(values, digits = 1), "%")), 
                  color = "black") +
  # operational standard line
  geom_hline(
    aes(
      yintercept = 0.95,
      linetype = "95% operational standard"
    ),
    colour = "#F46A25",
    linewidth = 1.2
  ) +
  # manual legend
  scale_linetype_manual(
    name = NULL,
    values = c(
      "95% operational standard" = "dashed"
    )
  ) +
  # reverse the legend order
  guides(
    colour = guide_legend(order = 1, nrow = 2, reverse = TRUE),
    linetype = guide_legend(order = 2, nrow = 2, byrow = TRUE)
  ) +
  scale_y_continuous(limits = c(0.8, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank()
  ) 

save_figure(3, vte_by_month_by_org_type, 1574, 960)


# //////////////////////////////////////////////////////////////////////////////
#
##  Historical VTE % by month  ----
#
# //////////////////////////////////////////////////////////////////////////////

# calculating by month
historical_month <- df_joined |>
  filter(date >= start_period_quarter & date <= end_period_quarter) |>
  group_by(date, period_readable) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions),
    date = min(date)
  ) |>
  mutate(percentage = 100 * x / n) |>
  arrange(date) |>
  select(date, period_readable, percentage)

# creating tibble of no data during pause
month_gap <- tibble(
  date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
  period_readable = c("No\ndata"),
  percentage = c(NA)
)

# getting the middle month percentage in each quarter for the labelling
percentage_chart_labels <- historical_month |>
  group_by(period_readable) |>
  summarise(date = median(date)) |>
  arrange(date) |>
  left_join(historical_month, join_by(date == date)) |>
  mutate(percentage_label = paste0(round(percentage, digits = 1), "%")) |>
  select(date, percentage_label)

# combining month data, paused data and labels
historical_month_chart <- bind_rows(historical_month, month_gap) |>
  mutate(date_chr = as.character(date)) |>
  arrange(date) |>
  left_join(percentage_chart_labels, join_by(date == date))

# create breaks and labels. Replace months with our period_readable names
historical_chart_labels <- historical_month_chart |>
  group_by(period_readable) |>
  summarise(date = median(date)) |>
  arrange(date) |>
  mutate(date = as.character(date)) |>
  mutate(
    period_readable = case_when(
      period_readable == "No data" ~ "No data",
      date == "2017-11-01" ~ "Q3\n17/18", 
      str_detect(period_readable, "Q2") ~ "Q2",
      str_detect(period_readable, "Q3") ~ "Q3",
      str_detect(period_readable, "Q4") ~ "Q4",
      TRUE ~ period_readable
    )
  )

# plotting chart
historical_vte_by_month <- 
  ggplot(historical_month_chart, 
         aes(x = date_chr, y = percentage / 100, group = 1)) +
  # all providers blue line
  geom_point(size = 3, na.rm = TRUE) + 
  geom_line(aes(linetype = "All providers of NHS-funded acute care"), colour = "#12436D", linewidth = 1.5) +
  # operational standard line
  geom_hline(
    aes(
      yintercept = 0.95,
      linetype = "95% operational standard"
    ),
    colour = "#F46A25",
    linewidth = 1.2
  ) +
  # manual legend
  scale_linetype_manual(
    name = NULL,
    values = c(
      "All providers of NHS-funded acute care" = "solid",
      "95% operational standard" = "dashed"
    )
  ) +
  # reverse the legend order
  guides(linetype = guide_legend(reverse = TRUE)) +
  # y-axis as percent
  scale_y_continuous(
    limits = c(0.8, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  # x-axis labels replaces all the month labels with our period_readable names
  scale_x_discrete(
    breaks = historical_chart_labels$date,
    labels = historical_chart_labels$period_readable,
    expand = expansion(add = c(0.2, 3))
  ) +
  labs(x = "", y = "") +
  # theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14, hjust=0),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank()
  ) 

save_figure(2, historical_vte_by_month, 1574, 960)


# //////////////////////////////////////////////////////////////////////////////
#
## VTE % by region ----
#
# //////////////////////////////////////////////////////////////////////////////

chart_region_all <- table2 |>
  pivot_longer(
    cols = 2:4,
    names_to = "org_type",
    values_to = "values"
  ) |>
  clean_names() |>
  mutate(nhs_region = str_wrap(nhs_region, width = 14)) |>
  filter(org_type == "All providers")

chart_region_all$values <- as.numeric(sub("%", "", chart_region_all$values))

vte_by_region <- 
  ggplot(chart_region_all, aes(fill = org_type, x = values, 
                               y = reorder(nhs_region, desc(nhs_region)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#12436D")) +
  geom_text(aes(label = paste0(round(values, digits = 1), "%")), 
            position = position_dodge2(width = 0.9), color = "white", 
            hjust = 1.2, vjust = 0.3, size = 4) +
  geom_vline(
    aes(
      xintercept = 95,
      linetype = "95% operational standard"
    ),
    colour = "#F46A25",
    linewidth = 1.2) +
  scale_linetype_manual(values = c("95% operational standard" = "dashed"), 
                        name = NULL) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)) +
    labs(x = "", y = "") +
  scale_x_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    legend.key.height = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank()
  ) 

save_figure(4, vte_by_region, 1574, 960)


# //////////////////////////////////////////////////////////////////////////////
#
## VTE % by region and org type ----
#
# //////////////////////////////////////////////////////////////////////////////

chart_region_org_type <- table2 |>
  pivot_longer(
    cols = 2:4,
    names_to = "org_type",
    values_to = "values"
  ) |>
  clean_names() |>
  mutate(nhs_region = str_wrap(nhs_region, width = 14)) |>
  filter(org_type != "All providers")

chart_region_org_type$values <- 
  as.numeric(sub("%", "", chart_region_org_type$values))

vte_by_region_by_org_type <- 
  ggplot(chart_region_org_type, aes(fill = org_type, x = values, 
                                    y = reorder(nhs_region, 
                                                desc(nhs_region)))) +
  geom_bar(position = position_dodge2(), stat = "identity") +
  scale_fill_manual(values = c("#28A197", "#801650")) +
  geom_text(aes(label = paste0(round(values, digits = 1), "%")), 
            position = position_dodge2(width = 0.9), 
            color = "white", hjust = 1.2, vjust = 0.3, size = 4) +
  geom_vline(
    aes(
      xintercept = 95,
      linetype = "95% operational standard"
    ),
    colour = "#F46A25",
    linewidth = 1.2) +
  scale_linetype_manual(values = c("95% operational standard" = "dashed"), 
                        name = NULL) +
  guides(
    fill = guide_legend(order = 1, nrow = 2, reverse = TRUE),
    linetype = guide_legend(order = 2)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    legend.key.height = unit(0.9, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 100), 
                     labels = scales::percent_format(scale = 1))

save_figure(5, vte_by_region_by_org_type, 1574, 1344)



# //////////////////////////////////////////////////////////////////////////////
#
## Funnel plot - Admissions against percentage ----
#
# //////////////////////////////////////////////////////////////////////////////

admissions_funnel <- df_joined |>
  filter(period == time_period) |>
  group_by(org_code, org_type) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(org_type = case_when(
    org_type == "NHS acute care provider" ~ "NHS acute care providers",
    org_type == "Independent provider" ~ "Independent sector providers",
    .default = org_type
  )) |>
  mutate(p = 100 * x / n)


# dispersion test, if >1 then dispersed
mod <- glm(n ~ x, family = "poisson", data = admissions_funnel)
summary(mod)
sum(mod$weights * mod$residuals^2) / mod$df.residual


fp <- funnel_plot(admissions_funnel,
                  numerator = x, denominator = n, group = org_code, title = "", 
                  data_type = "PR", limit = 99,
                  draw_unadjusted = FALSE, draw_adjusted = TRUE, 
                  sr_method = "SHMI", y_range = c(0, 1), 
                  label = "outlier_lower", max.overlaps = 100
)

funnel_data <- source_data(fp) |>
  left_join(mapping, join_by(group == organisation_code)) |>
  mutate(org_type = case_when(
    org_type == "NHS acute care provider" ~ "NHS acute care",
    org_type == "Independent provider" ~ "Independent sector",
    .default = org_type
  )) |>
  # selecting outliers that are below OD99LCL
  mutate(outlier = case_when(
    rr < OD99LCL ~ 1,
    .default = 0
  ))


funnel_table_preparation <- df_joined |>
  filter(period == time_period) |>
  group_by(org_name, org_code) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(percentage = x / n) |>
  ungroup() |>
  left_join(mapping_table, join_by(org_code == api_org_code)) |>
  mutate(
    "Provider" = gsub("And","and",
                      gsub("Nhs","NHS",
                           str_to_title(org_name, locale = "en"))),
    api_operated_by_name = api_operated_by_name |>
      str_to_title() |>
      str_replace("Uk", "UK") |>
      replace_na("-"),
    x = number(x, big.mark = ","), 
    n = number(n, big.mark = ",")) |> 
  select(org_code, Provider, api_operated_by_name, x, n, percentage) 


funnel_outliers <- funnel_data |> 
  filter(outlier == 1) |> 
  select(group) |> 
  left_join(funnel_table_preparation, join_by(group == org_code)) |>
  arrange(percentage) |>
  mutate(percentage = scales::percent(percentage, accuracy = 0.1)) |>
  rename(Code = group,
         "Risk assessed for VTE" = x,
         "Total admissions" = n,
         "Percentage risk assessed for VTE" = percentage,
         "Operated by" = api_operated_by_name
  ) 


admissions_percentage_funnel <- 
  ggplot(funnel_data, aes(y = ((numerator / denominator)), 
                          x = denominator)) +
  geom_line(aes(x = denominator, y = OD95LCL, 
                col = "95% Overdispersed", linetype = "95% Overdispersed"), 
            linewidth = 1, na.rm = TRUE) +
  geom_line(aes(x = denominator, y = OD99LCL, 
                col = "99.8% Overdispersed", linetype = "99.8% Overdispersed"), 
            linewidth = 1, na.rm = TRUE) +
  geom_line(aes(x = denominator, y = OD95UCL, 
                col = "95% Overdispersed", linetype = "95% Overdispersed"), 
            linewidth = 1, na.rm = TRUE) +
  geom_line(aes(x = denominator, y =  OD99UCL, 
                col = "99.8% Overdispersed", linetype = "99.8% Overdispersed"), 
            linewidth = 1, na.rm = TRUE) +
  geom_point(aes(colour = org_type)) +
  geom_segment(aes(x = 0, xend = max(denominator), y = 0.95, yend = 0.95, 
                   col = "Operational standard", 
                   linetype = "Operational standard"), linewidth = 1) +
  scale_color_manual(
    values = c(
      "99.8% Overdispersed" = "#A285D1",
      "95% Overdispersed" = "#3D3D3D",
      "Independent sector" = "#28A197",
      "NHS acute care" = "#801650",
      "Operational standard" = "#F46A25"
    ),
    limits = c(
      "NHS acute care",
      "Independent sector",
      "Operational standard",
      "95% Overdispersed",
      "99.8% Overdispersed"
    )
  ) +
  scale_linetype_manual(values = c("95% Overdispersed" = 2, 
                                   "99.8% Overdispersed" = 1, 
                                   "Operational standard" = 2), 
                        guide = "none") +
  labs(x = "Total admissions in quarter", 
       y = NULL) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(limits = c(0,1),
                     labels = scales::percent) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      nrow = 2,                           
      byrow = TRUE, 
      override.aes = list(size = 6, linetype = c(1, 1, 2, 2, 1))
    )
  )  +
  theme(
    axis.title = element_text(size = 14),
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 14),
    legend.text = element_text(color = "black", size = 14),
    axis.ticks = element_line(color = "grey"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.key.spacing.x = unit(0.3, "cm"),
    legend.title = element_blank()
  ) +
  geom_label_repel(
    aes(label = ifelse(outlier == 1,
                       as.character(group), NA
    )),
    size = 2.5, point.padding = 0, direction = "both", force = 2,
    min.segment.length = 0, na.rm = TRUE, max.overlaps = 100
  )

save_figure(7, admissions_percentage_funnel, 1574, 1152)


# //////////////////////////////////////////////////////////////////////////////
#
#  Maps ----
#
# //////////////////////////////////////////////////////////////////////////////


# calculating percentage by ICB
icb_percentage <- geospatial_mapping |>
  filter(period == time_period) |>
  group_by(icb_name) |>
  summarise(
    x = sum(number_of_vte_assessed_admissions),
    n = sum(total_admissions)
  ) |>
  mutate(p = 100 * x / n) |>
  mutate(bin = case_when(
    p >= 95 ~ "Yes",
    .default = "No"
  )) |>
  mutate(met = case_when(
    p >= 95 ~ "*",
    .default = ""
  )) |>
  select(icb_name, p, bin, met)


icb_figures <- icb_percentage |>
  select(icb_name, p, bin) |>
  group_by(bin) |>
  summarise(Number = n()) |>
  mutate("%" = round(100 * Number / sum(Number), digits = 1)) |>
  arrange(bin) |>
  adorn_totals()

icb_met <- icb_figures |>
  filter(bin == "Yes") |>
  pull("Number") |>
  round(digits = 0) |>
  as.character()
icb_not_met <- icb_figures |>
  filter(bin == "No") |>
  pull("Number") |>
  round(digits = 0) |>
  as.character()
icb_total <- icb_figures |>
  filter(bin == "Total") |>
  pull("Number") |>
  round(digits = 0) |>
  as.character()
icb_met_percent <- icb_figures |>
  filter(bin == "Yes") |>
  pull("%") |>
  round(digits = 0) |>
  as.character()
icb_not_met_percent <- icb_figures |>
  filter(bin == "No") |>
  pull("%") |>
  round(digits = 0) |>
  as.character()


icb_table <- icb_percentage |>
  arrange(desc(p)) |>
  mutate("Percentage" = paste0(round(p, digits = 1), "%")) |>
  mutate("ICB name" = 
           gsub("Integrated Care Board", "ICB", 
                gsub("And", 
                     "and", 
                     gsub("Nhs", "NHS", 
                          str_to_title(icb_name, locale = "en"))))) |>
  rename("Operational standard met" = bin) |>
  select("ICB name", Percentage, "Operational standard met")

# joining to ICB geojson file
icb_map <- nhs_icb |>
  rename(ICBNM = ends_with("NM")) |>
  mutate(ICBNM = str_to_upper(ICBNM)) |>
  left_join(
    icb_percentage,
    join_by(ICBNM == icb_name)
  )


min_p <- min(60, floor(min(icb_percentage$p) / 10) * 10)
midpoint <- ((100 - min_p) / 2) + min_p

# ICB plot
icb_map_plot <- ggplot(icb_map) +
  geom_sf(aes(fill = p), colour = "white", show.legend = TRUE) +
  geom_sf_text(aes(label = met),
               size = 6,
               colour = "white",
               fun.geometry = sf::st_centroid
  ) +
  scale_fill_gradient2(labels = scales::percent_format(scale = 1), 
                       low = "#F46A25", mid = "#E8EDEE", high = "#12436D", 
                       midpoint = midpoint, limits = c(min_p, 100)) +
  theme_void() +
  labs(caption = "Source: Office for National Statistics licensed under the Open Government Licence v.3.0 \nContains OS data © Crown copyright and database right 2023") +
  theme(
    legend.text = element_text(color = "black", size = 14),
    legend.key.height = unit(0.7, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.position = "top",
    legend.location = "plot",
    legend.justification = "left",
    legend.key.spacing.x = unit(1, "cm"),
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10)
  )

save_figure(6, icb_map_plot, 1382, 1728)
