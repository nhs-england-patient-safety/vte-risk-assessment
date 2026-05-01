# 0 admissions bullet points automatically generated
zero_admissions_bullets <- zero_admissions_list |> 
  mutate(org_name = str_to_title(org_name),
         org_name = str_replace_all(org_name, "\\bNhs|Ccg|Hq|Icb|[0-9][0-9][a-z]\\b", str_to_upper),
         org_name = str_replace_all(org_name, "\\bAnd\\b|\\bOf\\b", str_to_lower)) |>
  filter(period == time_period) |> 
  mutate(text = paste0("-   ",org_name, " (", org_code,").")) |>
  pull(text) |>
  paste0(collapse = "\n")


# filter data_quality file for each heading in section 5
data_quality <- bullet_points("data_quality")
org_change_comments <- bullet_points("org_change")
non_acute_comments <- bullet_points("non_acute")
audit_comments <- bullet_points("audit")
org_preparing_comments <- bullet_points("org_preparing")
no_submission_comments <- bullet_points("no_submission")
# concat the automated bullets from above with manual ones in data_quality file
zero_admissions_comments <- paste0(bullet_points("zero_admissions"), "\n",
                                   zero_admissions_bullets)

# bring in revisions notes
revisions_notes <- revisions_input |>
  filter(!is.na(text)) |>
  mutate(text = paste0("-   ", text)) |>
  filter(period == time_period) |> 
  pull(text) |> 
  paste0(collapse = "\n")