#get_acs_age_sex
# used by voting and vax.Rmd
# do ACS calls to get age distribution by town
#
# use next line to source
# source("~/Documents/R_local_repos/ctcorona/R/get_acs_age_sex.R")

require(tidyverse)
get_acs_age_sex <- function(acs_year = 2019, acs_vars = load_variables(acs_year, "acs5", cache = TRUE)) {
  sex_vars_full <- acs_vars |> filter(concept == "SEX BY AGE")
  sex_vars <- sex_vars_full$name
  # str_extract(sex_vars_full$label, "ale:!![0-9 A-Za-z]+") |> unique() |> str_replace("ale:!!","")
  age_groups <- tibble::tribble(
    ~census_age, ~age,
    "Total Female","Total Female",
    "Total Male",  "Total Male",
    "Total",       "Total",
    "Under 5 years",    "Under 18",
    "5 to 9 years",    "Under 18",
    "10 to 14 years",    "Under 18",
    "15 to 17 years",    "Under 18",
    "18 and 19 years",    "18-44",
    "20 years",    "18-44",
    "21 years",    "18-44",
    "22 to 24 years",    "18-44",
    "25 to 29 years",    "18-44",
    "30 to 34 years",    "18-44",
    "35 to 39 years",    "18-44",
    "40 to 44 years",    "18-44",
    "45 to 49 years",    "45-64",
    "50 to 54 years",    "45-64",
    "55 to 59 years",    "45-64",
    "60 and 61 years",    "45-64",
    "62 to 64 years",    "45-64",
    "65 and 66 years", "65+",
    "67 to 69 years", "65+",
    "70 to 74 years", "65+",
    "75 to 79 years", "65+",
    "80 to 84 years", "65+",
    "85 years and over", "65+"
  )
  sex_age_acs <- get_acs(geography = "county subdivision",  # for CT, that means towns
                         state = "CT",
                         geometry = "FALSE", # no map at this time
                         year = acs_year,
                         survey = "acs5",
                         variables = sex_vars,
                         summary_var = sex_vars[1]) %>%
    mutate(TOWN = str_extract(NAME, "[a-zA-Z ]+ town") |>
             str_replace(" town", ""),
           COUNTY = str_extract(NAME, "town, [a-zA-Z]+ County") |>
             str_replace(" County", "") |> str_replace("town, ", ""),
           sex_x_age_pct = estimate / summary_est,
           sex_x_age_pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) |>
    left_join(acs_vars |> select(name, label), by = c("variable" = "name"))
  sex_age_acs2 <- sex_age_acs |>
    mutate(census_age = case_when(
      label == "Estimate!!Total:" ~ "Total",
      label == "Estimate!!Total:!!Male:" ~ "Total Male",
      label == "Estimate!!Total:!!Female:" ~ "Total Female",
      TRUE ~ str_extract(label, "ale:!![0-9 A-Za-z]+") |>
        str_replace("ale:!!",""))
    ) |>
    left_join(age_groups, by = "census_age") |>
    filter(!str_detect(NAME, "not defined"))
  if (1 == 2) {
    sex_age_acs2 |>
      filter(TOWN == "Guilford") |>
      group_by(age) |>
      summarise(estimate = sum(estimate))
  }
  return(sex_age_acs2)
}
