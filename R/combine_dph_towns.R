# combine_dph_towns
# return something like the dph_towns row but for a sum of a bunch of towns

# for example, xx <- combine_dph_towns(dph_towns, category, county)

combine_dph_towns <- function(some_towns = dph_towns, ...) {
  collect <- some_towns  %>%
    group_by(..., date) %>%
    summarise(across(c(cases, deaths, confirmedcases, probablecases,
                       confirmeddeaths, probabledeaths, tests, tests_positive,
                       numberofnegatives, numberofindeterminates,
                       total_pop, rcases, rdeaths, new_cases, new_deaths,
                       rnew_cases, rnew_deaths, current_cases), sum, na.rm = TRUE),
              towns = n(), .groups = "drop") %>%
    mutate(across(c(current_cases, rnew_cases, rnew_deaths, cases, deaths),
                  ~ .x / (total_pop / 100000),
                  .names = "{col}_per100k"))
}

combine_town_geometries <- function(some_towns = town_geometries, ...) {
  collect <- some_towns  %>%
    left_join(dph_towns %>%
                filter(date == max(date, na.rm = TRUE)) %>%
                select(town, rnew_cases, rnew_deaths, cases, deaths, tests, tests_positive), by = "town") %>%
    group_by(...) %>%
    summarise(across(c(rnew_cases, rnew_deaths, cases, deaths, tests, tests_positive, total_pop), sum, na.rm = TRUE),
              geometry = st_combine(geometry),
              towns = n(), .groups = "drop") %>%
    mutate(across(c(rnew_cases, rnew_deaths, cases, deaths),
                  ~ .x / (total_pop / 100000),
                  .names = "{col}_per100k"))
}


# atotal = summarize_towns(dph_towns)
#
#
# dph_total %>% filter(date == max(date)) %>% select(rcases, rdeaths, covid_19_pcr_tests_reported, confirmedcases,
#                                                    rnew_deaths, current_per_100k, rnew_deaths_per1k)
# atotal %>% filter(date == max(date)) %>% select(rcases, rdeaths, numberoftests, numberofpositives,
#                                                 rnew_deaths, current_cases_per1k, rnew_deaths_per1k)
#

# summarize_towns(dph_towns %>% filter(county == "Fairfield", date == max(date))) %>% select(rnew_cases_per1k, rnew_deaths_per1k)
# dph_counties %>% filter(county == "Fairfield", date == max(date)) %>% select(rnew_cases_per1k, rnew_deaths_per1k)
