# combine_dph_towns
# return something like the dph_towns row but for a sum of a bunch of towns

# for example, xx <- combine_dph_towns(dph_towns, category, county)

combine_dph_towns <- function(some_towns = dph_towns, ...) {
  collect <- some_towns  %>%
    group_by(..., date) %>%
    summarise(across(c(cases, deaths, peopletested, confirmedcases, probablecases,
                       confirmeddeaths, probabledeaths, numberoftests, numberofpositives,
                       numberofnegatives, numberofindeterminates,
                       total_pop, rcases, rdeaths, new_cases, new_deaths,
                       rnew_cases, rnew_deaths, current_cases), sum, na.rm = TRUE),
              towns = n(), .groups = "drop") %>%
    mutate(across(c(current_cases, rnew_cases, rnew_deaths, cases, deaths),
                  ~ .x / (total_pop / 1000),
                  .names = "{col}_per1k"))
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
