# summary_of_towns
   # cum_deaths_per1k
   # cum_cases_per1k
   # recent_deaths_per1k
   # recent_cases_per1k
   # total_pop
   #

todo:  why is nh_pct_of_deaths 1.02 rather than 0.093?

summarize_towns <- function(some_towns = dph_towns,
                            as_of = max(some_towns$date, na.rm = TRUE),
                            nursing_homes = town_with_nursing) {
  some_towns %>%
    filter(date == as_of) %>%
    left_join(nursing_homes %>%
                filter(date == max(date)) %>%
                select(town, nh_deaths, corresponding_deaths = deaths), by = "town") %>%
    summarise(across(c(total_pop , current_cases, rnew_cases,
                     rnew_deaths, cases, deaths,
                     nh_deaths, corresponding_deaths), sum, na.rm = TRUE)) %>%
    mutate(nh_pct_of_deaths = ifelse(corresponding_deaths <= 0,
                                     NA_real_, deaths / corresponding_deaths))

}

summarize_towns(dph_towns %>% filter(county == "New Haven"))


