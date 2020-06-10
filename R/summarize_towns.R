# summary_of_towns
   # cum_deaths_per1k
   # cum_cases_per1k
   # recent_deaths_per1k
   # recent_cases_per1k
   # total_pop
   #


summarize_towns <- function(some_towns = dph_towns,
                            as_of = max(some_towns$date, na.rm = TRUE),
                            nursing_homes = town_with_nursing) {
   some_towns %>%
      filter(date == as_of) %>%
      left_join(nursing_homes %>%
                   filter(date == max(date)) %>%
                   select(town, nh_deaths, corresponding_deaths = deaths), by = "town") %>%
      summarise(n = n(), across(c(total_pop , current_cases, rnew_cases,
                                  rnew_deaths, cases, deaths,
                                  nh_deaths, corresponding_deaths), sum, na.rm = TRUE)) %>%
      mutate(nh_pct_of_deaths = ifelse(corresponding_deaths <= 0,
                                       NA_real_, nh_deaths / corresponding_deaths),
             across(c(current_cases, rnew_cases, rnew_deaths, cases, deaths),
                    ~ .x / (total_pop / 1000),
                    .names = "{col}_per1k")
             # current_per1k = current_cases / (total_pop /1000),
            )

}

# summarize_towns(dph_towns %>% filter(county == "New Haven"))


