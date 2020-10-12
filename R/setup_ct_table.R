setup_ct_tables <- function(tbl) {
  # assumes: tbl contains date, cases, deaths, tests, tests_positive
  # creates: new_cases, rcases, rnew_cases, current_cases, new_deaths, rdeaths, rnew_deaths, new_tests, new_tests_positive
  # tbl can either be ungrouped or may have one grouping variable (e.g., town or county)
  if (length(group_vars(tbl)) == 0) {
    all_dates <-tibble(date = seq(min(tbl$date, na.rm = TRUE), max(tbl$date, na.rm = TRUE), 1))
  } else if (length(group_vars(tbl)) == 1){
    group_var = tbl %>% select(groups(tbl)[[1]]) %>% unique() %>% pull(groups(tbl)[[1]])
    all_dates <- expand_grid(date = seq(min(tbl$date, na.rm = TRUE), max(tbl$date, na.rm = TRUE), 1),
                      group_var = tbl %>% select(groups(tbl)[[1]]) %>% unique() %>% pull(groups(tbl)[[1]]))
    names(all_dates)[2] <- group_vars(tbl)[1]
  } else stop(paste("In setup_ct_tables, only handles one grouping variable.", group_vars(tbl)))

  # change so first we compute the new cases, next full_join to dates, then compute rolling averages
  tbl <- tbl %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(new_cases = cases - lag(cases),
           new_deaths = deaths - lag(deaths),
           new_tests = tests - lag(tests),
           new_tests_positive = tests_positive - lag(tests_positive)) %>%
    # new_tests =  covid_19_pcr_tests_reported - lag(covid_19_pcr_tests_reported)) %>%
    full_join(all_dates, by = names(all_dates)) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_, na.rm = TRUE),
           rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_, na.rm = TRUE),
           current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_, na.rm = TRUE),
           # current_per_100k =  (state_info$total_pop[1] / 100000) / current_cases,
           rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_, na.rm = TRUE),
           rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_, na.rm = TRUE))
  # rnew_cases_per1k = rnew_cases / (state_info$total_pop[1] / 1000),
  # rnew_deaths_per1k = rnew_deaths / (state_info$total_pop[1] / 1000))
}



# xx <- dph_total %>% filter(month(date) >= 7) %>% select(date, cases, rcases) %>% arrange(date)
# xx <-  xx %>% mutate(y = roll_mean(cases, n=7, align = "right", na.rm = TRUE, fill = NA_real_))
