library(directlabels)

xx0 <- dph_total %>%
  filter(date > ymd("2021-01-01"), !is.na(cases)) %>%
  # filter(date < ymd("2021-01,22")) %>%
  select(date, starts_with("cases_age")) %>%
  pivot_longer(cols = starts_with("cases_age"),
      names_to = "age", values_to = "cases") %>%
  group_by(age)

all_dates <- expand_grid(date = seq(min(xx0$date, na.rm = TRUE), max(xx0$date, na.rm = TRUE), 1),
                group_var = unique(xx0$age))
names(all_dates)[2] <- group_vars(xx0)[1]

xx1 <- xx0 %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  full_join(all_dates, by = names(all_dates)) %>%
  arrange(date, .by_group = TRUE)

xx <- xx1 %>%
  mutate(
    new_cases = if_else(is.na(new_cases), 0, new_cases),
    rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_, na.rm = TRUE),
    rnew_cases = if_else(is.nan(rnew_cases), NA_real_, rnew_cases))


p1 <- ggplot(data = xx %>% filter(date > ymd("2021-01-01")),
             aes(x = date, y = rnew_cases, colour = age)) + geom_line()
direct.label(p1)

p2 <- ggplot(data = yy %>% filter(date > ymd("2021-01-01")),
            aes(x = date, y = rnew_cases, colour = agegroups)) + geom_line()
direct.label(p2)


