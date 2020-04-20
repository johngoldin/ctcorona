# New process for daily CT stats
library(dplyr)
library(tidyverse, quietly = TRUE)
library(sf, quietly = TRUE)
options(tigris_use_cache = TRUE)
library(tigris, quietly = TRUE)
library(tidycensus, quietly = TRUE)
library(viridis, quietly = TRUE)
library(ggrepel)
options(tigris_class = "sf")
library(lubridate, quietly = TRUE)
library(janitor)
library(fuzzyjoin) # for interval_left_join
library(broom)
library(RSocrata)
library(RcppRoll)

path_to_post <- "~/Dropbox/Programming/R_Stuff/can_i_blog_too/content/post/2020-03-29-covid19-cases-in-connecticut/"


if (!exists("county_geometries")) load("census_population.RData")
if (!exists("county_geometries")) {
  county_geometries <- counties(state = "CT", cb = FALSE)
  census_population <- get_acs(geography = "county",
                           variables = "B01003_001",
                           state = "CT",
                           geometry = TRUE) %>%
    mutate(county = str_replace(NAME, " County, Connecticut", ""))
  # save(county_geometries, census_population, file = "census_population.RData")
}

if (!exists("nyt_series")) load("nyt_series.RData")
if (!exists("nyt_series")) {
  # from https://github.com/nytimes/covid-19-data
  nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  nyt_series <- nyt_counties %>%
    filter(state == "Connecticut")
  # save(nyt_series, file = "nyt_series.RData")
}

dph_counties <- read.socrata("https://data.ct.gov/resource/bfnu-rgqt.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), hospital = as.numeric(hospitalization)) %>%
  select(-dateupdated, -hospitalization)
if (min(dph_counties$date) != ymd("2020-03-08")) dph_counties <- dph_counties %>%
  bind_rows(
    nyt_series %>% filter(date < min(dph_counties$date)) %>%
      select(county, cases, deaths, date) %>%
      arrange(county, date) %>%
      mutate(hospital = NA_real_, cnty_cod = NA_character_))
usethis::ui_info("Most recent county data is {ui_value(max(dph_counties$date, na.rm = TRUE))}. Earliest is {ui_value(min(dph_counties$date, na.rm = TRUE))}.")
if ((dph_counties %>% count(county, date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("dph_counties contains multiple rows on the same date.")

# at this point we should have full
# do rolling average
dph_counties <- dph_counties %>%
  arrange(county, date) %>%
  group_by(county) %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_))
dph_counties <- dph_counties %>% ungroup()


dph_towns <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(lastupdatedate), cases = as.numeric(confirmedcases),
         deaths = as.numeric(deaths), caserate = as.numeric(caserate)) %>%
  rename(per_100k = caserate) %>%
  select(-lastupdatedate, -confirmedcases, -town_no) %>%
  arrange(town, date) %>%
  group_by(town) %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_))
dph_towns <- dph_towns %>% ungroup()

dph_total <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date), cases = as.numeric(cases),
         deaths = as.numeric(deaths), hospital = as.numeric(hospitalizations)) %>%
  mutate_at(vars(starts_with("cases_")), as.numeric) %>%
  select( -hospitalizations)

if (min(dph_total$date) != ymd("2020-03-08")) dph_total <- dph_total %>%
  bind_rows(
    nyt_series %>% filter(date < min(dph_total$date)) %>%
      select(cases, deaths, date) %>%
      group_by(date) %>%
      summarise(cases = sum(cases), deaths = sum(deaths)) %>%
      ungroup() %>%
      mutate(hospital = NA_real_, state = "CONNECTICUT",
             cases_age0_9 = NA_real_, cases_age10_19 = NA_real_,
             cases_age20_29 = NA_real_, cases_age30_39 = NA_real_,
             cases_age40_49 = NA_real_, cases_age50_59 = NA_real_,
             cases_age60_69 = NA_real_,cases_age70_79 = NA_real_,
             cases_age80_older = NA_real_)) %>%
  arrange(date) %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_))

usethis::ui_info("Most recent statewide data is {ui_value(max(dph_total$date, na.rm = TRUE))}. Earliest is {ui_value(min(dph_total$date, na.rm = TRUE))}.")
if ((dph_total %>% count(date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("dph_total contains multiple rows on the same date.")


last_date <- max(dph_total$date)
usethis::ui_info("Last date seen: {usethis::ui_value(last_date)}. Earliest is {ui_value(min(dph_counties$date, na.rm = TRUE))}.")
usethis::ui_info("Confirmed cases:            {usethis::ui_value(dph_total$cases[dph_total$date == last_date])}")
usethis::ui_info("Confirmed deaths:           {usethis::ui_value(dph_total$deaths[dph_total$date == last_date])}")
usethis::ui_info("Confirmed hospitalizations: {usethis::ui_value(dph_total$hospital[dph_total$date == last_date])}")

dph_age <- read.socrata("https://data.ct.gov/resource/ypz6-8qyf.json",
                         app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), rate = as.numeric(rate)) %>%
         rename(per_100k = rate) %>%
  select(-dateupdated)

dph_gender <- read.socrata("https://data.ct.gov/resource/qa53-fghg.json",
                       app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), rate = as.numeric(rate)) %>%
  rename(per_100k = rate) %>%
  select(-dateupdated)

if ((max(dph_total$date) != max(dph_counties$date)) |
    (max(dph_total$date) != max(dph_towns$date)) |
    (max(dph_total$date) != max(dph_age$date)) |
    (max(dph_total$date) != max(dph_gender$date)))
  usethis::ui_stop(paste("CT DPH dates. \ntotal:", max(dph_total$date), " counties:", max(dph_counties$date),
                          " towns:", max(dph_towns$date), " age:", max(dph_age$date), " gender:", max(dph_gender$date)))

# there's also zip code level monitoring at: https://data.ct.gov/resource/javn-ujwr.json

exec_orders <- tibble(
  date = as.Date(c("2020-03-10", "2020-03-17", "2020-03-23", "2020-03-26", "2020-04-11")),
  label = c("prohibit large\ngatherings", "cancel\nclasses", "restrict\nbusiness", "5 or fewer",
            "covid-19 death\nchanged")
) %>%
  left_join(dph_counties %>% filter(county == "Fairfield") %>% select(date, cases), by = "date") %>%
  mutate(county = "Fairfield")

# Setup week ranges starting with most recent week and working backward
nweeks = 4
week_setup <- tibble(
  start_period = (max(dph_counties$date) + 1) - weeks(seq(nweeks, 1, -1)),
) %>%
  mutate(end_period = start_period + days(6),
         week = str_c(str_sub(start_period, 6, 10), " to ", str_sub(end_period, 6, 10)),
         week = str_replace_all(week, "-", "/"))

ct <- dph_counties %>%
  mutate(county = fct_reorder2(county, date, rcases)) %>%
  fuzzyjoin::interval_left_join(week_setup,
                                by = c("date" = "start_period", "date" = "end_period"))

save_county_levels <- levels(ct$county)

date_range <- unique(ct$date)
label_height_cases <- function(county, date) {
  # ht <- ct$cases[(ct$county == county) & (ct$date == date)]
  ht <- ct$rcases[(ct$county == county) & (ct$date == max(ct$date))]
  # ht <- ct$cases[(ct$county == county) & (ct$date == date_range[length(date_range) - 2])]
  if (length(ht) == 0) ht <- 0.5
  return(ht)
}
label_height_deaths <- function(county, date) {
  ht <- ct$rdeaths[(ct$county == county) & (ct$date == max(ct$date))]
  if (length(ht) == 0) ht <- 0.5
  return(ht)
}
label_height_hospital <- function(county, date) {
  ht <- ct$hospital[(ct$county == county) & (ct$date == max(ct$date))]
  if (length(ht) == 0) ht <- 0.5
  return(ht)
}
label_height_new_cases <- function(county, date) {
  ht <- ct$rnew_cases[(ct$county == county) & (ct$date == max(ct$date))]
  if (length(ht) == 0) ht <- 0.5
  return(ht)
}
label_height_new_deaths <- function(county, date) {
  ht <- ct$rnew_deaths[(ct$county == county) & (ct$date == max(ct$date))]
  if (length(ht) == 0) ht <- 0.5
  return(ht)
}
for_county_labels <- tibble(
  # date = date_range[seq(length(date_range) - 2 + 1, length(date_range) - 1 + 1 - length(unique(ct$county)), -1)],
  date = max(date_range),
  county = levels(ct$county),
  rcases = 0,
  rdeaths = 0,
  hospital = 0,
  rnew_cases = 0,
  rnew_deaths = 0
) %>%
  mutate(rcases = map2_dbl(county, date, label_height_cases),
         rdeaths = map2_dbl(county, date, label_height_deaths),
         hospital = map2_dbl(county, date, label_height_hospital),
         rnew_cases = map2_dbl(county, date, label_height_new_cases),
         rnew_deaths = map2_dbl(county, date, label_height_new_deaths)
  )
# log minor breaks taken from https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot
log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm = TRUE)) - 1;
    maxx         = ceiling(max(log10(x), na.rm = TRUE)) + 1;
    n_major      = maxx - minx + 1;
    major_breaks = seq(minx, maxx, by = 1)
    minor_breaks =
      rep(log10(seq(1, 9, by = 1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}

as_week <- function(d) {
  sprintf("%s-%02d", month(d, label = TRUE, abbr = TRUE), day(d))
}
# down below I change levels so that facet chart will match geography
# ct$county <- factor(ct$county, levels = save_county_levels)
pcases <- ggplot(data = ct, aes(x = date, y = rcases, colour = county)) +
  geom_point() + geom_line() + xlab(NULL) +
  scale_x_date(date_minor_breaks = "1 day", date_breaks = "1 week", labels = as_week) +
  theme_minimal() +
  theme(legend.position = "none")
pcases <- pcases +
  geom_text_repel(data = for_county_labels, aes(label = county),
                  show.legend = FALSE, hjust = 0, vjust = 0.5,
                  # check_overlap = FALSE,
                  # nudge_x = 0.21,
                  direction = "y") +
  expand_limits(x = max(ct$date) + 1) +
  labs(
    title = "Cumulative COVID19 Cases in Connecticut by County",
    caption = "Source: CT Department of Public Health (data.ct.gov) & New York Times"
  )
pcases_nonlog <- pcases  +
  ylab("confirmed cases") +
  labs(subtitle = "(seven-day rolling average)")

# annotation_custom(grob = ggplotGrob(county_map), xmin = date_range[2], xmax = date_range[13],
#                   ymin = 10 ^ (log10(max(ct$cases)) * 0.66),
#                   ymax = max(ct$cases))
pcases <- pcases +
  ylab("confirmed cases (log scale)") +
  labs(subtitle = "(seven-day rolling average, log scale)") +
  scale_y_log10(minor_breaks=log10_minor_break()) +
  geom_text(data = exec_orders, aes(y = cases , label = label),
            hjust = 0.5, vjust = 0, size = 3, colour = "darkgrey", nudge_y = 0)


pdeaths <- ggplot(data = ct %>% filter(rdeaths > 0) %>%
                    mutate(rdeaths = ifelse(rdeaths == 0, NA_real_, rdeaths))
                  , aes(x = date, y = rdeaths, colour = county)) +
  geom_point() + geom_line() + xlab(NULL) +
  scale_x_date(date_minor_breaks = "1 day", limits = c(ymd("2020-03-14"), NA),
               date_breaks = "1 week", labels = as_week) +
  theme_minimal() +
  theme(legend.position = "none")

pdeaths <- pdeaths + geom_text_repel(data = for_county_labels %>% filter(rdeaths > 0),
                                     aes(label = county),
                                     hjust = 0, vjust = 0.0,
                                     # check_overlap = FALSE,
                                     show.legend = FALSE,
                                     # nudge_x = 0.21,
                                     direction = "y") +
  expand_limits(x = max(ct$date) + 1)
pdeaths_log <- pdeaths +
  scale_y_log10(minor_breaks=log10_minor_break()) +
  ylab("deaths (log scale)") +
  labs(
    title = "Cumulative COVID19 Deaths in Connecticut by County",
    subtitle = "(seven-day rolling average, log scale)",
    caption = "Source: CT Department of Public Health (data.ct.gov) & New York Times"
  )
pdeaths <- pdeaths +
  ylab("deaths") +
  labs(
    title = "Cumulative COVID19 Deaths in Connecticut by County",
    subtitle = "(seven-day rolling average)",
    caption = "Source: CT Department of Public Health (data.ct.gov) & New York Times"
  )

pnew_cases <- ggplot(data = ct, aes(x = date, y = rnew_cases, colour = county)) +
  geom_point() + geom_line() + xlab(NULL) +
  scale_x_date(date_minor_breaks = "1 day", date_breaks = "1 week", labels = as_week) +
  theme_minimal() +
  theme(legend.position = "none")
pnew_cases <- pnew_cases +
  geom_text_repel(data = for_county_labels, aes(label = county),
                  show.legend = FALSE, hjust = 0, vjust = 0.5,
                  # check_overlap = FALSE,
                  # nudge_x = 0.21,
                  direction = "y") +
  expand_limits(x = max(ct$date) + 1) +
  ylab("confirmed cases") +
  labs(
    subtitle = "(seven-day rolling average)",
    title = "Daily New COVID19 Cases in Connecticut by County",
    caption = "Source: CT Department of Public Health (data.ct.gov) & New York Times"
  )

pnew_deaths <- ggplot(data = ct, aes(x = date, y = rnew_deaths, colour = county)) +
  geom_point() + geom_line() + xlab(NULL) +
  scale_x_date(date_minor_breaks = "1 day", date_breaks = "1 week", labels = as_week) +
  theme_minimal() +
  theme(legend.position = "none")
pnew_deaths <- pnew_deaths +
  geom_text_repel(data = for_county_labels, aes(label = county),
                  show.legend = FALSE, hjust = 0, vjust = 0.5,
                  # check_overlap = FALSE,
                  # nudge_x = 0.21,
                  direction = "y") +
  expand_limits(x = max(ct$date) + 1) +
  ylab("covid-19 deaths") +
  labs(
    subtitle = "(seven-day rolling average)",
    title = "Daily New COVID19 Deaths in Connecticut by County",
    caption = "Source: CT Department of Public Health (data.ct.gov) & New York Times"
  )
# annotation_custom(grob = ggplotGrob(county_map), xmin = date_range[2], xmax = date_range[13],
#                   ymin = 10 ^ (log10(max(ct$cases)) * 0.66),
#                   ymax = max(ct$cases))
# pnew_cases <- pnew_cases +
#   ylab("confirmed cases (log scale)") +
#   labs(subtitle = "(seven-day rolling average, log scale)") +
#   scale_y_log10(minor_breaks=log10_minor_break()) +
#   geom_text(data = exec_orders, aes(y = cases , label = label),
#             hjust = 0.5, vjust = 0, size = 3, colour = "darkgrey", nudge_y = 0)

phospital <- ggplot(data = ct %>% filter(!is.na(hospital)),
                    aes(x = date, y = hospital, colour = county)) +
  geom_point() + geom_line() + xlab(NULL) +
  scale_x_date(date_minor_breaks = "1 day", limits = c(ymd("2020-03-14"), NA),
               date_breaks = "1 week", labels = as_week) +
  theme_minimal() +
  theme(legend.position = "none")
phospital <- phospital + geom_text_repel(data = for_county_labels %>% filter(hospital > 0),
                                         aes(label = county),
                                         hjust = 0, vjust = 0.5,
                                         # check_overlap = FALSE,
                                         show.legend = FALSE,
                                         # nudge_x = 0.21,
                                         direction = "y")
phospital <- phospital +
  ylab("in hospital") +
  labs(
    title = "COVID19 Patients Currently in Hospital",
    subtitle = NULL,
    caption = "Source: Connecticut Hospital Association, via CT DPH"
  )

dph_pop <- census_population %>%
  left_join(ct %>% filter(date == max(date))) %>%
  mutate(cases_per_pop = cases / (estimate / 100000),
         per_pop_label =  as.character(round(cases_per_pop, 1)))
county_centroid <- st_centroid(dph_pop) # use to place town labels
county_map <- ggplot() +
  geom_sf(data = dph_pop, aes(fill = cases_per_pop)) +
  scale_fill_gradient(low = "white", high = "grey") +
  geom_sf_text(data = county_centroid, aes(label = paste0(county, "\n", per_pop_label, " per 100K")), color = "black", size = 3) +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal()  +
  # theme(legend.position = "none") +
  labs(title = "Cumulative Confirmed Cases by Connecticut County",
       subtitle = "cases per 100K of population for most recent report date",
       fill = "cases per 100K",
       caption = "Source: US Census, tidycensus package")

ct <- ct %>%
  mutate(county = factor(county,
                         levels = c("Litchfield", "Hartford", "Tolland", "Windham", "Fairfield", "New Haven", "Middlesex", "New London")),
         week = fct_rev(factor(week)))

doubling_cases <- ct %>%
  filter(!is.na(week), cases > 0) %>%
  group_by(county, week) %>%
  nest() %>%
  mutate(
    data_days = map(data, nrow),
    fit_cases = map(data, ~ lm(log(cases) ~ date, data = .x)),
    tidied_cases = map(fit_cases, tidy),
    glance_cases = map(fit_cases, glance)
  ) %>%
  unnest(tidied_cases) %>%
  filter(data_days >= 5) %>%
  select(county, week, data, fit_cases, term, estimate) %>%
  pivot_wider(values_from = estimate, names_from = term) %>%
  mutate(doubling = log(2) / log(1 + date))

doubling_deaths <- ct %>%
  filter(!is.na(week), deaths > 0) %>%
  group_by(county, week) %>%
  nest() %>%
  mutate(
    data_days = map(data, nrow),
    fit_deaths = map(data, ~lm(log(deaths) ~ date, data = .x )),
    tidied_deaths = map(fit_deaths, tidy),
    glance_deaths = map(fit_deaths, glance)
  ) %>%
  unnest(tidied_deaths) %>%
  filter(data_days >= 5) %>%
  select(county, week, data, fit_deaths, term, estimate, data_days) %>%
  pivot_wider(values_from = estimate, names_from = term) %>%
  mutate(doubling = log(2) / log(1 + date))
doubling <-
  bind_rows(
    doubling_cases %>%
      select(county, week = week, daily_rate = date, doubling) %>%
      mutate(measure = "cases"),
    doubling_deaths %>%
      select(county, week = week, daily_rate = date, doubling) %>%
      mutate(measure = "deaths")
  )

p_doubling <- ggplot(data = doubling %>% filter(doubling < 9), aes(x = week, y = doubling, fill = doubling)) +
  geom_col(position = position_dodge2(reverse = TRUE, padding = 0.1)) +
  coord_flip() +
  xlab(NULL) + ylab("doubling time (in days)") +
  facet_wrap(measure ~ county, ncol = 4) +
  labs(title = "Doubling Time of Cumulative Count of Confirmed Cases",
       subtitle = "(longer doubling time is better!)",
       caption = "Doubling time is the number of days for the cumulative count to double.") +
  theme(legend.position = "none")

ggsave("log_cases.png", plot = pcases, path = path_to_post,
       width = 7, height = 6, units = "in")

ggsave("cases_nonlog.png", plot = pcases_nonlog, path = path_to_post,
       width = 7, height = 6, units = "in")

ggsave("county_map.png", plot = county_map, path = path_to_post,
       width = 6, height = 5, units = "in")

ggsave("deaths.png", plot = pdeaths, path = path_to_post,
       width = 7, height = 6, units = "in")
ggsave("log_deaths.png", plot = pdeaths_log, path = path_to_post,
       width = 7, height = 6, units = "in")
ggsave("new_cases.png", plot = pnew_cases, path = path_to_post,
       width = 7, height = 6, units = "in")
ggsave("new_deaths.png", plot = pnew_deaths, path = path_to_post,
       width = 7, height = 6, units = "in")
ggsave("hospitalizations.png", plot = phospital, path = path_to_post,
       width = 7, height = 6, units = "in")
ggsave("doubling.png", plot = p_doubling, path = path_to_post,
       width = 7, height = 8, units = "in")

