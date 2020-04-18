# New process for daily CT stats

library(tidyverse, quietly = TRUE)
library(sf, quietly = TRUE)
library(tigris, quietly = TRUE)
library(tidycensus, quietly = TRUE)
library(viridis, quietly = TRUE)
options(tigris_use_cache = TRUE)
library(ggrepel)
options(tigris_class = "sf")
library(lubridate, quietly = TRUE)
library(janitor)
library(fuzzyjoin) # for interval_left_join
library(broom)
library(RSocrata)

path_to_post <- "~/Dropbox/Programming/R_Stuff/can_i_blog_too/content/post/2020-03-29-covid19-cases-in-connecticut/"


if (!exists("county_geometries")) load("ct_population.RData")
if (!exists("county_geometries")) {
  county_geometries <- counties(state = "CT", cb = FALSE)
  ct_population <- get_acs(geography = "county",
                           variables = "B01003_001",
                           state = "CT",
                           geometry = TRUE) %>%
    mutate(county = str_replace(NAME, " County, Connecticut", ""))
  # save(county_geometries, ct_population, file = "ct_population.RData")
}

if (!exists("nyt_series")) load("nyt_series.RData")
if (!exists("nyt_series")) {
  # from https://github.com/nytimes/covid-19-data
  nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  nyt_series <- nyt_counties %>%
    filter(state == "Connecticut")
  # save(nyt_series, file = "nyt_series.RData")
}

ct_counties <- read.socrata("https://data.ct.gov/resource/bfnu-rgqt.json",
                   app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), hospital = as.numeric(hospitalization)) %>%
  select(-dateupdated, -hospitalization)
usethis::ui_info("Most recent county data is {ui_value(max(ct_counties$date, na.rm = TRUE))}.")
if ((ct_counties %>% count(county, date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("ct_counties contains multiple rows on the same date.")


ct_towns <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(lastupdatedate), cases = as.numeric(confirmedcases),
         deaths = as.numeric(deaths), caserate = as.numeric(caserate)) %>%
  rename(per_100k = caserate) %>%
  select(-lastupdatedate, -confirmedcases, -town_no)

ct_total <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date), cases = as.numeric(cases),
         deaths = as.numeric(deaths), hospital = as.numeric(hospitalizations)) %>%
  mutate_at(vars(starts_with("cases_")), as.numeric) %>%
  select( -hospitalizations)

ct_age <- read.socrata("https://data.ct.gov/resource/ypz6-8qyf.json",
                         app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), rate = as.numeric(rate)) %>%
         rename(per_100k = rate) %>%
  select(-dateupdated)

ct_gender <- read.socrata("https://data.ct.gov/resource/qa53-fghg.json",
                       app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths), rate = as.numeric(rate)) %>%
  rename(per_100k = rate) %>%
  select(-dateupdated)


yy <- ct %>%
  left_join(xx %>% select(-dateupdated, -cnty_cod),
            by = c("date" = "date", "county" = "county"))

yy %>% filter(!is.na(cases.y), cases.x != cases.y, deaths.x != deaths.y) %>%
  select(date, county, cases.y, deaths.y)
