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
library(covid19us)
# library(MMWRweek)

start_time <- Sys.time()

covid_project <- "can_i_get_a_clean_project"
path_to_post <- "~/Dropbox/Programming/R_Stuff/can_i_blog_too/content/blog/2020-03-29-covid19-cases-in-connecticut/"
path_to_post_june <- "~/Dropbox/Programming/R_Stuff/can_i_blog_too/content/blog/2020-06-22-tracking-covid-19-in-connecticut/"
path_to_static_june <- "~/Dropbox/Programming/R_Stuff/can_i_blog_too/static/blog/2020-06-22-tracking-covid-19-in-connecticut/"
path_to_covid_project <- paste0("~/Documents/R_local_repos/can_i_blog_too/content/project/covid/", covid_project, "/")

path_to_ctcorona <- "~/Documents/R_local_repos/ctcorona/data/"
path_to_county_html <- paste0(path_to_covid_project, "county_html/")
load(paste0(path_to_ctcorona, "dph_datasets.RData"))
load(paste0(path_to_ctcorona, "census_population.RData"))

# from https://www.ctdatahaven.org/sites/ctdatahaven/files/UConnCPR%20Changing%20Demographics-5%20CTs%202004.pdf
# The Changing Demographics of Connecticut — 1990 to 2000. by Center for Population Research  May. 31, 2004
Five_Connecticuts <- read_delim(paste0(path_to_ctcorona, "Five_Connecticuts.txt"),"\t", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(category = factor(category, levels = c("Urban Core", "Urban Periphery",
                           "Wealthy", "Suburban", "Rural")))

if (!exists("town_geometries") &
    file.exists(paste0(path_to_ctcorona, "census_population.RData"))) load(paste0(path_to_ctcorona, "census_population.RData"))
if (!exists("town_geometries")) {
  source("R/setup_geometries.R")
}
add_names <- function(p, df, color = "darkgray", size = 3) {
  p + geom_text(data = df,
                aes(label = names, x = lon, y = lat), size = size, colour = color) +
    xlab(NULL) + ylab(NULL) + coord_sf(datum = NA) + theme_minimal()
}
if (1 == 2) {
  p_old <- ggplot(data = town_geometries) + geom_sf(aes(fill = age_65_plus_pct)) +
    geom_sf(data = county_geometries, colour = "gray", fill = NA)
  p_old <-add_names(p_old, df = town_geometries %>% filter(total_pop > 70000) %>%
             select(names = town, lat = INTPTLAT, lon = INTPTLON) )
  p_income <- ggplot(data = town_geometries) + geom_sf(aes(fill = family_income)) +
    geom_sf(data = county_geometries, colour = "gray", fill = NA)
  p_income <-add_names(p_income, df = town_geometries %>% filter(total_pop > 70000) %>%
             select(names = town, lat = INTPTLAT, lon = INTPTLON) )
  p_poverty <- ggplot(data = town_geometries) + geom_sf(aes(fill = poverty_pct)) +
    geom_sf(data = county_geometries, colour = "gray", fill = NA)
  p_poverty <-add_names(p_poverty, df = town_geometries %>% filter(total_pop > 70000) %>%
             select(names = town, lat = INTPTLAT, lon = INTPTLON) )
  p_college_plus <- ggplot(data = town_geometries) + geom_sf(aes(fill = college_plus_pct)) +
    geom_sf(data = county_geometries, colour = "gray", fill = NA)
  p_college_plus <-add_names(p_college_plus, df = town_geometries %>% filter(total_pop > 70000) %>%
             select(names = town, lat = INTPTLAT, lon = INTPTLON) )
  p_density <- ggplot(data = town_geometries) + geom_sf(aes(fill = density)) +
    geom_sf(data = county_geometries, colour = "gray", fill = NA)
  p_density <-add_names(p_density, df = town_geometries %>% filter(total_pop > 70000) %>%
             select(names = town, lat = INTPTLAT, lon = INTPTLON) )

  make_town_map <- function(d = town_geometries, var,
                            cg = county_geometries,
                            pop_cutoff = 70000,
                            town_labels = "darkgray",
                            county_lines = "gray") {
    ggplot(data = d) + geom_sf(aes(fill = {{ var }})) +
      geom_sf(data = cg, color = county_lines, fill = NA) +
      geom_text(data = d %>% filter(total_pop > pop_cutoff),
                    aes(label = town, x = INTPTLON, y = INTPTLAT),
                size = 3, colour = town_labels) +
      xlab(NULL) + ylab(NULL) + coord_sf(datum = NA) + theme_minimal()
  }
  # make_town_map(town_geometries, college_plus_pct) %>% print()
}

if (!exists("nyt_series")) load(paste0(path_to_ctcorona, "nyt_series.RData"))
if (!exists("nyt_series")) {
  # from https://github.com/nytimes/covid-19-data
  nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  nyt_series <- nyt_counties %>%
    filter(state == "Connecticut")
  # save(nyt_series, file = paste0(path_to_ctcorona, "nyt_series.RData"))
}

start_loading_socrata_data <- Sys.time()

source('R/setup_ct_table.R')

# ct covid data resources: https://data.ct.gov/stories/s/wa3g-tfvc

dph_reports <- read.socrata("https://data.ct.gov/resource/bqve-e8um.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>% arrange(desc(date)) %>%
  mutate(date = as_date(date))

dph_counties <- read.socrata("https://data.ct.gov/resource/bfnu-rgqt.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths) %>%
  mutate(date = as_date(dateupdated),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
         tests = NA_real_, tests_positive = NA_real_,
         hospital = as.numeric(hospitalization),
         confirmedcases = as.numeric(confirmedcases),
         confirmeddeaths = as.numeric(confirmeddeaths),
         probablecases = as.numeric(probablecases),
         probabledeaths = as.numeric(probabledeaths)) %>%
  select(-dateupdated, -hospitalization)
if (min(dph_counties$date) != ymd("2020-03-08")) dph_counties <- dph_counties %>%
  bind_rows(
    nyt_series %>% filter(date < min(dph_counties$date)) %>%
      select(county, cases, deaths, date) %>%
      arrange(county, date) %>%
      mutate(hospital = NA_real_, cnty_cod = NA_character_))
usethis::ui_info("Most recent county data is {ui_value(max(dph_counties$date, na.rm = TRUE))}. Earliest is {ui_value(min(dph_counties$date, na.rm = TRUE))}.")
if ((dph_counties %>% count(county, date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("dph_counties contains multiple rows on the same date.")

dph_counties <- dph_counties %>%
  group_by(county) %>%
  setup_ct_tables()
# View(xx %>% select(date, county, cases, deaths, new_cases, rcases, rnew_cases, current_cases) %>% filter(date > ymd("2020-06-20")) %>% arrange(county, date))
dph_counties <- dph_counties %>%
  arrange(county, date) %>%
  group_by(county) %>%
  left_join(county_info %>% select(county, total_pop), by = "county") %>%
  mutate(rnew_cases_per100k = rnew_cases / (total_pop / 100000),
         rnew_deaths_per100k = rnew_deaths / (total_pop / 100000))
dph_counties <- dph_counties %>% ungroup()


dph_towns <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = towntotalcases, deaths = towntotaldeaths,
         tests = numberoftests, tests_positive = numberofpositives,
         confirmedcases = townconfirmedcases, confirmeddeaths = townconfirmeddeaths,
         probablecases = townprobablecases, probabledeaths = townprobabledeaths) %>%
  mutate(date = as_date(lastupdatedate),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
         confirmedcases = as.numeric(confirmedcases),
         confirmeddeaths = as.numeric(confirmeddeaths),
         probablecases = as.numeric(probablecases),
         probabledeaths = as.numeric(probabledeaths),
         towncaserate = as.numeric(towncaserate),
         tests = as.numeric(tests),
         ratetested100k = as.numeric(ratetested100k),
         tests_positive = as.numeric(tests_positive),
         numberofnegatives = as.numeric(numberofnegatives),
         numberofindeterminates = as.numeric(numberofindeterminates)) %>%
  rename(per_100k = towncaserate) %>%
  select(-lastupdatedate,  -town_no) %>%
  group_by(town) %>%
  setup_ct_tables() %>%
  left_join(town_info %>% select(town, county, category, total_pop)) %>%
  group_by(town) %>%
  mutate(rnew_cases_per100k = rnew_cases / (total_pop / 100000),
         rnew_deaths_per100k = rnew_deaths / (total_pop / 100000))
dph_towns <- dph_towns %>% ungroup()
dph_towns$category <- factor(dph_towns$category,
                             c("Urban Core", "Urban Periphery",
                               "Wealthy", "Suburban", "Rural"))
# dph_towns$county <- factor(dph_towns$county,
#                             levels = c("Litchfield", "Hartford", "Tolland", "Windham", "Fairfield", "New Haven", "Middlesex", "New London"),
#                             labels = c("Litchfield County", "Hartford County", "Tolland County", "Windham County", "Fairfield County", "New Haven County", "Middlesex County", "New London County"))

towns_recent_weeks <- dph_towns %>%
  filter((date == (max(dph_towns$date) - 14)) | (date == max(dph_towns$date))) %>%
    group_by(town, category, county, total_pop) %>% arrange(date) %>%
  summarise(across(c(cases, deaths, tests_positive, tests),
                   ~ last(.x) - first(.x)), .groups = "drop") %>%
  mutate(hit_rate = tests_positive / tests)

counties_recent_weeks <- dph_counties %>%
  filter((date == (max(dph_counties$date) - 14)) | (date == max(dph_counties$date))) %>%
  group_by(county, total_pop) %>% arrange(date) %>%
  summarise(across(c(cases, deaths), ~ last(.x) - first(.x)), .groups = "drop")

# prison info:

# Locations in Connecticut
# Bridgeport Correctional Center (inmate population 712)
# Brooklyn Correctional Institution (inmate population 454) Brooklyn
# Cheshire Correctional Institution (inmate population 1322)
# Corrigan-Radgowski Correctional Center (inmate population 760) Montville
# Garner Correctional Institution (inmate population 584) Newtown
# Hartford Correctional Center (inmate population 933)
# MacDougall-Walker Correctional Institution (inmate population 1522) Suffield
# Manson Youth Institution (inmate population 322) Cheshire
# New Haven Correctional Center (inmate population 718)
# Northern Correctional Institution (inmate population 135) Somers
# Osborn Correctional Institution (inmate population 1322)  Somers
# Robinson Correctional Institution (inmate population 1449) Enfield
# Willard-Cybulski Correctional Institution (inmate population 1142) Enfield
# York Correctional Institution (inmate population 898) Niantic (women)

# code to look for anomalous data
# decreases <- dph_total %>% arrange(date) %>%
#   filter((lag(cases) > cases) | (cases > lead(cases)) |
#            (lag(deaths) > deaths) | (deaths > lead(deaths))) %>% select(date, cases, deaths)
#
# decreases_towns <- dph_towns %>% arrange(town, date) %>% group_by(town) %>%
#   filter((lag(cases) > cases) | (cases > lead(cases)) |
#            (lag(deaths) > deaths) | (deaths > lead(deaths))) %>% select(town, date, cases, deaths)

dph_total <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths,
         hospital = hospitalizedcases,
         tests = covid_19_tests_reported) %>%
  mutate(date = as_date(date),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
         tests = as.numeric(tests),
         tests_positive = as.numeric(confirmedcases),
         hospital = as.numeric(hospital),
         confirmedcases = as.numeric(confirmedcases),
         confirmeddeaths = as.numeric(confirmeddeaths),
         probablecases = as.numeric(probablecases),
         probabledeaths = as.numeric(probabledeaths)) %>%
  mutate_at(vars(starts_with("cases_")), as.numeric)
# NYT series may go back earlier than the state dataset
if (min(dph_total$date) != ymd("2020-03-08")) dph_total <- dph_total %>%
  bind_rows(
    nyt_series %>% filter(date < min(dph_total$date)) %>%
      select(cases, deaths, date) %>%
      group_by(date) %>%
      summarise(cases = sum(cases), deaths = sum(deaths), .groups = "drop") %>%
      ungroup() %>%
      mutate(hospital = NA_real_, state = "CONNECTICUT",
             cases_age0_9 = NA_real_, cases_age10_19 = NA_real_,
             cases_age20_29 = NA_real_, cases_age30_39 = NA_real_,
             cases_age40_49 = NA_real_, cases_age50_59 = NA_real_,
             cases_age60_69 = NA_real_,cases_age70_79 = NA_real_,
             cases_age80_older = NA_real_))
dph_total <- dph_total %>%
  setup_ct_tables()
# change so first we compute the new cases, next full_join to dates, then compute rolling averages

dph_total <- dph_total %>%
  arrange(date) %>%
  mutate_at(vars(starts_with("cases_")), as.numeric) %>%
  mutate(current_per_100k =  (state_info$total_pop[1] / 100000) / current_cases,
         rnew_cases_per100k = rnew_cases / (state_info$total_pop[1] / 100000),
         rnew_deaths_per100k = rnew_deaths / (state_info$total_pop[1] / 100000))

if (!exists("dph_nursing_facilities")) if (file.exists(paste0(path_to_ctcorona, "dph_nursing_facilities.RData"))) load(paste0(path_to_ctcorona, "dph_nursing_facilities.RData"))
if (!exists("dph_nursing_facilities")) {
  dph_nursing_facilities <- read.socrata("https://data.ct.gov/resource/rm6f-b9qj.json",
                                         app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
    as_tibble() %>%
    select(level_of_care, facility_name, address, town, county, state,
           medicare_certified, medicaid_certified, total_licensed_beds,
           licensed_ccnh_beds, licensed_ccnh_beds_occupied,
           licensed_rhns_beds, licensed_rhns_beds_occupied,
           X_18:unknown_age, male, female, unknown_gender,
           white:other_or_unknown, reporting_year, geocoded_column.type,
           geocoded_column.coordinates,
           ct_credential_number, federal_provider_number,
           rhns_room_rate_private_1_bed, rhns_room_rate_semi_private_2_beds)
  check_fed_id <- dph_nursing_facilities %>% filter(is.na(as.numeric(federal_provider_number)), reporting_year == max(reporting_year, na.rm = TRUE)) %>%
    select(federal_provider_number, ct_credential_number, facility_name)
  if (nrow(check_fed_id) > 1) stop("More than one row is missing federal provider number")
  nursing_link <- dph_nursing_facilities %>%
    filter(reporting_year == max(reporting_year, na.rm = TRUE)) %>%
    mutate(federal_provider_number = as.numeric(federal_provider_number)) %>%
    select(town, county, federal_provider_number, ct_credential_number) %>%
    unique()
  if (length(nursing_link$ct_credential_number) !=
      length(nursing_link$federal_provider_number)) stop("nursing_link IDs are not unique [daily_ct_stats.R]")
  # save(dph_nursing_facilities, nursing_link, file = paste0(path_to_ctcorona, "dph_nursing_facilities.RData"))
}

# dph_community seems to have changed API url
# Jill thinks Connecticut policy based on this:
# https://globalepidemics.org/wp-content/uploads/2020/06/key_metrics_and_indicators_v4.pdf?fbclid=IwAR34FFyEO8AedxRYKSRsKTYpH4mT1sZ4P-92_zrWX6E3siAWdv_mlSZp2Ls
# dph_community_old <- read.socrata("https://data.ct.gov/resource/ivm4-azet.json",
#                               app_token = Sys.getenv("CTDATA_APP1_TOKEN"))
#   as_tibble() %>%
#   rename(date = dateupdated) %>%
#   # left_join(town_info %>% select(town, total_pop), by = "town") %>%
#   mutate(date = as_date(date),
#          across(c(population, rate, tests), as.numeric),
#          raw_cases = cases,
#          raw_rate = rate,
#          week = epiweek(date - 7),
#          # week ending the previous Saturday
#          week_ending = floor_date(date, unit = "week", week_start = 6),
#          alt_cases = case_when(
#            cases == "<5" ~ rate * (population / 100000),
#            TRUE ~ as.numeric(cases)),
#          cases =  round(rate * (population / 100000), 0),
#          rate = (cases / population) * 100000,
#          daily_rate = rate / 7,
#          creation_date = max(date, na.rm = TRUE)
#   ) %>%
#   relocate(date, week_ending, town, daily_rate, cases, rate, raw_cases, raw_rate, alt_cases, population) %>%
#   left_join(town_geometries %>%
#               as_tibble() %>%
#               select(town, county, HS_only, district_number, district_type,
#                      district_name, total_pop), by = "town")
# dph_community  may change as new tests come in, so save a version each week
# save(dph_community, file = paste0(path_to_ctcorona, "dph_community_",
#                                   str_replace_all(max(dph_community$creation_date, na.rm = TRUE), "-", ""), ".RData"))

  dph_community <- read.socrata("https://data.ct.gov/resource/s22x-83rd.json",
                                app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
    as_tibble() %>%
    # left_join(town_info %>% select(town, total_pop), by = "town") %>%
    mutate(date = as_date(date),
           across(c(dailyratecases, newtests, pctpos), as.numeric),
           week = epiweek(date - 7),
           # week ending the previous Saturday
           week_ending = floor_date(date, unit = "week", week_start = 6),
           creation_date = max(date, na.rm = TRUE)
    ) %>%
    rename(rnew_cases_per100k = dailyratecases, town = city) %>%
    relocate(date, week_ending, town, rnew_cases_per100k) %>%
    left_join(town_geometries %>%
                as_tibble() %>%
                select(town, county, HS_only, district_number, district_type,
                       district_name, total_pop), by = "town")


# dph_nursing_cases has one row per nursing home per date of nursing home report
dph_nursing_cases <- read.socrata("https://data.ct.gov/resource/wyn3-qphu.json",
                                  app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date_last_updated),
                        nh_cases = as.numeric(residents_with_covid),
                        licensed_beds = as.numeric(licensed_beds),
                        nh_lab_confirmed_deaths = as.numeric(covid_19_associated_lab_confirmed),
                        nh_probable_deaths = as.numeric(covid_19_associated_deaths_probable),
                        nh_deaths = nh_lab_confirmed_deaths + nh_probable_deaths) %>%
  select(-date_last_updated, -covid_19_associated_lab_confirmed, -covid_19_associated_deaths_probable) %>%
  mutate(nh_cases = ifelse(is.na(nh_cases), 0, nh_cases),
         nh_deaths = ifelse(is.na(nh_deaths), 0, nh_deaths),
         nh_probable_deaths = ifelse(is.na(nh_probable_deaths), 0, nh_probable_deaths),
         nh_lab_confirmed_deaths = ifelse(is.na(nh_lab_confirmed_deaths), 0, nh_lab_confirmed_deaths))
# https://data.cms.gov/profile/edit/settings
# cms_nursing_direct <- read.socrata( "https://data.cms.gov/resource/s2uc-8wxp.json?provider_state=CT",
#                                     app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
#   as_tibble()

dph_cms_nursing_raw <- read.socrata("https://data.ct.gov/resource/w8wc-65i5.json",
                                app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(week_ending))
dph_cms_nursing <- dph_cms_nursing_raw %>%
  mutate(federal_provider_number = as.numeric(federal_provider_number)) %>%
  left_join(nursing_link %>%
              select(federal_provider_number, ct_credential_number,
                     town), by = "federal_provider_number") %>%
  select(date, federal_provider_number, ct_credential_number, town,
         starts_with("provider"), residents_weekly_confirmed,
         residents_weekly_suspected, residents_total_confirmed,
         residents_total_suspected, residents_weekly_all_deaths,
         residents_total_all_deaths, total_resident_covid_19_deaths,
         residents_weekly_covid_19, residents_total_covid_19,
         number_of_all_beds, total_number_of_occupied,
         county, geolocation.type, geolocation.coordinates)
#
# The earlier series (dph_nursing_cases) is cumulative counts.
# The later series (dph_cms_nursing) appears to have cumulative
# counts but they are not usable. We need to do both series as
# new cases in each reporting period and then combine that (allowing
# for differences in the number of days between periods).

# dph_cms_nursing %>% select(ct_credential_number, federal_provider_number, provider_name, provider_address, provider_city, provider_zip_code, residents_total_covid_19, date) %>% filter(!is.na(ct_credential_number), date == max(date))
# examine comparison of dph_cms_nursing and dph_nursing_cases

######### working here #############################
# asif_nursing_cases <- dph_cms_nursing %>%
#   select(ct_license_number = ct_credential_number, town, nursing_home = provider_name,
#          licensed_beds = number_of_all_beds,
#          date, nh_cases = residents_total_covid_19,
#          nh_lab_confirmed_deaths = total_resident_covid_19_deaths) %>%
#   mutate(across(c(licensed_beds, nh_cases,
#          nh_lab_confirmed_deaths), as.numeric))

# dph_nursing_cases %>% filter(ct_license_number == "1000C") %>%
#   select(date, nursing_home, nh_cases, nh_lab_confirmed_deaths) %>%
#   arrange(date)

# asif_nursing_cases %>% filter(ct_license_number == "1000C") %>%
#   select(date, nursing_home, nh_cases, nh_lab_confirmed_deaths)
# dph_cms_nursing %>% filter(ct_credential_number == "1000C") %>%
#   select(date, residents_weekly_confirmed, residents_total_confirmed, total_resident_covid_19_deaths) %>%
#   arrange(date)
# cms <- dph_cms_nursing %>% filter(ct_credential_number == "1000C") %>% arrange(date)
# state <- dph_nursing_cases %>% filter(ct_license_number == "1000C") %>% arrange(date)

nursing_combined <- dph_nursing_cases %>%
  mutate(source = "state",
         residents_weekly_confirmed = NA_real_,
         residents_weekly_covid_19_deaths = NA_real_,
         residents_total_suspected = NA_real_,
         provider_name = tolower(nursing_home),
         all_deaths = NA_real_) %>%
  select(source, ct_credential_number = ct_license_number, date,
         residents_weekly_confirmed, nh_cases, all_deaths,
         residents_weekly_covid_19_deaths,
         nh_deaths, nh_lab_confirmed_deaths, nh_probable_deaths,
         provider_name, town,
         licensed_beds) %>%
  bind_rows(dph_cms_nursing %>%
              mutate(nh_cases = as.numeric(residents_total_confirmed),
                     nh_deaths = as.numeric(residents_total_covid_19),
                     all_deaths = as.numeric(residents_total_all_deaths),
                     residents_weekly_confirmed = as.numeric(residents_weekly_confirmed),
                     residents_total_suspected = as.numeric(residents_total_suspected),
                     residents_weekly_covid_19_deaths = as.numeric(residents_weekly_covid_19),
                     source = "cms",
                     provider_name = tolower(provider_name),
                     nh_lab_confirmed_deaths = NA_real_,
                     nh_probable_deaths = NA_real_,
                     licensed_beds = as.numeric(number_of_all_beds)) %>%
              select(source, ct_credential_number, date, residents_weekly_confirmed,
                     residents_total_suspected, nh_cases, all_deaths,
                     residents_weekly_covid_19_deaths, nh_deaths,
                     provider_name, town, licensed_beds, nh_lab_confirmed_deaths, nh_probable_deaths))
  # View(nursing_combined %>% arrange(ct_credential_number, date))
  p_cases = ggplot(data = nursing_combined %>% group_by(date, source) %>%
           summarise(nh_cases = sum(nh_cases, na.rm = TRUE),
                     nh_deaths = sum(nh_deaths, na.rm = TRUE),
                     all_deaths = sum(all_deaths, na.rm = TRUE), .groups = "drop"),
         aes(x = date, y = nh_cases, colour = source))+
    geom_point() + geom_line()
byname <- nursing_combined %>% arrange(town, provider_name, date, source) %>%
  select(date, town, source, provider_name, nh_cases, nh_deaths)
elim_park <- nursing_combined %>% filter(str_detect(provider_name, "elim park baptist home")) %>% arrange(date, source)
riverside_cms <- dph_cms_nursing_raw %>% filter(str_detect(tolower(provider_name), "riverside health")) %>% arrange(date)
riverside <- nursing_combined %>% filter(str_detect(provider_name, "riverside health")) %>% arrange(date, source)
# town_with_nursing is dph_towns but only with the same dates as used for
# the nursing home reports. Idea is to be able to remove nursing homes from town data
town_with_nursing <- dph_towns %>%
  filter(date %in% unique(dph_nursing_cases$date)) %>%
  left_join(dph_nursing_cases %>% group_by(town, date) %>%
              summarise(nh_cases = sum(nh_cases), nh_probable_deaths = sum(nh_probable_deaths),
                        nh_lab_confirmed_deaths = sum(nh_lab_confirmed_deaths),
                        .groups = "drop") %>%
              mutate(nh_deaths = nh_lab_confirmed_deaths + nh_probable_deaths),
            by = c("date", "town")) %>%
  left_join(dph_nursing_facilities %>% group_by(town) %>%
              mutate(total_licensed_beds = as.numeric(total_licensed_beds), reporting_year = as.numeric(reporting_year)) %>%
              filter(reporting_year == max(reporting_year, na.rm = TRUE)) %>%
              summarise(total_licensed_beds = sum(total_licensed_beds, na.rm = TRUE),
                        .groups = "drop"),
            by = "town") %>%
  left_join(town_info %>% select(town, age_65_plus, age_65_plus_pct), by = "town") %>%
  # mutate(nh_cases = ifelse(is.na(nh_cases), 0, nh_cases),
  #        nh_deaths = ifelse(is.na(nh_deaths), 0, nh_deaths)) %>%
  mutate(nh_death_pct = if_else(!is.na(nh_deaths) & (deaths > 0), nh_deaths / deaths, NA_real_) %>% round(3)) %>%
  arrange(desc(nh_death_pct))

  # dph_assisted_living <- read.socrata("https://data.ct.gov/resource/wjua-euxh.json",
  #                                     app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  #   as_tibble() %>%
  #   mutate(date = as_date(date_last_updated),
  #          beds = as.numeric(total_number_of_beds),
  #          cases = as.numeric(residents_with_laboratory), # lab confirmed
  #          deaths_confirmed = as.numeric(covid_19_associated_deaths),
  #          deaths_probable = as.numeric(covid_19_associated_deaths_1),
  #          deaths = deaths_confirmed + deaths_probable)

  doc_covid <- read.socrata("https://data.ct.gov/resource/6t8i-du3u.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
    as_tibble() %>%
    mutate(date = as.Date(dateupdated), value = as.numeric(value),
           variable = case_when(
             str_detect(variable, "Inmates Pos") ~ "inmates_positive",
             str_detect(variable, "Northern CI") ~ "northern_med",
             str_detect(variable, "Inmates Medically Cleared") ~ "inmates_cleared",
             str_detect(variable, "Inmate Deaths") ~ "inmate_deaths",
             str_detect(variable, "Staff Pos") ~ "staff_positive",
             str_detect(variable, "Staff Returned") ~ "staff_returned",
             str_detect(variable, "Medical Iso\\. Unit") ~ "inmates isolated",
             TRUE ~ variable
           )) %>%
    select(-dateupdated) %>%
    pivot_wider(id_col = date, values_from = value, names_from = variable) %>%
    arrange(date)

  cdc_covid <- read.socrata("https://data.cdc.gov/resource/9mfq-cb36.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
    as_tibble() %>%
    mutate(date = as.Date(submission_date),
           positive = as.numeric(tot_cases),
           state_original = state,
           state = ifelse(state == "NYC", "NY", state)) %>%
    group_by(state, date) %>%
    summarise(positive = sum(positive, na.rm = TRUE), .groups = "drop") %>%
    left_join(state_pop %>% group_by(NAME, state) %>%
                summarise(state_pop = last(summary_est), .groups = "drop"),
              by = "state") %>%
    arrange(state, date) %>%
    filter(!(state %in% c("AS", "VI", "FSM", "GU", "MP", "PW", "RMI", "PR"))) %>%
    group_by(state) %>%
    mutate(cases_per100k = positive / (state_pop / 100000),
           new_cases = positive - lag(positive),
           rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
           renw_cases_per100k = rnew_cases  / (state_pop / 100000))



  # arrange(date) %>%
  # mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
  #        rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
  #        new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
  #        rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
  #        current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_),
  #        rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_))

  # compare New York Times with covid19 tracking project:
  # nyt_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
  #                       col_types = cols(date = col_date(format = "%Y-%m-%d")))
  # xx <- nyt_states %>% arrange(state, date) %>%
  #   filter(date == ymd("2020-06-30"))
  # cv <- covid19_project %>% filter(date == ymd("2020-06-30")) %>% select(state, positive, positive_cases_viral, NAME)
  # combined <- xx %>% left_join(cv %>% select(state_abbrev = state, state = NAME, positive))
  # combined %>% filter(abs(positive - cases) > 250) %>% select(state, positive, cases)
  # p <- ggplot(data = combined, aes(x = positive, y = cases)) + geom_label(aes(label = state_abbrev))

  covid19_project <- get_states_daily() %>%
    left_join(state_pop %>% group_by(NAME, state) %>%
                summarise(state_pop = last(summary_est), .groups = "drop"),
              by = "state") %>%
    arrange(state, date) %>%
    group_by(state) %>%
    mutate(cases_per100k = positive / (state_pop / 100000),
           new_cases = positive - lag(positive),
           rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
           renw_cases_per100k = rnew_cases  / (state_pop / 100000))

ct_vac_county <- read.socrata("https://data.ct.gov/resource/5g42-tpzq.json",
                              app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(county = county_of_residence) %>%
  mutate(date = as_date(date),
         population = as.numeric(population)) %>%
  mutate_at(vars(contains("vacci")), as.numeric) %>%
  filter(!(county %in% c("Residence out of state", "Address pending validation")))

ct_vac_town <- read.socrata("https://data.ct.gov/resource/pdqi-ds7f.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date_updated)) %>%
  mutate_at(vars(contains("population")), as.numeric) %>%
  mutate_at(vars(contains("first_dose")), as.numeric) %>%
  rename(svi = at_least_one_census_tract,
         first_dose = first_dose_coverage,
         population = estimated_population,
         pop65_74 = estimated_population_aged_65_to_74,
         pop75over = estimated_population_aged_75_and_above,
         first_dose65_74 = first_doses_administered_age_65_to_74,
         first_dose65_74_pct = first_dose_coverage_age_65_to_74,
         first_dose75over = first_doses_administered_age_75_and_over,
         first_dose75over_pct = first_dose_coverage_age_75_and_over) %>%
  mutate(first_dose65over_pct = if_else((pop65_74 + pop75over) > 0,
                                        (first_dose65_74 + first_dose75over) / (pop65_74 + pop75over),
                                        NA_real_))

ct_vac_age <- read.socrata("https://data.ct.gov/resource/vjim-iz5e.json",
                              app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date),
         population = as.numeric(population)) %>%
  mutate_at(vars(contains("vacci")), as.numeric)

ct_vac_state <- read.socrata("https://data.ct.gov/resource/tttv-egb7.json",
                           app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date_updated),
         population = as.numeric(population)) %>%
  select(-date_updated) %>%
  mutate_at(vars(contains("vacci")), as.numeric)

ct_vac_race <- read.socrata("https://data.ct.gov/resource/xkga-ifz3.json",
                              app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date_reported = as_date(date_reported),
         population = as.numeric(population)) %>%
  mutate_at(vars(contains("vacci")), as.numeric) %>%
  rename(date = date_reported)

# # compare cdc and covid_tracking
# p_compare <- ggplot(data = cdc_covid %>% filter(state == "CT"), aes(x = date, y = rnew_cases)) +
#   geom_point(colour = "blue") +
#   geom_point(data = covid19_project %>% filter(state == "CT"), colour = "red")

usethis::ui_info("Most recent statewide data is {ui_value(max(dph_total$date, na.rm = TRUE))}. Earliest is {ui_value(min(dph_total$date, na.rm = TRUE))}.")
if ((dph_total %>% count(date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("dph_total contains multiple rows on the same date.")

last_date <- max(dph_total$date)

usethis::ui_info("Last date seen: {usethis::ui_value(last_date)}. Earliest is {ui_value(min(dph_counties$date, na.rm = TRUE))}.")
usethis::ui_info("cases          :            {usethis::ui_value(dph_total$cases[dph_total$date == last_date])}  +{usethis::ui_value(dph_total$new_cases[dph_total$date == last_date])}")
usethis::ui_info("deaths         :            {usethis::ui_value(dph_total$deaths[dph_total$date == last_date])}  +{usethis::ui_value(dph_total$new_deaths[dph_total$date == last_date])}")
usethis::ui_info("tests          :            {usethis::ui_value(dph_total$tests[dph_total$date == last_date])}  +{usethis::ui_value(dph_total$new_tests[dph_total$date == last_date])}")
usethis::ui_info("percent positive:            {usethis::ui_value(round(dph_total$new_cases[dph_total$date == last_date] / dph_total$new_tests[dph_total$date == last_date], 3))} ")
usethis::ui_info("Confirmed hospitalizations: {usethis::ui_value(dph_total$hospital[dph_total$date == last_date])}  {usethis::ui_value(dph_total$hospital[dph_total$date == (last_date - 1)])}")

dph_age <- read.socrata("https://data.ct.gov/resource/ypz6-8qyf.json",
                         app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths, per_100k = totalcaserate) %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths)) %>%
  select(-dateupdated) %>%
  group_by(agegroups) %>%
  mutate(tests = 0, tests_positive = 0, hospital = 0) %>%
  setup_ct_tables()

# dph_counties <- dph_counties %>%
#   group_by(county) %>%
#   setup_ct_tables()

dph_gender <- read.socrata("https://data.ct.gov/resource/qa53-fghg.json",
                       app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths, per_100k = totalcaserate) %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths)) %>%
  select(-dateupdated)

if ((max(dph_total$date) != max(dph_counties$date)) |
    (max(dph_total$date) != max(dph_towns$date)) |
    (max(dph_total$date) != max(dph_age$date)) |
    (max(dph_total$date) != max(dph_gender$date)))
  usethis::ui_stop(paste("CT DPH dates. \ntotal:", max(dph_total$date), " counties:", max(dph_counties$date),
                          " towns:", max(dph_towns$date), " age:", max(dph_age$date), " gender:", max(dph_gender$date)))

# there's also zip code level monitoring at: https://data.ct.gov/resource/javn-ujwr.json

exec_orders <- tibble(
  date = as.Date(c("2020-03-10", "2020-03-17", "2020-03-23", "2020-03-26", "2020-04-11", "2020-04-20")),
  label = c("prohibit large\ngatherings", "cancel\nclasses", "restrict\nbusiness", "5 or fewer",
            "covid-19 death\nchanged", "masks required")
) %>%
  left_join(dph_counties %>% filter(county == "Fairfield") %>% select(date, cases), by = "date") %>%
  mutate(county = "Fairfield")

save(dph_reports, dph_total, dph_towns, dph_counties, dph_nursing_cases,
     dph_age, town_with_nursing, dph_assisted_living,
     towns_recent_weeks, counties_recent_weeks, doc_covid,
     covid19_project, cdc_covid,
     ct_vac_state, ct_vac_county, ct_vac_town, ct_vac_age, ct_vac_race,
     file = paste0(path_to_ctcorona, "dph_datasets.RData"))
finish_loading_socrata_data <- Sys.time()

# Setup week ranges starting with most recent week and working backward
nweeks = 5
week_setup <- tibble(
  start_period = (max(dph_counties$date) + 1) - weeks(seq(nweeks, 1, -1)),
) %>%
  mutate(end_period = start_period + days(6),
         week = str_c(str_sub(start_period, 6, 10), " to ", str_sub(end_period, 6, 10)),
         week = str_replace_all(week, "-", "/"))

ct <- dph_counties %>%
  mutate(county = fct_reorder2(county, date, rcases)) %>%
  fuzzyjoin::interval_left_join(week_setup,
                                by = c("date" = "start_period", "date" = "end_period")) %>%
  # left_join(county_info %>% ungroup() %>% select(county, total_pop, age_65_plus), by = "county") %>%
  mutate(cases_per_100k =  current_cases /(total_pop / 100000))
# town_history pulls out the data in weekly chunks
town_history <- dph_towns %>%
  left_join(week_setup, by = c("date" =  "end_period")) %>%
  filter(!is.na(week)) %>%
  left_join(town_geometries, by = "town")
the_scale <- c(0, 0.02, 0.05, 0.08, 1, 1.5)
p <- ggplot(data = town_history) +
  geom_sf(data = town_history, aes(fill = rnew_cases_per100k, geometry = geometry)) +
  facet_wrap(~ week) +
  # scale_fill_gradient(low="lightblue", high="red",
  #                   breaks= the_scale,
  #                   limits=c(0, 1.5), guide = "legend")
  scale_fill_gradient(limits = c(0, 1.5), breaks = the_scale)
  scale_fill_continuous(#low="lightblue", high="red",
                      # breaks= the_scale,
                      guide = "legend")

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
ct <- ct %>%
  mutate(county = factor(county,
                         levels = c("Litchfield", "Hartford", "Tolland", "Windham", "Fairfield", "New Haven", "Middlesex", "New London")),
         week = fct_rev(factor(week)))

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

county_centroid <- st_centroid(county_geometries) %>%
  left_join(dph_counties %>% filter(date == max(date, na.rm = TRUE)) %>%
              select(county, rnew_cases_per100k), by = "county")
county_map <- ggplot() +
  geom_sf(data = county_geometries %>%
            left_join(dph_counties %>% filter(date == max(date, na.rm = TRUE)) %>%
                        select(county, rnew_cases_per100k),
                      by = "county"),
          # need aes(geometry = geometry) because joined sf to a tibble and so lost sf
          aes(fill = rnew_cases_per100k, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "grey") +
  geom_sf_text(data = county_centroid, aes(label = paste0(county, "\n", round(rnew_cases_per100k, 3), " per 100K")), color = "black", size = 3) +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal()  +
  # theme(legend.position = "none") +
  labs(title = "New Cases by Connecticut County",
       subtitle = "per 1K of population as of recent report date",
       fill = "cases per 1K",
       caption = "Source: US Census, tidycensus package")
# > dph_counties %>% group_by(county) %>% filter(date == max(date)) %>% select(county, cases, rnew_cases, rnew_cases_per100k, total_pop)
# # A tibble: 8 x 5
# # Groups:   county [8]
# county     cases rnew_cases rnew_cases_per100k total_pop
# <chr>      <dbl>      <dbl>            <dbl>     <dbl>
#   1 Fairfield  16277      36.7            0.0389    944348
# 2 Hartford   11189      63.1            0.0706    894730
# 3 Litchfield  1439       3.71           0.0203    183031
# 4 Middlesex   1223       7              0.0428    163368
# 5 New Haven  12021      29.1            0.0339    859339
# 6 New London  1198      12.3            0.0457    268881
# 7 Tolland      885       3              0.0198    151269
# 8 Windham      463       6.71           0.0576    116538

# doubling_cases <- ct %>%
#   filter(!is.na(week), cases > 0) %>%
#   group_by(county, week) %>%
#   nest() %>%
#   mutate(
#     data_days = map(data, nrow),
#     fit_cases = map(data, ~ lm(log(cases) ~ date, data = .x)),
#     tidied_cases = map(fit_cases, tidy),
#     glance_cases = map(fit_cases, glance)
#   ) %>%
#   unnest(tidied_cases) %>%
#   filter(data_days >= 5) %>%
#   select(county, week, data, fit_cases, term, estimate) %>%
#   pivot_wider(values_from = estimate, names_from = term) %>%
#   mutate(doubling = log(2) / log(1 + date))
#
# doubling_deaths <- ct %>%
#   filter(!is.na(week), deaths > 0) %>%
#   group_by(county, week) %>%
#   nest() %>%
#   mutate(
#     data_days = map(data, nrow),
#     fit_deaths = map(data, ~lm(log(deaths) ~ date, data = .x )),
#     tidied_deaths = map(fit_deaths, tidy),
#     glance_deaths = map(fit_deaths, glance)
#   ) %>%
#   unnest(tidied_deaths) %>%
#   filter(data_days >= 5) %>%
#   select(county, week, data, fit_deaths, term, estimate, data_days) %>%
#   pivot_wider(values_from = estimate, names_from = term) %>%
#   mutate(doubling = log(2) / log(1 + date))
# doubling <-
#   bind_rows(
#     doubling_cases %>%
#       select(county, week = week, daily_rate = date, doubling) %>%
#       mutate(measure = "cases"),
#     doubling_deaths %>%
#       select(county, week = week, daily_rate = date, doubling) %>%
#       mutate(measure = "deaths")
#   )

# p_doubling <- ggplot(data = doubling %>% filter(doubling < 9), aes(x = week, y = doubling, fill = doubling)) +
#   geom_col(position = position_dodge2(reverse = TRUE, padding = 0.1)) +
#   coord_flip() +
#   xlab(NULL) + ylab("doubling time (in days)") +
#   facet_wrap(measure ~ county, ncol = 4) +
#   labs(title = "Doubling Time of Cumulative Count of Confirmed Cases",
#        subtitle = "(longer doubling time is better!)",
#        caption = "Doubling time is the number of days for the cumulative count to double.") +
#   theme(legend.position = "none")

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
# ggsave("doubling.png", plot = p_doubling, path = path_to_post,
#        width = 7, height = 8, units = "in")


final_processing_time <- Sys.time()
total_time <- final_processing_time - start_time
socrata_time <- finish_loading_socrata_data - start_loading_socrata_data
usethis::ui_info("socrata time: {usethis::ui_value(round(socrata_time, 1))} ")
usethis::ui_info("total time: {usethis::ui_value(round(total_time, 1))} ")

source("R/render_town_summary_html.R")
