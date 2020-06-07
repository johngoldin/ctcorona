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
path_to_ctcorona <- "~/Documents/R_local_repos/ctcorona/"
# from https://www.ctdatahaven.org/sites/ctdatahaven/files/UConnCPR%20Changing%20Demographics-5%20CTs%202004.pdf
# The Changing Demographics of Connecticut — 1990 to 2000. by Center for Population Research  May. 31, 2004
Five_Connecticuts <- read_delim("Five_Connecticuts.txt","\t", escape_double = FALSE, trim_ws = TRUE)


if (!exists("town_geometries") &
    file.exists(paste0(path_to_ctcorona, "census_population.RData"))) load(paste0(path_to_ctcorona, "census_population.RData"))
if (!exists("town_geometries")) {
#   acs_vars <- load_variables(2017, "acs5", cache = TRUE) %>%
#   mutate(table_id = str_sub(name, 1, 6),
#          # Race generally is in parentheses after the concept name.
#          # But for a few cases, something else is in parentheses first. So I
#          # am going to blank out that stuff and then assume whatever I find inside
#          # of parentheses is race.
#          concept = str_replace_all(concept,
#            c("\\(IN 2017 INFLATION-ADJUSTED DOLLARS\\)" = "",
#              "\\(EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS\\)" = "",
#              "\\(SSI\\)" = "",
#              "\\(INCLUDING LIVING ALONE\\)" = "",
#              "\\(IN MINUTES\\)" = "",
#              "\\(DOLLARS\\)" = "",
#              "\\(CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI\\)" = "--CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI--",
#              "\\(CAR, TRUCK, OR VAN\\)" = "--CAR, TRUCK, OR VAN--",
#              "\\(\\)" = ""
#          )),
#          race = str_extract(concept, "\\(.+\\)"),
#          race = str_replace(race, "\\(", ""),
#          race = str_replace(race, "\\)", ""))
#          # I should have been able to do this in one line, but it doesn't seem to work:
#          # race = str_extract(concept, "\\((.*?)\\)"))
# B17010_variables <- vars %>%
#   filter(table_id == "B17010", is.na(race)) %>%
#   pluck("name")
  # test_function <- function(df, var_name = item){
  #   df %>%
  #     mutate({{var_name}} := 1, "{{ var_name }}_two" := 2)
  # }
  # test_function(college_plus_state_acs, var_name = stuff)
  # test2 <- fetch_acs(variables = vars_65_plus, var_for_summary = "B01001_001", geography = "county", var_name = age_65_plus)
  # test <- get_acs(variables = vars_65_plus, geography = "county", survey = "acs5", geometry = FALSE, summary_var = "B01001_001")
  vars_65_plus <- paste0("B01001_0", c(20:25, 44:49))
  vars_under_25 <- c(paste0("B01001_00", c(3:9)), paste0("B01001_0", c(10, 27:34)))
  vars_enrolled_student <- paste0("B14004_0", c("03", "08", "19", "24"))
  # B19013001 is median household income CPI adjusted dollars
  # B11001001 is total number of households
  vars_college_plus <- c("B15003_022", "B15003_023", "B15003_024", "B15003_025") # total B15003_001   college or grad, 25 years and older
  vars_poverty <- "B17001_002" # "B17001_001" is total
  source("R/fetch_acs.R")

  college_plus_state_acs <- fetch_acs(variables = vars_college_plus, var_for_summary = "B15003_001",
                           geography = "state", var_name = college_plus)
  college_plus_county_acs <- fetch_acs(variables = vars_college_plus, var_for_summary = "B15003_001",
                           geography = "county", var_name = college_plus)
  college_plus_town_acs <- fetch_acs(variables = vars_college_plus, var_for_summary = "B15003_001",
                           geography = "county subdivision", var_name = college_plus)

  poverty_state_acs <- fetch_acs(variables = "B17001_002", var_for_summary = "B17001_001",
                           geography = "state", var_name = poverty)
  poverty_county_acs <- fetch_acs(variables = "B17001_002", var_for_summary = "B17001_001",
                           geography = "county", var_name = poverty)
  poverty_town_acs <- fetch_acs(variables = "B17001_002", var_for_summary = "B17001_001",
                           geography = "county subdivision", var_name = poverty)
  family_inc_state_acs <- fetch_acs(variables = "B19113_001", var_for_summary = "B19113_001",
                           geography = "state", var_name = family_income) %>%
    select(-family_income_pct, -family_income_pct_moe, -total_pop)
  family_inc_county_acs <- fetch_acs(variables = "B19113_001", var_for_summary = "B19113_001",
                           geography = "county", var_name = family_income)  %>%
    select(-family_income_pct, -family_income_pct_moe, -total_pop)
  family_inc_town_acs <- fetch_acs(variables = "B19113_001", var_for_summary = "B19113_001",
                           geography = "county subdivision", var_name = family_income)  %>%
    select(-family_income_pct, -family_income_pct_moe, -total_pop)
  hh_inc_state_acs <- fetch_acs(variables = "B19013_001", var_for_summary = "B19013_001",
                           geography = "state", var_name = hh_income) %>%
    select(-hh_income_pct, -hh_income_pct_moe, -total_pop)
  hh_inc_county_acs <- fetch_acs(variables = "B19013_001", var_for_summary = "B19013_001",
                           geography = "county", var_name = hh_income)  %>%
    select(-hh_income_pct, -hh_income_pct_moe, -total_pop)
  hh_inc_town_acs <- fetch_acs(variables = "B19013_001", var_for_summary = "B19013_001",
                           geography = "county subdivision", var_name = hh_income)  %>%
    select(-hh_income_pct, -hh_income_pct_moe, -total_pop)
  old_65_state_acs <- fetch_acs(variables = vars_65_plus, var_for_summary = "B01001_001",
                           geography = "state", var_name = age_65_plus)
  old_65_county_acs <- fetch_acs(variables = vars_65_plus, var_for_summary = "B01001_001",
                           geography = "county", var_name = age_65_plus)
  old_65_town_acs <- fetch_acs(variables = vars_65_plus, var_for_summary = "B01001_001",
                           geography = "county subdivision", var_name = age_65_plus)
  young_state_acs <- fetch_acs(variables = vars_under_25, var_for_summary = "B01001_001",
                           geography = "state", var_name = under_25)
  young_county_acs <- fetch_acs(variables = vars_under_25, var_for_summary = "B01001_001",
                           geography = "county", var_name = under_25)
  young_town_acs <- fetch_acs(variables = vars_under_25, var_for_summary = "B01001_001",
                           geography = "county subdivision", var_name = under_25)
  # of population over 25, percent college degree or higher
  college_state_acs <- fetch_acs(variables = vars_enrolled_student, var_for_summary = "B01001_001",
                           geography = "state", var_name = coll_student)
  college_county_acs <- fetch_acs(variables = vars_enrolled_student, var_for_summary = "B01001_001",
                           geography = "county", var_name = coll_student)
  college_town_acs <- fetch_acs(variables = vars_enrolled_student, var_for_summary = "B01001_001",
                           geography = "county subdivision", var_name = coll_student)
  state_info <- college_plus_state_acs %>%
    select(-total_pop) %>%
    left_join(poverty_state_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(family_inc_state_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(hh_inc_state_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(old_65_state_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(young_state_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(college_state_acs %>% select(-total_pop),
              by = c("GEOID", "NAME"))
  county_info <- college_plus_county_acs %>%
    select(-total_pop) %>%
    left_join(poverty_county_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(family_inc_county_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(hh_inc_county_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(old_65_county_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(young_county_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(college_county_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    rename(county = NAME)
  # do town and county geometries here because I need it to fetch county name
  county_geometries <- tigris::counties(state = "CT", cb = FALSE) %>%
    select(-NAME) %>%
    mutate(INTPTLAT = as.numeric(INTPTLAT), INTPTLON = as.numeric(INTPTLON)) %>%
    left_join(county_info, by = "GEOID") %>%
    mutate(density = total_pop / (ALAND / 2589988.1103))
  town_geometries_save <- tigris::county_subdivisions(state = "CT", cb = FALSE) %>%
    filter(NAME != "County subdivisions not defined") %>%
    mutate(INTPTLAT = as.numeric(INTPTLAT), INTPTLON = as.numeric(INTPTLON)) %>%
    left_join(county_geometries %>% as_tibble() %>% select(COUNTYFP, county), by = "COUNTYFP")
  town_info <- college_plus_town_acs %>%
    select(-total_pop) %>%
    left_join(poverty_town_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(family_inc_town_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(hh_inc_town_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(old_65_town_acs,
              by = c("GEOID", "NAME")) %>%
    left_join(young_town_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(college_town_acs %>% select(-total_pop),
              by = c("GEOID", "NAME")) %>%
    left_join(Five_Connecticuts, by = c("NAME" = "town")) %>%
    left_join(town_geometries_save %>% as_tibble() %>% select(NAME, county, ALAND), by = "NAME") %>%
    mutate(density = total_pop / (ALAND / 2589988.1103)) %>%
    rename(town = NAME)

  town_geometries <- town_geometries_save %>%
    select(-NAME) %>%
    left_join(town_info, by = "GEOID")
  town_categories <- town_info %>% group_by(county, category) %>%

  save(county_geometries, town_geometries, town_info, county_info, state_info, file = paste0(path_to_ctcorona, "census_population.RData"))
  # load(paste0(path_to_ctcorona, "census_population.RData"))
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
}

if (!exists("nyt_series")) load(paste0(path_to_ctcorona, "nyt_series.RData"))
if (!exists("nyt_series")) {
  # from https://github.com/nytimes/covid-19-data
  nyt_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  nyt_series <- nyt_counties %>%
    filter(state == "Connecticut")
  # save(nyt_series, file = "paste0(path_to_ctcorona, nyt_series.RData"))
}

# ct covid data resources: https://data.ct.gov/stories/s/wa3g-tfvc
dph_counties <- read.socrata("https://data.ct.gov/resource/bfnu-rgqt.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths) %>%
  mutate(date = as_date(dateupdated),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
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

# at this point we should have full
# do rolling average
dph_counties <- dph_counties %>%
  arrange(county, date) %>%
  group_by(county) %>%
  left_join(county_info %>% select(county, total_pop), by = "county") %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_),
         rnew_cases_per1k = rnew_cases / (total_pop / 1000),
         rnew_deaths_per1k = rnew_deaths / (total_pop / 1000))
dph_counties <- dph_counties %>% ungroup()


dph_towns <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = towntotalcases, deaths = towntotaldeaths,
         confirmedcases = townconfirmedcases, confirmeddeaths = townconfirmeddeaths,
         probablecases = townprobablecases, probabledeaths = townprobabledeaths) %>%
  mutate(date = as_date(lastupdatedate),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
         confirmedcases = as.numeric(confirmedcases),
         confirmeddeaths = as.numeric(confirmeddeaths),
         probablecases = as.numeric(probablecases),
         probabledeaths = as.numeric(probabledeaths),
         towncaserate = as.numeric(towncaserate),
         peopletested = as.numeric(peopletested),
         ratetested100k = as.numeric(ratetested100k), numberoftests = as.numeric(numberoftests),
         numberofpositives = as.numeric(numberofpositives),
         numberofnegatives = as.numeric(numberofnegatives),
         numberofindeterminates = as.numeric(numberofindeterminates)) %>%
  rename(per_100k = towncaserate) %>%
  select(-lastupdatedate,  -town_no) %>%
  arrange(town, date) %>%
  left_join(town_info %>% select(town, county, category, total_pop)) %>%
  group_by(town) %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_),
         rnew_cases_per1k = rnew_cases / (total_pop / 1000),
         rnew_deaths_per1k = rnew_deaths / (total_pop / 1000))
dph_towns <- dph_towns %>% ungroup()

dph_total <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                            app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths,
         hospital = hospitalizedcases) %>%
  mutate(date = as_date(date),
         cases = as.numeric(cases), deaths = as.numeric(deaths),
         covid_19_pcr_tests_reported = as.numeric(covid_19_pcr_tests_reported),
         hospital = as.numeric(hospital),
         confirmedcases = as.numeric(confirmedcases),
         confirmeddeaths = as.numeric(confirmeddeaths),
         probablecases = as.numeric(probablecases),
         probabledeaths = as.numeric(probabledeaths)) %>%
   mutate_at(vars(starts_with("cases_")), as.numeric) %>%
  mutate(rcases = roll_mean(cases, 7, align = "right", fill = NA_real_),
         rdeaths = roll_mean(deaths, 7, align = "right", fill = NA_real_),
         new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths),
         rnew_cases = roll_mean(new_cases, 7, align = "right", fill = NA_real_),
         current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_),
         current_per_100k =  (state_info$total_pop[1] / 100000) / current_cases,
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_),
         rnew_cases_per1k = rnew_cases / (state_info$total_pop[1] / 1000),
         rnew_deaths_per1k = rnew_deaths / (state_info$total_pop[1] / 1000))
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
           ct_full_credential_code, ct_credential_number,
           rhns_room_rate_private_1_bed, rhns_room_rate_semi_private_2_beds)
  # save(dph_nursing_facilities, file = paste0(path_to_ctcorona, "dph_nursing_facilities.RData"))
}

# dph_nursing_cases has one row per nursing home per date of nursing home report
dph_nursing_cases <- read.socrata("https://data.ct.gov/resource/wyn3-qphu.json",
                                  app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  mutate(date = as_date(date_last_updated), nh_cases = as.numeric(residents_with_covid),
                        licensed_beds = as.numeric(licensed_beds),
                        lab_confirmed = as.numeric(covid_19_associated_lab_confirmed),
                        nh_deaths = as.numeric(covid_19_associated_deaths_probable)) %>%
  select(-date_last_updated, -covid_19_associated_lab_confirmed, -covid_19_associated_deaths_probable) %>%
  mutate(nh_cases = ifelse(is.na(nh_cases), 0, nh_cases),
         nh_deaths = ifelse(is.na(nh_deaths), 0, nh_deaths))
# town_with_nursing is dph_towns but only with the same dates as used for
# the nursing home reports. Idea is to be able to remove nursing homes from town data
town_with_nursing <- dph_towns %>%
  filter(date %in% unique(dph_nursing_cases$date)) %>%
  left_join(dph_nursing_cases %>% group_by(town, date) %>%
              summarise(nh_cases = sum(nh_cases), nh_deaths = sum(nh_deaths), beds = sum(licensed_beds)),
            by = c("date", "town")) %>%
  left_join(town_info %>% select(town, age_65_plus, age_65_plus_pct), by = "town") %>%
  mutate(nh_cases = ifelse(is.na(nh_cases), 0, nh_cases),
         nh_deaths = ifelse(is.na(nh_deaths), 0, nh_deaths)) %>%
  mutate(nh_death_pct = if_else(deaths > 0, nh_deaths / deaths, NA_real_) %>% round(3)) %>%
  arrange(desc(nh_death_pct))



# NYT series may go back earlier than the state dataset
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
         current_cases = roll_sum(new_cases, 14, align = "right", fill = NA_real_),
         rnew_deaths = roll_mean(new_deaths, 7, align = "right", fill = NA_real_))

usethis::ui_info("Most recent statewide data is {ui_value(max(dph_total$date, na.rm = TRUE))}. Earliest is {ui_value(min(dph_total$date, na.rm = TRUE))}.")
if ((dph_total %>% count(date) %>% filter(n > 1) %>% nrow()) > 0) usethis::ui_oops("dph_total contains multiple rows on the same date.")

save(dph_total, dph_towns, dph_counties, dph_nursing_cases, town_with_nursing, file = "dph_datasets.RData")

last_date <- max(dph_total$date)
usethis::ui_info("Last date seen: {usethis::ui_value(last_date)}. Earliest is {ui_value(min(dph_counties$date, na.rm = TRUE))}.")
usethis::ui_info("Confirmed cases:            {usethis::ui_value(dph_total$cases[dph_total$date == last_date])}  +{usethis::ui_value(dph_total$new_cases[dph_total$date == last_date])}")
usethis::ui_info("Confirmed deaths:           {usethis::ui_value(dph_total$deaths[dph_total$date == last_date])}  +{usethis::ui_value(dph_total$new_deaths[dph_total$date == last_date])}")
usethis::ui_info("Confirmed hospitalizations: {usethis::ui_value(dph_total$hospital[dph_total$date == last_date])}  {usethis::ui_value(dph_total$hospital[dph_total$date == (last_date - 1)])}")

dph_age <- read.socrata("https://data.ct.gov/resource/ypz6-8qyf.json",
                         app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
  as_tibble() %>%
  rename(cases = totalcases, deaths = totaldeaths, per_100k = totalcaserate) %>%
  mutate(date = as_date(dateupdated), cases = as.numeric(cases),
         deaths = as.numeric(deaths)) %>%
  select(-dateupdated)

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
town_history <- dph_towns %>%
  left_join(week_setup, by = c("date" =  "end_period")) %>%
  filter(!is.na(week)) %>%
  left_join(town_geometries, by = "town")
p <- ggplot(data = town_history) +
  geom_sf(data = town_history, aes(fill = rnew_cases, geometry = geometry)) +
  facet_wrap(~ week)


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
              select(county, rnew_cases_per1k), by = "county")
county_map <- ggplot() +
  geom_sf(data = county_geometries %>%
            left_join(dph_counties %>% filter(date == max(date, na.rm = TRUE)) %>%
                        select(county, rnew_cases_per1k),
                      by = "county"),
          # need aes(geometry = geometry) because joined sf to a tibble and so lost sf
          aes(fill = rnew_cases_per1k, geometry = geometry)) +
  scale_fill_gradient(low = "white", high = "grey") +
  geom_sf_text(data = county_centroid, aes(label = paste0(county, "\n", round(rnew_cases_per1k, 4), " per 1K")), color = "black", size = 3) +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal()  +
  # theme(legend.position = "none") +
  labs(title = "Cumulative Confirmed Cases by Connecticut County",
       subtitle = "cases per 1K of population for most recent report date",
       fill = "cases per 1K",
       caption = "Source: US Census, tidycensus package")


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

