state_abbreviations <- read_csv("data/state_abbreviations.txt",
                                col_names = FALSE)
names(state_abbreviations) <- c("state_name", "state")
state_pop <-  get_acs(geography = "state",
                      # state = "CT",
                      geometry = "FALSE", # no map at this time
                      survey = "acs1",
                      variables = paste0("B01001_0", c(20:25, 44:49)),
                      summary_var = "B01001_001") %>%
  filter(estimate > 0) %>%
  group_by(NAME, GEOID) %>%
  left_join(state_abbreviations, by = c("NAME" = "state_name"))
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

town_info$category <- factor(town_info$category,
                             c("Urban Core", "Urban Periphery",
                               "Wealthy", "Suburban", "Rural"))

town_geometries <- town_geometries_save %>%
  select(-NAME, -county, -ALAND) %>%
  left_join(town_info, by = "GEOID")
# town_categories <- town_info %>% group_by(county, category) %>%

# population by town from 2020 census redistricting dataset
total2020 <- get_decennial(variables = c("P3_001N"), # P3_001N us 18 and over,
                           summary_var = "P1_001N",
                           geography = "county subdivision",
                           # county = "New Haven",
                           # sumfile = "SF3",
                           state = "CT", cache = TRUE, year = 2020) |>
  mutate(town = str_extract(NAME,".* town") |> str_replace(" town", ""),
         pct_under18 = (summary_value - value) / summary_value) |>
  rename(pop2020 = summary_value, over17 = value) |>
  filter(pop2020 > 0) |>
  select(-GEOID, -variable, -NAME)

source("R/setup_school_info.R")

save(county_geometries, town_geometries, town_info, county_info,
     state_info, state_pop, total2020,
     file = paste0(path_to_ctcorona, "census_population.RData"))
# load(paste0(path_to_ctcorona, "census_population.RData"))
