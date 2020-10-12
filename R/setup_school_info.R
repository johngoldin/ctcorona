

region_additions <- tribble(
  ~district, ~town,
  "06", "Warren",
  "06", "Goshen",
  "06", "Morris",
  "10", "Burlington",
  "10", "Harwinton",
  "12", "Bridgewater",
  "12", "Roxbury",
  "12", "Washington",
  "13", "Middlefield",
  "13", "Durham",
  "14", "Bethlehem",
  "14", "Woodbury",
  "15", "Middlebury",
  "15", "Southbury",
  "16", "Prospect",
  "16", "Beacon Falls",
  "17", "Haddam",
  "17", "Killingworth",
  "18", "Lyme",
  "18", "Old Lyme",
) %>% mutate(district = paste("Regional High School District", district))

# ctschools <- read.socrata("https://data.ct.gov/resource/9k2y-kqxn.json",
#                             app_token = Sys.getenv("CTDATA_APP1_TOKEN")) %>%
#   as_tibble()

library(tigris)
if (!("district_number" %in% names(town_geometries))) {
  tigris_schools <- school_districts("Connecticut", class = "sf")
  tigris_secondary <- school_districts("Connecticut", class = "sf", type = "secondary")
  tigris_elementary <- school_districts("Connecticut", class = "sf", type = "elementary")

  # tigris_schools, have high school and not in regional
  tigris_schools <- tigris_schools %>%
    mutate(town = str_replace(NAME, " School District", "")) %>%
    filter(!str_detect(NAME, "Regional")) %>%
    filter(!str_detect(NAME, "Not Defined"))

  # ggplot(town_geometries) +
  #   geom_sf(color = "gray") +
  #   geom_sf(data = tigris_schools, colour = "red", fill = NA) +
  #   geom_sf(data = tigris_secondary, colour = "blue", fill = NA)

  # these are the regional school districts: K thru 12
  regional <- st_join(
    tigris_secondary,
    town_geometries,
    join = st_contains,
    suffix = c(".town", ".school"),
    left = FALSE,
    largest = FALSE
  ) %>% as_tibble() %>% rename(district = NAME) %>% select(district, town)

  # was having weird problem such that it could not find this file
  regional_districts_info <- read_tsv("~/Documents/R_local_repos/ctcorona/regional_districts_info.tsv") %>%
    mutate(number = str_extract(District, "\\d\\d"))
  # regional_districts_info <- regional_districts_info %>% mutate(number = str_extract(District, "\\d\\d"))

  # `school_regions` is what we want to end up with. One row per town for school_regions
  school_regions <- bind_rows(regional, region_additions) %>%
    left_join(regional_districts_info %>%
                select(district = District, HS_only, number),
              by = "district") %>%
    select(-district) %>%
    mutate(HS_only = as.logical(HS_only)) %>%
    rename(district_number = number) %>%
    arrange(district_number, town)

  no_high_school <- town_geometries %>%
    filter(!(town %in% tigris_schools$town) &
             !(town %in% school_regions$town))
  school_regions <- school_regions %>%
    mutate(district_type = case_when(
      HS_only ~ "Secondary Region",
      town %in% no_high_school$town ~ "Tuition Out",
      !HS_only ~ "K-12 Region",
      TRUE ~ "????"
    ))

  town_geometries <- town_geometries %>%
    left_join(school_regions, by = "town")

  town_geometries <- town_geometries %>%
    mutate(district_type = case_when(
      is.na(HS_only) & (town %in% no_high_school$town) ~ "No Secondary",
      is.na(HS_only) ~ "Town District",
      HS_only ~ "Secondary Region",
      !HS_only ~ "K-12 Region",
      TRUE ~ "No Secondary"
    )  %>%
      factor(levels = c("Town District", "K-12 Region", "Secondary Region", "No Secondary")),
    district_name = ifelse(is.na(district_number), town, district_number))
}
