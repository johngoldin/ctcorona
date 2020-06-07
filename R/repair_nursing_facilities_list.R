# fix dph_nursing_facilities so that it matches better to dph_nursing_homes.
# changes a few town names and mostly fixes typography in facility names.

# Ledgecrest Health Care is in Kensington
# Touchpoints at Manchester only in 2013 2014
# Candlewood Valley Health & Rehabilitation Center 2013 2014
# West Hartford Health and Rehabilitation Center 2013 2014
# Rocky Hill    John L. Levitow Health Care Center not found in facilities
# Edgehill Health Center (as opposed to Edgehill) 2013 2014
# Norwalk       Cassena Care at Norwalk 2013 2014


# first name is covid list nursing_home name, second is facility_name, with ~ as delimeter
# change the second into the first
convert1 <- c("Seabury~Seabury Health Center",
              "Ledgecrest Health Care~Ledgecrest Health Care Center, Inc.",
              "Bloomfield Center for Nursing and Rehabilitation~Bloomfield Health Care Center",
              "Springs at Watermark 3030 Park~The Springs @ Watermark 3030 Park",
              "Caleb Hitchcock Health Center~Caleb Hitchcock Health Center at Duncaster",
              "Pines At Bristol Center for Nursing & Rehabilitation~The Pines at Bristol",
              "Pines At Bristol Center for Nursing & Rehabilitation~The Pines at Bristol Center for Nursing and Rehabilitation",
              "Sheridan Woods Health Care Center~Sheriden Woods Health Care Center",
              "Elim Park Baptist Home~Elim Park Baptist Home, Inc.",
              # "Maple View Health & Rehabilitation Center~Maple View Health and Rehabilitation Center",
              "Maple View Health & Rehabilitation Center~Maple View Health and Rehabilitation",
              "Apple Rehab Middletown~Apple Rehab Middletown (dba Highview Health Care Center)",
              "Ingraham Manor~Ingraham Manor of Bristol Hospital",
              "Cassena Care at Norwalk~Cassena Care Norwalk",
              "Summit at Plantsville~The Summit at Plantsville",
              "60 West~Securecare Options Inc. dba 60 West",
              "Edgehill Health Center~Edgehill",
              "Lutheran Home of Southbury~The Lutheran Home of Southbury",
              "Cambridge Health and Rehabilitation Center~Cambridge Manor",
              "Woodlake at Tolland Rehabilitation and Nursing Center~Prospect ECHN ElderCare Serv. dba Woodlake at Tolland",
              "Lord Chamberlain Nursing and Rehabilitation Center~Lord Chamberlain Nursing & Rehabilitation Center",
              "Lord Chamberlain Manor~Chamberlain Healthcare",
              "Fresh River Healthcare~Kettle Brook Care Center, dba Fresh River Healthcare",
              "Aaron Manor Nursing and Rehabilitation~Aaron Manor Nursing and Rehabilitation Center",
              "Chesterfields Health Care Center~Chesterfield's Health Care Center",
              "Filosa Nursing & Rehabilitation~Filosa for Nursing and Rehabilitation",
              "Regency House Nursing and Rehabilitation Center~Regency House of Wallingford",
              "Riverside Health & Rehabilitation Center~Riverside Health and Rehabilitation Center",
              "Whispering Pines~Whispering Pines Rehabilitation and Nursing Center LLC",
              "Carolton Chronic & Convalescent Hospital~Carolton Chronic & Convalescent Hospital, Inc.",
              "Ludlowe Center for Health & Rehabilitation~Ludlowe Center for Health and Rehabilitation",
              "Glastonbury Health Care Center~Glastonbury Health Care Center, Inc.",
              "Greenwich Woods~Greenwich Woods Rehabilitation",
              "Nathaniel Witherall~The Nathaniel Witherell",
              "Mystic Healthcare & Rehabilitation Center~Mystic Healthcare and Rehabilitation Center",
              "Guilford House~The Guilford House",
              "Hamden Rehabilitation & Health Care Center~Hamden Rehabilitation and Health Care Center",
              "Avery Nursing Home/Noble Building~Avery Heights Health Care Center",
              "Chelsea Place~Chelsea Place Care Center, LLC",
              "Crestfield Rehabilitation Center~Crestfield Rehabilitation Center and Fenwood Manor",
              "Westside Care Center~Westside Care Center, LLC",
              "Marlborough Health Care Center~Marlborough Health and Rehabilitation Center",
              "Water's Edge Center for Health & Rehabilitation~Water's Edge Center for Health and Rehabilitation",
              "Milford Health and Rehabilitation Center~Milford Health and Rehabilitlation Center",
              "West River Rehab Center~West River Rehabilitation Center",
              "Advanced Center For Nursing And Rehabilitation~Advanced Nursing and Rehabilitation Center of New Haven",
              "Grimes Center~Grimes  Center",
              "The Mary Wade Home~The Mary Wade Home, Inc.",
              "Harbor Village~Harbor Village North",
              "Village Crest Center for Health & Rehabilitation~Village Crest Center for Health and Rehabilitation",
              "Bel Air Manor~Bel-Air Manor Nursing & Rehabilitation Center",
              "Newtown Rehabilitation & Health Care Center~Newtown Rehabilitation and Health Care Center",
              "Bride Brook Health & Rehabilitation Center~Bride Brook Health and Rehabilitation Center",
              "Notre Dame Health and Rehabilitation Center~Notre Dame Convalescent Homes, Inc. (dba) Notre Dame Health and Rehab Center",
              "Orange Health Care Center~Orange Healthcare Rehabilitation Center",
              "Portland Care and Rehabilitation~Portland  Care and Rehabilitation Centre",
              "Apple Rehab Rocky Hill~Apple Rehab of Rocky Hill",
              "Apple Rehab Avon~Apple Rehab Avon d/b/a Brightview Nursing and Retirement",
              "Bishop-Wicke Health and Rehabilitation~Bishop Wicke Health & Rehab Center",
              "Gardner Heights Health Care Center~Gardner Heights Health Care Center, Inc.",
              "Hewitt Health & Rehabilitation Center~Hewitt Health and Rehabilitation Center, Inc.",
              "Cassena Care at Stamford~Cassena Care of Stamford",
              "Suffield House~The Suffield House",
              "Woodlake at Tolland Rehabilitation and Nursing Center~Woodlake at Tolland",
              "Litchfield Woods~Litchfield Woods Health Care Center",
              "Wolcott Hall Nursing Center~Wolcott Hall Nursing Center, Inc.",
              "Cheshire House Health Care Facility & Rehabilitation Center~Cheshire House",
              "Grove Manor Nursing Home~Grove Manor Nursing Home, Inc.",
              "Waterbury Gardens Nursing and Rehabilitation~Waterbury Gardens Nursing and Rehab",
              "Hughes Health and Rehabilitation~Hughes Health and Rehabilitation, Inc.",
              "Saint Mary Home~St. Mary Home",
              "St. Joseph's Living Center~St. Joseph's Living Center, Inc.",
              "Avalon Health Center~Avalon Health Care Center at StoneRidge",
              "Avery Nursing Home/Noble Building~Avery Nursing Home/ Noble Building",
              "Beechwood~Beechwood Rehabilitation & Nursing Center",
              "Bishop-Wicke Health and Rehabilitation~Bishop Wicke Health Center",
              "Andrew House Health Care~Andrew House",
              "Cheshire House Health Care Facility & Rehabilitation Center~Cheshire House Health Care Facility & Rehabilitation",
              "Cook-Willow Health & Rehabilitation Center~Cook-Willow Health Center",
              "Gladeview Health Care Center~Gladview Health Care Center",
              "Golden Hill Rehab Pavilion~Golden Hill Health Care Center",
              "Greenwich Woods~Greenwich Woods Health Care Center",
              "Grimes Center~Grimes  Center - Yale New Haven Health",
              "Hamden Rehabilitation & Health Care Center~Hamden Health Care Center",
              "Hebrew Center for Health and Rehabilitation~Hebrew Home and Hospital, Inc.",
              "Greentree Manor Nursing and Rehab Center~Greentree Manor Nursing and Rehabilitation Center",
              "Long Ridge Post-Acute Care~Long Ridge of Stamford",
              "Maple View Health & Rehabilitation Center~Maple View Center for Health and Rehabilitation",
              "Marlborough Health Care Center~Marlborough Health Center, Inc.",
              "Windsor Health and Rehabilitation Center~Windsor Health and Rehabilitation Center, LLC",
              "Montowese Health & Rehabilitation Center~Montowese Health & Rehabilitation Center, Inc.",
              "New London Sub-Acute and Nursing~New London Rehab and Care of Waterford",
              "Newington Rapid Recovery Rehab Center~Newington Health Care Center",
              "Norwich Sub-acute and Nursing~Norwichtown Rehabilitation and Care Center",
              "Notre Dame Health and Rehabilitation Center~Notre Dame Convalescent Home, Inc.",
              "Orange Health Care Center~Orange Health and Rehabilitation Center",
              "Parkville Care Center~Park Place Health Center",
              "Pilgrim Manor Care Center~Pilgrim Manor",
              "Salmon Brook Rehab and Nursing~Salmon Brook Center",
              "Skyview Rehab and Nursing~Skyview Center",
              "Wadsworth Glen Health Care Center~Wadsworth Glen Health Care and Rehabilitation",
              "West River Rehab Center~West River Health Care Center",
              "Westport Rehabilitation Complex~Westport Health Care Center",
              "Whitney Rehabilitation Care Center~Whitney Manor Convalescent Center, Inc.",
              "Candlewood Valley Health & Rehabilitation Center~New Milford Rehabilitation, LLC", # based on same address
              "Touchpoints at Manchester~Bidwell Care Center, LLC of Manchester, CT d/b/a/ Touchpoints at Manchester",
              "Grandview Rehabilitation and Healthcare Center~Parkside Rehabilitation & Healthcare Center, d/b/a Grandview Rehabilitation and Healthcare Center",
              "West Hartford Health and Rehabilitation Center~Brookview Corp.  d/b/a West Hartford Health and Rehabilitation Center"
              )
xx <- str_split_fixed(convert1, "~", 2)
for_conversion <- tibble(covid_list = xx[,1], facility_name = xx[, 2])

# check whether names are unique:
# dph_nursing_homes %>% group_by(nursing_home, date) %>% count() %>% filter(n > 1)
dph_nursing_facilities %>% group_by(facility_name, reporting_year) %>% count() %>% filter(n > 1)

# note these problems:
problems <- for_conversion %>% filter(!(str_to_lower(facility_name) %in% str_to_lower(dph_nursing_facilities$facility_name[dph_nursing_facilities$reporting_year == "2019"])))
# # A tibble: 6 x 2
# covid_list                                        facility_name
# <chr>                                             <chr>
#   1 mystic healthcare & rehabilitation center         "mystic in facilities, groton in covid"
# 2 grandview rehabilitation and healthcare center    ""
# 3 john l. levitow health care center                "unknown"
# 4 maple view health & rehabilitation center         "maple view center for health and rehabilita…
# 5 summit at plantsville                             "unknown"
# 6 woodlake at tolland rehabilitation and nursing c… "woodlake at tolland"

# which key in next step is not unique?
duplicate_key <- for_conversion %>%
  mutate(key = str_to_lower(facility_name)) %>%
  count(key) %>% filter(n > 1)

# find the latest listing for each facility_name and adjust names to match covid list
dph_nursing_facilities2 <- dph_nursing_facilities %>%
  mutate(key = str_to_lower(facility_name), original_facility_name = "") %>%
  group_by(key) %>% arrange(reporting_year) %>%
  slice_tail() %>% ungroup() %>%
  rows_upsert(for_conversion %>%
                # filter(!(str_to_lower(facility_name) %in% str_to_lower(problems$facility_name))) %>%
                mutate(key = str_to_lower(facility_name)) %>%
                rename(original_facility_name = facility_name, facility_name = covid_list),
              by = "key") %>%
  group_by(facility_name) %>% arrange(reporting_year) %>%
  slice_tail() %>% ungroup() %>%
  select(-key)

dph_nursing_facilities2 %>%
  arrange(facility_name, reporting_year) %>%
  mutate(possible1 = ifelse((town == lag(town)) & (address == lag(address)) & (reporting_year != lag(reporting_year)), lag(facility_name), "")) %>%
  filter(possible1 != "") %>%
  mutate(possible1 = ifelse(reporting_year == "2019", paste0(facility_name, "~", possible1), paste0(possible1, "~", facility_name))) %>%
  select(facility_name, reporting_year, possible1, town, address) %>%
  View()

bad_reporting_year <- dph_nursing_facilities2 %>%
  filter(facility_name %in% dph_nursing_homes$nursing_home, (is.na(reporting_year) | (reporting_year != "2019"))) %>%
  select(facility_name, reporting_year, town, original_facility_name)
dph_nursing_facilities %>%
  filter(town %in% bad_reporting_year$town) %>%
  arrange(town, facility_name, reporting_year) %>% select(facility_name, town, reporting_year, address) %>%
  View()

# Now check which rows in dph_nursing_homes is not in dph_nursing_facilities2
not_in_facilities_list <- dph_nursing_homes %>%
  filter(!(str_to_lower(nursing_home) %in% str_to_lower(dph_nursing_facilities2$facility_name))) %>%
  filter(date == max(dph_nursing_homes$date)) %>%
  select(town, nursing_home) %>% arrange(town, nursing_home)
dph_nursing_facilities %>%
  filter(town %in% not_in_facilities_list$town) %>%
  arrange(town, facility_name, reporting_year) %>% select(facility_name, town, reporting_year, address) %>%
  View()

combined <- dph_nursing_facilities2 %>%
  filter(reporting_year == "2019") %>%
  left_join(dph_nursing_homes, by = c("facility_name" = "nursing_home"))


dph_nursing_facilities2 %>%
  mutate(long = map_dbl(geocoded_column.coordinates,
                        ~ ifelse(is.null(.x), NA_real_, unlist(.x)[[1]])),
         lat = map_dbl(geocoded_column.coordinates,
                        ~ ifelse(is.null(.x), NA_real_, unlist(.x)[[2]]))) %>%
  select(-geocoded_column.coordinates) %>%
  write_csv(path = "nursing_facilities_revised.csv")


