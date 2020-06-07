

dph_pop <- census_population %>%
  left_join(ct %>% filter(date == max(date))) %>%
  mutate(cases_per_pop = cases / (estimate / 100000),
         per_pop_label =  as.character(round(cases_per_pop, 1)))
county_centroid <- st_centroid(dph_pop) # use to place town labels


county_map <- ggplot() +
  geom_sf(data = dph_pop %>% select(geometry, cases_per_pop), aes(fill = cases_per_pop)) +
  scale_fill_gradient(low = "white", high = "grey") +
  geom_sf_text(data = county_centroid, aes(label = paste0(county, "\n", per_pop_label, " per 100K")), color = "black", size = 3) +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal()  +
  # theme(legend.position = "none") +
  labs(title = "Cumulative Confirmed Cases by Connecticut County",
       subtitle = "cases per 100K of population for most recent report date",
       fill = "cases per 100K",
       caption = "Source: US Census, tidycensus package")

toown_map <- geom_sf(data = town_geometries, aes(fill = pct65plus)) +
  geom_sf(data = county_geometries, size = 2, fill = NA,  colour = "lightgray") +
  geom_sf_text(data = town_geometries, aes(label = town), size = 2) +
  coord_sf(datum = NA, label_axes = "----")  +
  xlab("") + ylab("") + theme_minimal()

left_join(tigris::county_subdivisions(state = "CT", cb = FALSE) %>%
            filter(NAME != "County subdivisions not defined") %>%
            select(GEOID, INTPTLAT, INTPTLON, geometry),
          by = "GEOID")
