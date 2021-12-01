

library(tidyverse)

covid_project <- "can_i_get_a_clean_project"
path_to_post <- paste0("~/Documents/R_local_repos/",covid_project, "/content/blog/2020-03-29-covid19-cases-in-connecticut/")
path_to_post_june <- paste0("~/Documents/R_local_repos/", covid_project, "/content/blog/2020-06-22-tracking-covid-19-in-connecticut/index.en_files/")
path_to_static_june <- paste0("~/Documents/R_local_repos/", covid_project, "/static/blog/2020-06-22-tracking-covid-19-in-connecticut/") # maybe no longer used
path_to_covid_project <- paste0("~/Documents/R_local_repos/", covid_project, "/content/project/covid/index_files/")

pres_ct2020 <- read_csv("data/PresidentialElection.csv")
names(pres_ct2020) <- c("candidate", "party", "office", "town", "vote", "district")
# View(pres_ct2020)

town_vote <- pres_ct2020 |> group_by(office, town, party) |>
  summarise(total = sum(vote)) |>
  pivot_wider(id_cols = c(office, town), values_from = total, values_fill = 0,
              names_from = party)
names(town_vote) <- str_replace_all(names(town_vote), " Party", "")
town_vote <- town_vote |>
  summarise(total = Democratic + Green + Libertarian + Republican + `Write In`,
            pct_repub = Republican / total,
            pct_lib = Libertarian / total,
            pct_green = Green / total)
p_libertarian <- ggplot(data = town_vote, aes(x = pct_repub, y = pct_lib)) +
  geom_point(aes(colour = total))
p_green <- ggplot(data = town_vote, aes(x = pct_repub, y = pct_green)) +
  geom_point(aes(colour = total))
p_third  <- ggplot(data = town_vote, aes(x = pct_lib, y = pct_green)) +
  geom_point(aes(colour = total))
town_vote |>
  filter(pct_green > 0.01) # Cornwall is outlier for Green
town_vote |>
  filter(pct_lib > 0.02) |>
  ungroup() |>
  select(town, pct_repub, pct_lib, pct_green, total) # Scotland is highest for lib
p_libertarian_size <- ggplot(data = town_vote, aes(x = total, y = pct_lib)) +
  geom_point(aes(colour = total))
p_repub_size <- ggplot(data = town_vote, aes(x = total, y = pct_repub)) +
  geom_text(aes(label = town), size = 2.5) +
  xlab("Size of Total 2020 Vote") + ylab("Percent Trump")
save(town_vote, file = paste0(path_to_ctcorona, "vote2020.RData"))


