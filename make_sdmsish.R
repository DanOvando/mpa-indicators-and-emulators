library(tidyverse)

library(sf)

library(here)

library(ranger)
world <-
  rnaturalearth::ne_countries(returnclass = "sf", continent = c("North America","South America") , scale = "medium") 

epo <- st_crop(world, y = c(xmin = -150, xmax = -70, ymin = -50, ymax = 50))

epo_crs <- st_crs(epo)

dar <- read_csv(here("data", "DarMain1996-2023.csv")) |>
  janitor::clean_names() |>
  mutate(
    month = lubridate::month(set_date, label = TRUE),
    day = lubridate::day(set_date),
    year = lubridate::year(set_date),
    year_day = lubridate::yday(set_date)
  ) |>
  ungroup() |>
  mutate(setid = 1:length(flag),
         nmonth = as.numeric(month)) |>
  filter(capacity > 182, lon < 0, year != 2003) |>
  arrange(set_date) # per c_21_04 These measures are applicable to
# all CPCs’ purse-seine vessels of IATTC capacity classes 4 to 6 (more than 182 metric tons
# carrying capacity), and to all their longline vessels over 24 meters length overall, that fish for
# yellowfin, bigeye and skipjack tunas in the Convention Area


tuna_cps <- dar |> 
  filter(year > 2015) |> 
  group_by(lon, lat) |> 
  summarise(bet = mean(betc),
            yft = mean(yftc), 
            skj = mean(skjc)) |> 
  pivot_longer(bet:skj, names_to = "critter", values_to = "cps") |> 
  ungroup() 


shark_cps <- read_csv(here("data", "epo_shark_catch.csv")) |> 
  filter(year > 2015) |> 
  group_by(lat, lon, shark_group) |> 
  summarise(cps = mean(shark_catch)) |> 
  mutate(
         shark_group = tolower(shark_group)) |> 
  rename(critter = shark_group)
  
critter_lookup <- tribble(~critter, ~sciname,
                          "bet","thunnus obesus",
                          "yft", "thunnus albacares",
                          "skj", "katsuwonus pelamis",
                          "silky shark", "carcharhinus falciformis",
                          "hammerhead sharks", "sphyrna zygaena",
                          "oceanic whitetip shark", "carcharhinus longimanus",
                          "mako sharks", "isurus oxyrinchus")


cps <- tuna_cps |> 
  bind_rows(shark_cps) |> 
  mutate(latlon = lat * lon) |> 
  filter(!is.na(critter), !str_detect(critter, "unidentified")) |> 
  left_join(critter_lookup, by = "critter") |> 
  filter(!is.na(sciname))



sdmsish <- cps |>
  group_by(sciname) |>
  nest() |>
  mutate(sdmish = purrr::map(data, ~ ranger(cps ~ lat + lon + latlon, data = .x)))

epo_mesh <- expand.grid(lat = seq(min(tuna_cps$lat), max(tuna_cps$lat), length.out = 20),
                        lon = seq(min(tuna_cps$lon), max(tuna_cps$lon), length.out = 20)) |> 
  mutate(latlon = lat * lon)

sdmsish <- sdmsish |> 
  mutate(pred = map(sdmish, ~ epo_mesh |> mutate(pred_cps = predict(.x, data = epo_mesh)$predictions)))
  
sdmsish <- sdmsish |> 
  unnest(cols = pred)  |> 
  group_by(sciname) |> 
  mutate(scaled_pred_cps = as.numeric(scale(pred_cps))) |> 
  select(-data,-sdmish)

popsizeish <- sdmsish |> 
  group_by(sciname) |> 
  summarise(abundance = sum(pred_cps)) 

popsizeish |> 
  ggplot(aes(reorder(sciname, abundance), abundance)) +
  geom_col() + 
  coord_flip()

sdmsish |> 
  ggplot() + 
  geom_tile(aes(lon, lat,fill =  scaled_pred_cps)) + 
  facet_wrap(~sciname) +
  scale_fill_viridis_c()

write_rds(sdmsish, here("data","epo_sdmsish.rds"))

write_rds(popsizeish, here("data","epo_popsizeish.rds"))


