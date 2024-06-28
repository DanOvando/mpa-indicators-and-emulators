# setup -------------------------------------------------------------------
library(tidyverse)

library(ggExtra)

library(marlin)

library(sf)

library(rerddap)

library(rnaturalearth)

library(here)

library(janitor)

library(progress)

library(furrr)

library(ranger)

library(rpart)

library(tidymodels)

library(themis)

library(cmdstanr)

library(Rcpp)

Rcpp::sourceCpp(here('src', "sim_pt_mpa.cpp"))

options(dplyr.summarise.inform = FALSE)

theme_set(theme_minimal(base_size = 14))

run_experiments <- TRUE

n_states <- 10

results_name <- "test"

results_dir <- file.path("results", results_name)

plot_dir <- file.path(results_dir, "plots")

plot_width <- 8

plot_height = 5

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  
  dir.create(plot_dir, recursive = TRUE)
  
}

set.seed(24)

seasons <- 2

experiment_workers <- 5

dx <-  100000

dy <- 20000

patch_area <- 1


resolution <- c(12, 12)

patches <- prod(resolution)

foos <- list.files(here("R"))

walk(foos, ~ source(here("R", .x)))

# load data ---------------------------------------------------------------

# get maps
campas <-
  sf::st_read(here("data", "CA_MPA_boundaries", "ds582"), ) |>
  janitor::clean_names()

campas_crs <- sf::st_crs(campas)

ca <-
  rnaturalearth::ne_states(country = "united states of america", returnclass = "sf") |>
  filter(name == "California") |>
  janitor::clean_names() |>
  sf::st_transform(campas_crs)

ca_crs <- sf::st_crs(ca)


ca_raster <- stars::st_rasterize(ca["scalerank"])$scalerank


ca_raster |>
  as.data.frame() |>
  mutate(x = 1:length(V1)) |>
  pivot_longer(
    starts_with("V"),
    names_to = "y",
    values_to = "sst",
    names_prefix = list(y = "V"),
    names_transform = list(y = as.integer)
  ) |>
  ggplot(aes(x, -y, color = sst)) +
  geom_point()


campas |>
  ggplot() +
  geom_sf(data = ca) +
  geom_sf(aes(fill = type))

campas_raster <-
  stars::st_rasterize(campas["acres"], dx = dx, dy = 200)

# get environmental data

# get bathymetry

if (!file.exists(here('data', "bathy.rds"))) {
  a = ed_search(query = 'bathymetry', which = "grid")
  
  bath_dat <- info('srtm30plus_v11_bathy')
  
  (bathy <- griddap(
    bath_dat,
    latitude = c(32, 43),
    longitude = c(-125, -116),
    stride = 20,
    fmt = "csv"
  ))
  
  
  bathy <- bathy %>%
    filter(!is.na(elev) & elev > -2000)
  
  bathy <- st_as_sf(
    bathy,
    coords = c("longitude", "latitude"),
    remove = FALSE,
    crs = "WGS84"
  ) |>
    st_transform(campas_crs)
  
  
  write_rds(bathy, file = here("data", "bathy.rds"))
} else {
  bathy <-  read_rds(file = here("data", "bathy.rds"))
  
  
}


bathy_raster <-
  stars::st_rasterize(bathy["elev"], dx = 100000, dy = 20000)$elev


bathy_coords <- sf::st_coordinates(bathy)

bathy <- bathy |>
  bind_cols(bathy_coords)


bathy |>
  ggplot() +
  geom_sf(aes(color = elev)) +
  geom_sf(data = ca) +
  geom_sf(data = campas) +
  scale_color_viridis_c(option = "magma")


# get SST

if (!file.exists(here('data', "sst.rds"))) {
  sst_dat <- info('jplMURSST41')
  
  sst <- griddap(
    sst_dat,
    latitude = c(32, 43),
    longitude = c(-125,-116),
    stride = 10,
    time = c("2010-01-01", "2011-01-01"),
    fmt = "csv"
  )
  
  write_rds(sst, file = here("data", "sst.rds"))
  
  
} else {
  sst <- read_rds(file = here("data", "sst.rds"))
  
}

sst <- st_as_sf(
  sst,
  coords = c("longitude", "latitude"),
  remove = FALSE,
  crs = "WGS84"
) |>
  st_transform(campas_crs) |>
  mutate(year = lubridate::year(time),
         month = lubridate::month(time))

sst_coords <- sf::st_coordinates(sst)

sst <- sst |>
  bind_cols(sst_coords)

sst_raster <-
  (stars::st_rasterize(sst["analysed_sst"], dx = 100000, dy = 20000)$analysed_sst)

sst_raster |>
  as_tibble() |>
  mutate(x = 1:length(V1)) |>
  pivot_longer(
    starts_with("V"),
    names_to = "y",
    values_to = "sst",
    names_prefix = list(y = "V"),
    names_transform = list(y = as.integer)
  ) |>
  ggplot(aes(x, -y, color = sst)) +
  geom_point() +
  scale_color_viridis_c(option = "magma")

# sst |>
#   filter(!is.na(analysed_sst), year == max(year)) |>
#   group_by(year, month, X, Y) |>
#   summarise(mean_sst = mean(analysed_sst)) |>
#   ggplot() +
#   geom_sf(aes(color = mean_sst)) +
#   geom_sf(data = ca) +
#   geom_sf(data = campas) +
#   scale_color_viridis_c(option = "magma") +
#   facet_wrap( ~ month)


# set up simulations ------------------------------------------------------
if (run_experiments) {
  campas_bbox <- sf::st_bbox(campas)
  
  lon_range <- seq(campas_bbox$xmin, campas_bbox$xmax, by = 10000)
  
  lat_range <- seq(campas_bbox$ymin, campas_bbox$ymax, by = 10000)
  
  state_experiments <-
    tibble(
      kiss = sample(c(TRUE,FALSE), n_states, replace = TRUE),
      mpa_response = sample(c("stay", "leave"), n_states, replace = TRUE),
      habitat_smoothness = runif(n_states, 1e-3, 1),
      max_abs_cor = runif(n_states, 1e-3, 1),
      spatial_q = sample(
        c(TRUE,FALSE),
        n_states,
        replace = TRUE,
        prob = c(1,3)
      ),
      spatial_allocation = sample(c("ppue", "rpue","revenue"), n_states, replace = TRUE),
      fleet_model = sample(c("open access", "constant effort"), n_states, replace = TRUE)
    ) %>%
    mutate(state_id = 1:nrow(.))
  
  critters <-
    tibble(
      scientific_name = c(
        "paralabrax clathratus",
        "ophiodon elongatus",
        "scorpaenichthys marmoratus"
      )
    )
  

  port_locations <-
    tibble(x = c(1, resolution[1]), y = c(1, resolution[2])) # coordinates to test impact of highly disparate ports
  
  
  # convert maps into grid form for simulation
  
  # some ideas here. get the SST data and then add a bit of buffer to it
  # Then, clip the CA layer to that just to have the edges as well
  
  # prepare critters
  
  process_correlations <- function(cores) {
    cores <- cores[upper.tri(cores)]
    
    max_cor <- max(cores)
    
    min_cor <- min(cores)
    
    range_cor <- max_cor - min_cor
    
    mean_abs_cor <- mean(abs(cores))
    
    mean_cor <- mean(cores)
    
    
    proc_cores <- data.frame(
      max_cor = max_cor,
      min_cor = min_cor,
      range_cor = range_cor,
      mean_abs_cor = mean_abs_cor,
      mean_cor = mean_cor
    )
    
  }
  
  state_experiments <- state_experiments %>%
    mutate(
      habitats = pmap(
        list(kp = habitat_smoothness,
             max_abs_cor = max_abs_cor),
        sim_habitat,
        critters = critters$scientific_name,
        resolution = resolution,
        patch_area = patch_area,
        .progress = "Simulating habitats"
      )
    ) %>%
    mutate(critter_correlations = map(habitats, ~process_correlations(.x$critter_correlations))) |> 
    unnest(cols = critter_correlations) |> 
    mutate(habitats = map(
      habitats,
      ~ .x$critter_distributions  |> select(-patch) |> group_by(critter) |> nest(.key = "habitat")
    )) |>
    unnest(cols = habitats) %>%
    ungroup() |>
    mutate(
      seasonal_movement = sample(c(FALSE, TRUE), length(state_id), replace = TRUE),
      spawning_aggregation = sample(c(TRUE, FALSE), length(state_id), replace = TRUE),
      spawning_season = sample(1:seasons, length(state_id), replace = TRUE),
      f_v_m = runif(length(state_id), 0.01, 0.25),
      adult_diffusion = runif(
        length(state_id),
        
        min = 0 * patches * patch_area,
        max = patches * patch_area
      ),
      steepness = runif(length(state_id), min = 0.6, max = 1),
      ssb0 = rlnorm(length(state_id), log(10),0.6),
      recruit_diffusion = runif(length(state_id),
                                min = 0,
                                max = patches * patch_area),
      hyperallometry = sample(c(1,2),length(state_id), replace = TRUE),
      density_dependence = sample(
        c(
          "global_habitat",
          "local_habitat",
          "pre_dispersal",
          "post_dispersal"
        ),
        length(state_id),
        replace = TRUE
      )
    ) %>%
    mutate(
      spawning_season = ifelse(spawning_aggregation, spawning_season, NA),
      ontogenetic_shift = sample(c(TRUE, FALSE), length(state_id), replace = TRUE)
    ) |> 
    mutate(ontogenetic_shift = ifelse(kiss,FALSE, ontogenetic_shift)) |> 
    mutate(density_dependence = ifelse(ontogenetic_shift, "local_habitat",density_dependence))
  
  state_experiments <- state_experiments %>%
    rename(scientific_name = critter) |>
    mutate(
      critter = pmap(
        list(
          sciname = scientific_name,
          habitat = habitat,
          seasonal_movement = seasonal_movement,
          spawning_aggregation = spawning_aggregation,
          spawning_season = spawning_season,
          f_v_m = f_v_m,
          adult_diffusion = adult_diffusion,
          recruit_diffusion = recruit_diffusion,
          density_dependence = density_dependence,
          hyper = hyperallometry,
          ontogenetic_shift = ontogenetic_shift,
          steepness = steepness,
          ssb0 = ssb0,
          kiss = kiss
        ),
        create_experiment_critters,
        resolution = resolution,
        seasons = seasons,
        .progress = "creating critters"
      )
    )
  
  
  # aggregate into lists of fauna
  state_experiments <- state_experiments %>%
    group_by(state_id) %>%
    nest() %>%
    mutate(fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)),
           sels = map(fauna, ~ runif(length(.x), 0.25, 1.75)),
           prices = map(fauna, ~ runif(length(.x), 1, 5)))
  

  state_experiments <- state_experiments %>%
    ungroup() %>%
    mutate(use_ports = sample(c(FALSE, TRUE), nrow(.), replace = TRUE)) %>%
    mutate(
      fleet = pmap(
        list(
          fauna = fauna,
          state = data,
          sels = sels,
          prices = prices,
          use_ports = use_ports
        ),
        create_experiment_fleet,
        port_locations = port_locations,
        resolution = resolution
      )
    )
  
  # add in starting conditions
  init_condit <- function(fauna, fleets, years = 100) {
    starting_trajectory <-
      simmar(fauna = fauna,
             fleets = fleets,
             years = years)
    
    # plot_marlin(check)
    
    starting_conditions <-
      starting_trajectory[(length(starting_trajectory) - seasons + 1):length(starting_trajectory)]
    # starting_trajectory[1:length(starting_trajectory)]
    
    proc_starting_conditions <-
      process_marlin(starting_conditions, keep_age = FALSE)
    
    out <- list(starting_conditions = starting_conditions,
                proc_starting_conditions = proc_starting_conditions)
    
  }
  
  state_experiments <- state_experiments %>%
    mutate(tmp = map2(fauna, fleet, init_condit, .progress = "simulating initial conditions"))
  
  state_experiments$starting_conditions <-
    map(state_experiments$tmp, "starting_conditions")
  
  state_experiments$proc_starting_conditions <-
    map(state_experiments$tmp, "proc_starting_conditions")
  
  state_experiments <- state_experiments %>%
    select(-tmp)
  
  for (i in 1:nrow(state_experiments)) {
    tmp <- state_experiments$proc_starting_conditions[[i]]$fauna |>
      filter(step == max(step)) |> 
      group_by(critter) |> 
      mutate(n = n / max(n)) |>
      ggplot(aes(x, y, fill = n)) +
      geom_tile() +
      facet_wrap( ~ critter) +
      scale_fill_viridis_c()
    
    tmp2 <- state_experiments$proc_starting_conditions[[i]]$fleet |>
      filter(step == max(step)) |> 
      group_by(fleet, step) |>
      mutate(effort = effort / max(effort, na.rm = TRUE)) |>
      ggplot(aes(x, y, fill = effort)) +
      geom_tile() +
      facet_grid(step ~ fleet , labeller = label_both) +
      scale_fill_viridis_c()
    
    # tmp3 <- state_experiments$proc_starting_conditions[[i]]$fleet |>
    #   group_by(fleet, step) |>
    #   summarise(effort = sum(effort)) |> 
    #   ggplot(aes(step, effort)) +
    #   geom_line() +
    #   facet_wrap(~ fleet , labeller = label_both) +
    #   scale_fill_viridis_c()
    # 
    
    ggsave(file.path(
      results_dir,
      "plots",
      glue::glue("state_{i}_habitat.pdf")
    ), tmp)
    
    ggsave(file.path(
      results_dir,
      "plots",
      glue::glue("state_{i}_fleet_allocation.pdf")
    ), tmp2)
    
    
  }
  
  # fit two-patch biomass dynamics (twopbd) emulations
  emulated_state_experiments <- state_experiments |> 
    mutate(twopbd_params = map2(fauna, fleet, safely(fit_twopbd))) |> 
    mutate(fit_worked = map_lgl(twopbd_params, ~ is.null(.x$error))) |> 
    filter(fit_worked) |> 
    mutate(twopbd_params = map(twopbd_params, "result")) |> 
    select(state_id, fleet,twopbd_params) |> 
    unnest(cols = twopbd_params)
  
  emulated_experiment_results <-
    expand_grid(
      state_id = unique(emulated_state_experiments$state_id),
      prop_mpa = seq(0.05, 0.95, by = 0.05)
    ) |>
    left_join(emulated_state_experiments,
              relationship = "many-to-many",
              by = "state_id") |>
    mutate(mpa_experiment = pmap(
      list(
        params = twopbd_params,
        fleet = fleet,
        depletion = depletion,
        local_dd = local_dd,
        p_mpa = prop_mpa
      ),
      run_twopbd_mpas,
      .progress = "running emulator experiments"
    ))
  
  write_rds(emulated_experiment_results,
            file = file.path(results_dir, "emulated_experiment_results.rds"))
  
  
  # emulated_experiment_results |>
  #   filter(state_id == 2) |>
  #   select(critter, prop_mpa,mpa_experiment) |>
  #   unnest(cols = mpa_experiment) |>
  #   filter(year == max(year)) |>
  #   ggplot(aes(prop_mpa, yield_mpa / yield_bau - 1)) +
  #   geom_line() +
  #   facet_wrap(~critter, scales = "free_y")

  placement_experiments <- expand_grid(
    placement_strategy = c("target_fishing", "area","avoid_fishing"),
    prop_mpa = seq(0, 1, by = 0.05),
    critters_considered = seq(
      length(state_experiments$fauna[[1]]),
      length(state_experiments$fauna[[1]]),
      by = 1
    ),
    placement_error = c(0)
  ) %>%
    group_by_at(colnames(.)[!colnames(.) %in% c("temp", "prop_mpa")]) %>%
    nest() %>%
    ungroup() %>%
    mutate(placement_id = 1:nrow(.)) %>%
    unnest(cols = data)
  
  write_rds(placement_experiments,
            file = file.path(results_dir, "placement_experiments.rds"))
  
  write_rds(state_experiments,
            file = file.path(results_dir, "state_experiments.rds"))
  
  write_rds(emulated_state_experiments,
            file = file.path(results_dir, "emulated_state_experiments.rds"))
  
  tmp <-
    map(state_experiments$starting_conditions,
        ~ map_df(.x,  ~ sum(.x[[1]]$ssb_p_a) / .x[[1]]$ssb0))
  
  check <-
    tibble(state_id = state_experiments$state_id, tmp = (tmp)) %>%
    unnest(cols = tmp) %>%
    pivot_longer(cols = -state_id,
                 names_to = "critter",
                 values_to = "depletion")
  
 init_dep_plot <-  check %>%
    ggplot(aes(depletion)) +
    geom_histogram() +
    facet_wrap( ~ critter)
 
 init_dep_plot
  
  # run simulations ---------------------------------------------------------
  future::plan(future::multisession, workers = experiment_workers)
  
  experiment_results <-
    vector(mode = "list", length = nrow(placement_experiments))
  
  pb <- progress_bar$new(
    format = "  Running MPA Experiments [:bar] :percent eta: :eta",
    total = nrow(placement_experiments),
    clear = FALSE,
    width = 60
  )
  
  pb$tick(0)
  
  for (p in 1:nrow(placement_experiments)) {
    # memory problem trying to do it all at once so breaking it up a bit
    
    
    # a <- Sys.time()
    tmp <- state_experiments |> 
      ungroup() %>%
      mutate(
        results = future_pmap(
          list(
            starting_conditions = starting_conditions,
            proc_starting_conditions = proc_starting_conditions,
            fauna = fauna,
            fleets = fleet
          ),
          run_mpa_experiment,
          placement_strategy = placement_experiments$placement_strategy[p],
          prop_mpa = placement_experiments$prop_mpa[p],
          critters_considered = placement_experiments$critters_considered[p],
          placement_error = placement_experiments$placement_error[p],
          resolution = resolution,
          patch_area = patch_area,
          .progress = FALSE,
          .options = furrr_options(seed = 42)
        )
      )
    

    tmp$results <-
      purrr::set_names(tmp$results, state_experiments$state_id)
    # Sys.time() - a
    
    experiment_results[[p]] <- tmp$results
    
    pb$tick()
    
    
  } # close p loop
  
  future::plan(future::sequential)
  
  write_rds(experiment_results,
            file = file.path(results_dir, "experiment_results.rds"))
  
  
} else {
  experiment_results <-
    read_rds(file = file.path(results_dir, "experiment_results.rds"))
  
  placement_experiments <-
    read_rds(file = file.path(results_dir, "placement_experiments.rds"))
  
  state_experiments <-
    read_rds(file = file.path(results_dir, "state_experiments.rds"))
  
  emulated_experiment_results <-  read_rds(file = file.path(results_dir, "emulated_experiment_results.rds"))

  
}

# process simulations -----------------------------------------------------


results <- placement_experiments %>%
  mutate(temp = experiment_results,
         state_id = map(experiment_results, names)) %>%
  unnest(cols = c(temp, state_id)) |>
  mutate(fauna = map(temp, c("results", "fauna")),
         fleets = map(temp, c("results", "fleets"))) |>
  select(-temp) |>
  mutate(id = glue::glue("placement_id-{placement_id}_state_id-{state_id}")) |>
  select(id, everything())


fauna_results <- results |>
  select(-fleets) |>
  unnest(cols = fauna)

fauna_benchmark <- fauna_results |>
  filter(prop_mpa == 0)

fleet_results <- results |>
  select(-fauna) |>
  unnest(cols = fleets)

fleet_benchmark <- fleet_results |>
  filter(prop_mpa == 0)

benchmark <- results |>
  filter(prop_mpa == 0) |>
  group_by(id) |>
  nest(.key = "control")

# create results ----------------------------------------------------------

# first, calculate no-MPA baseline for metrics in question

tmp <- results |>
  filter(prop_mpa > 0) |>
  group_by(id, placement_id, state_id, prop_mpa) |>
  nest(.key = "treatment") |>
  arrange(id) |>
  left_join(benchmark, by = "id")

calculate_outcomes <- function(treatment, control) {
  # calculate treatment
  
  
  treatment_fauna <- treatment$fauna[[1]] |>
    group_by(critter, step) |>
    summarise(
      biomass = sum(b),
      spawning_biomass = sum(ssb),
      numbers = sum(n),
      mpa_biomass = sum(b[mpa]),
      fished_biomass = sum(b[!mpa])
    ) |>
    ungroup() |>
    pivot_longer(-c(critter, step)) |>
    mutate(fleet = "nature")
  
  
  treatment_fleets <- treatment$fleets[[1]] |>
    group_by(fleet, critter, step) |>
    summarise(
      catch = sum(catch, na.rm = TRUE),
      revenue = sum(revenue, na.rm = TRUE),
      effort = sum(effort, na.rm = TRUE),
      mean_cpue = mean(cpue, na.rm = TRUE)
    ) |>
    ungroup() |>
    pivot_longer(-c(fleet, critter, step))
  
  
  # calculate benchmark
  
  mpas <- treatment$fauna[[1]] |>
    filter(step == max(step)) |>
    select(x, y, mpa) |>
    unique()
  
  control_fauna <- control$fauna[[1]] |>
    select(-mpa) |>
    left_join(mpas, by = c("x", "y")) |>
    group_by(critter, step) |>
    summarise(
      biomass = sum(b),
      spawning_biomass = sum(ssb),
      numbers = sum(n),
      mpa_biomass = sum(b[mpa]),
      fished_biomass = sum(b[!mpa])
    ) |>
    ungroup() |>
    pivot_longer(-c(critter, step), values_to = "control_value") |>
    mutate(fleet = "nature")
  
  
  control_fleets <- control$fleets[[1]] |>
    select(-mpa) |>
    left_join(mpas, by = c("x", "y")) |>
    group_by(fleet, critter, step) |>
    summarise(
      catch = sum(catch, na.rm = TRUE),
      revenue = sum(revenue, na.rm = TRUE),
      effort = sum(effort, na.rm = TRUE),
      mean_cpue = mean(cpue, na.rm = TRUE)
    ) |>
    ungroup() |>
    pivot_longer(-c(fleet, critter, step), values_to = "control_value")
  
  
  
  treatment_outcomes <- treatment_fauna |>
    bind_rows(treatment_fleets)
  
  
  control_outcomes <- control_fauna |>
    bind_rows(control_fleets)
  
  outcomes <- treatment_outcomes |>
    left_join(control_outcomes, by = c("step", "critter", "fleet", "name")) |>
    mutate(percent_mpa_effect = value / control_value - 1)
  
  # out <- list(base_fauna = base_fauna, base_fleet = base_fleets)
  
  return(outcomes)
}


calculate_gradients <- function(x, prop_mpa) {
  fauna <-  x$fauna[[1]]
  
  fleets <-  x$fleets[[1]]
  
  
  if (prop_mpa > 0 & prop_mpa < 1) {
    biomass_gradient <- fauna |>
      group_by(critter, step) |>
      nest() |>
      mutate(biomass_gradient = map_dbl(
        data,
        ~ as.numeric(lm(log(b + 1e-6) ~ distance_to_mpa_edge + ssb0, data = .x |> filter(!mpa))$coefficients["distance_to_mpa_edge"])
      )) |>
      select(-data) |>
      mutate(fleet = "nature")
    
    cpue_gradient <- fleets |>
      group_by(fleet, critter, step) |>
      nest() |>
      mutate(cpue_gradient = map_dbl(
        data,
        ~ as.numeric(lm(log(cpue + 1e-6) ~ distance_to_mpa_edge + ssb0, data = .x |> filter(!mpa))$coefficients["distance_to_mpa_edge"])
      )) |>
      select(-data)
    
    effort_gradient <- fleets |>
      group_by(fleet, critter, step) |>
      nest() |>
      mutate(effort_gradient = map_dbl(
        data,
        ~ as.numeric(lm(log(effort + 1e-6) ~ distance_to_mpa_edge, data = .x |> filter(!mpa))$coefficients["distance_to_mpa_edge"])
      )) |>
      select(-data)
    
    fleet_gradients <- cpue_gradient |>
      left_join(effort_gradient, by = c("step", "fleet", "critter"))
    
    out <- fleet_gradients |>
      bind_rows(biomass_gradient) |>
      ungroup()
  } else {
    out <- data.frame(step = NA,
                      critter = NA,
                      fleet = NA)
  }
  
  
  return(out)
  
}

tmp <- tmp |>
  ungroup() |>
  # slice(1) |>
  mutate(
    outcomes = map2(treatment, control, calculate_outcomes, .progress = "calculating MPA outcomes"),
    gradients = map2(treatment, prop_mpa, calculate_gradients, .progress = "calculating MPA gradients")
  )

# then, calculate MPA outcomes


state_variables <- state_experiments |>
  select(state_id, data) |>
  unnest(cols = data) |>
  select(
    state_id,
    kiss,
    mpa_response,
    starts_with("sigma"),
    starts_with("spatial"),
    fleet_model
  ) |>
  unique() |>
  mutate(state_id = as.character(state_id))

species_variables <- state_experiments |>
  select(state_id, data) |>
  unnest(cols = data) |>
  mutate(state_id = as.character(state_id)) |> 
  select(-habitat,-critter)

mpa_outcomes <- tmp |>
  select(-treatment, -control, -gradients) |>
  unnest(cols = outcomes) |>
  ungroup() |>
  left_join(species_variables, by = c("state_id", "critter" = "scientific_name")) |>
  left_join(placement_experiments, by = c("placement_id", "prop_mpa"))

mpa_gradients <- tmp |>
  select(-treatment, -control, -outcomes) |>
  unnest(cols = gradients) |>
  ungroup()

slopes <- mpa_gradients |>
  group_by(fleet, prop_mpa) |>
  summarise(
    real_neg_slope = mean(cpue_gradient < -0.025, na.rm = TRUE),
    real_pos_slope = mean(cpue_gradient > 0.025, na.rm = TRUE)
  )

mpa_outcomes <- mpa_outcomes |>
  left_join(mpa_gradients,
            by = join_by(id, placement_id, state_id, prop_mpa, critter, step, fleet))

# make plots --------------------------------------------------------------

fauna_results |>
  filter(between(prop_mpa, 0.2, 0.3),!mpa) |>
  ggplot(aes(total_mpa_distance, b, color = prop_mpa)) +
  geom_point() +
  facet_wrap( ~ critter) +
  scale_color_viridis_c()



mpa_outcomes |>
  filter(name %in% c("biomass", "catch", "mean_cpue")) |>
  ggplot(aes(percent_mpa_effect)) +
  geom_histogram() +
  facet_wrap( ~ name)

spillover_vs_catch_plot <- mpa_outcomes |>
  filter(!is.na(cpue_gradient), kiss) |>
  filter(name %in% c("catch")) |>
  ggplot(aes(cpue_gradient, pmin(1.5, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.5) +
  scale_x_continuous(
    name = "Change in CPUE per unit distance from MPA", 
    labels = scales::percent
  ) +
  scale_y_continuous(name = "Change in Total Catch", labels = scales::percent) +
  scale_color_viridis_c(
    name = "% area in MPA",
    labels = scales::percent,
    option = "magma",
    guide = guide_colorbar(
      barwidth = unit(12, "lines"),
      frame.colour = "black"
    )
  ) +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off")  + 
  labs(caption = "Negative x-axis values impy stronger spillover signal")


line_vs_catch_plot <- mpa_outcomes |>
  filter(!is.na(effort_gradient)) |>
  filter(name %in% c("catch")) |>
  ggplot(aes(effort_gradient, pmin(1.5, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.5) +
  scale_x_continuous(
    name = "Change in Effort per unit distance from MPA", 
    labels = scales::percent
  ) +
  scale_y_continuous(name = "Change in Total Catch", labels = scales::percent) +
  scale_color_viridis_c(
    name = "% area in MPA",
    labels = scales::percent,
    option = "magma",
    guide = guide_colorbar(
      barwidth = unit(12, "lines"),
      frame.colour = "black"
    )
  ) +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off")  + 
  labs(caption = "Negative x-axis values impy stronger fishing the line signal")


spillover_vs_biomass_plot <- mpa_outcomes |>
  filter(!is.na(biomass_gradient)) |>
  filter(name %in% c("biomass")) |>
  ggplot(aes(biomass_gradient, pmin(1.5, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.33) +
  scale_x_continuous(
    name = "Change in CPUE per unit distance from MPA", 
    labels = scales::percent
  ) +
  scale_y_continuous(name = "Change in Total Biomass", labels = scales::percent) +
  scale_color_viridis_c(
    name = "% area in MPA",
    labels = scales::percent,
    option = "magma",
    guide = guide_colorbar(
      barwidth = unit(12, "lines"),
      frame.colour = "black"
    )
  ) +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off")


mpa_outcomes |>
  filter(!is.na(effort_gradient), placement_strategy == "area") |>
  filter(name %in% c("catch")) |>
  ggplot(aes(effort_gradient, pmin(2, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.75) +
  facet_wrap( ~ name) +
  scale_x_continuous(name = "Change in Effort per unit distance from Nearest MPA border", labels = scales::percent) +
  scale_y_continuous(name = "Change in total catch caused by MPA", labels = scales::percent) +
  facet_wrap( ~ critter, scales = "free") +
  scale_color_viridis_c(name = "% area in MPA", labels = scales::percent)

mpa_outcomes |>
  filter(!is.na(effort_gradient)) |>
  filter(name %in% c("catch")) |>
  ggplot(aes(effort_gradient, pmin(2, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.75) +
  facet_wrap( ~ name) +
  scale_x_continuous(name = "Change in effort per unit distance from Nearest MPA border", labels = scales::percent) +
  scale_y_continuous(name = "Change in total catch cased by MPA", labels = scales::percent) +
  scale_color_viridis_c(name = "% area in MPA", labels = scales::percent)

cpue_gradient_vs_biomass_plot <- mpa_outcomes |>
  filter(!is.na(biomass_gradient), placement_strategy == "area") |>
  filter(name %in% c("biomass")) |>
  ggplot(aes(biomass_gradient, pmin(2, percent_mpa_effect), color = prop_mpa)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, alpha = 0.75) +
  facet_wrap( ~ name) +
  scale_x_continuous(name = "Change in CPUE with distance from MPA", labels = scales::percent) +
  scale_y_continuous(name = "Change in biomass inside MPA caused by MPA", labels = scales::percent) +
  facet_wrap( ~ critter, scales = "free") +
  scale_color_viridis_c(name = "% area in MPA", labels = scales::percent)



# cpue_gradient_vs_catch_plot <- mpa_outcomes |>
#   filter(!is.na(biomass_gradient), placement_strategy == "area") |>
#   filter(name %in% c("catch")) |>
#   ggplot(aes(biomass_gradient, pmin(2, percent_mpa_effect), color = prop_mpa)) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   geom_point(size = 3, alpha = 0.75) +
#   facet_wrap( ~ name) +
#   scale_x_continuous(name = "Change in CPUE with distance from MPA", labels = scales::percent) +
#   scale_y_continuous(name = "Change in biomass inside MPA caused by MPA", labels = scales::percent) +
#   facet_wrap( ~ critter, scales = "free") +
#   scale_color_viridis_c(name = "% area in MPA", labels = scales::percent)
# 


# drivers of outcomes -----------------------------------------------------

outcome_drivers <- mpa_outcomes |> 
  select(percent_mpa_effect, state_id, placement_id, critter, prop_mpa, name) |> 
  left_join(species_variables, by = c("state_id", "critter" = "scientific_name")) |> 
  left_join(placement_experiments, by = c("placement_id","prop_mpa")) |> 
  mutate(positive_effect = percent_mpa_effect > 0) |> 
  select(-ends_with("_id")) |> 
  ungroup() |> 
  mutate(placebo = rnorm(length(percent_mpa_effect)))

test <- mpa_outcomes |> 
  select(percent_mpa_effect, state_id, placement_id, critter, prop_mpa, name) |> 
  left_join(species_variables, by = c("state_id", "critter" = "scientific_name")) |> 
  left_join(placement_experiments, by = c("placement_id","prop_mpa")) |> 
  mutate(percent_mpa_effect = pmin(100,100 * percent_mpa_effect))  |> 
  pivot_wider(names_from = "name", values_from = "percent_mpa_effect")


quad_labels <- data.frame(x = c(50,50,-24,-24),y =c(50,-50,50,-50), label = c("Win-Win", "Win-Lose","Lose-Win","Lose-Lose"))

thirty_protected_plot <- test |>
  filter(between(prop_mpa, 0.2, 0.4)) |>
  ggplot(aes(biomass, catch)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(aes(color = f_v_m), alpha = 0.25, size = 3) +
  geom_text(
    data = quad_labels,
    aes(x, y, label = label),
    size = 6,
    color = "red"
  ) +
  scale_color_viridis_c(
    "BAU Fishing Mortality",
    breaks = c(0,0.25),
    labels = c("Low","High"),
    limits = c(0, 0.25),
    option = "plasma",
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "black",
      barwidth =  unit(11, "lines")
    )
  )  +
  scale_x_continuous(name = "% Change in Species Biomass", limits = c(-50,100)) +
  scale_y_continuous(name = "% Change in Species Catch") +
  theme(legend.position = "bottom") + 
  labs(caption = "20-40% of area in MPA")

thirty_protected_plot

thirty_protected_plot <- ggMarginal(thirty_protected_plot, type = "histogram", fill = "steelblue") 

thirty_protected_plot

# compare to emulated -----------------------------------------------------

quad_labels <-
  data.frame(
    x = c(50, 50, -25, -25),
    y = c(50, -50, 50, -50),
    label = c("Win-Win", "Win-Lose", "Lose-Win", "Lose-Lose")
  )

emulated_model_results <- emulated_experiment_results |> 
  select(state_id, prop_mpa, critter, mpa_experiment, depletion) |> 
  unnest(cols = mpa_experiment) |> 
  filter(year == max(year)) |> 
  mutate(emulated_percent_mpa_effect = pmin(5,yield_mpa / yield_bau - 1),
         rough_f = 1 - depletion)


plot_stuff <- emulated_model_results |>
  mutate(
    yield_effect = pmin(100, 100 * (yield_mpa / yield_bau - 1)),
    biomass_effect = pmin(100, 100 * (b_mpa / b_bau - 1))
  ) 



basic_thirty_protected_plot <- plot_stuff |>
  filter(between(prop_mpa, 0.2, 0.4)) |>
  ggplot(aes(biomass_effect, yield_effect, color = rough_f)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(alpha = 0.25, size = 3) +
  geom_text(
    data = quad_labels,
    aes(x, y, label = label),
    size = 6,
    color = "red"
  ) +
  scale_color_viridis_c(
    "BAU Fishing Mortality",
    breaks = c(0,1),
    labels = c("Low","High"),
    limits = c(0, 1),
    option = "plasma",
    guide = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "black",
      barwidth =  unit(11, "lines")
    )
  )  +
  scale_x_continuous(name = "% Change in Species Biomass", limits = c(-50,100)) +
  scale_y_continuous(name = "% Change in Species Catch") +
  theme(legend.position = "bottom") + 
  labs(caption = "20-40% of area in MPA")

basic_thirty_protected_plot <- ggMarginal(basic_thirty_protected_plot, type = "histogram", fill = "steelblue") 


basic_thirty_protected_plot


full_model_results <- mpa_outcomes |> 
  select(state_id, placement_id, critter, prop_mpa, percent_mpa_effect, name) |> 
  filter(name %in% c("catch", "biomass")) |> 
  mutate(state_id = as.integer(state_id))

 emulated_yield_and_biomass_results <- emulated_model_results |>
   mutate(
     catch_effect = (yield_mpa / yield_bau - 1),
     biomass_effect = (b_mpa / b_bau - 1)
   ) |>
   select(state_id, critter, prop_mpa, ends_with("_effect")) |> 
   pivot_longer(ends_with("_effect"), values_to = "emulated_percent_mpa_effect", names_to = "name") |> 
   mutate(name = str_remove_all(name, "_effect"))
   

comparison <- full_model_results |> 
  left_join(emulated_yield_and_biomass_results, by = c("state_id", "critter","prop_mpa", "name"))

# breaks <- seq(-1,1, by = .5)
# 
# labels <- c(paste0(seq(-100,50, by = 50),"%"), expression(paste("",geq,"100%")))

a <- data.frame(x = runif(1000,-1,1)) |> 
  mutate(y = rnorm(1000,x, .4)) |> 
  mutate(name = "biomass")

b <- a |> 
  mutate(name = "catch")

example_plot <- a |> bind_rows(b)|> 
  ggplot(aes(pmin(1,x), pmin(1,y))) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(alpha = 0.25, size = 2, color = "steelblue") + 
  geom_abline(slope = 1, intercept = 0, color = "tomato") + 
  facet_wrap(~name, scales = "free") + 
  scale_x_continuous("Complex-er Model MPA Effect", labels = scales::percent, limits = c(-.5,1), oob = squish) + 
  scale_y_continuous("Simpler Model MPA Effect", labels = scales::percent, limits = c(-1,1)) + 
  labs(caption = "Simpler model tuned to emulate complex model. 20-40% MPA") + 
  scale_fill_viridis_c()



marlin_vs_pt_plot <- comparison |> 
  left_join(placement_experiments |> select(placement_id, prop_mpa, placement_strategy), by = c("placement_id", "prop_mpa")) |>
  left_join(state_variables |> select(state_id, kiss, fleet_model) |> mutate(state_id = as.integer(state_id)), by = "state_id") |> 
  filter(between(prop_mpa, 0.2, 0.4), fleet_model == "constant effort") |>
  ggplot(aes(pmin(1,percent_mpa_effect), pmin(1,emulated_percent_mpa_effect))) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(alpha = 0.25, size = 2, color = "steelblue") + 
  geom_abline(slope = 1, intercept = 0, color = "tomato") + 
  facet_wrap(~name, scales = "free") + 
  scale_x_continuous("Complex-er Model MPA Effect", labels = scales::percent, limits = c(-.5,1), oob = squish) + 
  scale_y_continuous("Simpler Model MPA Effect", labels = scales::percent, limits = c(-.5,1)) + 
  labs(caption = "Simpler model tuned to emulate complex model. 20-40% MPA") + 
  scale_fill_viridis_c()

# marlin_vs_pt_plot <- ggMarginal(marlin_vs_pt_plot, type = "histogram")



# outcome drivers ---------------------------------------------------------



outcome_drivers |> 
  group_by(name) |> 
  summarise(mean(positive_effect, na.rm = TRUE))
  
catch_effect_tree <-
  rpart(
    positive_effect ~ .,
    data = outcome_drivers |> filter(kiss, name == "catch") |> select(-percent_mpa_effect)
  )

rpart.plot::rpart.plot(catch_effect_tree)


biomass_effect_forest <-
  ranger(
    positive_effect ~ .,
    importance = "permutation",
    data = outcome_drivers |> filter(
      name == "biomass",
      between(prop_mpa, 0.1, 0.4),
      !is.na(spawning_season)
    ) |> select(-percent_mpa_effect, -name)
  )

biomass_importance <-
  tibble(name = names((
    ranger::importance(biomass_effect_forest)
  )),
  importance = (ranger::importance(biomass_effect_forest)),
  outcome = "biomass")

biomass_importance_plot <- biomass_importance |>
  ggplot(aes(reorder(name, importance), importance)) +
  geom_col() +
  coord_flip() + 
  scale_x_discrete(name = '') + 
  scale_y_continuous(name = "Variable Importance for positive biomass impact")


catch_effect_forest <-
  ranger(
    positive_effect ~ .,
    importance = "permutation",
    data = outcome_drivers |> filter(name == "catch",!is.na(spawning_season)) |> select(-percent_mpa_effect,-name)
  )


catch_importance <-
  tibble(name = names((
    ranger::importance(catch_effect_forest)
  )),
  importance = (ranger::importance(catch_effect_forest)),
  outcome = "catch")

catch_importance |> 
  bind_rows(biomass_importance) |> 
  group_by(outcome) |> 
  mutate(importance = importance / max(importance)) |> 
  ggplot(aes(reorder(name, importance), importance, color = outcome)) +
  geom_point() +
  coord_flip() + 
  scale_x_discrete(name = '') + 
  scale_y_continuous(name = "Variable Importance for positive catch impact")


catch_importance_plot <- catch_importance |>
  ggplot(aes(reorder(name, importance), importance)) +
  geom_col() +
  coord_flip() + 
  scale_x_discrete(name = '') + 
  scale_y_continuous(name = "Variable Importance for positive catch impact")

catch_importance_plot




biomass_effect_tree <-
  rpart(positive_effect ~ ., data = outcome_drivers |> filter(!kiss, name == "biomass", prop_mpa <= 0.4) |> select(-percent_mpa_effect))

rpart.plot::rpart.plot(biomass_effect_tree)

# a classification model just with MPA size, placement strategy, f_v_m, adult diffusion, or something like that 


quadrant_outcomes <- outcome_drivers |> 
  filter(name %in% c("biomass","catch")) |> 
  select(-positive_effect, -placebo) |> 
  pivot_wider(names_from = name, values_from = percent_mpa_effect) |> 
  mutate(biomass_win = ifelse(biomass >= 0,"win","lose"), 
         catch_win = ifelse(catch >= 0,"win","lose"),
         outcome = factor(glue::glue("biomass:{biomass_win}-catch:{catch_win}"))) |> 
  mutate(across(where(is.numeric), ~ replace_na(.x,0)))

quadrant_model <-
  ranger(outcome ~ .,
         data = quadrant_outcomes |> select(-biomass, -catch, -ends_with("_win")) |> mutate(placebo = rnorm(length(outcome))),
         importance = "permutation"
  )

outcome_confusion <-
  data.frame(truth = quadrant_outcomes$outcome, prediction = quadrant_model$predictions) |>
  yardstick::conf_mat(truth, prediction)

autoplot(outcome_confusion, type = "heatmap")

quadrant_importance <-
  tibble(name = names((
    ranger::importance(quadrant_model)
  )),
  importance = (ranger::importance(quadrant_model)),
  outcome = "quadrant") |> 
  arrange(desc(importance))

outcome_importance_plot <- catch_importance |> 
  bind_rows(biomass_importance) |> 
  bind_rows(quadrant_importance) |> 
  # mutate(importance = scales::rescale(importance, to = c(0,1))) |> 
  group_by(outcome) |> 
  mutate(importance = importance / max(importance)) |> 
  mutate(importance = pmax(0,if_else(importance <= importance[name == "placebo"], 0, importance))) |>
  ggplot(aes(outcome,reorder(name, importance), fill = importance)) +
  geom_tile(color = "black") +
  scale_x_discrete(name = 'Outcome Measure', expand = c(0,0)) + 
  scale_y_discrete(name = "", expand = c(0,0)) + 
    scale_fill_viridis_c(
      option = "magma",
      name = "Importance",
      na.value = "transparent",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        barheight = unit(12, "lines")
      )
    )
  

outcome_importance_plot



balanced_outcomes <- recipe(~., quadrant_outcomes) %>%
  step_upsample(outcome, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL) |> 
  filter(between(prop_mpa, 0.3,0.31))

basic_quadrant_model <- ranger(outcome ~ .,
         data = balanced_outcomes |> 
           select(outcome, prop_mpa, recruit_diffusion, f_v_m, adult_diffusion, placement_strategy),
         importance = "permutation"
  )

quadrant_prediction_performance <-
  data.frame(truth = balanced_outcomes$outcome, prediction = basic_quadrant_model$predictions, prop_mpa = balanced_outcomes$prop_mpa)
  
  
quadrant_confusion <- yardstick::conf_mat(quadrant_prediction_performance |> filter(prop_mpa < 0.4, prop_mpa > 0.1),truth, prediction)

confusion_plot <- autoplot(quadrant_confusion, type = "heatmap")

confusion_plot

# save things -------------------------------------------------------------



plots <- ls()[str_detect(ls(), "_plot$")]

purrr::walk(
  plots,
  \(x) ggsave(
    filename = file.path(plot_dir, glue::glue("{x}.pdf")),
    plot = get(x),
    width = plot_width,
    height = plot_height
  )
)

purrr::walk(
  plots,
  \(x) write_rds(
    get(x),
    file = file.path(results_dir, glue::glue("{x}.rds"))
  )
)
