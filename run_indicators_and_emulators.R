#### runs medium MPA indicator and emulator examples, where simple means multiple species and one fleet ####

# setup -------------------------------------------------------------------

foos <- list.files(here::here("R"))

purrr::walk(foos, ~ source(here::here("R", .x)))

prep_run(n_states = 150, run_name = "v0.1", drop_patches = TRUE) # loads packages and creates and returns some global variables for the analysis

library(tictoc)

resolution <- c(rx, ry)

# difficulties <- c("simple")

difficulties <- c("complex","medium","simple")

# difficulties <- c("complex")

difficulty_species <- list(
  simple = c("lutjanus malabaricus"),
  medium = c(
    "lutjanus malabaricus",
    "pristipomoides filamentosus",
    "epinephelus fuscoguttatus",
    "carcharhinus amblyrhynchos"
  ),
  complex = c(
    "lutjanus malabaricus",
    "pristipomoides filamentosus",
    "epinephelus fuscoguttatus",
    "carcharhinus amblyrhynchos"
  )
)

baseline_state_experiments <-
  tibble(
    kiss = sample(c(FALSE, TRUE), n_states, replace = TRUE),
    mpa_response = sample(c("stay", "leave"), n_states, replace = TRUE),
    habitat_patchiness = runif(n_states, 1e-3, .25),
    max_abs_cor = runif(n_states, 1e-3, 1),
    spatial_q = sample(
      c(TRUE, FALSE),
      n_states,
      replace = TRUE,
      prob = c(1, 3)
    ),
    spatial_allocation = sample(c("ppue", "rpue", "revenue"), n_states, replace = TRUE),
    fleet_model = sample(c("constant effort"), n_states, replace = TRUE)
  ) %>%
  mutate(state_id = 1:nrow(.))

port_locations <-
  tibble(x = c(1, resolution[1]), y = c(1, resolution[2])) # coordinates to test impact of highly disparate ports



for (difficulty in difficulties) {
  tic()
  set.seed(42)
  critters <-
    tibble(scientific_name = difficulty_species[[difficulty]])
  
  state_experiments <- baseline_state_experiments %>%
    mutate(
      habitats = pmap(
        list(kp = habitat_patchiness, max_abs_cor = max_abs_cor),
        sim_habitat,
        critters = critters$scientific_name,
        resolution = resolution,
        patch_area = patch_area,
        .progress = "Simulating habitats"
      )
    ) %>%
    mutate(critter_correlations = map(habitats, ~ process_correlations(.x$critter_correlations))) |>
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
      f_v_m = runif(length(state_id), 0.01, 0.33),
      adult_diffusion = runif(length(state_id), min = 0, max = 1.25 * patch_area),
      steepness = runif(length(state_id), min = 0.6, max = 1),
      ssb0 = rlnorm(length(state_id), log(100 * patches), 0.6),
      recruit_diffusion = runif(length(state_id), min = 0, max =  1.25 * patch_area),
      hyperallometry = sample(c(1, 2), length(state_id), replace = TRUE),
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
      spawning_season = ifelse(spawning_aggregation, NA, NA),
      ontogenetic_shift = sample(c(TRUE, FALSE), length(state_id), replace = TRUE)
    ) |>
    mutate(ontogenetic_shift = ifelse(kiss, FALSE, ontogenetic_shift)) |>
    mutate(density_dependence = ifelse(ontogenetic_shift, "local_habitat", density_dependence))
  
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
    mutate(
      fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)),
      sels = map(fauna, ~ runif(length(.x), 0.1, 1.25)),
      prices = map(fauna, ~ runif(length(.x), 1, 10))
    )
  
  # state_experiments$fauna[[1]]$`lutjanus malabaricus`$diffusion_foundation[[1]] |> image()
  
  # state_experiments$fauna[[1]]$`lutjanus malabaricus`$movement_matrix[[1]] |> image()
  #
  # stop()
  
  state_experiments <- state_experiments %>%
    ungroup() %>%
    mutate(use_ports = sample(c(TRUE,FALSE), nrow(.), replace = TRUE)) %>%
    mutate(
      fleet = pmap(
        list(
          fauna = fauna,
          state = data,
          sels = sels,
          prices = prices,
          use_ports = use_ports
        ),
        create_fleets,
        difficulty = difficulty,
        port_locations = port_locations,
        resolution = resolution
      )
    )
  
  # add in starting conditions
  init_condit <- function(fauna, fleets, years = 125) {
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
    mutate(tmp = map2(
      fauna,
      fleet,
      init_condit,
      .progress = glue::glue("simulating initial {difficulty} conditions")
    ))
  
  state_experiments$starting_conditions <-
    map(state_experiments$tmp, "starting_conditions")
  
  state_experiments$proc_starting_conditions <-
    map(state_experiments$tmp, "proc_starting_conditions")
  
  state_experiments <- state_experiments %>%
    select(-tmp)
  
  state_depletions <-
    map_df(state_experiments$starting_conditions,
           ~ map_df(.x, ~ map_df(.x, ~ sum(.x$ssb_p_a) / .x$ssb0)),
           .id = "state_id") |>
    pivot_longer(-state_id, names_to = "critter", values_to = "step_depletion") |>
    group_by(state_id, critter) |>
    summarise(depletion = mean(step_depletion)) |>
    mutate(state_id = as.integer(state_id))
  
  state_experiments <- state_experiments |>
    left_join(state_depletions |> group_by(state_id) |> nest(.key = "depletion"),
              by = "state_id")
  
  init_dep_plot <-  state_depletions %>%
    ggplot(aes(depletion)) +
    geom_histogram() +
    facet_wrap(~ critter)
  
  ggsave(file.path(fig_dir, glue::glue("{difficulty}_init_dep.pdf")), init_dep_plot)
  
  
  for (i in 1:nrow(state_experiments)) {
    tmp <- state_experiments$proc_starting_conditions[[i]]$fauna |>
      filter(step == max(step)) |>
      group_by(critter) |>
      mutate(n = n / max(n)) |>
      ggplot(aes(x, y, fill = n)) +
      geom_tile() +
      facet_wrap(~ critter) +
      scale_fill_viridis_c(limits = c(0, 1))
    
    tmp2 <- state_experiments$proc_starting_conditions[[i]]$fleet |>
      filter(step == max(step)) |>
      group_by(fleet, step) |>
      mutate(effort = effort / max(effort, na.rm = TRUE)) |>
      ggplot(aes(x, y, fill = effort)) +
      geom_tile() +
      facet_grid(step ~ fleet , labeller = label_both) +
      scale_fill_viridis_c()
    
    
    ggsave(file.path(
      fig_dir,
      glue::glue("{difficulty}_state_{i}_habitat.pdf")
    ), tmp)
    
    ggsave(file.path(
      fig_dir,
      glue::glue("{difficulty}_state_{i}_fleet_allocation.pdf")
    ), tmp2)
    
    
  }
  # fit two-patch biomass dynamics (twopbd) emulations
  emulated_state_experiments <- state_experiments |>
    mutate(twopbd_params = map2(
      fauna,
      fleet,
      safely(fit_twopbd),
      .progress = glue("fitting {difficulty} emulators")
    )) |>
    mutate(fit_worked = map_lgl(twopbd_params, ~ is.null(.x$error))) |>
    filter(fit_worked) |>
    mutate(twopbd_params = map(twopbd_params, "result")) |>
    select(state_id, fleet, twopbd_params) |>
    unnest(cols = twopbd_params)
  
  emulated_experiment_results <-
    expand_grid(
      state_id = unique(emulated_state_experiments$state_id),
      prop_mpa = seq(0.05, 0.95, by = 0.05)
    ) |>
    left_join(emulated_state_experiments,
              relationship = "many-to-many",
              by = "state_id") |>
    left_join(state_depletions, by = c("state_id", "critter")) |>
    mutate(mpa_experiment = pmap(
      list(
        params = twopbd_params,
        fleet = fleet,
        depletion = depletion,
        local_dd = local_dd,
        p_mpa = prop_mpa
      ),
      run_twopbd_mpas,
      .progress = glue::glue("running {difficulty} emulator experiments")
    ))
  
  write_rds(emulated_experiment_results, file = file.path(
    results_dir,
    glue("{difficulty}_emulated_experiment_results.rds")
  ))
  
  placement_experiments <- expand_grid(
    placement_strategy = c("target_fishing", "area", "avoid_fishing"),
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
  
  write_rds(placement_experiments, file = file.path(
    results_dir,
    glue("{difficulty}_placement_experiments.rds")
  ))
  
  write_rds(state_experiments |> select(-contains("starting_conditions")), file = file.path(results_dir, glue("{difficulty}_state_experiments.rds")))
  
  write_rds(emulated_state_experiments, file = file.path(
    results_dir,
    glue("{difficulty}_emulated_state_experiments.rds")
  ))
  
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
        results = pmap(
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
          drop_patches = drop_patches
        )
      )
    
    
    tmp$results <-
      purrr::set_names(tmp$results, state_experiments$state_id)
    # Sys.time() - a
    
    experiment_results[[p]] <- tmp$results 
    
    pb$tick()
    
    
  } # close p loop
  
  future::plan(future::sequential)
  
  write_rds(experiment_results, file = file.path(results_dir, glue(
    "{difficulty}_experiment_results.rds"
  )))
  rm(experiment_results)
  
  processed_sims <- process_sims(difficulty_level = difficulty, results_dir = results_dir, drop_patches = drop_patches)
  
  write_rds(processed_sims, file = file.path(results_dir, glue("{difficulty}_processed_sims.rds")))
  
  tmp <- processed_sims$mpa_outcomes |>
    select(percent_mpa_effect,
           fleet,
           state_id,
           placement_id,
           critter,
           prop_mpa,
           name) |>
    left_join(processed_sims$species_variables,
              by = c("state_id", "critter" = "scientific_name")) |>
    left_join(placement_experiments, by = c("placement_id", "prop_mpa")) # |>
  # mutate(percent_mpa_effect = pmin(100, 100 * percent_mpa_effect))
  
  fleet_outcomes <- tmp |>
    filter(fleet != "nature") |>
    pivot_wider(names_from = "name", values_from = "percent_mpa_effect")
  
  nature_outcomes <- tmp |>
    filter(fleet == "nature") |>
    select(ends_with("id"), critter, prop_mpa, name, percent_mpa_effect) |>
    pivot_wider(names_from = "name", values_from = "percent_mpa_effect")
  
  test <- fleet_outcomes |>
    left_join(nature_outcomes,
              by = join_by(state_id, placement_id, critter, prop_mpa))
  
  
  quad_labels <- data.frame(
    x = c(50, 50, -24, -24),
    y = c(50, -50, 50, -50),
    label = c("Win-Win", "Win-Lose", "Lose-Win", "Lose-Lose")
  )
  thirty_protected_plot <- test |>
    filter(between(prop_mpa, 0.2, 0.4)) |>
    ggplot(aes(biomass, catch)) +
    geom_vline(xintercept = 0, color = "black") +
    geom_hline(yintercept = 0, color = "black") +
    geom_point(aes(color = f_v_m), alpha = 0.25, size = 3) +
    # geom_text(
    #   data = quad_labels,
    #   aes(x, y, label = label),
    #   size = 6,
    #   color = "red"
    # ) +
    scale_color_viridis_c(
      "BAU Fishing Mortality",
      breaks = c(0, .5),
      labels = c("Low", "High"),
      limits = c(0, .5),
      option = "plasma",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        barwidth =  unit(11, "lines")
      )
    )  +
    scale_x_continuous(name = "X Change in Species Biomass", oob = squish, limits = c(NA, 0.5)) +
    scale_y_continuous(name = "X Change in Species Catch", oob = squish, limits = c(NA,0.5)) +
    theme(legend.position = "bottom") +
    labs(caption = "20-40% of area in MPA")
  
  
  
  thirty_protected_plot
  
  ggsave(file.path(
    fig_dir,
    glue::glue("{difficulty}_thirty_protected_plot.pdf")
  ), thirty_protected_plot)
  
  
  thirty_protected_plot <- ggMarginal(thirty_protected_plot,
                                      type = "histogram",
                                      fill = "steelblue")
  
  thirty_protected_plot
  toc()
  
  rm(list = c("emulated_experiment_results","state_experiments"))
} # close difficulty loop
