#### runs medium MPA indicator and emulator examples, where simple means multiple species and one fleet ####

# setup -------------------------------------------------------------------

foos <- list.files(here::here("R"))

purrr::walk(foos, ~ source(here::here("R", .x)))

prep_run(
  n_states = 4,
  run_name = "indicators_test",
  drop_patches = FALSE,
  experiment_workers = 8,
  rx = 20,
  ry = 20,
  patch_area = 5
) # loads packages and creates and returns some global variables for the analysis


save_experiments <- FALSE


library(tictoc)

project <- "indicators"

resolution <- c(rx, ry)

mpa_years <- 20

difficulties <- c("complex", "medium", "simple")
# difficulties <- c("epo")

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
  ),
  epo = c("thunnus albacares",
          "katsuwonus pelamis",
          "thunnus obesus",
          "carcharhinus falciformis",
          "isurus oxyrinchus",
          "sphyrna zygaena",
          "carcharhinus longimanus")
)


critter_templates <- map(unique(list_c(difficulty_species)),
                         ~ marlin::create_critter(scientific_name = .x, seasons = seasons)) |>
  set_names(unique(list_c(difficulty_species)))



# if ("epo" %in% difficulties){
#   
#   sdmsish <- read_rds(here("data","epo_sdmsish.rds"))
#   
#   popsizeish <- read_rds(here("data","epo_popsizeish.rds"))
#   
# }

baseline_state_experiments <-
  tibble(
    kiss = sample(c(FALSE, TRUE), n_states, replace = TRUE),
    mpa_response = sample(c("stay", "leave"), n_states, replace = TRUE),
    habitat_patchiness = runif(n_states, 1e-3, .1),
    max_abs_cor = runif(n_states, 1e-3, 1),
    spatial_q = sample(
      c(TRUE, FALSE),
      n_states,
      replace = TRUE,
      prob = c(1, 3)
    ),
    spatial_allocation = sample(c("ppue", "rpue", "revenue"), n_states, replace = TRUE),
    fleet_model = sample(c("constant_effort", "open_access"), n_states, replace = TRUE)
  ) %>%
  mutate(state_id = 1:nrow(.))

port_locations <-
  tibble(x = c(1, resolution[1]), y = c(1, resolution[2])) # coordinates to test impact of highly disparate ports

future::plan(future::multisession, workers = experiment_workers)


for (difficulty in difficulties) {
  tic()
  set.seed(42)
  critters <-
    tibble(scientific_name = difficulty_species[[difficulty]])
  message("creating habitats")

  state_experiments <- baseline_state_experiments %>%
    mutate(
      habitats = future_pmap(
        list(kp = habitat_patchiness, max_abs_cor = max_abs_cor),
        sim_habitat,
        critters = critters$scientific_name,
        resolution = resolution,
        patch_area = patch_area,
        .progress = TRUE,
        .options = furrr_options(seed = TRUE)
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
      f_v_m = runif(length(state_id), 0.01, 0.24),
      adult_diffusion = sample(c(1, 10, 100), length(state_id), replace = TRUE),
      recruit_diffusion = sample(c(1, 10, 100), length(state_id), replace = TRUE),
      steepness = runif(length(state_id), min = 0.6, max = 1),
      b0 = rlnorm(length(state_id), log(100 * patches), 0.6),
      hyperallometry = sample(c(1, 2), length(state_id), replace = TRUE),
      sigma_rec = sample(c(0, 0.2, 0.8), length(state_id), replace = TRUE),
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
  
  state_experiments$b0 <- ifelse(str_detect(state_experiments$critter,("carcharhinus|sphyrna|prionace")),state_experiments$b0 / 10,state_experiments$b0) # try and keep shark popsize on average smaller than others
  
  message("finished habitats")
  
  message("creating critters")
  state_experiments <- state_experiments %>%
    rename(scientific_name = critter) |>
    mutate(
      critter = future_pmap(
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
          b0 = b0,
          kiss = kiss,
          sigma_rec = sigma_rec
        ),
        create_experiment_critters,
        critter_templates = critter_templates,
        resolution = resolution,
        seasons = seasons,
        .progress = TRUE,
        .options = furrr_options(seed = TRUE)
      )
    )
  message("finished critters")
  
  
  # function to randomize selectivity parameters
  selfoo <- function(x,i){
    out <-   list(
      sel_start = runif(i, .05, 2),
      # proportion of length 50% mature at 50% selectivity
      sel_delta = runif(i, 1e-3, .25),
      # offset for 95% selectivity
      sel05_anchor = runif(i, 0, 0.9),
      sel_at_linf = runif(i, 0, 1)
    )
    
    out$sel05_anchor <- out$sel05_anchor * out$sel_start # must be smaller than length at 50% selectivity
    
    return(out)
    
  }
  state_experiments <- state_experiments %>%
    group_by(state_id) %>%
    nest() %>%
    mutate(
      fauna = map(data, ~ .x$critter %>% set_names(.x$scientific_name)),
      sels = map(fauna, ~ map2(1:2, length(.x), selfoo)),
      sel_form = map(fauna, ~ map2(1:2,length(.x), ~sample(c("uniform","logistic","double_normal"), .y, replace = TRUE))),
      prices = map(fauna, ~ map2(1:2, length(.x),~runif(.y, 1, 10))),
      use_ports = map(fauna,~sample(c(TRUE,FALSE), 2, replace = TRUE))
    ) |> 
    ungroup()
  # state_experiments$fauna[[1]]$`lutjanus malabaricus`$diffusion_foundation[[1]] |> image()
  
  # state_experiments$fauna[[1]]$`lutjanus malabaricus`$movement_matrix[[1]] |> image()
  
  # state_experiments$fauna[[1]][[4]]$b0
  # stop()
  state_experiments <- state_experiments %>%
    mutate(
      fleet = pmap(
        list(
          fauna = fauna,
          state = data,
          sels = sels,
          sel_form = sel_form,
          prices = prices,
          use_ports = use_ports
        ),
        create_fleets,
        difficulty = difficulty,
        port_locations = port_locations,
        resolution = resolution,
        .progress = "making fleets"
      )
    )
  
    # prepare recruitment deviate generator
    state_experiments <- state_experiments |>
      mutate(
        max_abs_cor_rec = sample(c(0, .66), n(), replace = TRUE),
        rec_dev_cov_and_cor = map2(fauna, max_abs_cor_rec, create_rec_dev_cov_and_cor)
      )
    
 

  # add in starting conditions
  init_condit <- function(fauna, fleets, rec_dev_cov_and_cor, years = 125) {
    starting_trajectory <-
      simmar(fauna = fauna,
             fleets = fleets,
             years = years,
             cor_rec = rec_dev_cov_and_cor$cor)
    
    # plot_marlin(check)
    
    starting_conditions <-
      starting_trajectory[(length(starting_trajectory) - seasons + 1):length(starting_trajectory)]
    # starting_trajectory[1:length(starting_trajectory)]
    
    proc_starting_conditions <-
      process_marlin(starting_conditions, keep_age = FALSE)
    
    out <- list(starting_conditions = starting_conditions,
                proc_starting_conditions = proc_starting_conditions)
    
  }
  
  message(glue::glue("simulating initial {difficulty} conditions"))
  state_experiments <- state_experiments %>%
    mutate(tmp = future_pmap(
      list(fauna = fauna,
      fleet = fleet,
      rec_dev_cov_and_cor = rec_dev_cov_and_cor),
      init_condit,
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    ))
  message(glue::glue("finished simulating initial {difficulty} conditions"))
  
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
    facet_wrap( ~ critter)
  
  ggsave(file.path(fig_dir, glue::glue("{difficulty}_init_dep.pdf")), init_dep_plot)
  
  
  for (i in 1:nrow(state_experiments)) {

    tmp <- state_experiments$proc_starting_conditions[[i]]$fauna |>
      filter(step == max(step)) |>
      group_by(critter) |>
      mutate(n = n / max(n)) |>
      ggplot(aes(x, y, fill = n)) +
      geom_tile() +
      facet_wrap( ~ critter) +
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
  
  
  # generate recruitment deviates to be shared across all MPA sizes
  
state_experiments <- state_experiments |> 
  mutate(log_rec_devs = map2(fauna, rec_dev_cov_and_cor, generate_rec_devs, years = mpa_years))
  
# generate placement experiments

  placement_experiments <- expand_grid(
    placement_strategy = c("target_fishing", "area", "avoid_fishing"),
    prop_mpa = seq(0, 0.6, by = 0.05),
    critters_considered = seq(
      length(state_experiments$fauna[[1]]),
      length(state_experiments$fauna[[1]]),
      by = 1
    ),
    placement_error = c(0),
    observation_error =c(0,.1)
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
  
  write_rds(state_experiments |> select(-contains("starting_conditions")),
            file = file.path(results_dir, glue("{difficulty}_state_experiments.rds")))
  
  # future::plan(future::multisession, workers = experiment_workers)
  
  # experiment_results <-
  #   vector(mode = "list", length = nrow(placement_experiments))
  #
  experiment_results <-
    vector(mode = "list", length = 2) # for memory-concious mode, only save reference and the current MPA size
  
  processed_sims <- total_processed_sims <-
    vector(mode = "list", length = nrow(placement_experiments) - 1) # for memory-concious mode, only save reference and the current MPA size
  
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
            fleets = fleet,
            log_rec_devs = log_rec_devs
          ),
          run_mpa_experiment,
          placement_strategy = placement_experiments$placement_strategy[p],
          prop_mpa = placement_experiments$prop_mpa[p],
          critters_considered = placement_experiments$critters_considered[p],
          placement_error = placement_experiments$placement_error[p],
          resolution = resolution,
          patch_area = patch_area,
          drop_patches = drop_patches,
          steps_to_keep = "before_after",
          years = mpa_years,
          keep_age = TRUE,
          .options = furrr_options(seed = TRUE)
        )
      )
    
    
    tmp$results <-
      purrr::set_names(tmp$results, state_experiments$state_id)
    
    needed <- tmp$results
    rm(tmp)
    gc()
    # Sys.time() - a

    # to save memory, iteratively comparing a given MPA size to MPA size = 0
    if (placement_experiments$prop_mpa[p] > 0)   {

      experiment_results[[2]] <- needed
      
      processed_sims[[p-1]] <- process_sims(
        difficulty_level = difficulty,
        results_dir = results_dir,
        drop_patches = drop_patches,
        project = project,
        experiment_results = experiment_results,
        placement_experiments = placement_experiments[c(zerofinder,p), ],
        state_experiments = state_experiments,
        load_results = save_experiments,
        observation_error = placement_experiments$observation_error[p],
        aggregate = FALSE
      )
      
      
      total_processed_sims[[p-1]] <- process_sims(
        difficulty_level = difficulty,
        results_dir = results_dir,
        drop_patches = drop_patches,
        project = project,
        experiment_results = experiment_results,
        placement_experiments = placement_experiments[c(zerofinder,p), ],
        state_experiments = state_experiments,
        load_results = save_experiments,
        observation_error = placement_experiments$observation_error[p],
        aggregate = TRUE
      )
      
    } else {
      # store baseline case with no MPA
      
      zerofinder <- p # mark where the last zero MPA size was
      
      experiment_results[[1]] <- needed
      
    }
    
    pb$tick()

  } # close p loop
  
  
  if (save_experiments) {
    write_rds(experiment_results, file = file.path(
      results_dir,
      glue("{difficulty}_experiment_results.rds")
    ))
    rm(experiment_results)
    experiment_results <- NULL
  }
  
  # processed_sims <- process_sims(
  #   difficulty_level = difficulty,
  #   results_dir = results_dir,
  #   drop_patches = drop_patches,
  #   project = project,
  #   experiment_results = experiment_results,
  #   load_results = save_experiments
  # )

  
  components <- names(processed_sims[[1]])
  
  components <- components[!components %in% c("fauna_results", "difficulty")]
  

 # combine each component of the placement experiemnts into one dataframe per metric, in some way that may not be pretty but works
   
 flat_processed_sims <- flat_total_processed_sims <-  vector(mode = "list", length = length(components)) |> 
   set_names(components)


 for (i in components){
   
   tmp <-  map(processed_sims,i) |> 
     list_rbind()
     
   flat_processed_sims[[i]] <- tmp
   
   tmp <-  map(total_processed_sims,i) |> 
     list_rbind()
   
   flat_total_processed_sims[[i]] <- tmp
   
 }
 
 flat_processed_sims$species_variables <- processed_sims[[1]]$species_variables
 
 flat_processed_sims$state_variables <- processed_sims[[1]]$state_variables
 
 flat_processed_sims$difficulty <- difficulty
 
 flat_total_processed_sims$difficulty <- difficulty
 
  rm(processed_sims)
  
  rm(total_processed_sims)
  
  write_rds(flat_processed_sims, file = file.path(results_dir, glue("{difficulty}_processed_sims.rds")))
  
  write_rds(flat_total_processed_sims, file = file.path(results_dir, glue("{difficulty}_total_processed_sims.rds")))
  
  tmp <- flat_processed_sims$mpa_outcomes |>
    filter(step == max(step)) |> 
    select(percent_mpa_effect,
           fleet,
           state_id,
           placement_id,
           critter,
           prop_mpa,
           name) |>
    left_join(flat_processed_sims$species_variables,
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
    scale_x_continuous(name = "X Change in Species Biomass",
                       oob = squish,
                       limits = c(NA, 1)) +
    scale_y_continuous(name = "X Change in Species Catch",
                       oob = squish,
                       limits = c(NA, 1)) +
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
  
  rm(list = c("state_experiments"))
} # close difficulty loop
future::plan(future::sequential)

