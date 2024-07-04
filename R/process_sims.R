process_sims <- function(difficulty_level = "complex", results_dir) {
  
  experiment_results <-
    read_rds(file = file.path(results_dir, paste0(difficulty_level,"_experiment_results.rds")))
  
  placement_experiments <-
    read_rds(file = file.path(results_dir, paste0(difficulty_level,"_placement_experiments.rds")))
  
  state_experiments <-
    read_rds(file = file.path(results_dir, paste0(difficulty_level,"_state_experiments.rds")))
  
  emulated_experiment_results <-  read_rds(file = file.path(results_dir, paste0(difficulty_level,"_emulated_experiment_results.rds")))
  
  
  results <- placement_experiments %>%
    mutate(temp = experiment_results,
           state_id = map(experiment_results, names)) %>%
    unnest(cols = c(temp, state_id)) |>
    mutate(fauna = map(temp, c("results", "fauna")), fleets = map(temp, c("results", "fleets"))) |>
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
        mutate(biomass_gradient = map_dbl(data, ~ as.numeric(
          lm(
            log(b + 1e-6) ~ distance_to_mpa_edge + ssb0,
            data = .x |> filter(!mpa)
          )$coefficients["distance_to_mpa_edge"]
        ))) |>
        select(-data) |>
        mutate(fleet = "nature")
      
      cpue_gradient <- fleets |>
        group_by(fleet, critter, step) |>
        nest() |>
        mutate(cpue_gradient = map_dbl(data, ~ as.numeric(
          lm(
            log(cpue + 1e-6) ~ distance_to_mpa_edge + ssb0,
            data = .x |> filter(!mpa)
          )$coefficients["distance_to_mpa_edge"]
        ))) |>
        select(-data)
      
      effort_gradient <- fleets |>
        group_by(fleet, critter, step) |>
        nest() |>
        mutate(effort_gradient = map_dbl(data, ~ as.numeric(
          lm(
            log(effort + 1e-6) ~ distance_to_mpa_edge,
            data = .x |> filter(!mpa)
          )$coefficients["distance_to_mpa_edge"]
        ))) |>
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
    select(-habitat, -critter)
  
  mpa_outcomes <- tmp |>
    select(-treatment, -control, -gradients) |>
    unnest(cols = outcomes) |>
    ungroup() |>
    left_join(species_variables,
              by = c("state_id", "critter" = "scientific_name")) |>
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
  
  out <- list(mpa_outcomes = mpa_outcomes,
              fauna_results = fauna_results,
              difficulty = difficulty_level,
              species_variables = species_variables,
              state_variables = state_variables)
  
  return(out)
  
}
