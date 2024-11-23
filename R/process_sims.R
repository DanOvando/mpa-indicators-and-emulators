process_sims <- function(difficulty_level = "complex",
                         results_dir,
                         drop_patches = TRUE,
                         project = "emulators",
                         load_results = TRUE,
                         experiment_results = NULL,
                         placement_experiments = NULL,
                         state_experiments = NULL,
                         observation_error = 0 ,
                         aggregate = FALSE) {
  if (load_results) {
    experiment_results <-
      read_rds(file = file.path(
        results_dir,
        paste0(difficulty_level, "_experiment_results.rds")
      ))
    
    placement_experiments <-
      read_rds(file = file.path(
        results_dir,
        paste0(difficulty_level, "_placement_experiments.rds")
      ))
    
    state_experiments <-
      read_rds(file = file.path(
        results_dir,
        paste0(difficulty_level, "_state_experiments.rds")
      ))
  } # close load results
  if (project == "emulators") {
    emulated_experiment_results <-  read_rds(file = file.path(
      results_dir,
      paste0(difficulty_level, "_emulated_experiment_results.rds")
    ))
  }
  
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
  
  calculate_outcomes <- function(treatment, control, aggregate = FALSE) {
    # calculate treatment
    treatment_fauna <- treatment$fauna[[1]] |>
      ungroup() |> 
      mutate(critter = if_else(rep(aggregate,n()),"all_critters",critter)) |> 
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
    # browser()

    if (!drop_patches){
    mpas <- treatment$fauna[[1]] |> 
      select(x,y,mpa) |> 
      unique()
    }
    
    treatment_fleets <- treatment$fleets[[1]] |>
      ungroup() |> 
      mutate(fleet = if_else(rep(aggregate,n()), "all_fleets", fleet)) |> 
      mutate(critter = if_else(rep(aggregate,n()), "all_critters", critter)) |> 
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
    # mpas <- treatment$fauna[[1]] |>
    #   filter(step == max(step)) |>
    #   select(x, y, mpa) |>
    #   unique()
    #

    if (!drop_patches){
    control_fauna <- control$fauna[[1]] |>
      select(-mpa) |>
      left_join(mpas, by = c("x", "y"))
     
    } else {
      control_fauna <- control$fauna[[1]]
    }
    
    control_fauna <- control_fauna |> 
      ungroup() |> 
      mutate(critter = if_else(rep(aggregate,n()), "all_critters", critter)) |> 
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
      ungroup() |> 
      mutate(fleet = if_else(rep(aggregate,n()), "all_fleets", fleet)) |> 
      mutate(critter = if_else(rep(aggregate,n()), "all_critters", critter)) |> 
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
  
  
  tmp <- tmp |>
    ungroup() |>
    # slice(1) |>
    mutate(
      outcomes = map2(treatment, control, calculate_outcomes,aggregate = aggregate, .progress = "calculating MPA outcomes")
    )
  
  if (!drop_patches) {
    tmp <- tmp |>
      mutate(
        indicators = map2(
          treatment,
          prop_mpa,
          calculate_indicators,
          observation_error = observation_error,
          aggregate = aggregate,
          .progress = "calculating MPA indicators"
        )
        
      )
  } else {
    tmp$indicators <- vector(mode = "list", length = nrow(tmp))
  }
  
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
    select(-treatment, -control, -indicators) |>
    unnest(cols = outcomes) |>
    ungroup() |>
    left_join(species_variables,
              by = c("state_id", "critter" = "scientific_name")) |>
    left_join(placement_experiments, by = c("placement_id", "prop_mpa"))
  
  
  if (!drop_patches) {
    mpa_indicators <- tmp |>
      select(-treatment, -control, -outcomes) |>
      unnest(cols = indicators) |>
      ungroup()
    # 
    # slopes <- mpa_gradients |>
    #   group_by(fleet, prop_mpa) |>
    #   summarise(
    #     real_neg_slope = mean(ind_cpue_gradient < -0.025, na.rm = TRUE),
    #     real_pos_slope = mean(ind_cpue_gradient > 0.025, na.rm = TRUE)
    #   )

    mpa_outcomes <- mpa_outcomes |>
      left_join(mpa_indicators,
                by = join_by(id, placement_id, state_id, prop_mpa, critter, step))
  } # close drop patches
  
  out <- list(
    mpa_outcomes = mpa_outcomes,
    difficulty = difficulty_level,
    species_variables = species_variables,
    state_variables = state_variables
  )
  
  return(out)
  
}
