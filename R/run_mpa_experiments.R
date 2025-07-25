run_mpa_experiment <-
  function(placement_strategy = "depletion",
           fleet_model = NULL,
           mosaic = TRUE,
           starting_conditions,
           proc_starting_conditions,
           prop_mpa = 0.3,
           fauna,
           fleets,
           log_rec_devs = NULL,
           placement_error = 0,
           mpa_response = NULL,
           critters_considered = NA,
           random_mpas = FALSE,
           max_delta = 1,
           resolution,
           patch_area,
           years = 50,
           future_habitat = list(),
           drop_patches = TRUE,
           keep_age = FALSE,
           steps_to_keep = "after",
           mpa_offset = 2) {
    options(dplyr.summarise.inform = FALSE)
    
    # library(rsample)
    # 
    # library(workflowsets)
    
    Rcpp::sourceCpp(here("src", "select_contiguous_mpa.cpp"))
    

    patches <- prod(resolution)

        # fleets <- purrr::modify_in(fleets, list(1, "mpa_response"), ~mpa_response)

    # if (!is.null(fleet_model)) {
    #   fleets <-
    #     purrr::modify_in(fleets, list(1, "fleet_model"), ~fleet_model)
    # }


    mpas <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) |>
      mutate(patch = 1:n())

    n_mpa <- round(prop_mpa * patches)

    # set up open access

    #
    if (all(is.na(critters_considered))) {
      critters_considered <- length(fauna)
    }

    critters_considered <- sample(names(fauna), critters_considered, replace = FALSE)

    if (placement_strategy == "depletion") {
      # place MPAs in proportion to depletion-weighted spawning stock biomass
      depletion <-
        map_df(starting_conditions[1], ~ (1 - (map_df(.x, ~ sum(.x$ssb_p_a) / .x$ssb0))) %>% # depletion of each species
          pivot_longer(everything(), names_to = "critter", values_to = "weight"), .id = "step") %>%
        select(-step)

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(critter, step) %>%
        mutate(patch_weight = ssb / sum(ssb) * weight) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(patch_weight)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))

      # decision: pick MPA basis step based on the step with the highest variation in patch weights, e.g. spawning aggregation season should have higher varition thatn a uniform situation
      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <- priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))
    } else if (placement_strategy == "rate") {
      # place in proportion to depletion weighted catch relative to total catch. So, cells in which most of the catch comes from really depleted species, higher priority

      depletion <-
        map_df(starting_conditions[1], ~ (1 - (map_df(.x, ~ sum(.x$ssb_p_a) / .x$ssb0))) %>% # depletion of each species
          pivot_longer(everything(), names_to = "critter", values_to = "weight"), .id = "step") %>%
        select(-step)

      priorities <- proc_starting_conditions$fauna %>%
        left_join(depletion, by = "critter") %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c * weight) / sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))

      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <- priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))
    } else if (placement_strategy == "avoid_fishing") {
      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(patch_weight)

      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <- priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange((patch_weight))

      if (!mosaic) {
        tmp <- mpas |>
          left_join(priorities,by =  join_by(patch)) |>
          select(x, y, patch, patch_weight) |>
          mutate(patch_weight = -patch_weight)

        contig_priorities <- select_contiguous_mpa(tmp |> select(-patch), k = n_mpa)

        contig_priorities <- contig_priorities$coordinates |>
          mutate(mpa = TRUE)

        priorities <- tmp |>
          left_join(contig_priorities, by = join_by(x, y)) |>
          filter(!is.na(mpa))
      }
    } else if (placement_strategy == "target_fishing") {
      priorities <- proc_starting_conditions$fauna %>%
        filter(critter %in% critters_considered) %>%
        group_by(patch, step) %>%
        summarise(patch_weight = sum(c)) %>%
        ungroup() %>%
        mutate(patch_weight = patch_weight * rlnorm(nrow(.), 0, placement_error)) %>%
        arrange(desc(patch_weight))


      step_priority <- priorities %>%
        group_by(step) %>%
        summarise(pwsd = sd(patch_weight)) %>%
        filter(pwsd == max(pwsd))

      priorities <- priorities %>%
        filter(step == step_priority$step[1]) %>%
        arrange(desc(patch_weight))

      if (!mosaic) {
        tmp <- mpas |>
          left_join(priorities,by =  join_by(patch)) |>
          select(x, y, patch, patch_weight)

        contig_priorities <- select_contiguous_mpa(tmp |> select(-patch), k = n_mpa)

        contig_priorities <- contig_priorities$coordinates |>
          mutate(mpa = TRUE)

        priorities <- tmp |>
          left_join(contig_priorities, by = join_by(x, y)) |>
          filter(!is.na(mpa))
      }
    } else if (placement_strategy == "area") {
      priorities <- proc_starting_conditions$fauna %>%
        group_by(patch) %>%
        summarise(patch_weight = unique(patch)) %>%
        arrange((patch_weight))
    } else {
      stop("invalid placement strategy")
    }


    # place MPA
    if (n_mpa > 0) {
      mpa_locs <- priorities$patch[1:n_mpa]
    } else {
      mpa_locs <- -999
    }

    ssb0s <- map(fauna, ~ tibble(ssb0_p = as.numeric(.x$ssb0_p), patch = 1:length(.x$ssb0_p), b0_p = as.numeric(.x$b0_p))) |>
      list_rbind(names_to = "critter") |>
      group_by(patch) |>
      mutate(
        ssb0_p_total = sum(ssb0_p),
        b0_p_total = sum(b0_p)
      ) |>
      ungroup()
    
    mpas <- mpas |>
      mutate(mpa = patch %in% mpa_locs)

    mpas_and_ssb0s <- ssb0s |>
      left_join(mpas, by = "patch")

    # run MPA simulation
    starting_step <- marlin::clean_steps(last(names(starting_conditions)))

    processed_step <- marlin::process_step(last(names(starting_conditions)))
    
    mpa_sim <- simmar(
      fauna = fauna,
      fleets = fleets,
      years = years,
      manager = list(mpas = list(
        locations = mpas,
        mpa_year = processed_step$year + mpa_offset
      )),
      habitat = future_habitat,
      starting_step = starting_step,
      keep_starting_step = FALSE,
      initial_conditions = starting_conditions[[length(starting_conditions)]],
      log_rec_devs = log_rec_devs
    )

    steps <- marlin::clean_steps(names(mpa_sim))

    ## XXX add in before here later if needed for BACI ##

    # keep step in year one of MPA, 15 years after MPA, and in final year of simulation
    # steps_to_keep <- c(steps[1], steps[15 * seasons], last(steps))

    if (steps_to_keep == "before_after") {
      keepers <- c(steps[1], last(steps))
    } else if (steps_to_keep == "after") {
      keepers <- last(steps)
    } else if (steps_to_keep == "before_during_after") {
      keepers <- c(steps[1], 15, last(steps))
    } else if (steps_to_keep == "all") {
      keepers <- steps
    }


    out <- marlin::process_marlin(mpa_sim, steps_to_keep = keepers, keep_age = keep_age)

    mpa_distances <- marlin::get_distance_to_mpas(mpas, resolution = resolution, patch_area = patch_area) |>
      select(-patch)

    out$fauna <- out$fauna |>
      left_join(mpa_distances, by = c("x", "y")) |>
      left_join(mpas_and_ssb0s |> select(-patch, -mpa), by = c("x", "y", "critter"))

    out$fleets <- out$fleets |>
      left_join(mpa_distances, by = c("x", "y")) |>
      left_join(mpas_and_ssb0s |> select(-patch, -mpa), by = c("x", "y", "critter"))

    if (drop_patches) {
      # hacky step to get rid of patches when you don't need them to save memory
      out$fauna <- out$fauna |>
        group_by(step, critter, age, year, season, mpa) |>
        summarise(
          n = sum(n),
          b = sum(b),
          ssb = sum(ssb),
          c = sum(c, na.rm = TRUE)
        ) |>
        ungroup()


      out$fleets <- out$fleets |>
        group_by(step, critter, fleet, year, season, mpa) |>
        summarise(
          catch = sum(catch, na.rm = TRUE),
          revenue = sum(revenue, na.rm = TRUE),
          effort = sum(effort, na.rm = TRUE)
        ) |>
        ungroup() |>
        mutate(cpue = catch / effort)
    }



    outcomes <- list()

    outcomes$results <- out

    outcomes$mpa <- mpas

    return(outcomes)
  } # close run_mpa_experiment