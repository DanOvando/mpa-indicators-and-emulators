fit_twopbd <- function(fauna, fleets, state_id, difficulty, years) {
  # when fleets < critters not always possible to get all critters to target depletion, so pretend single species system just to fit dynamics
  
  out <- vector("list", length(fauna))
  for (f in seq_along(fauna)) {
    tmp_fauna <- fauna[f]
    
    tmp_fleets <- fleets
    
    # pull out timing of density dependence for each critter
    dd_type <- map_df(tmp_fauna, "rec_form") |>
      pivot_longer(everything(), names_to = "critter", values_to = "rec_form") |>
      mutate(local_dd = as.numeric(str_detect(rec_form, "local")))
    
    
    resolution <- tmp_fauna[[1]]$resolution # resolution of the system
    
    baseline <- simmar(tmp_fauna, tmp_fleets) # run the baseline conditions
    
    # # # pull out baseline SSB depletion
    # depletion <- purrr::map_df(baseline[[length(baseline)]], ~ sum(.x$b_p_a) / tmp_fauna[[1]]$b0) |>
    #   pivot_longer(everything(), names_to = "critter", values_to = "depletion")
    
    #
    # tune effort to get to the target depletion level
    depfoo <- function(fmult, target, tmp_fleets, tmp_fauna) {
      tmp_fleets <- purrr::modify_in(tmp_fleets, list(1, "base_effort"), \(x) x * fmult)
      
      fished_down <- simmar(tmp_fauna, tmp_fleets, years = years)
      
      initial_conditions <- fished_down[[length(fished_down)]]
      
      fished_bs <- map_dbl(initial_conditions,  ~ sum(.x$b_p_a))
      
      b0s <- map_dbl(tmp_fauna, "b0")
      
      mean_initial_depletion <- mean(fished_bs / b0s)
      
      # print(mean_initial_depletion)
      
      ss <- (mean_initial_depletion - target) ^ 2
      
      return(ss)
    }
    
    hmm <-  nlminb(
      0.01,
      depfoo,
      target = .1,
      tmp_fleets = tmp_fleets,
      tmp_fauna = tmp_fauna,
      lower = .01,
      upper = 20
    )
    
    if (hmm$objective > 2){
       stop("failed to reach desired depletion, rejecting emulation")
    }
    
    tmp_fleets <- purrr::modify_in(tmp_fleets, list(1, "base_effort"), \(x) x * hmm$par)
    
    fished_down <- simmar(tmp_fauna, tmp_fleets, years = years) # fish the populations down to target depletion level
    
    downward_slide = purrr::modify_depth(fished_down, 2, \(x) data.frame(biomass = sum(x$b_p_a), catch = sum(x$c_p_a)))
    
    downward_slide = map(downward_slide, \(x) list_rbind(x, names_to = "critter")) |>
      list_rbind(names_to = "step") |>
      mutate(step = (str_remove_all(step, "step_"))) |>
      separate_wider_delim(step,
                           delim = "_",
                           names = c("year", "season")) |>
      mutate(
        season = as.integer(season),
        decimal_season = season / max(season) - 1 / max(season),
        time_step = as.integer(year) + decimal_season
      ) |>
      group_by(critter) |>
      nest() |>
      mutate(b_c_t = map(data, \(x) as.matrix(x |> select(biomass, catch))))
    
    # pull out catch and biomass trajectory here to fit to that to see if that helps get the catch scales better
    
    initial_conditions <- fished_down[[length(fished_down)]]
    
    initial_depletion <- sum(initial_conditions[[1]]$b_p_a) / initial_conditions[[1]]$b0
    
    # set up full fishing closure and mark where to move fish to
    close_everything <- expand_grid(x = 1:resolution[1], y = 1:resolution[2]) |>
      mutate(mpa = TRUE)
    
    close_everything$move_aside <- close_everything$x > round(resolution[1] / 2)
    
    
    # go through and move all the fish (numbers, biomass, and spawning biomass) to one side of the system
    initial_conditions <-
      purrr::modify_depth(
        initial_conditions,
        1,
        \(x) map_at(
          x,
          "n_p_a",
          \(x, move_aside) x * as.numeric(!move_aside),
          move_aside = close_everything$move_aside
        )
      )
    initial_conditions <-
      purrr::modify_depth(
        initial_conditions,
        1,
        \(x) map_at(
          x,
          "b_p_a",
          \(x, move_aside) x * as.numeric(!move_aside),
          move_aside = close_everything$move_aside
        )
      )
    
    initial_conditions <-
      purrr::modify_depth(
        initial_conditions,
        1,
        \(x) map_at(
          x,
          "b_p_a",
          \(x, move_aside) x * as.numeric(!move_aside),
          move_aside = close_everything$move_aside
        )
      )
    
    #rebuild the population from depleted and "moved aside" status to provide signal on growth rate and movement rate
    rebuild <- simmar(
      fauna = tmp_fauna,
      fleets = tmp_fleets,
      years = years,
      manager = list(mpas = list(
        locations = close_everything, mpa_year = 1
      )),
      initial_conditions = initial_conditions
    )
    
    # create a dataframe for each time step with a column indicating the patch, whether that patch is in the "scorched earth" or not, and the biomass
    # in that patch. So, this is constructing the two-patch analogy to the systems
    only_b = purrr::modify_depth(
      rebuild,
      2,
      \(x, scorched_earth) data.frame(b = rowSums(x$b_p_a)) |> mutate(
        scorched_earth = scorched_earth,
        patch = 1:nrow(close_everything)
      ),
      scorched_earth = close_everything$move_aside
    )
    
    # create a dataframe of biomass per "patch" (in the two patch system) per time step per critter
    critter_b_t_p = map(only_b, \(x) list_rbind(x, names_to = "critter")) |>
      list_rbind(names_to = "step") |>
      mutate(step = (str_remove_all(step, "step_"))) |>
      separate_wider_delim(step,
                           delim = "_",
                           names = c("year", "season")) |>
      mutate(
        season = as.integer(season),
        decimal_season = season / max(season) - 1 / max(season),
        time_step = as.integer(year) + decimal_season
      ) |>
      group_by(time_step, critter, scorched_earth) |>
      summarise(b = sum(b)) |>
      ungroup() |>
      pivot_wider(names_from = "scorched_earth", values_from = "b") |>
      group_by(critter) |>
      nest() |>
      mutate(b_t_p = map(data, \(x) as.matrix(x |> select(-time_step))))
    
    # tune twopsp ------------------------------------------
    foo <- function(b_t_p, b_c_t,local_dd, plim = 0.2,approx_mpa_size) {
      twopbd_data <- list(
        b_t_p = b_t_p,
        downward_b = b_c_t[,1],
        downward_catch = b_c_t[,2],
        mpa_size = approx_mpa_size,
        # by assumption in this phase
        n_t = nrow(b_t_p),
        n_p = ncol(b_t_p),
        local_dd = local_dd,
        plim = plim
      )
      
      sigh <- cmdstan_model(here("src", "fit_2pbd.stan"))
      
      fit <- sigh$optimize(data = twopbd_data, seed = 123)
    }

    critter_b_t_p <- critter_b_t_p |> 
      mutate(approx_mpa_size = map_dbl(b_t_p, ~last(.x[,1]) / (last(.x[,1]) + last(.x[,2]))))
    
    critter_b_t_p <- critter_b_t_p |>
      left_join(dd_type, by = "critter") |>
      left_join(downward_slide |> select(critter, b_c_t), by = "critter") |> 
      mutate(twopbd_fit = pmap(list(b_t_p = b_t_p, local_dd = local_dd, b_c_t = b_c_t,approx_mpa_size = approx_mpa_size), foo))
    
    
    get_params <- function(fit, vars = c("g", "k", "phi", "m")) {
      out <- fit$summary() |>
        filter(variable %in% vars)
    }
    
    get_diagnostics <- function(b_t_p,twopbd_fit){
      
      observed <- b_t_p |>
        as.data.frame() |>
        pivot_longer(everything(), names_to = "patch", values_to = "biomass") |>
        mutate(patch = as.numeric(as.logical(patch)) + 1) |>
        group_by(patch) |>
        mutate(year = 1:length(biomass))
      
      diagnostics <- tidybayes::spread_draws(twopbd_fit, hat_b_t_p[year, patch]) |>
        left_join(observed, by = c("patch", "year")) |>
        mutate(patch = factor(patch)) |> 
        ungroup()
      
      return(diagnostics)
      
    }
    
    critter_b_t_p <- critter_b_t_p |> 
      mutate(diagnostics = map2(b_t_p,twopbd_fit,get_diagnostics)) |> 
      mutate(fit_mape = map_dbl(diagnostics, ~yardstick::mape(.x |> filter(year > 1),hat_b_t_p, biomass)$.estimate))
    
    if (any(critter_b_t_p$fit_mape > 25)){
      # browser()
      stop("at least one MAPE between observed and predicted total biomass per patch was greater than 25%")
    }
    
    for (i in 1:nrow(critter_b_t_p)){
      
      tmp = critter_b_t_p$diagnostics[[i]] |>
        ggplot() +
        geom_point(aes(year, biomass, color = patch)) +
        geom_line(aes(year, hat_b_t_p, color = patch)) + 
        labs(caption = "Line are values estimated by low-resolution model, points data from high-resolution model",
             x = "Year", y = "Biomass")
      
      ggsave(file.path(
        fig_dir,
        glue::glue("{difficulty}_state_id_{state_id}_{critter_b_t_p$critter}_emulation_fit.pdf")
      ), tmp)
      
      
    }
    

    
    # if (names(fauna)[f] == "carcharhinus amblyrhynchos"){
    #   browser()
    #   
    # }
    # i <- 1
    # observed <- critter_b_t_p$b_t_p[[i]] |>
    #   as.data.frame() |>
    #   pivot_longer(everything(), names_to = "patch", values_to = "biomass") |>
    #   mutate(patch = as.numeric(as.logical(patch)) + 1) |>
    #   group_by(patch) |>
    #   mutate(year = 1:length(biomass))
    # 
    # predicted <- tidybayes::spread_draws(critter_b_t_p$twopbd_fit[[i]], hat_b_t_p[year, patch]) |>
    #   left_join(observed, by = c("patch", "year")) |>
    #   mutate(patch = factor(patch))
    # 
    # observed <- critter_b_t_p$b_c_t[[i]] |>
    #   as.data.frame()
    # 
    # 
    # test <- tidybayes::spread_draws(critter_b_t_p$twopbd_fit[[i]], hat_downward_b[year])
    # 
    # test2 <- tidybayes::spread_draws(critter_b_t_p$twopbd_fit[[i]], hat_downward_catch[year])
    # 
    # plot(observed$biomass, test$hat_downward_b)
    # abline(0,1)
    # 
    # plot(observed$catch, test2$hat_downward_catch)
    # abline(0,1)
    # 
    # a = predicted |>
    #   filter(year < 200) |>
    #   ggplot() +
    #   geom_point(aes(year, biomass, color = patch)) +
    #   geom_line(aes(year, hat_b_t_p, color = patch))
    # a
    out[[f]] <- critter_b_t_p |>
      select(-diagnostics) |> 
      mutate(twopbd_params = map(twopbd_fit, get_params)) |>
      select(critter, local_dd, twopbd_params, fit_mape)
  } # close loop over individual fauna
  
  out <- list_rbind(out)
  
  return(out)
  
}