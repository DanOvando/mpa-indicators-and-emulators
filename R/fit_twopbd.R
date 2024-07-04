fit_twopbd <- function(tmp_fauna, tmp_fleets){
  
  dd_type <- map_df(tmp_fauna, "rec_form") |>
    pivot_longer(everything(), names_to = "critter", values_to = "rec_form") |>
    mutate(local_dd = as.numeric(str_detect(rec_form, "local")))
  
  
  resolution <- tmp_fauna[[1]]$resolution
  
  baseline <- simmar(tmp_fauna, tmp_fleets)
  
  depletion <- purrr::map_df(baseline[[length(baseline)]], ~ sum(.x$ssb_p_a) / .x$ssb0) |> 
    pivot_longer(everything(), names_to = "critter", values_to = "depletion")
  
  
  depfoo <- function(fmult,target, tmp_fleets, tmp_fauna){
    
    tmp_fleets <- purrr::modify_in(tmp_fleets,list(1, "base_effort"), \(x) x * fmult)
    
    fished_down <- simmar(tmp_fauna, tmp_fleets, steps = 100)
    
    initial_conditions <- fished_down[[length(fished_down)]]
    
    initial_depletion <- sum(initial_conditions[[1]]$ssb_p_a ) / initial_conditions[[1]]$ssb0
    
    ss <- (initial_depletion - target)^2
    
    return(ss)
  }
  
  hmm <-  nlminb(0.1, depfoo, target = 0.1, tmp_fleets = tmp_fleets, tmp_fauna = tmp_fauna, lower = .1, upper = 5)
  tmp_fleets <- purrr::modify_in(tmp_fleets,list(1, "base_effort"), \(x) x * hmm$par)
  
  fished_down <- simmar(tmp_fauna, tmp_fleets)
  
  initial_conditions <- fished_down[[length(fished_down)]]
  
  initial_depletion <- sum(initial_conditions[[1]]$ssb_p_a ) / initial_conditions[[1]]$ssb0
  
  glue::glue("initial depletion is {initial_depletion}")

  close_everything <- expand_grid(x = 1:resolution[1], y= 1:resolution[2]) |> 
    mutate(mpa = TRUE)
  
  close_everything$move_aside <- close_everything$x > round(resolution[1] / 2)
  
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
        "ssb_p_a",
        \(x, move_aside) x * as.numeric(!move_aside),
        move_aside = close_everything$move_aside
      )
    )
  
  rebuild <- simmar(fauna = tmp_fauna,
                    fleets = tmp_fleets,
                    years = 100,
                    manager = list(mpas = list(locations = close_everything, mpa_year = 1)),
                    initial_conditions = initial_conditions)
  
  only_b = purrr::modify_depth(
    rebuild,
    2,
    \(x, scorched_earth) data.frame(ssb = rowSums(x$ssb_p_a)) |> mutate(
      scorched_earth = scorched_earth,
      patch = 1:nrow(close_everything)
    ),
    scorched_earth = close_everything$move_aside
  )
  
  # xx something gone wrong here, only some species getting booted out
  critter_b_t_p = map(only_b, \(x) list_rbind(x, names_to = "critter")) |>
    list_rbind(names_to = "step") |>
    mutate(step = (str_remove_all(step, "step_"))) |>
    separate_wider_delim(step, delim = "_", names = c("year", "season")) |>
    mutate(
      season = as.integer(season),
      decimal_season = season / max(season) - 1 / max(season),
      time_step = as.integer(year) + decimal_season
    ) |>
    group_by(time_step, critter, scorched_earth) |>
    summarise(ssb = sum(ssb)) |>
    ungroup() |> 
    pivot_wider(names_from = "scorched_earth", values_from = "ssb") |> 
    group_by(critter) |> 
    nest() |> 
    mutate(b_t_p = map(data, \(x) as.matrix(x |> select(-time_step))))
  
  
  # tune twopsp ------------------------------------------
  
  foo <- function(tmp, local_dd, plim = 0.2) {
    twopbd_data <- list(
      b_t_p = tmp,
      mpa_size = 0.5, # by assumption in this phase
      n_t = nrow(tmp),
      n_p = ncol(tmp),
      local_dd = local_dd,
      plim = plim
    )
    
    sigh <- cmdstan_model(here("src", "fit_2pbd.stan"))
    
    fit <- sigh$optimize(data = twopbd_data,
                         seed = 123)
  }
  
  
  critter_b_t_p <- critter_b_t_p |> 
    left_join(dd_type, by = "critter") |> 
    mutate(twopbd_fit = map2(b_t_p,local_dd, foo)) |> 
    left_join(depletion, by = "critter")
  
  
  
  
  get_params <- function(fit, vars = c("g","k","phi","m")){
    
    out <- fit$summary() |> 
      filter(variable %in% vars)
  }
  
  # i <- 1
  # observed <- critter_b_t_p$b_t_p[[i]] |>
  #   as.data.frame() |>
  #   pivot_longer(everything(), names_to = "patch", values_to = "biomass") |>
  #   mutate(patch = as.numeric(as.logical(patch)) + 1) |>
  #   group_by(patch) |>
  #   mutate(year = 1:length(biomass))
  # 
  # predicted <- tidybayes::spread_draws(critter_b_t_p$twopbd_fit[[i]],
  #                         hat_b_t_p[year,patch]) |>
  #   left_join(observed, by = c("patch", "year")) |>
  #   mutate(patch = factor(patch))
  # 
  # a = predicted |>
  #   filter(year < 200) |>
  #   ggplot() +
  #   geom_point(aes(year, biomass, color = patch)) +
  #   geom_line(aes(year, hat_b_t_p, color = patch))
  # 
  out <- critter_b_t_p |> 
    mutate(twopbd_params = map(twopbd_fit, get_params)) |>
    select(critter, local_dd, depletion, twopbd_params)
  

  return(out)
  
}