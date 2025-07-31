calculate_indicators <- function(x, prop_mpa, observation_error = 0, aggregate = FALSE) {

  # get rid of age structure
  fauna <-  x$fauna[[1]] |>
    ungroup() |> 
    mutate(critter = case_when(aggregate ~ "all_critters", !aggregate ~ critter)) |> 
    group_by(step, critter, patch, x, y, patch_name) |>
    summarise(
      biomass = sum(b),
      mean_length = weighted.mean(mean_length, w = n),
      mpa = unique(mpa),
      distance_to_mpa_edge = unique(distance_to_mpa_edge),
      ssb0_p =  ifelse(aggregate,sum(unique(ssb0_p)),unique(ssb0_p)),
      b0_p =  ifelse(aggregate,sum(unique(b0_p)),unique(b0_p))
    ) |>
    ungroup() |>
    mutate(
      biomass = rlnorm(length(biomass),log(biomass),observation_error),
      mean_length = rlnorm(length(mean_length),log(mean_length),observation_error),
      outside_distance = if_else(mpa, NA, distance_to_mpa_edge)) |>
    mutate(outside_bin = percent_rank(outside_distance)) |>
    mutate(
      mpa_proximity = case_when(outside_bin <= 0.2 ~ "near", outside_bin >= 0.8 ~ "far", .default = "nomans"),
      mpa_proximity = forcats::fct(mpa_proximity, levels = c("near", "nomans", "far")),
      mpa_proximity = fct_relevel(mpa_proximity, "far", "nomans")
    ) |>
    group_by(critter) |>
    mutate(sbiomass = as.numeric(scale(biomass)),
           smean_length = as.numeric(scale(mean_length))) |>
    ungroup()

  fleets <-  x$fleets[[1]] |>
    rename(tmpcatch = catch) |>
    filter(!is.na(tmpcatch)) |>
    mutate(fleet = case_when(aggregate ~ "all_fleets", !aggregate ~ fleet)) |> 
    mutate(critter = case_when(aggregate ~ "all_critters", !aggregate ~ critter)) |> 
    group_by(step, critter, patch, x, y, patch_name) |>
    summarise(
      catch = sum(tmpcatch),
      mean_length = weighted.mean(mean_length, w = tmpcatch),
      effort = sum(effort),
      mpa = unique(mpa),
      distance_to_mpa_edge = unique(distance_to_mpa_edge),
      ssb0_p =  ifelse(aggregate,sum(unique(ssb0_p)),unique(ssb0_p)),
      b0_p =  ifelse(aggregate,sum(unique(b0_p)),unique(b0_p))
    ) |>
    mutate(cpue = catch / effort) |>
    ungroup() |>
    mutate(
      catch = rlnorm(length(catch),log(catch),observation_error),
      mean_length = rlnorm(length(mean_length),log(mean_length),observation_error),
      effort = rlnorm(length(effort),log(effort),observation_error),
      outside_distance = if_else(mpa, NA, distance_to_mpa_edge)) |>
    mutate(outside_bin = percent_rank(outside_distance)) |>
    mutate(
      mpa_proximity = case_when(outside_bin <= 0.2 ~ "near", outside_bin >= 0.8 ~ "far", .default = "nomans"),
      mpa_proximity = forcats::fct(mpa_proximity, levels = c("near", "nomans", "far")),
      mpa_proximity = fct_relevel(mpa_proximity, "far", "nomans")
    ) |>
    group_by(critter) |>
    mutate(seffort = as.numeric(scale(effort)), scpue = as.numeric(scale(cpue))) |>
    ungroup()
  
  
  
  # fauna |>
  #   ggplot(aes(distance_to_mpa_edge, sbiomass, color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y")
  
  # fauna |>
  #   ggplot(aes(distance_to_mpa_edge, smean_length)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y")
  
  
  #
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (effort), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~step, scales = "free_y")
  # #
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (catch), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  
  # fleets |>
  #   group_by(critter) |>
  #   mutate(out = scale(cpue)) |>
  #   ggplot(aes(distance_to_mpa_edge, out, color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  
  # browser()
  
  # no_fauna <- fauna |>
  #   filter()
  
  enough_distance <- !any(c(
    n_distinct(fauna$mpa_proximity[!fauna$mpa]),
    n_distinct(fleets$mpa_proximity[!fleets$mpa])
  ) < 2)
  
  safelm <- purrr::safely(lm)
  
  coalmine <- fleets |>
    filter(mpa_proximity != "nomans") |>
    group_by(critter, step) |>
    nest() |>
    mutate(canary = map(data, ~ (
      safelm(seffort ~ mpa_proximity + b0_p, data = .x |> filter(!mpa))
    ))) |>
    mutate(canary = map(canary, "error")) |>
    mutate(canary = map_lgl(canary, is.null))
  
  canary_alive <- all(coalmine$canary)
  
  if (prop_mpa > 0 &
      prop_mpa < 1 & enough_distance & canary_alive) {
    
    
    survey_indicators <- fauna |>
      mutate(weight = abs(distance_to_mpa_edge)) |>
      group_by(critter, step) |>
      nest() |>
      mutate(ind_biomass_rr = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass) ~ mpa + b0_p,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_biomass_rr_raw = map_dbl(data, ~ as.numeric(
        lm(log(biomass) ~ mpa, data = .x, weights = weight)$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_length_rr_raw = map_dbl(data, ~ as.numeric(
        lm(
          log(mean_length) ~ mpa,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_length_rr = map_dbl(data, ~ as.numeric(
        lm(
          log(mean_length) ~ mpa + b0_p,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_biomass_gradient = map_dbl(data, ~ as.numeric(
        lm(log(biomass) ~ mpa_proximity + b0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      mutate(ind_biomass_gradient_raw = map_dbl(data, ~ as.numeric(
        lm(log(biomass) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      select(-data)
    
    fishery_indicators <- fleets |>
      filter(mpa_proximity != "nomans") |>
      group_by(critter, step) |>
      nest() |>
      mutate(ind_effort_gradient = map_dbl(data, ~ as.numeric(
        lm(log(effort) ~ mpa_proximity + b0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      mutate(ind_effort_gradient_raw = map_dbl(data, ~ as.numeric(
        lm(log(effort) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      mutate(ind_cpue_gradient = map_dbl(data, ~ as.numeric(
        lm(log(cpue) ~ mpa_proximity + b0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      mutate(ind_cpue_gradient_raw = map_dbl(data, ~ as.numeric(
        lm(log(cpue) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |>
      select(-data)
    
    baci <- fauna |>
      mutate(weight = abs(distance_to_mpa_edge)) |>
      filter(step == min(step) | step == max(step)) |>
      mutate(after = case_when(step == max(step) ~ TRUE, .default = FALSE)) |>
      mutate(baci = after & mpa) |>
      group_by(critter) |>
      nest() |>
      mutate(ind_biomass_baci_raw = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass) ~ mpa + after + baci,
          data = .x,
          weights = weight
        )$coefficients["baciTRUE"]
      ))) |>
      mutate(ind_biomass_baci = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass) ~ mpa + after + baci + b0_p,
          data = .x,
          weights = weight
        )$coefficients["baciTRUE"]
      ))) |>
      ungroup() |>
      select(-data)
    
    bag <- fauna |>
      filter(mpa_proximity != "nomans") |>
      mutate(weight = abs(distance_to_mpa_edge)) |>
      filter(step == min(step) | step == max(step)) |>
      mutate(after = case_when(step == max(step) ~ TRUE, .default = FALSE)) |>
      mutate(gradient = mpa_proximity == "near") |>
      mutate(bag = after & gradient) |>
      group_by(critter) |>
      nest() |>
      mutate(ind_biomass_bag_raw = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass) ~ gradient + after + bag,
          data = .x,
          weights = weight
        )$coefficients["bagTRUE"]
      ))) |>
      mutate(ind_biomass_bag = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass) ~ gradient + after + bag + b0_p,
          data = .x,
          weights = weight
        )$coefficients["bagTRUE"]
      ))) |>
      ungroup() |>
      select(-data)
    
  catch_ba <- fleets |>
      filter(step == min(step) | step == max(step), !mpa) |>
      mutate(after = case_when(step == max(step) ~ TRUE, .default = FALSE)) |>
      mutate(baci = after & mpa) |>
      group_by(critter) |>
      nest() |>
      mutate(ind_catch_ba_raw = map_dbl(data, ~ as.numeric(
        lm(log(catch) ~ after, data = .x, )$coefficients["afterTRUE"]
      ))) |>
      mutate(ind_catch_ba = map_dbl(data, ~ as.numeric(
        lm(log(catch) ~ after + b0_p, data = .x, )$coefficients["afterTRUE"]
      ))) |>
      ungroup() |>
      select(-data)
    
    out <- fishery_indicators |>
      left_join(survey_indicators, join_by(step, critter)) |>
      left_join(baci, by = "critter") |>
      left_join(bag, by = "critter") |>
      left_join(catch_ba, by = "critter")
    
  } else {
    out <- data.frame(step = NA, critter = NA)
    
    # out <- data.frame(step = NA,
    #                   critter = NA,
    #                   fleet = NA)
  }
  

  return(out)
  
}