calculate_indicators <- function(x, prop_mpa) {
  

  
  fauna <-  x$fauna[[1]] |>
    group_by(step, critter, patch, x, y, patch_name) |>
    summarise(
      biomass = sum(b),
      mean_length = weighted.mean(mean_length, w = n),
      mpa = unique(mpa),
      distance_to_mpa_edge = unique(distance_to_mpa_edge),
      ssb0_p =  unique(ssb0_p)
    ) |> 
    ungroup() |> 
    mutate(outside_distance = if_else(mpa,NA,distance_to_mpa_edge)) |> 
    mutate(outside_bin = percent_rank(outside_distance)) |> 
    mutate(mpa_proximity = case_when(outside_bin <= 0.2 ~ "near", outside_bin >= 0.8 ~ "far", .default = "nomans"),
           mpa_proximity = forcats::fct(mpa_proximity, levels = c("near", "nomans", "far")),
           mpa_proximity= fct_relevel(mpa_proximity,"far", "nomans")) |> 
    group_by(critter) |> 
    mutate(sbiomass = as.numeric(scale(biomass)),
           smean_length = as.numeric(scale(mean_length))) |> 
    ungroup()
  
  fleets <-  x$fleets[[1]] |> 
    rename(tmpcatch = catch) |> 
    filter(!is.na(tmpcatch)) |> 
    group_by(step, critter, patch, x, y, patch_name) |>
    summarise(
      catch = sum(tmpcatch),
      mean_length = weighted.mean(mean_length, w = tmpcatch),
      effort = sum(effort),
      mpa = unique(mpa),
      distance_to_mpa_edge = unique(distance_to_mpa_edge),
      ssb0_p =  unique(ssb0_p)
    ) |> 
    mutate(cpue = catch / effort) |> 
    ungroup() |> 
    mutate(outside_distance = if_else(mpa,NA,distance_to_mpa_edge)) |> 
    mutate(outside_bin = percent_rank(outside_distance)) |> 
    mutate(mpa_proximity = case_when(outside_bin <= 0.2 ~ "near", outside_bin >= 0.8 ~ "far", .default = "nomans"),
           mpa_proximity = forcats::fct(mpa_proximity, levels = c("near", "nomans", "far")),
            mpa_proximity= fct_relevel(mpa_proximity,"far", "nomans")) |> 
    group_by(critter) |> 
    mutate(seffort = as.numeric(scale(effort)),
           scpue = as.numeric(scale(cpue))) |> 
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

  #
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (seffort), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~fleet, scales = "free_y") 
  #
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (catch), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_grid(critter~fleet, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))

  # fleets |>
  #   group_by(critter, fleet) |> 
  #   mutate(out = scale(cpue)) |> 
  #   ggplot(aes(distance_to_mpa_edge, out, color = mpa_proximity)) +
  #   geom_point() +
  #   facet_grid(critter~fleet, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  #
  # browser()

enough_distance <- !any(c(n_distinct(fauna$mpa_proximity),n_distinct(fleets$mpa_proximity)) < 2)

  if (prop_mpa > 0 & prop_mpa < 1 & enough_distance) {

    survey_indicators <- fauna |>
      mutate(weight = abs(distance_to_mpa_edge)) |>
      group_by(critter, step) |>
      nest() |>
      mutate(ind_biomass_rr = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass + 1e-6) ~ mpa + ssb0_p,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_biomass_rr_raw = map_dbl(data, ~ as.numeric(
        lm(
          log(biomass + 1e-6) ~ mpa,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
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
          log(mean_length) ~ mpa + ssb0_p,
          data = .x,
          weights = weight
        )$coefficients["mpaTRUE"]
      ))) |>
      mutate(ind_biomass_gradient = map_dbl(data, ~ as.numeric(
        lm(sbiomass ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |> 
      mutate(ind_biomass_gradient_raw = map_dbl(data, ~ as.numeric(
        lm(sbiomass ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |> 
      select(-data) 
    
    
    fishery_indicators <- fleets |>
      filter(mpa_proximity != "nomans") |> 
      group_by(critter, step) |>
      nest() |>
      mutate(ind_effort_gradient = map_dbl(
        data,
        ~ as.numeric(lm(seffort ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_effort_gradient_raw = map_dbl(
        data,
        ~ as.numeric(lm(seffort ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_cpue_gradient = map_dbl(
        data,
        ~ as.numeric(lm(scpue ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_cpue_gradient_raw = map_dbl(
        data,
        ~ as.numeric(lm(scpue ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
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
          sbiomass ~ mpa + after + baci,
          data = .x,
          weights = weight
        )$coefficients["baciTRUE"]
      ))) |> 
      mutate(ind_biomass_baci = map_dbl(data, ~ as.numeric(
        lm(
          sbiomass ~ mpa + after + baci + ssb0_p,
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
          sbiomass ~ gradient + after + bag,
          data = .x,
          weights = weight
        )$coefficients["bagTRUE"]
      ))) |> 
      mutate(ind_biomass_bag = map_dbl(data, ~ as.numeric(
        lm(
          sbiomass ~ gradient + after + bag + ssb0_p,
          data = .x,
          weights = weight
        )$coefficients["bagTRUE"]
      ))) |> 
      ungroup() |> 
      select(-data)

    out <- fishery_indicators |>
      left_join(survey_indicators,join_by(step, critter)) |> 
      left_join(baci, by = "critter") |> 
      left_join(bag, by = "critter")

  } else {
    out <- data.frame(step = NA,
                      critter = NA)
    
    # out <- data.frame(step = NA,
    #                   critter = NA,
    #                   fleet = NA)
  }
  
  
  return(out)
  
}