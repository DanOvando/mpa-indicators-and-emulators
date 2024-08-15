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
           mpa_proximity= fct_relevel(mpa_proximity,"far", "nomans"))  
  
  fleets <-  x$fleets[[1]] |> 
    rename(tmpcatch = catch) |> 
    filter(!is.na(tmpcatch)) |> 
    group_by(step, critter, patch, x, y, patch_name, fleet) |>
    summarise(
      catch = sum(tmpcatch),
      mean_length = weighted.mean(mean_length, w = tmpcatch),
      effort = unique(effort),
      mpa = unique(mpa),
      distance_to_mpa_edge = unique(distance_to_mpa_edge),
      ssb0_p =  unique(ssb0_p)
    ) |> 
    mutate(cpue = catch / effort) |> 
    ungroup() |> 
    mutate(outside_distance = if_else(mpa,NA,distance_to_mpa_edge)) |> 
    mutate(outside_bin = percent_rank(outside_distance)) |> 
    mutate(mpa_proximity = case_when(outside_bin <= 0.2 ~ "near", outside_bin >= 0.8 ~ "far", .default = "nomans"),
           mpa_proximity= fct_relevel(mpa_proximity,"far", "nomans"))
  
  
  
  # fauna |>
  #   ggplot(aes(distance_to_mpa_edge, biomass, color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))

  # fauna |>
  #   ggplot(aes(distance_to_mpa_edge, mean_length)) +
  #   geom_point() +
  #   facet_wrap(~critter, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  # 
  
  # 
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (effort), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_wrap(~fleet, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  # 
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (catch), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_grid(critter~fleet, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  
  # fleets |>
  #   ggplot(aes(distance_to_mpa_edge, (cpue), color = mpa_proximity)) +
  #   geom_point() +
  #   facet_grid(critter~fleet, scales = "free_y") +
  #   scale_y_continuous(limits = c(0, NA))
  # 


  if (prop_mpa > 0 & prop_mpa < 1) {

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
        lm(log(biomass + 1e-6) ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |> 
      mutate(ind_biomass_gradient_raw = map_dbl(data, ~ as.numeric(
        lm(log(biomass + 1e-6) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"]
      ))) |> 
      select(-data) |>
      mutate(fleet = "nature")
    
    
    fishery_indicators <- fleets |>
      filter(mpa_proximity != "nomans") |> 
      group_by(fleet, critter, step) |>
      nest() |>
      mutate(ind_effort_gradient = map_dbl(
        data,
        ~ as.numeric(lm(log(effort + 1e-6) ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_effort_gradient_raw = map_dbl(
        data,
        ~ as.numeric(lm(log(effort + 1e-6) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_cpue_gradient = map_dbl(
        data,
        ~ as.numeric(lm(log(cpue + 1e-6) ~ mpa_proximity + ssb0_p, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      mutate(ind_cpue_gradient_raw = map_dbl(
        data,
        ~ as.numeric(lm(log(cpue + 1e-6) ~ mpa_proximity, data = .x |> filter(!mpa))$coefficients["mpa_proximitynear"])
      )) |>
      select(-data)
    
 
    out <- fishery_indicators |>
      bind_rows(survey_indicators) |>
      ungroup() |> 
      pivot_longer(starts_with("ind_"), names_to = "indicator", values_to = "indicator_value") |> 
      filter(!is.na(indicator_value))
  } else {
    out <- data.frame(step = NA,
                      critter = NA,
                      fleet = NA)
  }
  
  
  return(out)
  
}