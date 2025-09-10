create_fleets <-
  function(fauna,
           state,
           difficulty = "medium",
           sels,
           sel_form,
           prices,
           use_ports,
           port_locations = NULL,
           tune_type = "explt",
           effort_cost_exponent = 1,
           responsiveness = 1,
           effort_int = 1000,
           resolution,
           patch_area = 1) {
    
    ports <- vector( mode = "list", length = 2)
    for (i in seq_along(use_ports)){
      if (use_ports[i]){
        ports[[i]] <- port_locations
      }
      
    }

    if (difficulty == "medium"){
    
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[state$scientific_name == "reef_fish"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        b_spatial_q <-
          state$habitat[state$scientific_name == "tuna"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        c_spatial_q <-
          state$habitat[state$scientific_name == "grouper"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        d_spatial_q <-
          state$habitat[state$scientific_name == "shark"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- b_spatial_q <-  c_spatial_q <- d_spatial_q <- NULL
      }
    fleets <- list(
      "alpha" = create_fleet(
        list(
          "reef_fish" = Metier$new(
            critter = fauna$reef_fish,
            price = prices[[1]][1],
            sel_form = sel_form[[1]][1],
            sel_start = fauna[[1]]$length_50_mature * sels[[1]]$sel_start[1],
            sel_delta =  fauna[[1]]$length_50_mature* sels[[1]]$sel_delta[1],
            sel05_anchor = sels[[1]]$sel05_anchor[[1]] * fauna[[1]]$length_50_mature,
            sel_at_linf = sels[[1]]$sel_at_linf[[1]],
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = a_spatial_q
            
          ),
          "tuna" = Metier$new(
            critter = fauna$tuna,
            price = prices[[1]][2],
            sel_form = sel_form[[1]][2],
            sel_start = fauna[[2]]$length_50_mature * sels[[1]]$sel_start[2],
            sel_delta =  fauna[[2]]$length_50_mature* sels[[1]]$sel_delta[2],
            sel05_anchor = fauna[[2]]$length_50_mature* sels[[1]]$sel05_anchor[[2]],
            sel_at_linf = sels[[1]]$sel_at_linf[[2]],
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = b_spatial_q
            
          ),
          "grouper" = Metier$new(
            critter = fauna$grouper,
            price = prices[[1]][3],
            sel_form = sel_form[[1]][3],
            sel_start = fauna[[3]]$length_50_mature * sels[[1]]$sel_start[3],
            sel_delta =  fauna[[3]]$length_50_mature* sels[[1]]$sel_delta[3],
            sel05_anchor = fauna[[3]]$length_50_mature* sels[[1]]$sel05_anchor[[3]],
            sel_at_linf = sels[[1]]$sel_at_linf[[3]],
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = c_spatial_q
          ),
          "shark" = Metier$new(
            critter = fauna$shark,
            price = prices[[1]][4],
            sel_form = sel_form[[1]][4],
            sel_start = fauna[[4]]$length_50_mature * sels[[1]]$sel_start[4],
            sel_delta =  fauna[[4]]$length_50_mature* sels[[1]]$sel_delta[4],
            sel05_anchor = fauna[[4]]$length_50_mature* sels[[1]]$sel05_anchor[[4]],
            sel_at_linf = sels[[1]]$sel_at_linf[[4]],
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = d_spatial_q
          )
        ),
        base_effort = prod(resolution) + effort_int,
        cost_per_unit_effort = 20,
        cost_per_distance = 200,
        effort_cost_exponent = effort_cost_exponent,
        responsiveness = responsiveness,
        spatial_allocation = state$spatial_allocation[1],
        resolution = resolution,
        patch_area = patch_area,
        fleet_model = state$fleet_model[1],
        mpa_response = state$mpa_response[1],
        ports =ports[[1]][1,]
      )
    )
    } else if (difficulty == "complex"){
      
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[state$scientific_name == "reef_fish"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        b_spatial_q <-
          state$habitat[state$scientific_name == "tuna"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        c_spatial_q <-
          state$habitat[state$scientific_name == "grouper"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        d_spatial_q <-
          state$habitat[state$scientific_name == "shark"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- b_spatial_q <-  c_spatial_q <- d_spatial_q <- NULL
      }
      fleets <- list(
        "alpha" = create_fleet(
          list(
            "reef_fish" = Metier$new(
              critter = fauna$reef_fish,
              price = prices[[1]][1],
              sel_form = sel_form[[1]][1],
              sel_start = fauna[[1]]$length_50_mature * sels[[1]]$sel_start[1],
              sel_delta =  fauna[[1]]$length_50_mature* sels[[1]]$sel_delta[1],
              sel05_anchor = fauna[[1]]$length_50_mature* sels[[1]]$sel05_anchor[[1]],
              sel_at_linf = sels[[1]]$sel_at_linf[[1]],
              catchability = .1,
              p_explt = .8,
              sel_unit = "length",
              spatial_catchability = a_spatial_q
              
            ),
            "tuna" = Metier$new(
              critter = fauna$tuna,
              price = prices[[1]][2],
              sel_form = sel_form[[1]][2],
              sel_start = fauna[[2]]$length_50_mature * sels[[1]]$sel_start[2],
              sel_delta =  fauna[[2]]$length_50_mature* sels[[1]]$sel_delta[2],
              sel05_anchor = fauna[[2]]$length_50_mature* sels[[1]]$sel05_anchor[[2]],
              sel_at_linf = sels[[1]]$sel_at_linf[[2]],
              catchability = .1,
              p_explt = 0.25,
              sel_unit = "length",
              spatial_catchability = b_spatial_q
              
            ),
            "grouper" = Metier$new(
              critter = fauna$grouper,
              price = prices[[1]][3],
              sel_form = sel_form[[1]][3],
              sel_start = fauna[[3]]$length_50_mature * sels[[1]]$sel_start[3],
              sel_delta =  fauna[[3]]$length_50_mature* sels[[1]]$sel_delta[3],
              sel05_anchor = fauna[[3]]$length_50_mature* sels[[1]]$sel05_anchor[[3]],
              sel_at_linf = sels[[1]]$sel_at_linf[[3]],
              catchability = .1,
              p_explt = .95,
              sel_unit = "length",
              spatial_catchability = c_spatial_q
            ),
            "shark" = Metier$new(
              critter = fauna$shark,
              price = prices[[1]][4],
              sel_form = sel_form[[1]][4],
              sel_start = fauna[[4]]$length_50_mature * sels[[1]]$sel_start[4],
              sel_delta =  fauna[[4]]$length_50_mature* sels[[1]]$sel_delta[4],
              sel05_anchor = fauna[[4]]$length_50_mature* sels[[1]]$sel05_anchor[[4]],
              sel_at_linf = sels[[1]]$sel_at_linf[[4]],
              catchability = .1,
              p_explt = 0.5,
              sel_unit = "length",
              spatial_catchability = d_spatial_q
            )
          ),
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 20,
          cost_per_distance = 200,
          effort_cost_exponent = effort_cost_exponent,
          responsiveness = responsiveness,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          patch_area = patch_area,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[[1]][1,]
        ),
        "beta" = create_fleet(
          list(
            "reef_fish" = Metier$new(
              critter = fauna$reef_fish,
              price = prices[[2]][1],
              sel_form = sel_form[[2]][1],
              sel_start = fauna[[1]]$length_50_mature * sels[[2]]$sel_start[1],
              sel_delta =  fauna[[1]]$length_50_mature* sels[[2]]$sel_delta[1],
              sel05_anchor = fauna[[1]]$length_50_mature* sels[[2]]$sel05_anchor[[1]],
              sel_at_linf = sels[[2]]$sel_at_linf[[1]],
              catchability = .1,
              p_explt = .2,
              sel_unit = "length",
              spatial_catchability = a_spatial_q
              
            ),
            "tuna" = Metier$new(
              critter = fauna$tuna,
              price = prices[[2]][2],
              sel_form = sel_form[[2]][2],
              sel_start = fauna[[2]]$length_50_mature * sels[[2]]$sel_start[2],
              sel_delta =  fauna[[2]]$length_50_mature* sels[[2]]$sel_delta[2],
              sel05_anchor = fauna[[2]]$length_50_mature* sels[[2]]$sel05_anchor[[2]],
              sel_at_linf = sels[[2]]$sel_at_linf[[2]],
              catchability = .1,
              p_explt = 0.9,
              sel_unit = "length",
              spatial_catchability = b_spatial_q
              
            ),
            "grouper" = Metier$new(
              critter = fauna$grouper,
              price = prices[[2]][3],
              sel_form = sel_form[[2]][3],
              sel_start = fauna[[3]]$length_50_mature * sels[[2]]$sel_start[3],
              sel_delta =  fauna[[3]]$length_50_mature* sels[[2]]$sel_delta[3],
              sel05_anchor = fauna[[3]]$length_50_mature* sels[[2]]$sel05_anchor[[3]],
              sel_at_linf = sels[[2]]$sel_at_linf[[3]],
              catchability = .1,
              p_explt = 0.05,
              sel_unit = "length",
              spatial_catchability = c_spatial_q
            ),
            "shark" = Metier$new(
              critter = fauna$shark,
              price = prices[[2]][4],
              sel_form = sel_form[[2]][4],
              sel_start = fauna[[4]]$length_50_mature * sels[[2]]$sel_start[4],
              sel_delta =  fauna[[4]]$length_50_mature* sels[[2]]$sel_delta[4],
              sel05_anchor = fauna[[4]]$length_50_mature* sels[[2]]$sel05_anchor[[4]],
              sel_at_linf = sels[[2]]$sel_at_linf[[4]],
              catchability = .1,
              p_explt = 0.5,
              sel_unit = "length",
              spatial_catchability = d_spatial_q
            )
          ),
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 20,
          cost_per_distance = 100,
          effort_cost_exponent = effort_cost_exponent,
          responsiveness = responsiveness,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          patch_area = patch_area,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[[2]][2,]
        )
      )
      
      
    } else if (str_detect(difficulty, "simple")){
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- NULL
      }
      
      tmp <-   list(
        tmp = Metier$new(
          critter = fauna[[1]],
          price = prices[[1]][1],
          sel_form = sel_form[[1]][1],
          sel_start = fauna[[1]]$length_50_mature * sels[[1]]$sel_start[1],
          sel_delta =  fauna[[1]]$length_50_mature* sels[[1]]$sel_delta[1],
          sel05_anchor = fauna[[1]]$length_50_mature* sels[[1]]$sel05_anchor[[1]],
          sel_at_linf = sels[[1]]$sel_at_linf[[1]],
          catchability = .1,
          p_explt = 1,
          sel_unit = "length",
          spatial_catchability = a_spatial_q))
      
      names(tmp) = names(fauna)
      
      fleets <- list(
        "longline" = create_fleet(tmp,
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 20,
          cost_per_distance = 200,
          effort_cost_exponent = effort_cost_exponent,
          responsiveness = responsiveness,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          patch_area = patch_area,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[[1]][1,]
        ))
      
      
     
      
      
    } else if (difficulty == "epo"){
      
      
      fleets <- list(
        "longline" = create_fleet(
          list(
            "thunnus obesus" = Metier$new(
              critter = fauna$`thunnus obesus`,
              price = 10,
              sel_form = "logistic",
              sel_start = 100,
              sel_delta = .01,
              catchability = .1,
              p_explt = .5,
              sel_unit = "length"
            ),
            "katsuwonus pelamis" = Metier$new(
              critter = fauna$`katsuwonus pelamis`,
              price = 1.36,
              sel_form = "logistic",
              sel_start = 65,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .05,
              sel_unit = "length"
              
            ),
            "thunnus albacares" = Metier$new(
              critter = fauna$`thunnus albacares`,
              price = 7.52,
              sel_form = "logistic",
              sel_start = 90,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 0.28,
              sel_unit = "length"
              
            ),
            "carcharhinus longimanus" = Metier$new(
              critter = fauna$`carcharhinus longimanus`,
              price = 1.89,
              sel_form = "logistic",
              sel_start = 50,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .92,
              sel_unit = "length"
            ),
            "carcharhinus falciformis" = Metier$new(
              critter = fauna$`carcharhinus falciformis`,
              price = 2.16,
              sel_form = "logistic",
              sel_start = 110,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .79,
              sel_unit = "length"
              
            ),
            "prionace glauca" = Metier$new(
              critter = fauna$`prionace glauca`,
              price = 1,
              sel_form = "logistic",
              sel_start = 100,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .99,
              sel_unit = "length"
              
            ),
            "sphyrna zygaena" = Metier$new(
              critter = fauna$`sphyrna zygaena`,
              price = 1,
              sel_form = "logistic",
              sel_start = 100,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .92,
              sel_unit = "length"
              
            )
          ),
          effort_cost_exponent = effort_cost_exponent,
          responsiveness = responsiveness,
          base_effort = resolution^2,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          patch_area = patch_area,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[2,]
        ),
        "purseseine" = create_fleet(
          list(
            "thunnus obesus" = Metier$new(
              critter = fauna$`thunnus obesus`,
              price = 1,
              sel_form = "dome",
              sel_start = 50,
              sel_delta = 5,
              catchability = .1,
              p_explt = .5,
              sel_unit = "length"
              
            ),
            "katsuwonus pelamis" = Metier$new(
              critter = fauna$`katsuwonus pelamis`,
              price = 1.36,
              sel_form = "dome",
              sel_start = 50,
              sel_delta = 4,
              catchability = .1,
              p_explt = .95,
              sel_unit = "length"
              
            ),
            "thunnus albacares" = Metier$new(
              critter = fauna$`thunnus albacares`,
              price = 7.52,
              sel_form = "dome",
              sel_start = 60,
              sel_delta = 4,
              catchability = .1,
              p_explt = .72,
              sel_unit = "length"
              
            ),
            "carcharhinus longimanus" = Metier$new(
              critter = fauna$`carcharhinus longimanus`,
              price = 1.89,
              sel_form = "dome",
              sel_start = 160,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .02,
              sel_unit = "length"
              
            ),
            "carcharhinus falciformis" = Metier$new(
              critter = fauna$`carcharhinus falciformis`,
              price = 2.16,
              sel_form = "logistic",
              sel_start = 120,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .08,
              sel_unit = "length"
              
            ),
            "prionace glauca" = Metier$new(
              critter = fauna$`prionace glauca`,
              price = 5,
              sel_form = "logistic",
              sel_start = 110,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .04,
              sel_unit = "length"
            ),
            "sphyrna zygaena" = Metier$new(
              critter = fauna$`sphyrna zygaena`,
              price = 1,
              sel_form = "logistic",
              sel_start = 100,
              sel_delta = 0.01,
              catchability = .1,
              p_explt = .08,
              sel_unit = "length"
              
            )
          ),
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[2,],
          responsiveness = .5,
          cr_ratio = 1,
          effort_cost_exponent = effort_cost_exponent,
          responsiveness = responsiveness,
          base_effort = resolution^2,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          patch_area = patch_area
        )
      )
      
    }

    fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
  }
