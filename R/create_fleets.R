create_fleets <-
  function(fauna,
           state,
           difficulty = "medium",
           sels,
           prices,
           use_ports,
           port_locations = NULL,
           tune_type = "explt",
           effort_cost_exponent = 1,
           effort_int = 0,
           resolution) {
    

    
    if (use_ports == TRUE){
      ports <- port_locations
    } else {
      ports <- NULL
    }
    
    if (difficulty == "medium"){
    
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[state$scientific_name == "lutjanus malabaricus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        b_spatial_q <-
          state$habitat[state$scientific_name == "pristipomoides filamentosus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        c_spatial_q <-
          state$habitat[state$scientific_name == "epinephelus fuscoguttatus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        d_spatial_q <-
          state$habitat[state$scientific_name == "carcharhinus amblyrhynchos"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- b_spatial_q <-  c_spatial_q <- d_spatial_q <- NA
      }
    
    fleets <- list(
      "alpha" = create_fleet(
        list(
          "lutjanus malabaricus" = Metier$new(
            critter = fauna$`lutjanus malabaricus`,
            price = prices[1],
            sel_form = "logistic",
            sel_start = fauna$`lutjanus malabaricus`$length_50_mature * sels[1],
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = a_spatial_q
            
          ),
          "pristipomoides filamentosus" = Metier$new(
            critter = fauna$`pristipomoides filamentosus`,
            price = prices[2],
            sel_form = "logistic",
            sel_start = fauna$`pristipomoides filamentosus`$length_50_mature *  sels[2],
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = b_spatial_q
            
          ),
          "epinephelus fuscoguttatus" = Metier$new(
            critter = fauna$`epinephelus fuscoguttatus`,
            price = prices[3],
            sel_form = "logistic",
            sel_start = fauna$`epinephelus fuscoguttatus`$length_50_mature * sels[3],
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = c_spatial_q
          ),
          "carcharhinus amblyrhynchos" = Metier$new(
            critter = fauna$`carcharhinus amblyrhynchos`,
            price = prices[4],
            sel_form = "logistic",
            sel_start = fauna$`carcharhinus amblyrhynchos`$length_50_mature * sels[3],
            sel_delta = 0.01,
            catchability = .1,
            p_explt = 1,
            sel_unit = "length",
            spatial_catchability = d_spatial_q
          )
        ),
        base_effort = prod(resolution) + effort_int,
        cost_per_unit_effort = 10,
        cost_per_distance = 2000,
        effort_cost_exponent = effort_cost_exponent,
        spatial_allocation = state$spatial_allocation[1],
        resolution = resolution,
        fleet_model = state$fleet_model[1],
        mpa_response = state$mpa_response[1],
        ports = ports[1,]
      )
    )
    } else if (difficulty == "complex"){
      
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[state$scientific_name == "lutjanus malabaricus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        b_spatial_q <-
          state$habitat[state$scientific_name == "pristipomoides filamentosus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        c_spatial_q <-
          state$habitat[state$scientific_name == "epinephelus fuscoguttatus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        d_spatial_q <-
          state$habitat[state$scientific_name == "carcharhinus amblyrhynchos"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- b_spatial_q <-  c_spatial_q <- d_spatial_q <- NA
      }
      
      
      fleets <- list(
        "alpha" = create_fleet(
          list(
            "lutjanus malabaricus" = Metier$new(
              critter = fauna$`lutjanus malabaricus`,
              price = prices[1],
              sel_form = "logistic",
              sel_start = fauna$`lutjanus malabaricus`$length_50_mature * sels[1],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 1,
              sel_unit = "length",
              spatial_catchability = a_spatial_q
              
            ),
            "pristipomoides filamentosus" = Metier$new(
              critter = fauna$`pristipomoides filamentosus`,
              price = prices[2],
              sel_form = "logistic",
              sel_start = fauna$`pristipomoides filamentosus`$length_50_mature *  sels[2],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 1,
              sel_unit = "length",
              spatial_catchability = b_spatial_q
              
            ),
            "epinephelus fuscoguttatus" = Metier$new(
              critter = fauna$`epinephelus fuscoguttatus`,
              price = prices[3],
              sel_form = "logistic",
              sel_start = fauna$`epinephelus fuscoguttatus`$length_50_mature * sels[3],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 4,
              sel_unit = "length",
              spatial_catchability = c_spatial_q
            ),
            "carcharhinus amblyrhynchos" = Metier$new(
              critter = fauna$`carcharhinus amblyrhynchos`,
              price = prices[4],
              sel_form = "logistic",
              sel_start = fauna$`carcharhinus amblyrhynchos`$length_50_mature * sels[4],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 4,
              sel_unit = "length",
              spatial_catchability = d_spatial_q
            )
          ),
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 1000,
          cost_per_distance = 2000,
          effort_cost_exponent = effort_cost_exponent,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[1,]
        ),
        "beta" = create_fleet(
          list(
            "lutjanus malabaricus" = Metier$new(
              critter = fauna$`lutjanus malabaricus`,
              price = prices[1],
              sel_form = "logistic",
              sel_start = fauna$`lutjanus malabaricus`$length_50_mature * sels[4],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 4,
              sel_unit = "length",
              spatial_catchability = a_spatial_q
              
            ),
            "pristipomoides filamentosus" = Metier$new(
              critter = fauna$`pristipomoides filamentosus`,
              price = prices[2],
              sel_form = "logistic",
              sel_start = fauna$`pristipomoides filamentosus`$length_50_mature *  sels[3],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 4,
              sel_unit = "length",
              spatial_catchability = b_spatial_q
              
            ),
            "epinephelus fuscoguttatus" = Metier$new(
              critter = fauna$`epinephelus fuscoguttatus`,
              price = prices[3],
              sel_form = "logistic",
              sel_start = fauna$`epinephelus fuscoguttatus`$length_50_mature * sels[2],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 1,
              sel_unit = "length",
              spatial_catchability = c_spatial_q
            ),
            "carcharhinus amblyrhynchos" = Metier$new(
              critter = fauna$`carcharhinus amblyrhynchos`,
              price = prices[4],
              sel_form = "logistic",
              sel_start = fauna$`carcharhinus amblyrhynchos`$length_50_mature * sels[1],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 1,
              sel_unit = "length",
              spatial_catchability = d_spatial_q
            )
          ),
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 5,
          cost_per_distance = 1000,
          effort_cost_exponent = effort_cost_exponent,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[2,]
        )
      )
      
      
    } else if (difficulty == "simple"){
      
      if (all(state$spatial_q == TRUE)) {
        a_spatial_q <-
          state$habitat[state$scientific_name == "lutjanus malabaricus"][[1]] %>%
          pivot_wider(names_from = y, values_from = habitat) %>%
          select(-x) %>%
          as.matrix()
        
        
      } else {
        a_spatial_q <- NA
      }
      
      if (use_ports == TRUE){
        ports <- port_locations
      } else {
        ports <- NULL
      }
      
      fleets <- list(
        "longline" = create_fleet(
          list(
            "lutjanus malabaricus" = Metier$new(
              critter = fauna$`lutjanus malabaricus`,
              price = prices[1],
              sel_form = "logistic",
              sel_start = fauna$`lutjanus malabaricus`$length_50_mature * sels[1],
              sel_delta = 0.01,
              catchability = .1,
              p_explt = 1,
              sel_unit = "length",
              spatial_catchability = a_spatial_q)),
          base_effort = prod(resolution) + effort_int,
          cost_per_unit_effort = 10,
          cost_per_distance = 2000,
          effort_cost_exponent = effort_cost_exponent,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[1,]
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
          base_effort = resolution^2,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution,
          fleet_model = state$fleet_model[1],
          mpa_response = state$mpa_response[1],
          ports = ports[2,],
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
          base_effort = resolution^2,
          spatial_allocation = state$spatial_allocation[1],
          resolution = resolution
        )
      )
      
    }

    fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
  }
