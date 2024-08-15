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
  
      
      
    }
    
    fleets <- tune_fleets(fauna, fleets, tune_type = tune_type)
  }
