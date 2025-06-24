create_experiment_critters <-
  function(sciname,
           habitat,
           seasons = 1,
           adult_home_range = 50,
           recruit_home_range = 500,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           spawning_aggregation = FALSE,
           spawning_seasons = NA,
           steepness = 0.7,
           ssb0 = NA,
           b0 = 100,
           f_v_m,
           hyper = 1,
           resolution,
           density_dependence,
           kiss = FALSE,
           sigma_rec = 0,
           ac_rec = 0,
           critter_templates = NULL) {
    
    hab <- habitat %>%
      pivot_wider(names_from = y, values_from = habitat) %>%
      select(-x) %>%
      as.matrix()
    
    
    
    if (seasonal_movement) {
      hab <- list(hab,-hab - (min(-hab)))
      #
      #       h1 <-  expand_grid(x = 1:resolution, y = 1:resolution) %>%
      #         mutate(habitat =  dnorm(x, resolution / 2, 0.5 * resolution) *  dnorm(y, resolution / 2, 0.5 * resolution)) %>%
      #         mutate(habitat = habitat * (x >= 4))
      #
      #
      #       h2 <-  expand_grid(x = 1:resolution, y = 1:resolution) %>%
      #         mutate(habitat =  -.5 * x + 10) %>%
      #         mutate(habitat = habitat * (x < 4))
      #
      #
      #       bigeye_habitat <- h1 %>%
      #         pivot_wider(names_from = y, values_from = habitat) %>%
      #         select(-x) %>%
      #         as.matrix()
      #
      #       bigeye_habitat2 <- h2 %>%
      #         pivot_wider(names_from = y, values_from = habitat) %>%
      #         select(-x) %>%
      #         as.matrix()
      #
      #
      #       hab <- list(bigeye_habitat, bigeye_habitat2)
      
    } else {
      hab <- list(hab, hab)
    }
    if (spawning_aggregation & !all(is.na(spawning_seasons))){
      
      tmp_hab <- hab[[spawning_seasons]]
      
      tmp_hab[tmp_hab < quantile(tmp_hab,0.9)] <- 0 # remove all habitat except the core 90%
      
      hab[[spawning_seasons]] <- tmp_hab
      
    }
    
    
    if (is.na(spawning_seasons)){
      spawning_seasons <- 1:seasons
    }
    
    if (ontogenetic_shift) {
      recruit_habitat <-
        -hab[[spawning_seasons[[1]]]] - min(-hab[[spawning_seasons[[1]]]]) # place recruits in different places than adults
      # set recruitment form to allow for recruit habitat
      #
      density_dependence <- "local_habitat"
    } else {
      recruit_habitat <- hab[[spawning_seasons[[1]]]]
    }
    if (kiss){
      hab <- list()
      recruit_habitat <- list()
    }
    
    if (is.null(critter_templates)){
      critter <- marlin::create_critter(
        scientific_name = sciname,
        habitat = hab,
        recruit_habitat = recruit_habitat,
        adult_home_range = adult_home_range,
        recruit_home_range = recruit_home_range,
        density_dependence = density_dependence,
        fec_form = ifelse(str_detect(sciname,("carcharhinus|sphyrna|prionace")),"pups","weight"),
        pups = 6,
        seasons = seasons,
        fec_expo = hyper,
        steepness =  steepness,
        ssb0 = ssb0,
        b0 = b0, 
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec
      )
    } else {
      
      critter_template <- critter_templates[[sciname]]
      
      critter <- marlin::create_critter(
        query_fishlife = FALSE,
        scientific_name = sciname,
        m_at_age = critter_template$m_at_age,
        linf = critter_template$linf,
        vbk = critter_template$vbk,
        t0 = critter_template$t0,
        min_age = critter_template$min_age,
        max_age = critter_template$max_age,
        age_mature = critter_template$age_mature,
        weight_a = critter_template$weight_a,
        weight_b = critter_template$weight_b,
        habitat = hab,
        recruit_habitat = recruit_habitat,
        adult_home_range = adult_home_range,
        recruit_home_range = recruit_home_range,
        density_dependence = density_dependence,
        fec_form = ifelse(str_detect(
          sciname, ("carcharhinus|sphyrna|prionace")
        ), "pups", "weight"),
        pups = 8,
        seasons = seasons,
        fec_expo = hyper,
        steepness =  steepness,
        ssb0 = ssb0,
        b0 = b0,
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec
      )
      
    }
    

    
    critter$init_explt = max(critter$m_at_age) * f_v_m * critter$steepness * 0.66
    
    return(critter)
    
  }
