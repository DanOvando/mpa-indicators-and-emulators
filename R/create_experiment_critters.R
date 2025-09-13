create_experiment_critters <-
  function(critter_name,
           habitat,
           seasons = 1,
           seasonal_movement = FALSE,
           ontogenetic_shift = FALSE,
           spawning_aggregation = FALSE,
           spawning_seasons = NA,
           steepness = 0.7,
           b0 = 100,
           f_v_m,
           hyper = 1,
           resolution,
           patch_area,
           density_dependence,
           kiss = FALSE,
           sigma_rec = 0,
           ac_rec = 0,
           critter_templates = NULL,
           home_ranges = c("low" = 2, "medium" = 20, "high" = 200)) {
    
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
    
    
    if (critter_name == "shark"){
      
      #Oshitani, Shungo, Hideki Nakano, and Sho Tanaka. “Age and Growth of the Silky Shark Carcharhinus Falciformis from the Pacific Ocean.” Fisheries Science 69, no. 3 (2003): 456–64. https://doi.org/10.1046/j.1444-2906.2003.00645.x.
      # Neubauer, P, K Kim, K Large, and S Brouwer. “Stock Assessment of Silky Shark in the Western and Central Pacific Ocean.” Report to the Western and Central Pacific Fisheries Commission Scientific Committee. Twentieth Regular Session, 2024.
      
      
      max_shark_age <- 25
      ages <- seq(0, 25, by = 1 / seasons)
      
      length_at_age <- 216.4 * (1 - exp(-0.148 * ((ages) - -1.778991)))
      
      pups_at_age <- pmax(0,-8.6 + 0.098 * length_at_age)
      critter <- marlin::create_critter(
        query_fishlife = FALSE,
        scientific_name = "Carcharhinus falciformis",
        common_name = "silky shark",
        m = 0.21,
        lorenzen_c = -1,
        linf = 216.4,
        vbk = 0.148,
        t0 = -1.778991, # 50cm at birth
        min_age = 0,
        max_age = 25,
        age_mature = 6,
        weight_a = 0.0000273,
        weight_b = 2.86,
        steepness = 0.3,
        b0 = b0,
        fec_at_age = pups_at_age,
        adult_home_range = home_ranges["high"],
        recruit_home_range = home_ranges["low"],
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        patch_area = patch_area,
        seasons = seasons,
        habitat = hab,
        recruit_habitat = recruit_habitat,
        density_dependence = "post_dispersal",
        fec_expo = hyper,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec)

    } else if (critter_name == "tuna"){
      
      critter <- marlin::create_critter(
        query_fishlife = FALSE,
        scientific_name = "Thunnus albacares",
        common_name = "yellowfin tuna",
        m = 0.3,
        growth_model = "growth_cessation",
        lorenzen_c = -1,
        l0 = 18.85,
        rmax = 37.24,
        k =  0.89,
        t50 = 4.57,
        min_age = 0,
        max_age = 15,
        age_mature = 3,
        weight_a = 0.00004,
        weight_b = 2.86,
        habitat = hab,
        recruit_habitat = recruit_habitat,
        adult_home_range = home_ranges["high"],
        recruit_home_range = home_ranges["high"],
        density_dependence = density_dependence,
        seasons = seasons,
        fec_expo = hyper,
        steepness =  0.9,
        b0 = b0,
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        patch_area = patch_area,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec)
      
      
      
    } else if (critter_name == "grouper"){
      # https://sedarweb.org/assessments/sedar-47/
      critter <- marlin::create_critter(
        query_fishlife = FALSE,
        scientific_name = "Epinephelus fuscoguttatus",
        common_name = "goliath grouper",
        m = 0.16464,
        lorenzen_c = -.5746,
        linf = 222.11,
        vbk = 0.0937,
        t0 = -.68, 
        min_age = 0,
        max_age = 35,
        age_mature = 6,
        weight_a = 1e-5,
        weight_b = 3.151,
        b0 = b0,
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        patch_area = patch_area,
        seasons = seasons,
        habitat = hab,
        recruit_habitat = recruit_habitat,
        adult_home_range = home_ranges["low"],
        recruit_home_range = home_ranges["high"],
        density_dependence = density_dependence,
        fec_expo = hyper,
        steepness =  0.6,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec)
      
      
    } else if (critter_name == "reef_fish"){
      
      
      
      # library(FishLife)
      # 
      # edge_names = c( FishLife::FishBase_and_Morphometrics$tree$tip.label,
      #                 FishLife::FishBase_and_Morphometrics$tree$node.label[-1] ) # Removing root
      # 
      # #
      # which_g = match( "Seriola quinqueradiata", edge_names )
      # Table2023 = cbind(
      #   Mean = FishBase_and_Morphometrics$beta_gv[which_g,],
      #   SE = sqrt(diag(FishBase_and_Morphometrics$Cov_gvv[which_g,,]))
      # ) |>
      #   as.data.frame() |>
      #   rownames_to_column() |>
      #   mutate(across(-rowname, exp))
      # 
      # 
      # Table2023
      
      critter <- marlin::create_critter(
        query_fishlife = FALSE,
        scientific_name = "Seriola quinqueradiata",
        common_name = "reef fish",
        m = 0.4,
        lorenzen_c = -1,
        linf = 104,
        vbk = 2.090865e-01,
        t0 = -.5, 
        min_age = 0,
        max_age = 15,
        age_mature = 2,
        weight_a = 3e-4,
        weight_b = 2.6,
        spawning_seasons = spawning_seasons,
        resolution = resolution,
        patch_area = patch_area,
        seasons = seasons,
        habitat = hab,
        adult_home_range = home_ranges["medium"],
        recruit_home_range = home_ranges["medium"],
        density_dependence = density_dependence,
        fec_expo = hyper,
        steepness =  0.8,
        b0 = b0,
        sigma_rec = sigma_rec,
        ac_rec = ac_rec)

    }
  

    critter$init_explt = max(critter$m_at_age) * f_v_m
    
    return(critter)
    
  }
