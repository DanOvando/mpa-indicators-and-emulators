run_twopbd_mpas <-
  function(params, depletion, local_dd, p_mpa, years =  50, fleet) {
    # params <- emulated_state_experiments$twopbd_params[[1]]
    #
    # depletion <- emulated_state_experiments$depletion[[1]]
    #
    # local_dd <- emulated_state_experiments$local_dd[[1]]
    
    mpa_response <- fleet[[1]]$mpa_response
    
    params <-
      pivot_wider(params, names_from = variable, values_from = estimate)
    
    bmsy_k <-  (1 / (params$phi + 1) ^ (1 / params$phi))
    
    bmsy <- params$k / (params$phi + 1) ^ (1 / params$phi)
    
    umsy <- params$g
    
    msy <- umsy * bmsy
    
    b_k <- depletion
    
    b_bmsy <- b_k / bmsy_k
    
    u_umsy <-
      (params$phi + 1) / params$phi * (1 - (b_bmsy) ^ params$phi / (params$phi + 1)) # assume B is at EQ, I believe that's what they did for megadata
    
    baseline_b <- b_bmsy * bmsy
    
    baseline_u <- u_umsy * umsy
    
    basline_yield <- u_umsy * b_bmsy * msy
    if (mpa_response == "stay"){
      baseline_u <- pmin(0.999,1 - (1 - baseline_u) ^ (1 / (1 - p_mpa))) # displace effort to outside the MPA
      
    } 
    mpa_sim <-  sim_pt_mpa(
      r = params$g,
      k = params$k,
      init_b_inside = baseline_b * p_mpa,
      init_b_outside = baseline_b * (1 - p_mpa),
      m = params$m,
      u = baseline_u,
      p_mpa = p_mpa,
      local_dd = local_dd,
      years = years,
      phi = params$phi,
      pt = 1,
      plim = .2
    )
    out <-
      tibble(
        b_inside = mpa_sim$inside_b,
        b_outside = mpa_sim$outside_b,
        yield_mpa = mpa_sim$yield,
        b_bau = baseline_b,
        yield_bau = basline_yield
      ) |>
      mutate(
        b_mpa = b_inside + b_outside,
        depletion_bau = depletion,
        depletion_mpa = b_mpa / params$k,
        year = 1:years
      )

    return(out)
  }