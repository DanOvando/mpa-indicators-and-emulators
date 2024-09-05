generate_rec_devs <- function(fauna, rec_dev_cov_and_cor,years){
  
  sigma_recs <- purrr::map_dbl(fauna, "sigma_rec") # gather recruitment standard deviations
  
  ac_recs <- purrr::map_dbl(fauna, "ac_rec") # gather autocorrelation in recruitment standard deviations
  
  n_critters <-  length(fauna)
  if (!is.null(rec_dev_cov_and_cor)){
    
    critter_correlations <- rec_dev_cov_and_cor$cor
    
    covariance_rec <- critter_correlations * (sigma_recs %o% sigma_recs)
    
    seasons <- fauna[[1]]$seasons
    
    rec_steps <- (years + 1) * seasons
    # simulate autocorrlated recruitment deviates with cross-critter correlations
    log_rec_devs <- matrix(NA, nrow = rec_steps, ncol = n_critters, dimnames = list(1:(rec_steps), names(fauna)))
    
    log_rec_devs[1,] <- mvtnorm::rmvnorm(1,rep(0, n_critters),sigma = covariance_rec)
    
    for (i in 2:rec_steps){
      
      log_rec_devs[i, ] <- ac_recs *  log_rec_devs[i - 1, ] + sqrt(1 - ac_recs ^ 2) * mvtnorm::rmvnorm(1, rep(0,n_critters), sigma = covariance_rec)
      
    }
  } else {
    log_rec_devs <- NULL
  }
  
  
  return(log_rec_devs)
  
}