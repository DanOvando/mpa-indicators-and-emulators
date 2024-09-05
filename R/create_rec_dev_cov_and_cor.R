create_rec_dev_cov_and_cor <- function(fauna,max_abs_cor = 1){
  
  sigma_recs <- purrr::map_dbl(fauna, "sigma_rec") # gather recruitment standard deviations
  
  ac_recs <- purrr::map_dbl(fauna, "ac_rec") # gather autocorrelation in recruitment standard deviations
  
  n_critters <-  length(fauna)
  
  n_critter_cores <- n_critters * (n_critters + 1) / 2 - n_critters
  # n_species * (n_species + 1) / 2 # formula for the number of elements in the upper triangle of an n x n matric
  
  core_matrix <- matrix(0, nrow = n_critters, ncol = n_critters)
  
  is_positive_semidefinite <- FALSE
  while (is_positive_semidefinite == FALSE) {
    critter_cores <-
      runif(n_critter_cores, min = -max_abs_cor, max = max_abs_cor) # randomly generate correlations among species
    # Fill in the upper triangle of the matrix
    core_matrix[upper.tri(core_matrix)] <- critter_cores
    
    lower_triangle <- t(core_matrix)
    
    critter_correlations <- core_matrix + lower_triangle
    
    diag(critter_correlations) <- 1
    
    covariance_rec <- critter_correlations * (sigma_recs %o% sigma_recs)
    
    eigenvalues <- eigen(covariance_rec)$values
    
    is_positive_semidefinite <- all(eigenvalues >= 0)
    
    
  }
  out <- list(cov = covariance_rec, cor = critter_correlations)
 return(out)
  
}