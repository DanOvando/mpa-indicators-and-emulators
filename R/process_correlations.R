process_correlations <- function(cores) {
  
  if (length(cores) > 1) {
    cores <- cores[upper.tri(cores)]
    
    max_cor <- max(cores)
    
    min_cor <- min(cores)
  } else {
    max_cor <- min_cor <- 1
  }

  range_cor <- max_cor - min_cor
  
  mean_abs_cor <- mean(abs(cores))
  
  mean_cor <- mean(cores)
  
  proc_cores <- data.frame(
    max_cor = max_cor,
    min_cor = min_cor,
    range_cor = range_cor,
    mean_abs_cor = mean_abs_cor,
    mean_cor = mean_cor
  )
  
}
