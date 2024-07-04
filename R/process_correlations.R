process_correlations <- function(cores) {
  cores <- cores[upper.tri(cores)]
  
  max_cor <- max(cores)
  
  min_cor <- min(cores)
  
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
