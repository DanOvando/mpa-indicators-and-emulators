prep_run <- function(run_name = "test",
                     figure_text_size = 10,
                     seed = 42,
                     n_states = 10,
                     rx = 20,
                     ry = 20,
                     dx = 100000,
                     dy = 20000,
                     patch_area = 10,
                     experiment_workers = 5,
                     seasons = 2,
                     drop_patches = TRUE) {
  set.seed(seed)
  
  
  library(tidyverse)
  
  library(ggExtra)
  
  library(marlin)
  
  library(here)
  
  library(janitor)
  
  library(progress)
  
  library(furrr)
  
  library(ranger)
  
  library(rpart)
  
  library(tidymodels)
  
  library(themis)
  
  library(cmdstanr)
  
  library(Rcpp)
  
  library(glue)
  
  library(patchwork)
  
  Rcpp::sourceCpp(here('src', "sim_pt_mpa.cpp"))
  
  options(dplyr.summarise.inform = FALSE)
  
  
  # set up figures ----------------------------------------------------------
  
  theme_set(theme_minimal(base_size = figure_text_size))
  
  # prepare results location ---------------------------------------------------------
  
  if (Sys.getenv("RUN_NAME") == '') {
    run_name <- run_name
    
  } else {
    run_name <- Sys.getenv("RUN_NAME")
  }
  
  
  results_dir <<- here("results", run_name)
  
  
  
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
    
    dir.create(file.path(results_dir, "figs"), recursive = TRUE)
    
  }
  
  fig_dir <<- file.path(results_dir, "figs")
  
  
  n_states <<- n_states
  
  seasons <<- seasons
  
  experiment_workers <<- experiment_workers
  
  dx <<-  dx
  
  dy <<- dy
  
  rx <<-  rx
  
  ry <<- ry
  
  patch_area <<- patch_area
  
  patches <<- rx * ry

  drop_patches <<- drop_patches
}