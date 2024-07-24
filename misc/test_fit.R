library(tidyverse)

library(cmdstanr)

library(bayesplot)

library(ggdist)

foos <- list.files(here::here("R"))

purrr::walk(foos, ~ source(here::here("R", .x)))

prep_run(n_states = 10, run_name = "sigh") # loads packages and creates and returns some global variables for the analysis

Rcpp::sourceCpp(here("src", "sim_pt_mpa.cpp"))

local_dd <- 1

phi <- 0.188

test <- sim_pt_mpa(
  r = 0.2,
  k = 500,
  init_b_inside = 100,
  init_b_outside = 1,
  p_mpa = 0.5,
  local_dd = local_dd,
  phi = phi,
  pt = 1,
  plim = 1e-3,
  m = 0.666,
  u = 0,
  years = 50
)

test$outside_b  |> plot()

test$inside_b  |> lines()


growth = test$outside - lag(test$outside)

plot(lag(test$outside), growth)

b_t_p <- data.frame(inside = test$inside_b * rlnorm(length(test$inside_b),0,.05), outside = test$outside_b * rlnorm(length(test$inside_b),0,.05))


m <- 1

net_to_mpa <- - (m * (1 - 0.5) * (500 - 0.5 / (1 - 0.5) * 10))
net_to_mpa

test_data <- list(
  b_t_p = b_t_p,
  n_t = nrow(b_t_p),
  n_p = ncol(b_t_p),
  mpa_size = 0.5,
  local_dd = local_dd
)

plot(test_data$b_t_p[,1])
lines(test_data$b_t_p[,2])



sigh <- cmdstan_model(here("src", "fit_2pbd.stan"))

fit <- sigh$sample(data = test_data,
                   seed = 123,
                   parallel_chains = 4)

mcmc_hist(fit$draws(c("g","m","phi","k", "init_dep", "sigma")))

observed <- test_data$b_t_p |>
  as.data.frame() |>
  pivot_longer(everything(), names_to = "patch", values_to = "biomass") |>
  mutate(patch = if_else(patch == "inside",1,2)) |>
  group_by(patch) |>
  mutate(year = 1:length(biomass))

predicted <- tidybayes::spread_draws(fit, hat_b_t_p[year, patch], ndraws = 250) |>
  left_join(observed, by = c("patch", "year")) |>
  mutate(patch = factor(patch))

predicted |>
  ggplot() +
  geom_point(aes(year, biomass, color = patch)) +
  stat_lineribbon(aes(year, hat_b_t_p, fill = patch), alpha = 1/4)
