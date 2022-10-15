library(pacman)
pacman::p_load(here)
pacman::p_load_gh("stan-dev/cmdstanr")


stan_model <- cmdstan_model(here("compile/input/LCMCR.stan"), pedantic = TRUE, dir=here("compile/output"))
stan_model2 <- cmdstan_model(here("compile/input/LCMCR_2.stan"), pedantic = TRUE, dir=here("compile/output"))
stan_model3 <- cmdstan_model(here("compile/input/LCMCR_3.stan"), pedantic = TRUE, dir=here("compile/output"))
stan_model4 <- cmdstan_model(here("compile/input/LCMCR_4.stan"), pedantic = TRUE, dir=here("compile/output"))
stan_model5 <- cmdstan_model(here("compile/input/LCMCR_5_fixed_alpha.stan"), pedantic = TRUE, dir=here("compile/output"))

