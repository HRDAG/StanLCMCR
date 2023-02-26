library(pacman)
pacman::p_load(here, yaml)

if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

models_metadata <- read_yaml(here("compile", "hand", "models.yaml"))

models <- models_metadata$models

for (i in 1:length(models)) {
    stan_model <- cmdstan_model(here("compile", "input", paste(models[[i]]$name, ".stan", sep="")), pedantic = TRUE, dir=here("compile", "output"))
}
