library(pacman)
pacman::p_load(here, cmdstanr, yaml)

models_metadata <- read_yaml(here("compile", "hand", "models.yaml"))

models <- models_metadata$models

for (i in 1:length(models)) {
    stan_model <- cmdstan_model(here("compile", "input", paste(models[[i]]$name, ".stan", sep="")), pedantic = TRUE, dir=here("compile", "output"))
}
