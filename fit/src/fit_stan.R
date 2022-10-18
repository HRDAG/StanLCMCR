library(pacman)
pacman::p_load(here, cmdstanr, yaml, tidyverse)

fit_stan <- function(model, data, K=10, num_iter=2000, seed=19481210, chains=4, warmup=2000, adapt_delta=0.8, alpha=NULL, output_dir=NULL, output_basename=NULL) {
  data.factor <- data.frame(lapply(data, factor))
  
  stan_data_tabular <- data %>% 
    group_by_all() %>%
    summarize(cell_count = n())
  
  stan_data <- stan_data_tabular %>%
    select(-cell_count) %>%
    as.matrix()
  
  stan_data_list <- list(J = ncol(stan_data),
                         C = nrow(stan_data_tabular),
                         list_indicators = stan_data,
                         cell_count = stan_data_tabular$cell_count,
                         list_count = sapply(data, sum),
                         K = K,
                         alpha = alpha 
                         ) %>%
    compact()
  
  fit <- model$sample(data = stan_data_list,
                                seed = seed,
                                chains = chains,
                                parallel_chains = chains,
                                iter_warmup = warmup,
                                iter_sampling = num_iter,
                                adapt_delta = adapt_delta,
                                output_dir = output_dir,
                                output_basename = output_basename)

  fit
}

# Configuration file for fits
models_spec <- read_yaml(here("fit", "hand", "models.yaml"))
simulations <- read_yaml(here("fit", "hand", "simulations.yaml"))
fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
datasets <- simulations$data
models <- models_spec$models

for (i in 1:length(datasets)) {
    dataset_name <- datasets[[i]]$name
    df <- read.csv(here("fit", "input", paste(dataset_name, ".csv", sep="")))
    for (j in 1:length(models)) {
        model <- models[[j]]
        stan_model <- cmdstan_model(exe_file=here("fit", "input", model$name))
        run_name <- paste(model$name, dataset_name, sep="_")

        # only run the models that do not fix alpha
        if (!model$fixed_alpha) {
            fitted_model <- fit_stan(stan_model, df,
                                     K=fit_params$settings$K,
                                     num_iter=fit_params$settings$num_iter,
                                     seed=fit_params$seed,
                                     chains=fit_params$settings$chains,
                                     warmup=fit_params$settings$warmup,
                                     adapt_delta=fit_params$settings$adapt_delta,
                                     output_dir=here("fit", "output"),
                                     output_basename=run_name)

            fitted_model$save_object(file = here("fit", "output", paste(run_name, ".rds", sep="")))
        }
    }
}
