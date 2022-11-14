library(pacman)
pacman::p_load(here, cmdstanr, yaml, tidyverse)

fit_stan <- function(model, data, K=10, num_iter=2000, seed=19481210, chains=4, warmup=2000, adapt_delta=0.8, alpha=NULL, output_dir=NULL, output_basename=NULL) {
  data.factor <- data.frame(lapply(data, factor)) %>%
    select_if(function(col) length(levels(col)) > 1)
  
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
fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
datasets <- fit_params$datasets
models <- fit_params$models
alphas <- fit_params$alpha_sweep$alphas

###############################################
# Fit all of the model-dataset combinations
###############################################

# Wrap in a conditional to make it easy to skip fitting any models
if (!is.null(models)) {
    for (j in 1:length(models)) {
        model <- models[[j]]
        
        stan_model <- cmdstan_model(exe_file=here("fit", "input", "models", model))

        for (i in 1:length(datasets)) {
            dataset_name <- datasets[[i]]
            run_name <- paste(model, dataset_name, sep="_")
            df <- read.csv(here("fit", "input", "data", paste(dataset_name, ".csv", sep="")))

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

###############################################
# Sweep over alphas for a fixed dataset/model
###############################################

df <- read.csv(here("fit", "input", "data", paste(fit_params$alpha_sweep$dataset, ".csv", sep="")))

if (length(alphas) > 0) {
    for (i in 1:length(alphas)) {
        alpha_sweep_model <- cmdstan_model(exe_file=here("fit", "input", "models", fit_params$alpha_sweep$model))
        alpha <- alphas[[i]]

        run_name <- paste(fit_params$alpha_sweep$model, fit_params$alpha_sweep$dataset, alpha, sep="_")

        fitted_model <- fit_stan(alpha_sweep_model, df,
                                 K=fit_params$settings$K,
                                 num_iter=fit_params$settings$num_iter,
                                 seed=fit_params$seed,
                                 chains=fit_params$settings$chains,
                                 warmup=fit_params$settings$warmup,
                                 adapt_delta=fit_params$settings$adapt_delta,
                                 alpha=alpha, # This is the key argument that is omitted earlier
                                 output_dir=here("fit", "output"),
                                 output_basename=run_name)

        fitted_model$save_object(file = here("fit", "output", paste(run_name, ".rds", sep="")))
    }
}
