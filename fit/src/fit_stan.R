library(pacman)
pacman::p_load(here, cmdstanr, yaml, tidyverse)
source(here("fit", "src", "beta_priors.R"))

fit_stan <- function(model, data, K=10, num_iter=2000, seed=19481210, chains=4, warmup=2000, adapt_delta=0.8, alpha=NULL, output_dir=NULL, output_basename=NULL, lower=1.01, upper=3) {
  stan_data_tabular <- data %>% 
    group_by_all() %>%
    summarize(cell_count = dplyr::n())
  
  stan_data <- stan_data_tabular %>%
    select(-cell_count) %>%
    as.matrix()

  J <- ncol(stan_data)

  # This is only for models where we need to set the beta priors a and b
  recovered <- beta_params_from_expansion_factor_quantiles(0.025, 0.975, lower, upper, J, detailed=FALSE)
  
  stan_data_list <- list(J = J,
                         C = nrow(stan_data_tabular),
                         list_indicators = stan_data,
                         cell_count = stan_data_tabular$cell_count,
                         list_count = sapply(data, sum),
                         K = K,
                         alpha = alpha,
                         a=recovered$a,
                         b=recovered$b) %>%
      compact()

  print(stan_data_list)
  
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
