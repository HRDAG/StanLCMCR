library(pacman)
pacman::p_load(here, cmdstanr, yaml, readr, dplyr)
source(here("fit", "src", "beta_priors.R"))
source(here("fit", "src", "lib_fit_stan.R"))

# Configuration file for fits
fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
datasets <- fit_params$datasets
models <- fit_params$models
alphas <- fit_params$alpha_sweep$alphas

###############################################
# Fit all of the model-dataset combinations, either directly or via slurm
###############################################

# Wrap in a conditional to make it easy to skip fitting any models
if (!is.null(models)) {
    for (j in 1:length(models)) {
        model_dict <- models[[j]]
        model <- model_dict$model
        model_q025 <- model_dict$prior_q025
        model_q975s <- model_dict$prior_q975
        
        stan_model <- cmdstan_model(exe_file=here("fit", "input", "models", model))

        for (i in 1:length(datasets)) {
            dataset_name <- datasets[[i]]
            df <- read_csv(here("fit", "input", "data", paste(dataset_name, ".csv", sep="")))

            if (is.null(model_q025)) {
                model_q025 <- 1.01
                model_q975s <- c(5)
            }

            for (q975 in model_q975s) {
                run_name <- paste(model, q975, dataset_name, sep="_")

                rscript_args <- c(here("fit/src/fit_stan_cli.R"), "--model", model, "--dataset", dataset_name, "--q975", q975)

                if (fit_params$use_slurm) {
                    system2("srun", c("--ntasks=1", "--nodes=1", paste("--cpus-per-task=", fit_params$settings$chains, sep=""), paste("--output=", here("fit", "logs", paste(run_name, ".log", sep="")), sep=""), "Rscript", rscript_args), wait=FALSE)
                } else {
                    system2("Rscript", rscript_args)
                }
            }
        }
    }
}

###############################################
# Sweep over alphas for a fixed dataset/model: this is not parallelized and kind of made obsolete by commenting out
# the relevant part in the YAML, so this never runs. But it's here for replicability sake, since it's used for
# some of the blog post figures.
###############################################

df <- read_csv(here("fit", "input", "data", paste(fit_params$alpha_sweep$dataset, ".csv", sep="")))

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
