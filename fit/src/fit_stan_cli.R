library(pacman)
pacman::p_load(here, yaml, readr, dplyr, argparse)
source(here("fit", "src", "lib_fit_stan.R"))
source(here("fit", "src", "lib_fit_R.R"))

###############################################
# Fit the model/dataset combination specified in the arguments
###############################################

fit_params <- read_yaml(here("fit", "hand", "fit.yaml"))
settings <- fit_params$settings
R_settings <- fit_params$R_settings

parser <- ArgumentParser(description="Fit the model/dataset combination specified in the arguments")
parser$add_argument("--model", dest="model")
parser$add_argument("--q025", dest="model_q025", default=1.1, type="double")
parser$add_argument("--q975", dest="model_q975", default=5, type="double")
parser$add_argument("--dataset", dest="dataset_name")

args <- parser$parse_args()
model <- args$model
model_q025 <- args$model_q025
model_q975 <- args$model_q975
dataset_name <- args$dataset_name

df <- read_csv(here("fit", "input", "data", paste(dataset_name, ".csv", sep="")))

if (model == "R") {
    results <- fit_R(df,
          a_alpha=R_settings$a_alpha,
          b_alpha=R_settings$b_alpha,
          seed=fit_params$seed,
          buffer_size=R_settings$buffer_size,
          thinning=R_settings$thinning,
          K=R_settings$K,
          trace=R_settings$trace,
          burnin=R_settings$burnin,
          samples=R_settings$n_iters)

    estimates <- results[["estimates"]]
    sampler <- results[["sampler"]]

    saveRDS(estimates, file=here("fit", "output", paste("R", dataset_name, "estimates.rds", sep="_")))
    saveRDS(sampler, file=here("fit", "output", paste("R", dataset_name, "model.rds", sep="_")))
} else {
    stan_model <- cmdstan_model(exe_file=here("fit", "input", "models", model))

    run_name <- paste(model, model_q975, dataset_name, sep="_")
    fitted_model <- fit_stan(stan_model, df,
                             K=settings$K,
                             num_iter=settings$num_iter,
                             seed=fit_params$seed,
                             chains=settings$chains,
                             warmup=settings$warmup,
                             adapt_delta=settings$adapt_delta,
                             output_dir=here("fit", "output"),
                             output_basename=run_name,
                             lower=model_q025,
                             upper=model_q975)

    fitted_model$save_object(file = here("fit", "output", paste(run_name, ".rds", sep="")))
}
