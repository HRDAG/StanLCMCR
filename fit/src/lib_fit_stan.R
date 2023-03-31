library(pacman)
pacman::p_load(here, dplyr, purrr)

if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}


source(here("fit", "src", "beta_priors.R"))

fit_stan <- function(model, data, K=10, num_iter=2000, seed=19481210, chains=4, warmup=2000, adapt_delta=0.8, alpha=NULL, output_dir=NULL, output_basename=NULL, lower=1.01, upper=3) {
  stan_data_tabular <- data |> 
    group_by_all() |>
    summarize(cell_count = dplyr::n())
  
  stan_data <- stan_data_tabular |>
    select(-cell_count) |>
    as.matrix()

  J <- ncol(stan_data)

  # This is only for models where we need to set the beta priors a and b
  recovered <- beta_params_from_expansion_factor_quantiles(0.025, 0.975, lower, upper, J, detailed=FALSE)

  list_count <- sapply(data, sum)

  stan_data_list <- list(J = J,
                         C = nrow(stan_data_tabular),
                         list_indicators = stan_data,
                         cell_count = stan_data_tabular$cell_count,
                         list_count = list_count,
                         K = K,
                         alpha = alpha,
                         a=recovered$a,
                         b=recovered$b)

  stan_data_list <- purrr::compact(stan_data_list)

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

