library(pacman)
pacman::p_load(here, dplyr, purrr)

if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}


source(here("fit", "src", "beta_priors.R"))

get_R_init_vals <- function(data, a_alpha=0.25, b_alpha=0.25, seed=1, buffer_size = 10000, thinning = 100, in_list_label="1", not_in_list_label="0", K=10, trace=FALSE, burnin=10000, samples=2000, lower=NULL, upper=NULL) {
    R_sampler <- lcmCR(data, K=K, a_alpha=a_alpha, b_alpha=b_alpha, seed=seed, buffer_size = buffer_size, thinning = thinning, in_list_label=in_list_label, not_in_list_label=not_in_list_label, a_lambda=a_lambda, b_lambda=b_lambda)

    init_vals <- list(
                      N=R_sampler$Get_Param("n"),
                      alpha=R_sampler$Get_Param("alpha"),
                      lambda=exp(R_sampler$Get_Param("log_lambdaJK2")[,,1]),
                      # Get `breaks` from log_nuK
                  )
}

fit_stan <- function(model, data, K=10, num_iter=2000, seed=19481210, chains=4, warmup=2000, adapt_delta=0.8, alpha=NULL, output_dir=NULL, output_basename=NULL, lower=1.01, upper=3, use_R_inits=FALSE) {
  # Note: use_R_inits currently doesn't do anything.
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

