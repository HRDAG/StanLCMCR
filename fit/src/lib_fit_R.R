library(pacman)
pacman::p_load(dplyr, here)
library('LCMCR', lib.loc = here("opt"))

fit_R <- function(data, a_alpha=0.25, b_alpha=0.25, seed=1, buffer_size = 10000, thinning = 100, in_list_label="1", not_in_list_label="0", K=10, trace=FALSE, burnin=10000, samples=2000, lower=NULL, upper=NULL) {
    a_lambda = 1
    b_lambda = 1

    J <- ncol(data)

    if (!is.null(upper) && !is.null(lower)) {
      recovered <- beta_params_from_expansion_factor_quantiles(0.025, 0.975, lower, upper, J, detailed=FALSE)
      a_lambda = recovered$a
      b_lambda = recovered$b
    }

    data.factor <- data.frame(lapply(data, factor)) |> 
        select_if(function(col) length(levels(col)) > 1)
    R_sampler <- lcmCR(data.factor, K=K, a_alpha=a_alpha, b_alpha=b_alpha, seed=seed, buffer_size = buffer_size, thinning = thinning, in_list_label=in_list_label, not_in_list_label=not_in_list_label, a_lambda=a_lambda, b_lambda=b_lambda)

    if (trace) {
        R_sampler$Activate_Tracing()
        R_sampler$Set_Trace(c(R_sampler$Get_Param_List()))
    }

    R_estimates <- lcmCR_PostSampl(R_sampler, burnin = burnin, samples = samples, thinning = thinning, output = FALSE)

    list(estimates=R_estimates, sampler=R_sampler)
}
