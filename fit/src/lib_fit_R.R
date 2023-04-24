library(pacman)
pacman::p_load(dplyr, here)
library('LCMCR', lib.loc = here("opt"))

fit_R <- function(data, a_alpha=0.25, b_alpha=0.25, seed=1, buffer_size = 10000, thinning = 100, in_list_label="1", not_in_list_label="0", K=10, trace=FALSE, burnin=10000, samples=2000, a_lambda=NULL, b_lambda=NULL) {
    data.factor <- data.frame(lapply(data, factor)) |> 
        select_if(function(col) length(levels(col)) > 1)
    R_sampler <- lcmCR(data.factor, K=K, a_alpha=a_alpha, b_alpha=b_alpha, seed=seed, buffer_size = buffer_size, thinning = thinning, in_list_label=in_list_label, not_in_list_label=not_in_list_label, a_lambda=NULL, b_lambda=NULL)

    if (trace) {
        R_sampler$Activate_Tracing()
        R_sampler$Set_Trace(c(R_sampler$Get_Param_List()))
    }

    R_estimates <- lcmCR_PostSampl(R_sampler, burnin = burnin, samples = samples, thinning = thinning, output = FALSE)

    list(estimates=R_estimates, sampler=R_sampler)
}
