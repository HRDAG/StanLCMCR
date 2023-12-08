library(pacman)

pacman::p_load(
  "here",
  "transport", # for Wasserstein distance
  "distr", # for inverse digamma function, igamma
  "stats", # for fitting gamma quantiles, get.gamma.par
  "tidyverse"
)

# Lightly edited to remove plotting code from rriskDistributions:
#  https://github.com/BfRstats/rriskDistributions/blob/master/R/rriskDistributions_functions.R
# (had some installation issues with rriskDistributions across versions)
is.error <- function(x) inherits(x, "try-error")
get.gamma.par <- function(p = c(0.025, 0.5, 0.975), q, 
                          show.output = TRUE, plot = TRUE, 
                          tol = 0.001, fit.weights = rep(1, length(p)), 
                          scaleX = c(0.1, 0.9), ...) {
  #-----------------------------------------------------------------------------
  # checking consistency of the input data
  #-----------------------------------------------------------------------------
  if (!is.numeric(p) | !is.numeric(q) | !is.numeric(fit.weights)) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, not numerical items in the input vectors 'p', 'q' and/or 'fit.weights'!", call. = FALSE)
  }
  if (prod(order(p) == seq(1:length(p))) == 0 | prod(order(q) == seq(1:length(q))) == 0) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, the vector of probabilities/percentiles is not ordered!", call. = FALSE)
  }
  if (min(p) < 0 | max(p) > 1) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, items of the probability vector should lie between 0 and 1!", call. = FALSE)
  }
  if (min(q) < 0) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, percentiles are out of the domain [0, inf) => Gamma distribution couldn't be fitted!", call. = FALSE)
  }
  if (length(p) != length(q) | length(p) != length(fit.weights) | length(q) != length(fit.weights) ) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, 'p', 'q' and 'fit.weights' are not of the same length! The vectors of quantiles, probabilities and weightings should be of the same length.", call. = FALSE)
  }
  if (length(q) < 2) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, at least two quantiles must be known!", call. = FALSE)
  }
  if (!is.logical(show.output)) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, the argument 'show.output' should be logical!", call. = FALSE)
  }
  if (!is.logical(plot)) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, the argument 'plot' should be logical!", call. = FALSE)
  }
  if (!is.numeric(tol) | length(tol) != 1 | tol < 0) {
    # on.exit(return(invisible(NA)))
    stop("INVALID INPUT, the argument 'tol' should be a single positive numerical value!", call. = FALSE)
  }
  
  #-----------------------------------------------------------------------------
  # minimizing procedure
  #-----------------------------------------------------------------------------
  fit.weights.original <- fit.weights
  fit.weights <- fit.weights/sum(fit.weights)
  minimize <- function(theta) {
    summand <- suppressWarnings(stats::pgamma(q = q, 
                                              shape = theta[1], 
                                              rate = theta[2]) - p)
    summand <- summand * fit.weights
    sum(summand^2)
  }
  fit <- c(); fit$value <- tol + 1
  try1 <- try(
    fit <- stats::optim(par = c(1, 1), 
                        minimize, method = "L-BFGS-B", 
                        lower = c(0.001, 0.001), 
                        upper = c(10000, 10000)), 
    silent = TRUE
  )
  
  #-----------------------------------------------------------------------------
  # checking results
  #-----------------------------------------------------------------------------
  if (is.error(try1) || fit$value >= tol) {
    warning("The fitting procedure 'L-BFGS-B' has failed (convergence error occurred or specified tolerance not achieved)!", call. = FALSE)
    fit <- c(); fit$value <- tol + 1
    try2 <- try(
      fit <- stats::optim(par = c(1, 1),
                          minimize, 
                          method = "BFGS"), 
      silent = TRUE)
    if (is.error(try2) || fit$value >= tol) { 
      warning("The fitting procedure 'BFGS' has failed (convergence error occurred or specified tolerance not achieved)!", call. = FALSE) 
      Par <- NA
    } else if (fit$value < tol) {
      message("The fitting procedure 'BFGS' was successful!\n(Used this fallback optimization method because 'L-BFGS-B' has failed...)") 
      Par <- fit$par
      names(Par) <- c("shape", "rate")
      if (show.output) print(fit) 
    }
  } else if (fit$value < tol) {
    message("The fitting procedure 'L-BFGS-B' was successful!") 
    Par <- fit$par
    names(Par) <- c("shape", "rate")
    if (show.output) print(fit) 
  }
  
  #-----------------------------------------------------------------------------
  # output
  #-----------------------------------------------------------------------------
  return(Par)
}

idigamma_appx <- function(x) {
  1/log(1 + exp(-x))
}

invdigamma <- function(x) {
  # Uses a spline over a grid approximation for values lower than digamma(7.96)
  # (From distr package, source code: https://rdrr.io/cran/distr/src/R/igamma.R)
  # Otherwise, uses an analytical approximation:
  # https://math.stackexchange.com/a/3165404/471917
  #
  # error peaks around ~0.005 where it transitions between the two
  ifelse(x < digamma(7.96),  igamma(x), idigamma_appx(x))
}

beta_b_from_gamma <- function(alpha, beta, J, a) {
  # Want to find b for a beta distribution Y ~ beta(a, b)
  # where a is given, and where the mean of -log(Y) matches
  # the mean of X ~ gamma(alpha, beta)
  
  invdigamma(alpha / (J * beta) + digamma(a)) - a
}

variance_difference_moment_matching <- function(a, alpha, beta, J) {
  # we want to find (a, b) for Y_i ~ beta(a, b) such that we minimize the
  # difference between J*var(-log(Y_i)) and var(X) where X ~ Gamma(alpha, beta).
  #
  # This function expresses that difference, and is to be minimized over.
  #
  # Note that `b` is picked automatically to match the first moment.
  
  b <- beta_b_from_gamma(alpha, beta, J, a)
  
  -J * (trigamma(a + b) - trigamma(a)) - alpha / (beta**2)
}

gamma_params_to_beta_products <- function(alpha, beta, J) {
  # Given X ~ gamma(alpha, beta), we want to find
  # (a, b) such that X ~= log(Y_1 * Y_2 * ... * Y_J)
  # where Y_i ~ beta(a, b)
  
  found <- uniroot(variance_difference_moment_matching, c(0.0001, 100), alpha=alpha, beta=beta, J=J, tol=.Machine$double.eps, maxiter=50000)
  a <- found$root
  b <- beta_b_from_gamma(alpha, beta, J, a)
  
  list(a=a, b=b)
}

beta_params_from_expansion_factor_quantiles <- function(p_lower, p_upper, q_lower, q_upper, J, detailed=FALSE) {
  # p_lower, p_upper: lower and upper probabilities of expansion factor
  # q_lower, q_upper: lower and upper quantiles of expansion factors 
  #     corresponding to p_lower and p_upper
  # J: number of lists
  
  gamma_upper <- log(q_lower) - log(q_lower - 1)
  gamma_lower <- log(q_upper) - log(q_upper - 1)
  
  gamma_params_from_quantiles <- get.gamma.par(c(p_lower, p_upper), c(gamma_lower, gamma_upper), show.output=FALSE, plot=FALSE, tol=0.000001)
  gamma_alpha_from_quantiles <- gamma_params_from_quantiles[["shape"]]
  gamma_beta_from_quantiles <- gamma_params_from_quantiles[["rate"]]
  recovered <- gamma_params_to_beta_products(gamma_alpha_from_quantiles, gamma_beta_from_quantiles, J)
  
  if (detailed) {
    list(gamma_params=gamma_params_from_quantiles, recovered=recovered)
  }
  else {
    recovered
  }
}

create_beta_products <- function(a, b, J, N) {
  betas <- log(rbeta(N, a, b))
  for (j in 2:J) {
    betas <- betas + log(rbeta(N, a, b))
  }
  
  -betas
}

# Simulate data using known parameters for the beta distribution
a <- 0.09
b <- 1.94
J <- 33

# Generate quantiles of the expansion factor from the simulated data
beta_prods <- create_beta_products(a, b, J, 50000)

expansion_quantiles <- quantile(1 / (1 - exp(-beta_prods)), c(0.025, 0.5, 0.975))
lower <- expansion_quantiles[["2.5%"]]
upper <- expansion_quantiles[["97.5%"]]

lower <- 1.1
upper <- 3

# Recover parameters from original beta distributions
recovered_detailed <- beta_params_from_expansion_factor_quantiles(0.025, 0.975, lower, upper, J, detailed=TRUE)
recovered <- recovered_detailed$recovered
gamma_params <- recovered_detailed$gamma_params
sprintf("original beta params: (%f, %f)", a, b)
sprintf("recovered beta params: (%f, %f)", recovered$b, recovered$a)

# Compare expansion factors
beta_prods_2 <- create_beta_products(recovered$a, recovered$b, J, 50000)
expansion_quantiles_recovered <- quantile(1 / (1 - exp(-beta_prods_2)), c(0.025, 0.5, 0.975))
lower_recovered <- expansion_quantiles_recovered[["2.5%"]]
upper_recovered <- expansion_quantiles_recovered[["97.5%"]]

gamma_sim <- rgamma(50000, gamma_params[["shape"]], gamma_params[["rate"]])

#wasserstein1d(beta_prods_2, gamma_sim)
#hist(rbeta(50000, recovered$b, recovered$a))
#hist(1/(1-exp(-beta_prods_2)))
#plot(density(1/(1-exp(-beta_prods_2))))
#ggplot(data.frame(expansion.factor = 1/(1-exp(-beta_prods_2)))) +
#  geom_density(aes(x=expansion.factor)) +
#  coord_cartesian(xlim=c(0, 10)) +
#  geom_vline(xintercept=1.1, linetype="dashed") +
#  geom_vline(xintercept=3, linetype="dashed")

sprintf("original percentiles of expansion factor (2.5th, 50th, 97.5th): (%f, %f, %f)", lower, expansion_quantiles[["50%"]], upper)
sprintf("recovered percentiles of expansion factor (2.5th, 50th, 97.5th): (%f, %f, %f)", lower_recovered, expansion_quantiles_recovered[["50%"]], upper_recovered)

### Construction of table

uppers <- c(2, 3, 5, 10, 20)
Js <- seq(3, 40)
lower <- 1.1

results <- data.frame(lower=numeric(), upper=numeric(), J=numeric(),
                      ahat=numeric(), bhat=numeric(), recov_025=numeric(),
                      recov_500=numeric(), recov_975=numeric(), wasserstein=numeric())

for (up in uppers) {
  for (J in Js) {
    recovered_detailed <- beta_params_from_expansion_factor_quantiles(0.025, 0.975, lower, up, J, detailed=TRUE)
    
    gamma_params <- recovered_detailed$gamma_params
    gamma_simulated <- rgamma(50000, gamma_params[["shape"]], gamma_params[["rate"]])
    
    recovered <- recovered_detailed$recovered
    beta_prods_2 <- create_beta_products(recovered$a, recovered$b, J, 50000)
    expansion_quantiles_recovered <- quantile(1 / (1 - exp(-beta_prods_2)), c(0.025, 0.5, 0.975))
    lower_recovered <- expansion_quantiles_recovered[["2.5%"]]
    median_recovered <- expansion_quantiles_recovered[["50%"]]
    upper_recovered <- expansion_quantiles_recovered[["97.5%"]]
    
    w <- wasserstein1d(gamma_simulated, beta_prods_2)
    
    results <- bind_rows(results, tibble(lower=lower, upper=up, J=J,
                                         ahat=recovered$a, bhat=recovered$b,
                                         recov_025=lower_recovered,
                                         recov_500=median_recovered,
                                         recov_975=upper_recovered,
                                         wasserstein=w))
  }
}

write.csv(results, here("write", "output", "quantiles_vs_recovered.csv"), row.names = FALSE)
