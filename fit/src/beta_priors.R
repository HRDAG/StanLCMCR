library(pacman)

pacman::p_load(
  "here",
  "transport", # for Wasserstein distance
  "distr", # for inverse digamma function, igamma
  "rriskDistributions", # for fitting gamma quantiles, get.gamma.par
  "tidyverse"
)

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
  
  # IMPORTANT:
  # These two parameters are ordered, and the ordering is importance.
  #
  # This is swapped in Stan from this code, so we're going to swap them here!
  #
  # This is hacky, we should figure out exactly why they're swapped and rename some functions
  # so this is better...
  list(b=a, a=b)
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
