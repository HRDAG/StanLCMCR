//
// Authors:     SM
// Maintainers: MG
// Copyright:   2021, HRDAG, GPL v2 or later
// =========================================
// LCMCR.stan

data {
  int<lower=1> J; // number of lists
  int<lower=1> C; // number of observed cells in the dataset, up to 2^J-1
  int list_indicators[C, J]; // indicators of being in lists
  vector<lower=0>[C] cell_count; // cell count for each capture pattern
  int<lower=1> K; // number of latent classes
}


transformed data {
  real<lower=0> observed = sum(cell_count);
  int zeros[J] = rep_array(0,J);
}


parameters {
  vector<lower=0,upper=1>[J] lambda[K]; // list inclusion probabilities for each latent class
  vector<lower=0,upper=1>[K - 1] breaks; // break proportions for stick-breaking prior on pi
  real<lower=observed> N;
  real<lower=0> alpha; // stick-breaking prior parameter
}


transformed parameters {
  simplex[K] pi; // latent class mixing proportions
  vector[C] log_cell_probability; // log cell probability for each observed capture pattern
  real log_unobserved_cell_probability;
  // https://mc-stan.org/docs/2_26/stan-users-guide/arithmetic-precision.html#underflow-and-the-log-scale
  vector[K] log_pi;  // cache log calculation
  vector[K] lps_unobserved;
  { // https://discourse.mc-stan.org/t/better-way-of-modelling-stick-breaking-process/2530/2
  // see BDA3 p.548
    real sum_sticks_so_far = 0;
    pi[1] = breaks[1];
    sum_sticks_so_far = pi[1];
    for (k in 2:(K - 1)) {
      pi[k] = (1 - sum_sticks_so_far) * breaks[k];
      sum_sticks_so_far += pi[k];
    }
    pi[K] = 1 - sum_sticks_so_far;
  }
  log_pi = log(pi);  // cache log calculation
  lps_unobserved = log_pi;
  for (c in 1:C) {
    vector[K] lps = log_pi;
    for (k in 1:K) {
      lps[k] += bernoulli_lpmf(list_indicators[c] | lambda[k]); // https://mc-stan.org/docs/2_26/functions-reference/vectorization.html#evaluating-vectorized-log-probability-functions
    }
    log_cell_probability[c] = log_sum_exp(lps);
  }
  for (k in 1:K) {
    lps_unobserved[k] += bernoulli_lpmf(zeros | lambda[k]);
  }
  log_unobserved_cell_probability = log_sum_exp(lps_unobserved);
}


model {
  target += lchoose(N, observed) + (N - observed)*log_unobserved_cell_probability + cell_count' * log_cell_probability;
  target += -log(N);
  breaks ~ beta(1, alpha);
  alpha ~ gamma(0.25,0.25);
}
