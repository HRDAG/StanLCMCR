//
// Authors:     SZ,SM
// Maintainers: SZ
// Copyright:   2022, HRDAG, GPL v2 or later
// =========================================
//
// Fixing alpha in the data block

data {
  int<lower=1> J; // number of lists
  int<lower=1> C; // number of observed cells in the dataset, up to 2^J-1
  int list_indicators[C, J]; // indicators of being in lists
  vector<lower=0>[C] cell_count; // cell count for each capture pattern
  int<lower=1> K; // number of latent classes
  real<lower=0> alpha; // fixed alpha
}


transformed data {
  real<lower=0> observed = sum(cell_count);
  int zeros[J] = rep_array(0,J);
}


parameters {
  matrix<lower=0,upper=1>[J, K] lambda; // list inclusion probabilities for each latent class
  vector<lower=0,upper=1>[K-1] breaks; // break proportions for stick-breaking prior on pi

  real<lower=observed> N;
}


transformed parameters {
  matrix<lower=0,upper=1>[K, J] lambda_T; // list inclusion probabilities for each latent class
  vector[C] log_cell_probability; // log cell probability for each observed capture pattern
  real log_unobserved_cell_probability;
  // https://mc-stan.org/docs/2_26/stan-users-guide/arithmetic-precision.html#underflow-and-the-log-scale
  vector<lower=0,upper=1>[K] pi;
  vector<upper=0>[K] log_pi; 
  vector[K] lps_unobserved;
  
  log_pi[1] = log(breaks[1]); 
  {
    for (k in 2:(K-1)) {
      log_pi[k] = log(breaks[k]) + log1m(breaks[k-1]) - log(breaks[k-1]) + log_pi[k-1];
    } 
    log_pi[K] = log1m(breaks[K-1]) - log(breaks[K-1]) + log_pi[K - 1];
  }
  
  // reorder latent classes by pi
  for (i in 1:K) {
    lambda_T[i] = col(lambda, sort_indices_desc(log_pi)[i])';
  }
  
  log_pi = log_pi[sort_indices_desc(log_pi)];
  pi = exp(log_pi);
  
  // continue computation
  lps_unobserved = log_pi;
  for (c in 1:C) {
    vector[K] lps = log_pi;
    for (k in 1:K) {
      lps[k] += bernoulli_lpmf(list_indicators[c] | lambda_T[k]); // https://mc-stan.org/docs/2_26/functions-reference/vectorization.html#evaluating-vectorized-log-probability-functions
    }
    log_cell_probability[c] = log_sum_exp(lps);
  }
  for (k in 1:K) {
    lps_unobserved[k] += bernoulli_lpmf(zeros | lambda_T[k]);
  }
  log_unobserved_cell_probability = log_sum_exp(lps_unobserved);
}

model {
  target += lchoose(N, observed) + (N - observed)*log_unobserved_cell_probability + cell_count' * log_cell_probability;
  target += -log(N);
  
  breaks ~ beta(1, alpha);
}
