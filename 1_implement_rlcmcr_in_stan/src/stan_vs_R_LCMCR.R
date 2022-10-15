#
# Authors:     SZ
# Maintainers: SZ
# Copyright:   2022, HRDAG, GPL v2 or later
# =========================================
# CO-MSE/stan-R-compare/src/diagnose-differences-between-stan-and-R.R

# ----- setup

set.seed(19481210)

pacman::p_load(argparse, here, arrow, dplyr, tidyr, cmdstanr, rstan, posterior, LCMCR, ggplot2, broom, patchwork, scales, purrr)

data("kosovo_aggregate")
kosovo <- as_tibble(sapply(kosovo_aggregate, as.numeric) - 1) 

# ----- main
# Initialize stan models
stan_model <- cmdstan_model("./LCMCR.stan", pedantic = TRUE)
stan_model2 <- cmdstan_model("./LCMCR_2.stan", pedantic = TRUE)
stan_model3 <- cmdstan_model("./LCMCR_3.stan", pedantic = TRUE)
stan_model4 <- cmdstan_model("./LCMCR_4.stan", pedantic = TRUE)
stan_model5 <- cmdstan_model("./LCMCR_5_fixed_alpha.stan", pedantic = TRUE)

fit_stan <- function(model, data, K=10, num.iter=2000, seed=19481210, chains=4, warmup=2000, adapt.delta=0.8, alpha=NULL) {
  data.factor <- data.frame(lapply(data, factor))
  
  stan_data_tabular <- data %>% 
    group_by_all() %>%
    summarize(cell_count = n())
  
  stan_data <- stan_data_tabular %>%
    select(-cell_count) %>%
    as.matrix()
  
  stan_data_list <- list(J = ncol(stan_data),
                         C = nrow(stan_data_tabular),
                         list_indicators = stan_data,
                         cell_count = stan_data_tabular$cell_count,
                         list_count = sapply(data, sum),
                         K = K,
                         alpha = alpha 
                         ) %>%
    compact()
  
  fit <- model$sample(data = stan_data_list,
                                seed = 19481210,
                                chains = chains,
                                parallel_chains = chains,
                                iter_warmup = warmup,
                                iter_sampling = num.iter,
                                adapt_delta = adapt.delta)

  fit
}

# Function for comparing across the different models
run_panel <- function(data, K=10, num.iter=2000, seed=19481210, stan.adapt.delta=0.8, stan.warmup=2000, sweep.alpha=FALSE) {
  R.iter <- num.iter
  stan.iter <- num.iter
  stan.chains <- 4
  
  data.factor <- data.frame(lapply(data, factor))
  
  if (sweep.alpha) {
    stan_fit1 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup, alpha=0.1)
    stan_fit2 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup, alpha=0.5)
    stan_fit3 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup, alpha=1)
    stan_fit4 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup, alpha=2)
    stan_fit5 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup, alpha=10)
  }
  else{
    stan_fit1 <- fit_stan(stan_model, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup)
    stan_fit2 <- fit_stan(stan_model2, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup)
    stan_fit3 <- fit_stan(stan_model3, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup)
    stan_fit4 <- fit_stan(stan_model4, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup)
    stan_fit5 <- fit_stan(stan_model5, data, K=K, num.iter=num.iter, seed=seed, chains=stan.chains, adapt.delta = stan.adapt.delta, warmup = stan.warmup)
  }
  
  
  stan_estimates_df <- as.data.frame(stan_fit1$draws("N"))
  stan_estimates1 <- apply(stan_estimates_df, 1, base::sample, size = 1)
  num.divergent1 <- sum(stan_fit1$diagnostic_summary()$num_divergent)
  prop.divergent1 <- num.divergent1 / (stan.chains * stan.iter)
  
  stan_estimates2_df <- as.data.frame(stan_fit2$draws("N"))
  stan_estimates2 <- apply(stan_estimates2_df, 1, base::sample, size = 1)
  num.divergent2 <- sum(stan_fit2$diagnostic_summary()$num_divergent)
  prop.divergent2 <- num.divergent2 / (stan.chains * stan.iter)
  
  stan_estimates3_df <- as.data.frame(stan_fit3$draws("N"))
  stan_estimates3 <- apply(stan_estimates3_df, 1, base::sample, size = 1)
  num.divergent3 <- sum(stan_fit3$diagnostic_summary()$num_divergent)
  prop.divergent3 <- num.divergent3 / (stan.chains * stan.iter)
  
  stan_estimates4_df <- as.data.frame(stan_fit4$draws("N"))
  stan_estimates4 <- apply(stan_estimates4_df, 1, base::sample, size = 1)
  num.divergent4 <- sum(stan_fit4$diagnostic_summary()$num_divergent)
  prop.divergent4 <- num.divergent4 / (stan.chains * stan.iter)
  
  stan_estimates5_df <- as.data.frame(stan_fit5$draws("N"))
  stan_estimates5 <- apply(stan_estimates5_df, 1, base::sample, size = 1)
  num.divergent5 <- sum(stan_fit5$diagnostic_summary()$num_divergent)
  prop.divergent5 <- num.divergent5 / (stan.chains * stan.iter)
  
  R_sampler <- lcmCR(data.factor, K=K, a_alpha=0.25, b_alpha=0.25, seed=seed, buffer_size = 10000, thinning = 100, in_list_label="1", not_in_list_label="0")
  R_sampler$Activate_Tracing()
  R_sampler$Set_Trace(c(R_sampler$Get_Param_List()))
  R_estimates <- lcmCR_PostSampl(R_sampler, burnin = 10000, samples = R.iter, thinning = 100, output = FALSE)
  
  divergences <- bind_rows(
    tibble(num.divergent = num.divergent1, prop.divergent = prop.divergent1, model = "Stan1"),
    tibble(num.divergent = num.divergent2, prop.divergent = prop.divergent2, model = "Stan2"),
    tibble(num.divergent = num.divergent3, prop.divergent = prop.divergent3, model = "Stan3"),
    tibble(num.divergent = num.divergent4, prop.divergent = prop.divergent4, model = "Stan4"),
    tibble(num.divergent = num.divergent5, prop.divergent = prop.divergent5, model = "Stan5")
  )
  
  estimates <- bind_rows(
    tibble(estimates = stan_estimates1, model = "Stan1"),
    tibble(estimates = stan_estimates2, model = "Stan2"),
    tibble(estimates = stan_estimates3, model = "Stan3"),
    tibble(estimates = stan_estimates4, model = "Stan4"),
    tibble(estimates = stan_estimates5, model = "Stan5"),
    tibble(estimates = R_estimates, model ="R")
  ) 

  summaries <- estimates %>% group_by(model) %>%
    summarize(
      q025 = quantile(estimates, 0.025),
      q500 = quantile(estimates, 0.5),
      q975 = quantile(estimates, 0.975),
      ci_95_length = q975 - q025,
      mean = mean(estimates),
      sd = sd(estimates),
    ) %>% merge(divergences, by="model")
  
  ks_R_vs_stan1 <- ks.test( (estimates %>% filter(model == "R"))$estimates, (estimates %>% filter(model == "Stan1"))$estimates )
  ks_R_vs_stan2 <- ks.test( (estimates %>% filter(model == "R"))$estimates, (estimates %>% filter(model == "Stan2"))$estimates )
    
  list(estimates=estimates,
       summaries=summaries,
       stan_fit1=stan_fit1,
       stan_fit2=stan_fit2,
       stan_fit3=stan_fit3,
       stan_fit4=stan_fit4,
       stan_fit5=stan_fit5,
       R_sampler=R_sampler,
       ks_R_vs_stan1=ks_R_vs_stan1,
       ks_R_vs_stan2=ks_R_vs_stan2
     )
}

########################################
# Simulated datasets
########################################

simulate_mse_dataset <- function(N, list_capture_probs) {
  membership_in_lists <- data.frame(list(V1=as.numeric(runif(N) < list_capture_probs[1]) ))
  J <- length(list_capture_probs)
  
  for (j in 2:J) {
    membership_in_lists[,j] = as.numeric(runif(N) < list_capture_probs[j])
  }
  
  membership_in_lists
}


# Simulated example from Manrique-Vallier (2016)
data.sim.hetero1 <- rbind(
  simulate_mse_dataset(2000 * 0.9, c(0.033, 0.033, 0.099, 0.132, 0.033)),
  simulate_mse_dataset(2000 * 0.1, c(0.660, 0.825, 0.759, 0.990, 0.693))
) %>% filter(rowSums(across(everything())) > 0)

# More simulated examples (tweak on Manrique-Vallier (2016))
data.sim.hetero2 <- rbind(
  simulate_mse_dataset(2000 * 0.8, c(0.013, 0.013, 0.099, 0.132, 0.033)),
  simulate_mse_dataset(2000 * 0.2, c(0.560, 0.825, 0.759, 0.990, 0.593))
) %>% filter(rowSums(across(everything())) > 0)

# More simulated examples (tweak on Manrique-Vallier (2016))
data.sim.hetero3 <- rbind(
  simulate_mse_dataset(2000 * 0.8, c(0.113, 0.113, 0.199, 0.232, 0.133)),
  simulate_mse_dataset(2000 * 0.2, c(0.860, 0.925, 0.859, 0.990, 0.793))
) %>% filter(rowSums(across(everything())) > 0)

# More simulated examples (3 strata)
data.sim.hetero4 <- rbind(
  simulate_mse_dataset(2000 * 0.8, c(0.1, 0.05, 0.15, 0.2, 0.1)),
  simulate_mse_dataset(2000 * 0.1, c(0.5, 0.4, 0.6, 0.4, 0.5)),
  simulate_mse_dataset(2000 * 0.1, c(0.8, 0.9, 0.6, 0.7, 0.7))
) %>% filter(rowSums(across(everything())) > 0)

# More simulated examples (negative list dependencies across strata)
data.sim.hetero5 <- rbind(
  simulate_mse_dataset(2000 * 0.8, c(0.113, 0.113, 0.199, 0.832, 0.733)),
  simulate_mse_dataset(2000 * 0.2, c(0.860, 0.925, 0.859, 0.190, 0.193))
) %>% filter(rowSums(across(everything())) > 0)

results.hetero1 <- run_panel(data.sim.hetero1)
results.hetero2 <- run_panel(data.sim.hetero2)
results.hetero3 <- run_panel(data.sim.hetero3)
results.hetero4 <- run_panel(data.sim.hetero4)
results.hetero5 <- run_panel(data.sim.hetero5)
results.kosovo <- run_panel(kosovo)

results.hetero1.high.adapt <- run_panel(data.sim.hetero1, stan.adapt.delta=0.99)
results.hetero2.high.adapt <- run_panel(data.sim.hetero2, stan.adapt.delta=0.99)
results.hetero3.high.adapt <- run_panel(data.sim.hetero3, stan.adapt.delta=0.99)
results.hetero4.high.adapt <- run_panel(data.sim.hetero4, stan.adapt.delta=0.99)
results.hetero5.high.adapt <- run_panel(data.sim.hetero5, stan.adapt.delta=0.99)

results.hetero1.alpha.sweep <- run_panel(data.sim.hetero1, sweep.alpha = TRUE)

(results.hetero1.alpha.sweep$summaries %>% 
  mutate(Dataset="Sim 1") %>%
  mutate(model_ix = recode_factor(model, Stan1="0.1", Stan2="0.5", Stan3="1", Stan4="2", Stan5="10")) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent), size=2) +
    labs(x="alpha", y="% divergent") +
    scale_y_continuous(labels = scales::percent)) /
(results.hetero1.alpha.sweep$summaries %>% 
  mutate(alpha = recode_factor(model, Stan1="0.1", Stan2="0.5", Stan3="1", Stan4="2", Stan5="10")) %>%
  ggplot() +
  geom_point(aes(x=alpha, y=q500)) +
  geom_errorbar(aes(x=alpha, ymin=q025, ymax=q975), width=0.5) +
  geom_hline(yintercept=2000, linetype="dotted") +
  labs(x="alpha", y="Estimates with medians and 95% CIs"))

ggsave("../output/divergences-alpha-sweep-sim1.png", width=4, height=6)

rbind(
  results.hetero4.high.adapt$summaries %>% mutate(Dataset="Sim 4"),
  results.hetero5.high.adapt$summaries %>% mutate(Dataset="Sim 5")
) %>%
  mutate(model_ix = as.numeric(substr(model, 5, 5))) %>%
  filter(model_ix <= 4) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent, color=Dataset), size=4) +
    geom_line(aes(x=model_ix, y=prop.divergent, color=Dataset), linetype="dashed") +
    labs(x="Model", y="% divergent") +
    scale_y_continuous(labels = scales::percent)
    
rbind(
  results.hetero1$summaries %>% mutate(Dataset="Sim 1"),
  results.hetero2$summaries %>% mutate(Dataset="Sim 2"),
  results.hetero3$summaries %>% mutate(Dataset="Sim 3"),
  results.hetero4$summaries %>% mutate(Dataset="Sim 4"),
  results.hetero5$summaries %>% mutate(Dataset="Sim 5")
) %>%
  mutate(model_ix = as.numeric(substr(model, 5, 5))) %>%
  filter(model_ix <= 4) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent, color=Dataset), size=2) +
    geom_line(aes(x=model_ix, y=prop.divergent, color=Dataset), linetype="dashed") +
  labs(x="Model", y="% divergent") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent)

    
ggsave("../output/divergences-by-model.png", width=5, height=5)
    
rbind(
  results.hetero1.high.adapt$summaries %>% mutate(Dataset="Sim 1"),
  results.hetero2.high.adapt$summaries %>% mutate(Dataset="Sim 2"),
  results.hetero3.high.adapt$summaries %>% mutate(Dataset="Sim 3"),
  results.hetero4.high.adapt$summaries %>% mutate(Dataset="Sim 4"),
  results.hetero5.high.adapt$summaries %>% mutate(Dataset="Sim 5")
) %>%
  mutate(model_ix = as.numeric(substr(model, 5, 5))) %>%
  filter(model_ix <= 4) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent, color=Dataset), size=2) +
    geom_line(aes(x=model_ix, y=prop.divergent, color=Dataset), linetype="dashed") +
  labs(x="Model", y="% divergent") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent)


rbind(
results.hetero1$estimates %>% group_by(model) %>% summarize(q025 = quantile(estimates, 0.025), q500 = quantile(estimates, 0.5), q975 = quantile(estimates, 0.975), ci_95_length = q975 - q025, mean = mean(estimates), sd = sd(estimates),
    Dataset = "Sim 1"), 
results.hetero2$estimates %>% group_by(model) %>% summarize(q025 = quantile(estimates, 0.025), q500 = quantile(estimates, 0.5), q975 = quantile(estimates, 0.975), ci_95_length = q975 - q025, mean = mean(estimates), sd = sd(estimates),
    Dataset = "Sim 2"), 
results.hetero3$estimates %>% group_by(model) %>% summarize(q025 = quantile(estimates, 0.025), q500 = quantile(estimates, 0.5), q975 = quantile(estimates, 0.975), ci_95_length = q975 - q025, mean = mean(estimates), sd = sd(estimates),
    Dataset = "Sim 3"), 
results.hetero4$estimates %>% group_by(model) %>% summarize(q025 = quantile(estimates, 0.025), q500 = quantile(estimates, 0.5), q975 = quantile(estimates, 0.975), ci_95_length = q975 - q025, mean = mean(estimates), sd = sd(estimates),
    Dataset = "Sim 4"), 
results.hetero5$estimates %>% group_by(model) %>% summarize(q025 = quantile(estimates, 0.025), q500 = quantile(estimates, 0.5), q975 = quantile(estimates, 0.975), ci_95_length = q975 - q025, mean = mean(estimates), sd = sd(estimates),
    Dataset = "Sim 5")
) %>% 
  filter(model %in% c("R", "Stan1", "Stan4")) %>%
  mutate(model = recode_factor(model, R="R", Stan1 = "Stan (v1)", Stan4 = "Stan (v4)")) %>%
  ggplot() +
  geom_point(aes(x=Dataset, y=q500, group=model, color=model), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x=Dataset, ymin=q025, ymax=q975, group=model, color=model), width=0.5, position="dodge") +
  geom_hline(yintercept=2000, linetype="dotted") +
  labs(y="Estimates with medians and 95% CIs") +
  theme(legend.position="bottom")

ggsave("../output/R_vs_stan_vs_truth.png", width=5, height=4)

results.combined <- rbind(
  results.hetero1$estimates %>% mutate(Dataset = "Simulated 1"),
  results.hetero2$estimates %>% mutate(Dataset = "Simulated 2"),
  results.hetero3$estimates %>% mutate(Dataset = "Simulated 3"),
  results.hetero4$estimates %>% mutate(Dataset = "Simulated 4"),
  results.hetero5$estimates %>% mutate(Dataset = "Simulated 5")
)

ks.combined <- rbind(
  tidy(results.hetero1$ks_R_vs_stan2),
  tidy(results.hetero2$ks_R_vs_stan2),
  tidy(results.hetero3$ks_R_vs_stan2),
  tidy(results.hetero4$ks_R_vs_stan2),
  tidy(results.hetero5$ks_R_vs_stan2)
)

results.combined %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    xlab("Estimated population size") +
    ylab("Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green", "cyan")) +
    geom_vline(xintercept=2000, linetype="dashed") +
    facet_wrap(~ Dataset, scales = "free", ncol=1) +
    theme(legend.position = "top")
  

results.combined %>%
  filter(model %in% c("R", "Stan1", "Stan4")) %>%
  mutate(model = recode_factor(model, R="R", Stan1 = "Stan (v1)", Stan4 = "Stan (v4)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    xlab("Estimated population size") +
    ylab("Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
    geom_vline(xintercept=2000, linetype="dashed") +
    facet_wrap(~ Dataset, scales = "free", ncol=1) +
    theme(legend.position = "top")
  
ggsave("../output/simulated_diagnostics.png", height=7, width=4)

results.kosovo$summaries
results.kosovo$estimates %>% filter(model == "R") %>% summarize(mean=mean(estimates), median=median(estimates))

results.kosovo$estimates %>%
  filter(model %in% c("R", "Stan1", "Stan4")) %>%
  mutate(model = recode_factor(model, R="R", Stan1 = "Stan (v1)", Stan4 = "Stan (v4)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    labs(x="Estimated population size", y="Density", title="Kosovo") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    geom_vline(xintercept=10401, linetype="dashed") +
    theme(legend.position = "top") 

ggsave("../output/kosovo.png", height=4, width=4)
    
ks.kosovo <- ks.test( (results.kosovo$estimates %>% filter(model == "R"))$estimates, (results.kosovo$estimates %>% filter(model == "Stan4"))$estimates )
ks.test( (results.hetero1$estimates %>% filter(model == "R"))$estimates, (results.hetero1$estimates %>% filter(model == "Stan3"))$estimates )
ks.test( (results.hetero2$estimates %>% filter(model == "R"))$estimates, (results.hetero2$estimates %>% filter(model == "Stan3"))$estimates )
ks.test( (results.hetero3$estimates %>% filter(model == "R"))$estimates, (results.hetero3$estimates %>% filter(model == "Stan3"))$estimates )
ks.test( (results.hetero4$estimates %>% filter(model == "R"))$estimates, (results.hetero4$estimates %>% filter(model == "Stan3"))$estimates )
ks.test( (results.hetero5$estimates %>% filter(model == "R"))$estimates, (results.hetero5$estimates %>% filter(model == "Stan3"))$estimates )

results.kosovo$estimates %>%
  filter(model %in% c("R", "Stan3")) %>%
  mutate(dataset = "Kosovo", model = recode(model, Stan3 = "Stan")) %>%
ggplot() +
  geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
  theme_minimal() +
  labs(x="Estimated population size", y="Density", title="Kosovo") +
  scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
  geom_vline(xintercept=10401, linetype="dashed") +
  theme(legend.position = "top") +
  geom_text(data=data.frame(
    label=paste(
      paste("D=", ks.kosovo$statistic, sep=""),
      paste("p=", signif(ks.kosovo$p.value, 2), sep=""),
      sep=", "
    ),
    x=20000,
    y=3.75e-04
  ), mapping=aes(x=x, y=y, label=label), size=4)
  
ggsave("../output/kosovo.png", height=4, width=4)
  
### Other diagnostics

results.hetero1$stan_fit1$cmdstan_diagnose()
results.hetero1$stan_fit2$cmdstan_diagnose()
results.hetero1$stan_fit3$cmdstan_diagnose()
results.hetero1$stan_fit4$cmdstan_diagnose()
results.hetero1$stan_fit5$cmdstan_diagnose()

results.hetero2$stan_fit1$cmdstan_diagnose()
results.hetero2$stan_fit2$cmdstan_diagnose()

# Trace plot for LCMCR
plot.diagnostics.R <- function(results, varname, max_ix=4) {
  results.matrix <- results$R_sampler$Get_Trace(varname)
  varnames <- c()
  
  for (i in 1:dim(results.matrix)[2]) {
    varnames <- c(varnames, paste(varname, i, sep=""))
  }
  results.df <- results.matrix %>%
    as_tibble(.name_repair="unique") %>%
    setNames(varnames)
  
  results.df[varnames[1:max_ix]] %>%
    mutate(iteration=row_number())  %>%
    pivot_longer(c(!!varnames[1:max_ix])) %>%
    ggplot() +
      geom_point(aes(x=iteration,y=value, alpha=0.1, size=0.3)) +
      facet_wrap(~ name, ncol = 1)
}

# Trace plot for Stan
plot.chain.diagnostics.grid <- function(results, stan_fit, varnames, title="") {
  data <- as.data.frame.table(results[[stan_fit]]$draws()) %>%
    filter(variable %in% c(varnames) & chain %in% c(1, 2, 3, 4)) %>%
    mutate(iteration = as.numeric(iteration),
           Freq = as.numeric(Freq),
           chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"))
  
  ggplot(data) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    facet_grid(variable ~ chain) +
    labs(x="Iteration", y="Value", title=title)
}

plot.divergent.transitions <- function(stan_fit, chain, varname) {
  values = stan_fit$draws()[,chain,varname]
  div.df <- data.frame(divergent=as.vector(stan_fit$sampler_diagnostics()[,chain,"divergent__"])) %>%
    mutate("{varname}":=as.vector(stan_fit$draws()[,chain,varname]),
           iteration=row_number(),
           divergent=recode_factor(as.factor(divergent), `0`="No", `1`="Yes"))
  
  plot.dens <- ggplot(div.df) +
      geom_density(aes(x=!!sym(varname), color=divergent, group=divergent, linetype=divergent), alpha=0.5) +
      theme(legend.position="bottom")
  
  plot.trace <- ggplot(div.df) +
      geom_point(aes(x=iteration, y=!!sym(varname), color=divergent, shape=divergent), alpha=0.5) +
      theme(legend.position="bottom")
  
  plot.dens / plot.trace
}

plot.divergent.transitions.pair <- function(stan_fit, chain_num, var1, var2) {
  divergences <- as.vector(stan_fit$sampler_diagnostics()[,chain_num,"divergent__"])
  
  as.data.frame.table(stan_fit$draws()) %>%
    filter(chain == chain_num & variable %in% c(var1, var2)) %>%
    pivot_wider(names_from = c(variable), values_from=c(Freq)) %>%
  mutate(divergent = as.factor(divergences),
         iteration = row_number()) %>%
  ggplot() +
    geom_point(aes(x=!!sym(var1), y=!!sym(var2), color=divergent), alpha=0.5)
}

pis = c("pi[1]", "pi[2]", "pi[3]", "pi[4]") # Probability of belonging to the first four latent classes
lambda_r1s = c("lambda_ordered_first_row[1]", "lambda_ordered_first_row[2]", "lambda_ordered_first_row[3]", "lambda_ordered_first_row[4]") # Probability of belonging to the first four latent classes
lambda_ones = c("lambda[1,1]", "lambda[2,1]", "lambda[3,1]", "lambda[4,1]") # Probability of being on the first list across the first four latent classes
lambda_T_ones = c("lambda_T[1,1]", "lambda_T[2,1]", "lambda_T[3,1]", "lambda_T[4,1]") # Probability of being on the first list across the first four latent classes
breaks = c("breaks[1]", "breaks[2]", "breaks[3]", "breaks[4]") # Probability of being on the first list across the first four latent classes

plot.chain.diagnostics.grid(results.hetero1, "stan_fit1", pis, title="Simulated dataset 1; Original Stan")
ggsave("../output/label-switching-stan1.png", height=6, width=7)
plot.chain.diagnostics.grid(results.hetero1, "stan_fit2", pis, title="Simulated Dataset 1; Ordered Stan")
ggsave("../output/label-switching-stan2.png", height=6, width=7)

# Prior vs. posterior of alpha
as.data.frame(results.hetero1$stan_fit4$draws(c("alpha"))) %>%
  pivot_longer(cols=c("1.alpha", "2.alpha", "3.alpha", "4.alpha")) %>%
  ggplot() +
    geom_density(aes(x=value, color="posterior", linetype="posterior")) +
    geom_density(data=data.frame(x=exp(rnorm(5000, mean=0, sd=0.25))), aes(x=x, color="prior", linetype="prior")) +
    scale_color_manual(values=c("prior"="red", "posterior"="blue"), name="") +
    scale_linetype_manual(values=c("prior"="solid", "posterior"="dashed"), name="") +
    labs(x="alpha") +
    theme(legend.position = "bottom")

ggsave("../output/alpha_prior_posterior.png", width=4, height=4)

  
# Diagnostics for divergences
plot.chain.diagnostics.grid(results.hetero1, "stan_fit1", lambda_ones, title="Simulated dataset 1; Original Stan")
plot.chain.diagnostics.grid(results.hetero1, "stan_fit2", lambda_T_ones, title="Simulated Dataset 1; Ordered Stan")
plot.chain.diagnostics.grid(results.hetero1, "stan_fit1", breaks, title="Simulated dataset 1; Original Stan")
plot.chain.diagnostics.grid(results.hetero1, "stan_fit2", breaks, title="Simulated dataset 1; Ordered Stan")
plot.chain.diagnostics.grid(results.hetero1, "stan_fit1", c("alpha"), title="Simulated dataset 1; Original Stan")
plot.chain.diagnostics.grid(results.hetero1, "stan_fit2", c("alpha"), title="Simulated dataset 1; Ordered Stan")

plot.chain.diagnostics.grid(results.hetero1, "stan_fit4", c("alpha", "breaks[1]", "breaks[2]", "pi[1]", "pi[2]"), title="Simulated dataset 1; Log-normal prior")
plot.divergent.transitions(results.hetero1$stan_fit4, 4, "pi[2]")
results.hetero1.alpha.sweep$stan_fit4$cmdstan_diagnose()

plot.divergent.transitions(results.hetero1$stan_fit2, 3, "alpha")
ggsave("../output/divergent-alpha-stan2.png", height=5, width=4)

View(results.hetero1$stan_fit3$summary())
plot.divergent.transitions(results.hetero1$stan_fit3, 3, "alpha")
ggsave("../output/divergent-alpha-stan3.png", height=5, width=4)

plot.divergent.transitions(results.hetero1$stan_fit4, 3, "lambda[1,2]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "breaks[1]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "breaks[2]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "breaks[3]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "pi[1]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "pi[3]")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "alpha")
plot.divergent.transitions(results.hetero1$stan_fit4, 3, "pi[7]")

plot.divergent.transitions.pair(results.hetero1.alpha.sweep$stan_fit1, 3, "lambda_T[2,1]", "lambda_T[3,1]")
plot.divergent.transitions.pair(results.hetero1.alpha.sweep$stan_fit1, 3, "breaks[2]", "pi[2]")
plot.divergent.transitions(results.hetero1.alpha.sweep$stan_fit1, 3, "pi[2]")
plot.divergent.transitions.pair(results.hetero1$stan_fit4, 3, "alpha", "breaks[3]")

plot.chain.diagnostics.grid(results.hetero2, "stan_fit1", pis)
plot.chain.diagnostics.grid(results.hetero2, "stan_fit2", pis)
plot.chain.diagnostics.grid(results.hetero2, "stan_fit1", lambda_ones)
plot.chain.diagnostics.grid(results.hetero2, "stan_fit2", lambda_T_ones)
plot.chain.diagnostics.grid(results.hetero2, "stan_fit1", breaks, title="Simulated Dataset 2; Original Stan")
plot.chain.diagnostics.grid(results.hetero2, "stan_fit2", breaks, title="Simulated Dataset 2; Ordered Stan")

plot.divergent.transitions(results.hetero2$stan_fit2, 2, "breaks[2]")
plot.divergent.transitions(results.hetero2$stan_fit2, 4, "breaks[2]")

plot.divergent.transitions(results.hetero2$stan_fit2, 3, "alpha")
plot.divergent.transitions(results.hetero2$stan_fit2, 3, "lambda[1,2]")
plot.divergent.transitions(results.hetero2$stan_fit2, 3, "breaks[1]")
plot.divergent.transitions(results.hetero2$stan_fit2, 3, "breaks[3]")
plot.divergent.transitions(results.hetero2$stan_fit2, 3, "pi[3]")

#> R_sampler$Get_Param_List()
#[1] "J"             "K"             "M"             "a_alpha"       "alpha"        
#[6] "aux_JK2"       "b_alpha"       "count0K"       "countK"        "count_zIK"    
#[11] "k_star"        "log_lambdaJK2" "log_nuK"       "n"             "n0"           
#[16] "nuK"           "prob_zero"   

plot(results.hetero1$R_sampler$Get_Trace("alpha"))
plot.diagnostics.R(results.hetero1, "nuK")
plot.diagnostics.R(results.hetero2, "nuK")

