library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork)

# cmdstanR needed for trace plots
# also, custom repository
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 

fit_model6_co1 <- readRDS(here("write", "input", "fit", "LCMCR_6_anonymized-colombia-strata-1.rds"))
fit_model7_co1 <- readRDS(here("write", "input", "fit", "LCMCR_7_anonymized-colombia-strata-1.rds"))

fit_model4_co3 <- readRDS(here("write", "input", "fit", "LCMCR_4_anonymized-colombia-strata-3.rds"))
fit_model6_co3 <- readRDS(here("write", "input", "fit", "LCMCR_6_anonymized-colombia-strata-3.rds"))
fit_model7_co3 <- readRDS(here("write", "input", "fit", "LCMCR_7_anonymized-colombia-strata-3.rds"))

summaries <- summaries_all %>% filter(grepl("anonymized-colombia-strata-", Dataset))
estimates <- estimates_all %>% filter(grepl("anonymized-colombia-strata-", Dataset))

# Needed to prevent generating Rplots.pdf in current working directory
pdf(NULL)

###############################################
# Plot divergences by model and dataset
###############################################

summaries %>%
  filter(model != "R") %>%
  mutate(model_ix = as.numeric(substr(model, 7, 7))) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent, color=Dataset), size=2) +
    geom_line(aes(x=model_ix, y=prop.divergent, color=Dataset), linetype="dashed") +
  labs(x="Model", y="% divergent") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent)

ggsave(here("write", "output", "divergences-by-model-co.png"), width=5, height=5)
    

###############################################
# Plot median posterior estimates and 95% CIs 
###############################################

summaries %>%
  mutate(model = recode_factor(model, R="R", LCMCR_4 = "Unif. priors", LCMCR_6 = "Fixed priors", LCMCR_7 = "Fixed (no swap)")) %>%
  ggplot() +
  geom_point(aes(x=Dataset, y=q500, group=model, color=model), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x=Dataset, ymin=q025, ymax=q975, group=model, color=model), width=0.5, position="dodge") +
#  geom_hline(yintercept=2000, linetype="dotted") +
  labs(y="Estimates with medians and 95% CIs") +
  theme(legend.position="bottom")

ggsave(here("write", "output", "posterior-CIs-CO.png"), width=5, height=4)

###############################################
# Plot posterior densities
###############################################

estimates %>%
  mutate(model = recode_factor(model, R="R", LCMCR_6 = "Fixed priors", LCMCR_7 = "Fixed (no swap)")) %>%
  filter(model != "LCMCR_4") %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    xlab("Estimated population size") +
    ylab("Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
    facet_wrap(~ Dataset, scales = "free", ncol=1) +
    theme(legend.position = "top")
  
ggsave(here("write", "output", "posterior-densities-CO.png"), height=7, width=4)

###############################################
# Plot label-switching fix
###############################################

# Trace plot for Stan
plot.chain.diagnostics.grid <- function(stan_fit, varnames, title="") {
  data <- as.data.frame.table(stan_fit$draws()) %>%
    filter(variable %in% c(varnames) & chain %in% c(1, 2, 3, 4)) %>%
    mutate(iteration = as.numeric(iteration),
           Freq = as.numeric(Freq),
           chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"))
  
  ggplot(data) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    facet_grid(variable ~ chain) +
    labs(x="Iteration", y="Value", title=title)
}

pis = c("pi[1]", "pi[2]", "pi[3]", "pi[4]") # Probability of belonging to the first four latent classes
lambdas = c("lambda[1,1]", "lambda[1,2]", "lambda[1,3]", "lambda[1,4]") # Probability of belonging to the first four latent classes
#lambdas = c("lambda[1,1]", "lambda[2,1]", "lambda[3,1]", "lambda[4,1]") # Probability of belonging to the first four latent classes

plot.chain.diagnostics.grid(fit_model6_co1, pis)
ggsave(here("write", "output", "label-switching-stan6-co1.png"), height=6, width=7)

plot.chain.diagnostics.grid(fit_model7_co1, pis)
ggsave(here("write", "output", "label-switching-stan7-co1.png"), height=6, width=7)

plot.chain.diagnostics.grid(fit_model4_co3, pis)
plot.chain.diagnostics.grid(fit_model4_co3, lambdas)
plot.chain.diagnostics.grid(fit_model6_co3, lambdas)
plot.chain.diagnostics.grid(fit_model7_co3, lambdas)

###############################################
# Divergent transitions plot (trace plot and densities)
###############################################

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

plot.divergent.transitions(fit_model7_co1, 2, "pi[1]")
ggsave(here("write", "output", "divergent-pi-stan7-co1.png"), width=4, height=4)

plot.divergent.transitions(fit_model4_co3, 3, "pi[1]")

###############################################
# Colombia-specific density plot
###############################################

estimates %>%
  filter(model %in% c("R", "LCMCR_4", "LCMCR_6")) %>%
  mutate(model = recode_factor(model, R="R", LCMCR_4 = "Stan (uniform)", LCMCR_6 = "Stan (non-uniform)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    labs(x="Estimated population size", y="Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    theme(legend.position = "top") +
    facet_wrap(Dataset ~ ., scales="free", ncol=1)

ggsave(here("write", "output", "colombia-R-vs-stan.png"), height=4, width=4)


estimates %>%
  filter(model %in% c("LCMCR_6", "LCMCR_7")) %>%
  mutate(model = recode_factor(model, LCMCR_6 = "Stan (reordering)", LCMCR_7 = "Stan (no reordering)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    labs(x="Estimated population size", y="Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    theme(legend.position = "top") +
    facet_wrap(Dataset ~ ., scales="free", ncol=1)

ggsave(here("write", "output", "colombia-R-vs-stan-non-uniform.png"), height=4, width=4)