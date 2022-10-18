library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork)

summaries_all <- read.csv(here("write", "input", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "estimates.csv")) 

summaries_alpha_sweep <- read.csv(here("write", "input", "summaries_alphas.csv"))

fit_model1_dataset1 <- readRDS(here("write", "input", "LCMCR_sim1.rds"))
fit_model2_dataset1 <- readRDS(here("write", "input", "LCMCR_2_sim1.rds"))
fit_model4_dataset1 <- readRDS(here("write", "input", "LCMCR_4_sim1.rds"))

summaries <- summaries_all %>% filter(Dataset != "kosovo")
estimates <- estimates_all %>% filter(Dataset != "kosovo")

# Needed to prevent generating Rplots.pdf in current working directory
pdf(NULL)


###############################################
# Plot divergences by model and dataset
###############################################

summaries %>%
  filter(model != "R") %>%
  mutate(model = recode_factor(model, LCMCR="LCMCR_1")) %>%
  mutate(model_ix = as.numeric(substr(model, 7, 7))) %>%
  filter(model_ix <= 4) %>%
  ggplot() +
    geom_point(aes(x=model_ix, y=prop.divergent, color=Dataset), size=2) +
    geom_line(aes(x=model_ix, y=prop.divergent, color=Dataset), linetype="dashed") +
  labs(x="Model", y="% divergent") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::percent)

ggsave(here("write", "output", "divergences-by-model.png"), width=5, height=5)
    

###############################################
# Plot median posterior estimates and 95% CIs 
###############################################

summaries %>%
  filter(model %in% c("R", "LCMCR", "LCMCR_4")) %>%
  mutate(model = recode_factor(model, R="R", LCMCR = "Stan (v1)", LCMCR_4 = "Stan (v4)")) %>%
  ggplot() +
  geom_point(aes(x=Dataset, y=q500, group=model, color=model), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x=Dataset, ymin=q025, ymax=q975, group=model, color=model), width=0.5, position="dodge") +
  geom_hline(yintercept=2000, linetype="dotted") +
  labs(y="Estimates with medians and 95% CIs") +
  theme(legend.position="bottom")

ggsave(here("write", "output", "posterior-CIs.png"), width=5, height=4)

###############################################
# Plot posterior densities
###############################################

estimates %>%
  filter(model %in% c("R", "LCMCR", "LCMCR_4")) %>%
  mutate(model = recode_factor(model, R="R", LCMCR = "Stan (v1)", LCMCR_4 = "Stan (v4)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    xlab("Estimated population size") +
    ylab("Density") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
    geom_vline(xintercept=2000, linetype="dashed") +
    facet_wrap(~ Dataset, scales = "free", ncol=1) +
    theme(legend.position = "top")
  
ggsave(here("write", "output", "posterior-densities.png"), height=7, width=4)

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

plot.chain.diagnostics.grid(fit_model1_dataset1, pis)
ggsave(here("write", "output", "label-switching-stan1.png"), height=6, width=7)

plot.chain.diagnostics.grid(fit_model2_dataset1, pis)
ggsave(here("write", "output", "label-switching-stan2.png"), height=6, width=7)


###############################################
# Prior-posterior plot of alpha in 4th model
###############################################

# Prior vs. posterior of alpha
as.data.frame(fit_model4_dataset1$draws(c("alpha"))) %>%
  pivot_longer(cols=c("1.alpha", "2.alpha", "3.alpha", "4.alpha")) %>%
  ggplot() +
    geom_density(aes(x=value, color="posterior", linetype="posterior")) +
    geom_density(data=data.frame(x=exp(rnorm(5000, mean=0, sd=0.25))), aes(x=x, color="prior", linetype="prior")) +
    scale_color_manual(values=c("prior"="red", "posterior"="blue"), name="") +
    scale_linetype_manual(values=c("prior"="solid", "posterior"="dashed"), name="") +
    labs(x="alpha") +
    theme(legend.position = "bottom")

ggsave(here("write", "output", "alpha-prior-posterior.png"), width=4, height=4)


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

plot.divergent.transitions(fit_model2_dataset1, 3, "alpha")
ggsave(here("write", "output", "divergent-alpha-stan2.png"), width=4, height=4)


###############################################
# Kosovo-specific density plot
###############################################

estimates_all %>%
  filter(Dataset == "kosovo") %>%
  filter(model %in% c("R", "LCMCR", "LCMCR_4")) %>%
  mutate(model = recode_factor(model, R="R", LCMCR = "Stan (v1)", LCMCR_4 = "Stan (v4)")) %>%
  ggplot() +
    geom_density(aes(x = estimates, fill = model), alpha = 0.25) +
    theme_minimal() +
    labs(x="Estimated population size", y="Density", title="Kosovo") +
    scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    geom_vline(xintercept=10401, linetype="dashed") +
    theme(legend.position = "top") 

ggsave(here("write", "output", "kosovo.png"), height=4, width=4)


###############################################
# Alpha sweeps
###############################################

(summaries_alpha_sweep %>% 
  mutate(alpha_ordered = as.factor(alpha)) %>% #recode_factor(alpha, 0.1="0.1", 0.5="0.5", 1="1", 2="2", 10="10")) %>%
  ggplot() +
    geom_point(aes(x=alpha_ordered, y=prop.divergent), size=2) +
    labs(x="alpha", y="% divergent") +
    scale_y_continuous(labels = scales::percent)) /
(summaries_alpha_sweep %>% 
  mutate(alpha_ordered = as.factor(alpha)) %>% #recode_factor(alpha, 0.1="0.1", 0.5="0.5", 1="1", 2="2", 10="10")) %>%
  ggplot() +
  geom_point(aes(x=alpha_ordered, y=q500)) +
  geom_errorbar(aes(x=alpha_ordered, ymin=q025, ymax=q975), width=0.5) +
  geom_hline(yintercept=2000, linetype="dotted") +
  labs(x="alpha", y="Estimates with medians and 95% CIs"))

ggsave(here("write", "output", "divergences-alpha-sweep.png"), width=4, height=6)

