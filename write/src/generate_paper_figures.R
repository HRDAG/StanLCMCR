library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, ggridges, magrittr, LCMCR, coda)

theme_set(theme_bw())

# cmdstanR needed for trace plots
# also, custom repository
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 
estimates_thin <- estimates_all %>% filter(i %% 10 == 0)

summaries_strata <- summaries_all %>%
  filter(grepl("CO_strata", Dataset)) %>%
  mutate(strata=substr(Dataset, 11, 100),
         type=ifelse(grepl("[-_]", strata), 
                     ifelse(grepl("-[^c]", strata), "Stratified", "Substrata"), "Original"))

summaries_superstrata <- summaries_all %>%
  filter(grepl("CO_superstrata", Dataset)) %>%
  mutate(strata=substr(Dataset, 16, 100),
         type="Aggregated")

estimates_strata <- estimates_all %>%
  filter(grepl("CO_strata", Dataset)) %>%
  mutate(strata=substr(Dataset, 11, 100),
         type=ifelse(grepl("[-_]", strata), 
                     ifelse(grepl("-[^c]", strata), "Stratified", "Substrata"), "Original"))

estimates_superstrata <- estimates_all %>%
  filter(grepl("CO_superstrata", Dataset)) %>%
  mutate(strata=substr(Dataset, 16, 100),
         type="Aggregated")

summaries_co <- rbind(summaries_strata, summaries_superstrata)
estimates_co <- rbind(estimates_strata, estimates_superstrata)

## Simulations

summaries_substrata_ind <- summaries_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-independent", Dataset) | Dataset == "substrata_independent") %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 23, 23))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))


summaries_substrata_dep <- summaries_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-dependent", Dataset) | Dataset == "substrata_dependent") %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 21, 21))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

summaries_substrata_skinny_dep <- summaries_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-skinny-dependent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 28, 100))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

estimates_substrata_ind <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform"),
         Dataset = replace(Dataset, Dataset == "substrata_independent", "substrata-independent")) %>%
  filter(grepl("substrata-independent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 23, 23))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

estimates_substrata_dep <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform"),
         Dataset = replace(Dataset, Dataset == "substrata_dependent", "substrata-dependent")) %>%
  filter(grepl("substrata-dependent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 21, 100))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

estimates_substrata_skinny_dep <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-skinny-dependent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 28, 100))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

###############################################
# Fig 1. Showing the problem exists on CO data
###############################################

fig1_data <- estimates_co %>%
  filter(Dataset %in% c("CO_strata_g13-g17", "CO_superstrata_g13-g17"),
         model == "R",
         priors == "Uniform") %>%
  mutate(type=recode_factor(type, Stratified="Stratified"),
         family=substr(strata, 1, 3))

ggplot(fig1_data, aes(x=estimates, linetype=type, color=type, fill=type)) +
  geom_density(alpha=0.5) +
  geom_vline(xintercept = fig1_data$observed[1], linetype="dashed") +
  labs(x="Estimates", y="Density") +
  theme(legend.position = "top")

ggsave(here("write/output/shrinkage_exists_in_CO.pdf"), height=3, width=4)

View(summaries_strata)

###############################################
# Fig 2. Showing the problem exists in simulations with more strata
###############################################

estimates_substrata_dep %>%
  filter(model == "R", priors %in% c("Uniform", "(1.1, 5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, color=priors)) +
    geom_violin(alpha=0.5, draw_quantiles=c(0.5), position=position_dodge(width=0.2)) +
    geom_hline(yintercept = estimates_substrata_dep$observed[1], linetype="dashed") +
    geom_hline(yintercept = estimates_substrata_dep$truth[1], linetype="solid") +
    labs(x="Number of substrata", y="Estimates") +
    theme(legend.position = "top")
