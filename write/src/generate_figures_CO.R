library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, ggridges, magrittr)

theme_set(theme_minimal())

# cmdstanR needed for trace plots
# also, custom repository
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 

summaries_strata <- summaries_all %>%
  filter(grepl("CO_strata", Dataset)) %>%
  mutate(strata=substr(Dataset, 11, 100),
         type="Stratified")

summaries_superstrata <- summaries_all %>%
  filter(grepl("CO_superstrata", Dataset)) %>%
  mutate(strata=substr(Dataset, 16, 100),
         type="Aggregated")

estimates_strata <- estimates_all %>%
  filter(grepl("CO_strata", Dataset)) %>%
  mutate(strata=substr(Dataset, 11, 100),
         type="Stratified")

estimates_superstrata <- estimates_all %>%
  filter(grepl("CO_superstrata", Dataset)) %>%
  mutate(strata=substr(Dataset, 16, 100),
         type="Aggregated")

summaries_co <- rbind(summaries_strata, summaries_superstrata)
estimates_co <- rbind(estimates_strata, estimates_superstrata)

View(summaries_co)

# Needed to prevent generating Rplots.pdf in current working directory
# pdf(NULL)

###############################################
# Plot median posterior estimates and 95% CIs 
###############################################

summaries_co %>%
  mutate(model = recode_factor(model, R="R"), 
         type=recode_factor(type, Stratified="Stratified"), 
         strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=model, y=q500, ymin=q025, ymax=q975, color=type)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  geom_hline(yintercept=2000, linetype="dashed") +
  labs(x="Model", y="Estimates") +
  facet_grid(strata ~ ., scales = "free_y") +
  theme(legend.position = "top")

ggsave(here("write", "output", "posterior-CIs-CO.png"), width=5, height=4)

###############################################
# Plot posterior densities (R vs. priors)
###############################################

estimates_co %>%
  mutate(model = recode_factor(model, R="R"),
         type=recode_factor(type, Stratified="Stratified"), 
         strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  filter(model %in% c("R", "LCMCR_7_5")) %>%
  ggplot(aes(x=model, y=estimates, fill=type)) +
  geom_violin(position=position_dodge(width=0.2), alpha=0.5) +
  geom_hline(yintercept=2000, linetype="dashed") +
  #geom_line(data=summaries_substrata_dep %>%
  #            mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  #            filter(model %in% c("R", "Priors (5)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
  theme(legend.position = "top") +
  facet_grid(strata ~ ., scales = "free_y") +
  labs(x="Model", y="Estimates")

