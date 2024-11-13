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

View(summaries_all)

# Needed to prevent generating Rplots.pdf in current working directory
# pdf(NULL)

###############################################
# s1a, s1b, s2a, and s2b (our custom strata)
###############################################

summaries_co %>%
  filter(Dataset %in% c("CO_strata_s1a-combined", "CO_strata_s1b-combined", "CO_strata_s2a-combined", "CO_strata_s2b-combined",
                        "CO_superstrata_s1a", "CO_superstrata_s1b", "CO_superstrata_s2a", "CO_superstrata_s2b"),
         model == "R") %>%
  mutate(priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
         family=substr(strata, 1, 3)) %>%
  #strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=priors, y=q500, ymin=q025, ymax=q975, color=type)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Model", y="Estimates") +
  facet_grid(family ~ ., scales = "free_y") +
  theme(legend.position = "top")

# For diagnosing R vs Stan
summaries_co %>%
  filter(Dataset %in% c("CO_strata_g21", "CO_strata_g21-combined", "CO_strata_g22", "CO_strata_g22-combined")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
         family=substr(strata, 1, 3)) %>%
         #strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=priors, y=q500, ymin=q025, ymax=q975, color=model)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Model", y="Estimates") +
  facet_grid(family ~ type, scales = "free_y") +
  theme(legend.position = "top")

estimates_co %>%
  filter(Dataset %in% c("CO_strata_s1a-combined", "CO_strata_s1b-combined", "CO_strata_s2a-combined", "CO_strata_s2b-combined",
                        "CO_superstrata_s1a", "CO_superstrata_s1b", "CO_superstrata_s2a", "CO_superstrata_s2b"),
         model == "R") %>%
  mutate(priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
         family=substr(strata, 1, 3)) %>%
  ggplot(aes(x=priors, y=estimates, color=type)) +
  geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
  labs(x="Model", y="Estimates") +
  facet_grid(family ~ ., scales = "free_y") +
  theme(legend.position = "top")


###############################################
# g21 and g22, aggregated vs separated
###############################################

summaries_co %>%
  filter(Dataset %in% c("CO_strata_g21", "CO_strata_g21-combined", "CO_strata_g22", "CO_strata_g22-combined")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
         priors=recode_factor(priors, Uniform="Uniform"),
         family=substr(strata, 1, 3)) %>%
  #strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=priors, y=q500, ymin=q025, ymax=q975, color=type)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Model", y="Estimates") +
  facet_grid(family ~ model, scales = "free_y") +
  theme(legend.position = "top")

# Flipped, for diagnosing R vs Stan
summaries_co %>%
  filter(Dataset %in% c("CO_strata_g21", "CO_strata_g21-combined", "CO_strata_g22", "CO_strata_g22-combined")) %>%
  mutate(model = recode_factor(model, R="R"), 
         type=recode_factor(type, Substrata="Substrata"),
         priors=recode_factor(priors, Uniform="Uniform"),
         family=substr(strata, 1, 3)) %>%
         #strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=model, y=q500, ymin=q025, ymax=q975, color=type)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Model", y="Estimates") +
  facet_grid(priors ~ family, scales = "free_y") +
  theme(legend.position = "top")

###############################################
# Plot median posterior estimates and 95% CIs 
###############################################

summaries_co %>%
  filter(type %in% c("Stratified", "Aggregated") & strata %in% c("g1-g2", "g3-g6", "g7-g9", "g10-g2", "g13-g17", "g18-g19", "g20-24")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Stratified="Stratified"), 
         priors=recode_factor(priors, Uniform="Uniform"),
         strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=priors, y=q500, ymin=q025, ymax=q975, color=type)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Priors", y="Estimates") +
  facet_grid(strata ~ model, scales = "free_y") +
  theme(legend.position = "top")


ggsave(here("write", "output", "posterior-CIs-CO.png"), width=5, height=4)

# Flipped, to diagnose Stan vs. R
summaries_co %>%
  filter(type %in% c("Stratified", "Aggregated") &
         strata %in% c("g1-g2", "g3-g6", "g7-g9", "g10-g12")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Stratified="Stratified"), 
         strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  ggplot(aes(x=priors, y=q500, ymin=q025, ymax=q975, color=model)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Priors", y="Estimates") +
  facet_grid(strata ~ type, scales = "free_y") +
  theme(legend.position = "top")
