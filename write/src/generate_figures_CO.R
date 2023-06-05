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
         type="Stratified")

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
# g21 and g22, aggregated vs separated
###############################################

summaries_co %>%
  filter(Dataset %in% c("CO_strata_g21", "CO_strata-g21-combined", "CO_strata_g22", "CO_strata-g22-combined")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
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
  filter(Dataset %in% c("CO_strata_g21", "CO_strata-g21-combined", "CO_strata_g22", "CO_strata-g22-combined")) %>%
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

###############################################
# Plot median posterior estimates and 95% CIs 
###############################################

summaries_co %>%
  filter(type %in% c("Stratified", "Aggregated")) %>%
  mutate(model = recode_factor(model, R="R"), 
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Stratified="Stratified"), 
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
  filter(type %in% c("Stratified", "Aggregated")) %>%
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


###############################################
# Plot posterior densities (R vs. 5)
###############################################

estimates_co %>%
  mutate(model = recode_factor(model, R="R"),
         priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Stratified="Stratified"), 
         strata=recode_factor(strata, "g1-g2"="g1-g2", "g3-g6"="g3-g6", "g7-g9"="g7-g9", "g10-g12"="g10-g12")) %>%
  filter(priors %in% c("Uniform", "(1.1, 5)")) %>%
  ggplot(aes(x=model, y=estimates, fill=type)) +
  geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
  #geom_line(data=summaries_substrata_dep %>%
  #            mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  #            filter(model %in% c("R", "Priors (5)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
  theme(legend.position = "top") +
  facet_grid(strata ~ model, scales = "free_y") +
  labs(x="Model", y="Estimates")

###############################################
# Diagnostics
###############################################

fit_model_CO_g1 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g1.rds"))
fit_model_CO_g2 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g2.rds"))
fit_model_CO_g1_R <- readRDS(here("write", "input", "fit", "R_CO_strata_g1_estimates.rds"))
fit_model_CO_g2_R <- readRDS(here("write", "input", "fit", "R_CO_strata_g2_estimates.rds"))

fit_model_CO_g1_g2 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_superstrata_g1-g2.rds"))
fit_model_CO_g13_g17 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_superstrata_g13-g17.rds"))
fit_model_CO_g20_g24 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_superstrata_g20-24.rds"))
fit_model_CO_g13_g17_R_ests <- readRDS(here("write", "input", "fit", "R_CO_superstrata_g13-g17_estimates.rds"))

fit_model_CO_g21_R_3 <- readRDS(here("write", "input", "fit", "R_3_CO_strata_g21_estimates.rds"))

# Substratification of g21
fit_model_CO_g21 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21.rds"))
fit_model_CO_g21_00 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_00.rds"))
fit_model_CO_g21_01 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_01.rds"))
fit_model_CO_g21_02 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_02.rds"))
fit_model_CO_g21_03 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_03.rds"))
fit_model_CO_g21_04 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_04.rds"))

fit_model_CO_g21_LCMCR_7_3 <- readRDS(here("write", "input", "fit", "LCMCR_7_3_CO_strata_g21.rds"))


fit_model_CO_g22 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g22.rds"))


plot(fit_model_CO_g13_g17_R_ests, type="l")
plot(fit_model_CO_g1_R, type="l")
plot(fit_model_CO_g2_R, type="l")



get.trace.dataframe <- function(stan_fit, varnames, chains=c(1, 2, 3, 4)) {
  data <- as.data.frame.table(stan_fit$draws()) %>%
    filter(variable %in% c(varnames) & chain %in% chains) %>%
    mutate(iteration = as.numeric(iteration),
           Freq = as.numeric(Freq),
           chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"))
  
  data
}

plot.chain.diagnostics.grid <- function(stan_fit, varnames, title="", chains=c(1, 2, 3, 4)) {
  data <- get.trace.dataframe(stan_fit, varnames, chains)
  
  ggplot(data) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    geom_line(aes(x=iteration, y=Freq)) +
    facet_grid(chain ~ variable) +
    labs(x="Iteration", y="Value", title=title)
}

df.CO_g1 <- get.trace.dataframe(fit_model_CO_g1, c("N"), chains=1:8)
df.CO_g21 <- get.trace.dataframe(fit_model_CO_g21, c("N"), chains=1:8)
df.CO_g1_g2 <- get.trace.dataframe(fit_model_CO_g1_g2, c("N"), chains=1:8)
df.CO_g13_g17 <- get.trace.dataframe(fit_model_CO_g13_g17, c("N"), chains=1:8)

df.CO_g1_raw <- read_csv(here("fit/output/LCMCR_7_5_CO_strata_g1-1.csv"), comment="#")
length(df.CO_g1_raw$N)

# Currently Stan is subsampling thin=1000
# Want Stan subsampling that is 1/100 (to match R)
# R is set so that thin=100

length(fit_model_CO_g1_R)
fit_model_CO_g1_R_sampled <- fit_model_CO_g1_R[(seq(1, 100000, by=10))]

plot(fit_model_CO_g1_R_sampled[1:1000], type="l")
plot((df.CO_g1 %>% filter(chain == "Chain 3"))$Freq[1:1000], type="l")

effectiveSize(fit_model_CO_g1_R_sampled)

effectiveSize(rnorm(10))
effectiveSize((df.CO_g1 %>% filter(chain == "Chain 3"))$Freq)

ggplot(df.CO_g21) +
  geom_density(aes(x=Freq, color=chain))

ggplot(df.CO_g1_g2) +
  geom_density(aes(x=Freq, color=chain))

ggplot(df.CO_g13_g17) +
  geom_density(aes(x=Freq, color=chain))

fit_model_CO_g1$diagnostic_summary()

plot.chain.diagnostics.grid(fit_model_CO_g21, c("N"), chains=1:4)
plot.chain.diagnostics.grid(fit_model_CO_g21_LCMCR_7_3, c("N"), chains=1:4)
plot(fit_model_CO_g21_R_3, type="l")

plot.chain.diagnostics.grid(fit_model_CO_g22, c("N"), chains=1:8)


plot.chain.diagnostics.grid(fit_model_CO_g21_00, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21_01, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21_02, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21_03, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21_04, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21_00, c("pi[1]", "pi[2]", "pi[3]", "pi[4]", "pi[5]", "pi[6]"), chains=1:8)

plot.chain.diagnostics.grid(fit_model_CO_g1, c("N"), title="Stratum 1", chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g2, c("N"), title="Stratum 2", chains=1:8)

fit_model_CO_g21$diagnostic_summary()
fit_model_CO_g2$diagnostic_summary()

plot.chain.diagnostics.grid(fit_model_CO_g1_g2, c("N"), chains=1:8)

plot.chain.diagnostics.grid(fit_model_CO_g13_g17, c("N"))
plot.chain.diagnostics.grid(fit_model_CO_g20_g24, c("N"))

summaries_strata <- summaries_all %>%
  filter(grepl("CO_strata", Dataset)) %>%
  mutate(strata=substr(Dataset, 11, 100),
         type="Stratified")
