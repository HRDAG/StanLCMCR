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
fit_model_CO_g21_R_5 <- readRDS(here("write", "input", "fit", "R_5_CO_strata_g21_estimates.rds"))

# Substratification of g21
fit_model_CO_g21 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21.rds"))
fit_model_CO_g21_00 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_00.rds"))
fit_model_CO_g21_01 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_01.rds"))
fit_model_CO_g21_02 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_02.rds"))
fit_model_CO_g21_03 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_03.rds"))
fit_model_CO_g21_04 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g21_04.rds"))

fit_model_CO_g22 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_CO_strata_g22.rds"))


plot(fit_model_CO_g13_g17_R_ests, type="l")
plot(fit_model_CO_g1_R, type="l")
plot(fit_model_CO_g2_R, type="l")
plot(fit_model_CO_g21_R_3, type="l")
plot(fit_model_CO_g21_R_5, type="l")
plot(density(fit_model_CO_g21_R_5))
plot(density(fit_model_CO_g21_R_3[1:4000]))
lines(density(fit_model_CO_g21_R_3[4000:10000]), col="red")


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

plot.chain.diagnostics.grid(fit_model_CO_g21, c("N"), chains=1:8)
plot.chain.diagnostics.grid(fit_model_CO_g21, c("pi[1]", "pi[2]", "pi[3]", "pi[4]", "pi[5]", "pi[6]"), chains=1:8)
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