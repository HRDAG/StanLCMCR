library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, ggridges, magrittr, coda)

# cmdstanR and these rds files needed for trace plots
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

theme_set(theme_bw())

# Do not draw Rplots.pdf
pdf(NULL)

# Trace plot for Stan
plot.chain.diagnostics.grid <- function(stan_fit, varnames, title="") {
  data <- as.data.frame.table(stan_fit$draws()) %>%
    filter(variable %in% c(varnames) & chain %in% c(1, 2, 3, 4)) %>%
    mutate(iteration = as.numeric(iteration),
           Freq = as.numeric(Freq),
           chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"))
  
  ggplot(data) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    facet_grid(chain ~ variable) +
    labs(x="Iteration", y="Value", title=title)
}

#fit_model7_growing_25 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide_growing-25.rds"))

## Plot trace plots
#pis = c("pi[1]", "pi[2]", "pi[3]", "pi[4]") # Probability of belonging to the first four latent classes
#plot.chain.diagnostics.grid(fit_model7_growing_25, pis)
#plot.chain.diagnostics.grid(fit_model7_growing_25, c("N"))

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 

summaries_substrata_ind <- summaries_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-independent", Dataset) | Dataset == "substrata_independent") %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 23, 23))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))
      
estimates_substrata_ind <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform"),
        Dataset = replace(Dataset, Dataset == "substrata_independent", "substrata-independent")) %>%
  filter(grepl("substrata-independent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 23, 23))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))

summaries_substrata_dep <- summaries_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("substrata-dependent", Dataset) | Dataset == "substrata_dependent") %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 21, 21))) %>%
  mutate(n_substrata=replace_na(n_substrata, 1))
      
estimates_substrata_dep <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform"),
        Dataset = replace(Dataset, Dataset == "substrata_dependent", "substrata-dependent")) %>%
  filter(grepl("substrata-dependent", Dataset)) %>%
  mutate(n_substrata=as.numeric(substr(Dataset, 21, 100))) %>%
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
# Ridgeline plots of expansion factors
###############################################

summaries_substrata_ind %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot() +
    geom_point(aes(x = q025_expfac, y=priors)) +
    geom_point(aes(x = q975_expfac, y=priors)) +
    geom_point(aes(x = q500_expfac, y=priors), shape=1) +
    geom_segment(aes(x = q025_expfac, xend=q975_expfac, y=priors, yend=priors)) +
    geom_vline(aes(xintercept=expfac_truth), linetype="dashed") +
    scale_y_discrete(limits=rev) +
    stat_density_ridges(data=estimates_substrata_ind, mapping=aes(x=expfacs, y=priors), quantile_lines=TRUE, quantiles=c(0.025, 0.5, 0.975), rel_min_height = 0.01, alpha=0.5, scale=1) +
    coord_cartesian(xlim=c(1, 3.5)) +
    labs(x="Expansion factor", y="") +
    scale_color_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    theme(legend.position = "top") +
    facet_grid(n_substrata ~ model, scales="free")

# To compare R vs Stan
summaries_substrata_ind %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot() +
  geom_point(aes(x = q025_expfac, y=model)) +
  geom_point(aes(x = q975_expfac, y=model)) +
  geom_point(aes(x = q500_expfac, y=model), shape=1) +
  geom_segment(aes(x = q025_expfac, xend=q975_expfac, y=model, yend=model)) +
  geom_vline(aes(xintercept=expfac_truth), linetype="dashed") +
  scale_y_discrete(limits=rev) +
  stat_density_ridges(data=estimates_substrata_ind, mapping=aes(x=expfacs, y=model), quantile_lines=TRUE, quantiles=c(0.025, 0.5, 0.975), rel_min_height = 0.01, alpha=0.5, scale=1) +
  coord_cartesian(xlim=c(1, 3.5)) +
  labs(x="Expansion factor", y="") +
  scale_color_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
  theme(legend.position = "top") +
  facet_grid(n_substrata ~ priors, scales="free")

###############################################
# Plots of estimates/expfac. vs. number of lists
###############################################

#######
# Independent
######

# Estimates as bars
summaries_substrata_ind %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=priors)) +
    geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
    geom_point(position=position_dodge(width=0.3)) +
    geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of substrata that we break the data into", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") ) +
    theme(legend.position = "top") +
    facet_grid(model ~ ., scales="free")


# Estimates as violin plots    
estimates_substrata_ind %>%
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
  filter(model %in% c("R", "Priors (5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    #geom_line(data=summaries_substrata_ind %>%
    #            mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
    #            filter(model %in% c("R", "Priors (5)")), 
    #          aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
    theme(legend.position = "top") +
    labs(x="Number of strata", y="Expansion factor (dashed=truth)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") ) 

# Estimates as colored lines
summaries_substrata_ind %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, color=model, linetype=model, shape=model)) +
    geom_point() +
    geom_line() +
#    geom_ribbon(data=summaries_substrata_ind %>% filter(model == "R"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="red") +
#    geom_ribbon(data=summaries_substrata_ind %>% filter(model == "Priors (5)"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="purple") +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
    theme(legend.position = "top") +
    facet_grid(model ~ priors, scales="free")

#######
# Two latent classes: wide
######

# Estimates as bars
plot_substrata_dep_bars <- summaries_substrata_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=priors, linetype=priors)) +
    geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
    geom_point(position=position_dodge(width=0.3)) +
    geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
    #geom_line(position=position_dodge(width=0.3)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata (25 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
    theme(legend.position = "top") +
  facet_grid(model ~ ., scales="free")

plot_substrata_dep_bars

summaries_substrata_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model, linetype=model)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  #geom_line(position=position_dodge(width=0.3)) +
  geom_hline(yintercept=2000, linetype="dashed") +
  labs(x="Number of strata (25 lists)", y="Estimates (N=2000)") +
  scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
  theme(legend.position = "top") +
  facet_grid(priors ~ ., scales="free")

# Estimates as bars
plot_substrata_dep_two_bars <- summaries_substrata_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  filter(priors %in% c("(1.1, 1.5)", "(1.1, 5)")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=priors, linetype=priors)) +
    geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
    geom_point(position=position_dodge(width=0.3)) +
    geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
    geom_line(position=position_dodge(width=0.3)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata (25 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
    theme(legend.position = "top") +
    facet_grid(model ~ ., scales="free")

plot_substrata_dep_two_bars

summaries_substrata_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  filter(priors %in% c("(1.1, 1.5)", "(1.1, 5)")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model, linetype=model)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  geom_line(position=position_dodge(width=0.3)) +
  geom_hline(yintercept=2000, linetype="dashed") +
  labs(x="Number of strata (25 lists)", y="Estimates (N=2000)") +
  scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
  theme(legend.position = "top") +
  facet_grid(priors ~ ., scales="free")

# Estimates as violin plots (R vs. 5)
plot_substrata_dep_violin <- estimates_substrata_dep %>%
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
  filter(model %in% c("R", "Priors (5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    #geom_line(data=summaries_substrata_dep %>%
    #            mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
    #            filter(model %in% c("R", "Priors (5)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
    theme(legend.position = "none") +
    labs(x="Number of strata (25 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") ) 

# Estimates as violin plots (R vs 3)
estimates_substrata_dep %>%
  mutate(model = recode_factor(model, R="R", `Priors (3)` = "Priors (3)")) %>%
  filter(model %in% c("R", "Priors (3)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, fill=model)) +
  geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    #geom_line(data=summaries_substrata_dep %>%
    #            mutate(model = recode_factor(model, R="R", `Priors (3)` = "Priors (3)")) %>%
    #            filter(model %in% c("R", "Priors (3)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
    theme(legend.position = "top") +
    labs(x="Number of strata", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") ) 

# Estimates as colored lines
summaries_substrata_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, color=model, linetype=model, shape=model)) +
    geom_point() +
    geom_line() +
    geom_ribbon(data=summaries_substrata_dep %>% filter(model == "R"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="red") +
    geom_ribbon(data=summaries_substrata_dep %>% filter(model == "Priors (5)"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="purple") +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / 729, name = "Expansion factor") )  +
    theme(legend.position = "top")

#######
# Two latent classes: skinny (J=5)
######

skinny_observed <- 669

# Estimates as bars
plot_substrata_skinny_bars <-
  summaries_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=priors, linetype=priors)) +
    geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
    geom_point(position=position_dodge(width=0.3)) +
    geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
    geom_line(position=position_dodge(width=0.3)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata (4 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") )  +
    theme(legend.position = "top") +
  facet_grid(model ~ ., scales="free")

plot_substrata_skinny_bars

summaries_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model, linetype=model)) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  geom_line(position=position_dodge(width=0.3)) +
  geom_hline(yintercept=2000, linetype="dashed") +
  labs(x="Number of strata (4 lists)", y="Estimates (N=2000)") +
  scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") )  +
  theme(legend.position = "top") +
  facet_grid(priors ~ ., scales="free")

# Estimates as bars
plot_substrata_skinny_two_bars <- summaries_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  filter(model %in% c("R", "Priors (5)")) %>%
  ggplot(aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model, linetype=model)) +
    geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
    geom_point(position=position_dodge(width=0.3)) +
    geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
    geom_line(position=position_dodge(width=0.3)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata (4 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") )  +
    theme(legend.position = "top")

plot_substrata_skinny_two_bars

# Estimates as violin plots (R vs. 5)
plot_substrata_skinny_violin <- estimates_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
  filter(model %in% c("R", "Priors (5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    #geom_line(data=summaries_substrata_skinny_dep %>%
    #            mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) %>%
    #            filter(model %in% c("R", "Priors (5)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
    theme(legend.position = "top") +
    labs(x="Number of strata (4 lists)", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") ) 

# Estimates as violin plots (R vs 3)
estimates_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R", `Priors (3)` = "Priors (3)")) %>%
  filter(model %in% c("R", "Priors (3)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.2), alpha=0.5, draw_quantiles=c(0.5)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    #geom_line(data=summaries_substrata_skinny_dep %>%
    #            mutate(model = recode_factor(model, R="R", `Priors (3)` = "Priors (3)")) %>%
    #            filter(model %in% c("R", "Priors (3)")), aes(x=n_substrata, y=q500, ymin=q025, ymax=q975, color=model)) +
    theme(legend.position = "top") +
    labs(x="Number of strata", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") ) 

# Estimates as colored lines
summaries_substrata_skinny_dep %>%
  mutate(model = recode_factor(model, R="R")) %>%
  ggplot(aes(x=n_substrata, y=q500, color=model, linetype=model, shape=model)) +
    geom_point() +
    geom_line() +
    geom_ribbon(data=summaries_substrata_skinny_dep %>% filter(model == "R"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="red") +
    geom_ribbon(data=summaries_substrata_skinny_dep %>% filter(model == "Priors (5)"), mapping=aes(ymin=q025, ymax=q975), alpha=0.1, fill="purple") +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Number of strata", y="Estimates (N=2000)") +
    scale_y_continuous( "Estimates (N=2000)",  sec.axis = sec_axis(~ . / skinny_observed, name = "Expansion factor") )  +
    theme(legend.position = "top")

##################
# Joining together
##################
plot_substrata_skinny_bars | plot_substrata_dep_bars 
ggsave(here("write", "output", "substrata_experiments_all.pdf"), width=6, height=8)

plot_substrata_skinny_two_bars / plot_substrata_dep_two_bars 
ggsave(here("write", "output", "substrata_experiments_two_bars.pdf"), width=6, height=8)

plot_substrata_skinny_violin / plot_substrata_dep_violin 
ggsave(here("write", "output", "substrata_experiments_violin.pdf"), width=6, height=8)
