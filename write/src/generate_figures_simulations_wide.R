library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, ggridges)

# cmdstanR and these rds files needed for trace plots
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

theme_set(theme_minimal())

# Do not draw Rplots.pdf
pdf(NULL)

# Trace plot for Stan
plot.chain.diagnostics.grid <- function(stan_fit, varnames, title="") {
  data <- as.data.frame.table(stan_fit$draws()) |>
    filter(variable %in% c(varnames) & chain %in% c(1, 2, 3, 4)) |>
    mutate(iteration = as.numeric(iteration),
           Freq = as.numeric(Freq),
           chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"))
  
  ggplot(data) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    facet_grid(chain ~ variable) +
    labs(x="Iteration", y="Value", title=title)
}

fit_model7_5_small <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide3_small.rds"))
fit_model7_5_medium <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide3_medium.rds"))
fit_model7_5_large <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide3_large.rds"))

fit_model7_growing_25 <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide_growing-25.rds"))

# Plot trace plots
pis = c("pi[1]", "pi[2]", "pi[3]", "pi[4]") # Probability of belonging to the first four latent classes
plot.chain.diagnostics.grid(fit_model7_5_large, pis)
plot.chain.diagnostics.grid(fit_model7_5_large, c("N"))
plot.chain.diagnostics.grid(fit_model7_growing_25, c("N"))

fit_model7_5_large$draws()

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 

summaries <- summaries_all |>
  filter(grepl("wide", Dataset)) |>
  mutate(model = recode_factor(model, R="R", LCMCR_4_5 = "Uniform", `LCMCR_7_1.5`="Priors (1.5)", LCMCR_7_3 = "Priors (3)", LCMCR_7_5 = "Priors (5)")) 

summaries_wide3 <- summaries |>
  filter(grepl("wide3", Dataset)) |>
  mutate(Simulation=substr(Dataset, 5, 5), Size=substr(Dataset, 7, 100)) |>
  mutate(Size = recode_factor(Size, small = "N=300", medium = "N=600", large="N=900")) 
      
summaries_growing <- summaries |>
  filter(grepl("wide_growing-", Dataset)) |>
  mutate(J=as.numeric(substr(Dataset, 14, 100)))

summaries_growing_independent <- summaries |>
  filter(grepl("wide_growing_independent-", Dataset)) |>
  mutate(J=as.numeric(substr(Dataset, 26, 100)))
      
estimates <- estimates_all |> filter(grepl("wide", Dataset)) |>
    mutate(model = recode_factor(model, LCMCR_4_5 = "Uniform", `LCMCR_7_1.5`="Priors (1.5)", LCMCR_7_3 = "Priors (3)", LCMCR_7_5 = "Priors (5)"))

estimates_wide3 <- estimates |>
    filter(grepl("wide3", Dataset)) |>
    mutate(Simulation=substr(Dataset, 5, 5), Size=substr(Dataset, 7, 100)) |>
    mutate(Size = recode_factor(Size, small = "N=300", medium = "N=600", large="N=900"))

estimates_growing <- estimates |>
  filter(grepl("wide_growing-", Dataset)) |>
  mutate(J=as.factor(as.numeric(substr(Dataset, 14, 100))))

estimates_growing_independent <- estimates |>
  filter(grepl("wide_growing_independent-", Dataset)) |>
  mutate(J=as.factor(as.numeric(substr(Dataset, 26, 100))))


plot_estimates <- function(ests, sums) {
  p1 <- ests |>
    filter(Size == "N=300") |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = model), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("N=300") +
      geom_vline(data=sums |> filter(Size == "N=300"), aes(xintercept=expfac_truth), linetype="dashed") +
      #scale_fill_manual(name = "model", values = c("yellow", "firebrick1", "dodgerblue1", "purple", "black")) +
      theme(legend.position = "top") +
      guides(fill=guide_legend(nrow=2))

  p2 <- ests |>
    filter(Size == "N=600") |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = model), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("N=600") +
      geom_vline(data=sums |> filter(Size == "N=600"), aes(xintercept=expfac_truth), linetype="dashed") +
      #scale_fill_manual(name = "model", values = c("yellow", "firebrick1", "dodgerblue1", "purple", "black")) +
      theme(legend.position = "top") +
      guides(fill=guide_legend(nrow=2))

  p3 <- ests |>
    filter(Size == "N=900") |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = model), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("N=900") +
      geom_vline(data=sums |> filter(Size == "N=900"), aes(xintercept=expfac_truth), linetype="dashed") +
      #scale_fill_manual(name = "model", values = c("yellow", "firebrick1", "dodgerblue1", "purple", "black")) +
      theme(legend.position = "top") +
      guides(fill=guide_legend(nrow=2))

  p1 / p2 / p3
}

plot_estimates(estimates_wide3, summaries_wide3)
ggsave(here("write", "output", "posterior-expfac-densities-sim-T-wide3.png"), height=10, width=4)

###############################################
# Barbell plots of expansion factors for wide3 simulation
###############################################

summaries_wide3 |>
  #mutate(model = recode_factor(model, R="R")) |>
  ggplot() +
    geom_point(aes(x = q025_expfac, y=model)) +
    geom_point(aes(x = q975_expfac, y=model)) +
    geom_point(aes(x = q500_expfac, y=model), shape=1) +
    geom_segment(aes(x = q025_expfac, xend=q975_expfac, y=model, yend=model)) +
    geom_vline(aes(xintercept=expfac_truth), linetype="dashed") +
    scale_y_discrete(limits=rev) +
    labs(x="Expansion factor", y="") +
    #scale_color_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    theme(legend.position = "top") +
    facet_grid(Size ~ ., scales="free")

ggsave(here("write", "output", "expansion-factors-widesim.png"), height=10, width=8)

###############################################
# Ridgeline plots of expansion factors for wide3 simulation
###############################################

summaries_wide3 |>
  mutate(model = recode_factor(model, R="R", LCMCR_4="LCMCR_4")) |>
  ggplot() +
    geom_point(aes(x = q025_expfac, y=model)) +
    geom_point(aes(x = q975_expfac, y=model)) +
    geom_point(aes(x = q500_expfac, y=model), shape=1) +
    geom_segment(aes(x = q025_expfac, xend=q975_expfac, y=model, yend=model)) +
    geom_vline(aes(xintercept=expfac_truth), linetype="dashed") +
    scale_y_discrete(limits=rev) +
    stat_density_ridges(data=estimates_wide3, mapping=aes(x=expfacs, y=model), quantile_lines=TRUE, quantiles=c(0.025, 0.5, 0.975), rel_min_height = 0.01, alpha=0.5, scale=1) +
    coord_cartesian(xlim=c(1, 3.5)) +
    labs(x="Expansion factor", y="") +
    scale_color_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple", "yellow", "green")) +
    theme(legend.position = "top") +
    facet_wrap(Size ~ ., scales="free", ncol=1)

ggsave(here("write", "output", "expansion-factors-ridges-widesim.png"), height=6, width=4)

###############################################
# Plots of estimates/expfac. vs. number of lists
###############################################

#######
# Independent
######

# Estimates as bars
summaries_growing_independent |>
  ggplot(aes(x=J, y=q500, ymin=q025, ymax=q975, color=model)) +
    geom_errorbar(width=0.2, position=position_dodge(width=1)) +
    geom_point(position=position_dodge(width=1)) +
    geom_point(aes(y=q025), position=position_dodge(width=1), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=1), shape=1) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Lists", y="Estimates (N=2000)") +
    theme(legend.position = "top")

# Expfacs as bars
summaries_growing_independent |>
  ggplot(aes(x=J, y=q500_expfac, ymin=q025_expfac, ymax=q975_expfac, color=model)) +
    geom_errorbar(width=0.2, position=position_dodge(width=1)) +
    geom_point(position=position_dodge(width=1)) +
    geom_point(aes(y=q025_expfac), position=position_dodge(width=1), shape=1) +
    geom_point(aes(y=q975_expfac), position=position_dodge(width=1), shape=1) +
    geom_point(mapping=aes(x=J, y=expfac_truth), color="black", size=3, shape=3) +
    geom_line(mapping=aes(x=J, y=expfac_truth), color="black", linetype="dashed") +
    labs(x="Lists", y="Expansion factor (dashed=truth)") +
    theme(legend.position = "top")

# Estimates as violin plots    
estimates_growing_independent |>
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  filter(model %in% c("R", "Priors (5)")) |>
  mutate(J = as.factor(J)) |>
  ggplot(aes(x=J, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.7)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    theme(legend.position = "top") +
    labs(x="Lists", y="Expansion factor (dashed=truth)")

    
# Expfacs as violin plots    
estimates_growing_independent |>
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  filter(model %in% c("R", "Priors (5)")) |>
  mutate(J = as.factor(J)) |>
  ggplot() +
    geom_violin(aes(x=J, y=expfacs, fill=model), width=0.3, position=position_dodge(width=0.5)) +
    geom_violin(data=priors_df3, mapping=aes(x=as.factor(J), y=expfac_prior), width=0.2) +
    geom_point(data=summaries_growing_independent |> filter(model %in% c("R", "Priors (5)")), mapping=aes(x=as.factor(J), y=expfac_truth, group=1)) +
    geom_line(data=summaries_growing_independent |> filter(model %in% c("R", "Priors (5)")), mapping=aes(x=as.factor(J), y=expfac_truth, group=1), linetype="dashed") +
    theme(legend.position = "top") +
    labs(x="Lists", y="Expansion factor (dashed=truth)") +
    coord_cartesian(ylim=c(1, 10))
  

#######
# Two latent classes
######


# Estimates as bars
summaries_growing |>
  ggplot(aes(x=J, y=q500, ymin=q025, ymax=q975, color=model)) +
    geom_errorbar(width=0.2, position=position_dodge(width=1)) +
    geom_point(position=position_dodge(width=1)) +
    geom_point(aes(y=q025), position=position_dodge(width=1), shape=1) +
    geom_point(aes(y=q975), position=position_dodge(width=1), shape=1) +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Lists", y="Estimates (N=2000)") +
    theme(legend.position = "top")

# Expfacs as bars
summaries_growing |>
  ggplot(aes(x=J, y=q500_expfac, ymin=q025_expfac, ymax=q975_expfac, color=model)) +
    geom_errorbar(width=0.2, position=position_dodge(width=1)) +
    geom_point(position=position_dodge(width=1)) +
    geom_point(aes(y=q025_expfac), position=position_dodge(width=1), shape=1) +
    geom_point(aes(y=q975_expfac), position=position_dodge(width=1), shape=1) +
    geom_point(mapping=aes(x=J, y=expfac_truth), color="black", size=3, shape=3) +
    geom_line(mapping=aes(x=J, y=expfac_truth), color="black", linetype="dashed") +
    labs(x="Lists", y="Expansion factor (dashed=truth)") +
    theme(legend.position = "top")

# Estimates as violin plots    
estimates_growing |>
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  filter(model %in% c("R", "Priors (5)")) |>
  mutate(J = as.factor(J)) |>
  ggplot(aes(x=J, y=estimates, fill=model)) +
    geom_violin(position=position_dodge(width=0.7)) +
    geom_hline(yintercept=2000, linetype="dashed") +
    theme(legend.position = "top") +
    labs(x="Lists", y="Expansion factor (dashed=truth)")

    
# Expfacs as violin plots    
estimates_growing |>
  mutate(model = recode_factor(model, R="R", `Priors (5)` = "Priors (5)")) |>
  filter(model %in% c("R", "Priors (5)")) |>
  mutate(J = as.factor(J)) |>
  ggplot() +
    geom_violin(aes(x=J, y=expfacs, fill=model), width=0.3, position=position_dodge(width=0.5)) +
    geom_violin(data=priors_df3, mapping=aes(x=as.factor(J), y=expfac_prior), width=0.2) +
    geom_point(data=summaries_growing |> filter(model %in% c("R", "Priors (5)")), mapping=aes(x=as.factor(J), y=expfac_truth, group=1)) +
    geom_line(data=summaries_growing |> filter(model %in% c("R", "Priors (5)")), mapping=aes(x=as.factor(J), y=expfac_truth, group=1), linetype="dashed") +
    theme(legend.position = "top") +
    labs(x="Lists", y="Expansion factor (dashed=truth)") +
    coord_cartesian(ylim=c(1, 10))
  
##################
# These are ugly:
##################

# Expfacs
summaries_growing |>
  filter(model %in% c("R", "Priors (5)")) |>
  ggplot(aes(x=J, y=q500_expfac, ymin=q025_expfac, ymax=q975_expfac, color=model, fill=model)) +
    geom_ribbon(alpha=0.1) +
    geom_line() +
    geom_line(mapping=aes(x=J, y=expfac_truth), color="black", linetype="dashed") +
    labs(x="Lists", y="Expansion factors (truth=dashed)") +
    theme(legend.position = "top")

# Estimates
summaries_growing |>
  filter(model %in% c("R", "Priors (5)")) |>
  ggplot(aes(x=J, y=q500, ymin=q025, ymax=q975, color=model, fill=model)) +
    geom_ribbon(alpha=0.1) +
    geom_line() +
    geom_hline(yintercept=2000, linetype="dashed") +
    labs(x="Lists", y="Estimates (N=2000)") +
    theme(legend.position = "top") 
    
