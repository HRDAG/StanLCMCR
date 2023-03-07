library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork)

theme_set(theme_minimal())

# Do not draw Rplots.pdf
pdf(NULL)

# cmdstanR and these rds files needed for trace plots
#if (!require("cmdstanr")) {
#  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#  pacman::p_load(cmdstanr)
#}
#fit_model4_small <- readRDS(here("write", "input", "fit", "LCMCR_4_5_wide1_small.rds"))
#fit_model4_medium <- readRDS(here("write", "input", "fit", "LCMCR_4_5_wide1_medium.rds"))
#fit_model4_large <- readRDS(here("write", "input", "fit", "LCMCR_4_5_wide1_large.rds"))
#
#fit_model7_1.5_small <- readRDS(here("write", "input", "fit", "LCMCR_7_1.5_wide1_small.rds"))
#fit_model7_1.5_medium <- readRDS(here("write", "input", "fit", "LCMCR_7_1.5_wide1_medium.rds"))
#fit_model7_1.5_large <- readRDS(here("write", "input", "fit", "LCMCR_7_1.5_wide1_large.rds"))
#
#fit_model7_3_small <- readRDS(here("write", "input", "fit", "LCMCR_7_3_wide1_small.rds"))
#fit_model7_3_medium <- readRDS(here("write", "input", "fit", "LCMCR_7_3_wide1_medium.rds"))
#fit_model7_3_large <- readRDS(here("write", "input", "fit", "LCMCR_7_3_wide1_large.rds"))
#
#fit_model7_5_small <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide1_small.rds"))
#fit_model7_5_medium <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide1_medium.rds"))
#fit_model7_5_large <- readRDS(here("write", "input", "fit", "LCMCR_7_5_wide1_large.rds"))

summaries_all <- read.csv(here("write", "input", "summaries", "summaries.csv")) 
estimates_all <- read.csv(here("write", "input", "summaries", "estimates.csv")) 

summaries <- summaries_all |> filter(grepl("wide", Dataset)) |>
    mutate(Simulation=substr(Dataset, 5, 5),
           Size=substr(Dataset, 7, 100)) |>
    mutate(Size = recode_factor(Size, small = "N=300", medium = "N=600", large="N=900")) |>
    mutate(model = recode_factor(model, R="R", LCMCR_4_5 = "Uniform", `LCMCR_7_1.5`="Priors (1.5)", LCMCR_7_3 = "Priors (3)", LCMCR_7_5 = "Priors (5)")) 

estimates <- estimates_all |> filter(grepl("wide", Dataset)) |>
    mutate(Simulation=substr(Dataset, 5, 5),
           Size=substr(Dataset, 7, 100)) |>
    mutate(Size = recode_factor(Size, small = "N=300", medium = "N=600", large="N=900")) |>
    mutate(model = recode_factor(model, LCMCR_4_5 = "Uniform", `LCMCR_7_1.5`="Priors (1.5)", LCMCR_7_3 = "Priors (3)", LCMCR_7_5 = "Priors (5)"))

plot_estimates1 <- function(ests, sums) {
  p1 <- ests |>
    filter(model %in% c("Uniform", "Priors (1.5)")) |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = Size), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Prior (1.5)") +
      geom_vline(data=sums, aes(xintercept=expfac_truth, color=Size), linetype="dashed") +
      scale_fill_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      scale_color_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")

  p2 <- ests |>
    filter(model %in% c("Uniform", "Priors (3)")) |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = Size), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Prior (3)") +
      geom_vline(data=sums, aes(xintercept=expfac_truth, color=Size), linetype="dashed") +
      scale_fill_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      scale_color_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")

  p3 <- ests |>
    filter(model %in% c("Uniform", "Priors (5)")) |>
  ggplot() +
      geom_density(aes(x = expfacs, fill = Size), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Prior (5)") +
      geom_vline(data=sums, aes(xintercept=expfac_truth, color=Size), linetype="dashed") +
      scale_fill_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      scale_color_manual(name = "Size", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")

  p1 / p2 / p3
}

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

plot_estimates(estimates |> filter(Simulation == "1"), summaries |> filter(Simulation == "1"))
ggsave(here("write", "output", "posterior-expfac-densities-sim-T-wide1.png"), height=10, width=4)

plot_estimates(estimates |> filter(Simulation == "2"), summaries |> filter(Simulation == "2"))
ggsave(here("write", "output", "posterior-expfac-densities-sim-T-wide2.png"), height=10, width=4)

plot_estimates(estimates |> filter(Simulation == "3"), summaries |> filter(Simulation == "3"))
ggsave(here("write", "output", "posterior-expfac-densities-sim-T-wide3.png"), height=10, width=4)

###############################################
# Barbell plots of expansion factors
###############################################

summaries |>
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
    facet_grid(Size ~ Simulation, scales="free")

ggsave(here("write", "output", "expansion-factors-widesim.png"), height=10, width=8)

