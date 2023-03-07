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
summaries <- summaries_all |> filter(grepl("wide1_", Dataset))
estimates <- estimates_all |> filter(grepl("wide1_", Dataset))

estimates_renamed <- estimates |>
  mutate(model = recode_factor(model, LCMCR_4_5 = "Uniform", `LCMCR_7_1.5`="Priors (1.5)", LCMCR_7_3 = "Priors (3)", LCMCR_7_5 = "Priors (5)"),
         Dataset = recode_factor(Dataset, wide1_small = "N=300", wide1_medium = "N=600", wide1_large="N=900")) 

p1 <-estimates_renamed |>
    filter(model %in% c("Uniform", "Priors (1.5)")) |>
    ggplot() +
      geom_density(aes(x = expfacs, fill = Dataset), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Density") +
      scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")

p2 <-estimates_renamed |>
    filter(model %in% c("Uniform", "Priors (3)")) |>
    ggplot() +
      geom_density(aes(x = expfacs, fill = Dataset), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Density") +
      scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")

p3 <-estimates_renamed |>
    filter(model %in% c("Uniform", "Priors (5)")) |>
    ggplot() +
      geom_density(aes(x = expfacs, fill = Dataset), alpha = 0.25) +
      xlab("Estimated expansion factor") +
      ylab("Density") +
      scale_fill_manual(name = "Model", values = c("firebrick1", "dodgerblue1", "purple")) +
      theme(legend.position = "top")
  
p1 / p2 / p3

ggsave(here("write", "output", "posterior-expfac-densities-widesim.png"), height=10, width=4)

