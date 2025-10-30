library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, ggridges, magrittr, LCMCR, coda)

theme_set(theme_bw())

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

estimates_substrata_growing <- estimates_all %>%
  mutate(priors = recode_factor(priors, Uniform = "Uniform")) %>%
  filter(grepl("wide_growing-", Dataset)) %>%
  mutate(n_lists=as.numeric(substr(Dataset, 14, 100)))


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
  theme(legend.position = "top") +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL)
  )

ggsave(here("write/output/shrinkage_exists_in_CO.pdf"), height=3, width=4)

###############################################
# Fig 2a/b. Showing the problem exists in simulations with more strata; and that we fix it
###############################################

estimates_substrata_dep %>%
  filter(model == "R", priors %in% c("Uniform", "(1.1, 5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, color=priors)) +
    geom_violin(alpha=0.5, draw_quantiles=c(0.5), position=position_dodge(width=0.2)) +
    geom_hline(yintercept = estimates_substrata_dep$observed[1], linetype="dashed") +
    geom_hline(yintercept = estimates_substrata_dep$truth[1], linetype="solid") +
    annotate( "text", x = Inf,  y = estimates_substrata_dep$observed[1] - 200,   label = "Observed", hjust = 1.1,   color = "gray40", size = 4 ) +
    labs(x="Number of substrata", y="Estimates") +
    guides(color = guide_legend(title = "Priors:")) +
    theme(legend.position = "top") +
  ggtitle("J=20 lists")

ggsave(here("write/output/fix_shrinkage_in_simulation_dependent_J20.pdf"), height=3, width=4)

estimates_substrata_skinny_dep %>%
  filter(model == "R", priors %in% c("Uniform", "(1.1, 5)")) %>%
  mutate(n_substrata = as.factor(n_substrata)) %>%
  ggplot(aes(x=n_substrata, y=estimates, color=priors)) +
    geom_violin(alpha=0.5, draw_quantiles=c(0.5), position=position_dodge(width=0.2)) +
    geom_hline(yintercept = estimates_substrata_skinny_dep$observed[1], linetype="dashed") +
    geom_hline(yintercept = estimates_substrata_skinny_dep$truth[1], linetype="solid") +
    annotate( "text", x = Inf,  y = estimates_substrata_skinny_dep$observed[1] - 500,   label = "Observed", hjust = 1.1,   color = "gray40", size = 4 ) +
#    annotate( "text", x = 4.5,  y = 7500,   label = "J=4 lists", hjust = 1.1,   color = "black", size = 5 ) +
    labs(x="Number of substrata", y="Estimates") +
    guides(color = guide_legend(title = "Priors:")) +
    theme(legend.position = "top") +
  ggtitle("J=4 lists")

ggsave(here("write/output/fix_shrinkage_in_simulation_dependent_J4.pdf"), height=3, width=4)

###############################################
# Fig 3. To confirm, we systematically increase the number of lists.
# (K=1 or 2 depending on whether it's dependent or independent)
###############################################

estimates_substrata_growing %>%
  filter(model == "R", priors %in% c("Uniform", "(1.1, 5)")) %>%
  mutate(n_lists = as.factor(n_lists)) %>%
  ggplot(aes(x=n_lists, y=estimates, color=priors)) +
    geom_violin(alpha=0.5, draw_quantiles=c(0.5), position=position_dodge(width=0.2)) +
    geom_hline(yintercept = estimates_substrata_growing$observed[1], linetype="dashed") +
    geom_hline(yintercept = estimates_substrata_growing$truth[1], linetype="solid") +
    annotate( "text", x = Inf,  y = estimates_substrata_growing$observed[1] - 180,   label = "Observed", hjust = 1.1,   color = "gray40", size = 4 ) +
    labs(x="Number of lists", y="Estimates") +
    guides(color = guide_legend(title = "Priors:")) +
    theme(legend.position = "top")

ggsave(here("write/output/fix_shrinkage_in_simulation_dependent_growing.pdf"), height=3, width=4)

###############################################
# Fig 4. Showing the problem is resolved with CO data
###############################################

df_plot <- summaries_co %>%
  filter(Dataset %in% c("CO_strata_s1a-combined", "CO_strata_s1b-combined", "CO_strata_s2a-combined", "CO_strata_s2b-combined",
                        "CO_superstrata_s1a", "CO_superstrata_s1b", "CO_superstrata_s2a", "CO_superstrata_s2b"),
         model == "R") %>%
  mutate(priors = recode_factor(priors, Uniform="Uniform"),
         type=recode_factor(type, Substrata="Substrata"),
         family=substr(strata, 1, 3))

x_unif <- which(levels(df_plot$priors) == "Uniform")

ggplot(df_plot, aes(x=priors, y=q500, ymin=q025, ymax=q975, color=type)) +
  annotate("rect",
           xmin = x_unif - 0.3, xmax = x_unif + 0.3,
           ymin = -Inf, ymax = Inf,
           fill = "yellow", alpha = 0.3) +
  geom_errorbar(width=0.2, position=position_dodge(width=0.3)) +
  geom_point(position=position_dodge(width=0.3)) +
  geom_point(aes(y=q025), position=position_dodge(width=0.3), shape=1) +
  geom_point(aes(y=q975), position=position_dodge(width=0.3), shape=1) +
  labs(x="Priors", y="Estimates") +
  facet_grid(family ~ ., scales = "free_y") +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL))

ggsave(here("write/output/shrinkage_exists_in_CO_but_we_fix_it.pdf"), height=5, width=6)
