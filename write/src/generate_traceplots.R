library(pacman)

pacman::p_load(here, tidyverse, scales, patchwork, tools, forcats)

# cmdstanR needed for trace plots
# also, custom repository
if (!require("cmdstanr")) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  pacman::p_load(cmdstanr)
}

# Don't generate Rplots.pdf
pdf(NULL)

# This was a hack: better to read from hand.yaml
fns <- Sys.glob(here("write", "input", "fit", "LCMCR_*.rds")) 

path <- fns[[1]]
fn <- file_path_sans_ext(basename(path))
fn_short <- substr(fn, start=7, stop=10000)
starts_with_number <- suppressWarnings(!is.na(as.numeric(substr(fn_short, start=1, stop=1))))
fn_shortest <- fn_short
model_id <- 1
if (starts_with_number) {
    fn_shortest <- substr(fn_short, start=3, stop=10000)
    model_id <- substr(fn_short, start=1, stop=1)
}

stan_fit <- readRDS(path)

traces_all <- as.data.frame.table(stan_fit$draws()) %>%
  filter(variable %in% c("N") & chain %in% c(1, 2, 3, 4)) %>%
  mutate(iteration = as.numeric(iteration),
         Freq = as.numeric(Freq),
         chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"),
         display_name = fn_short,
         sort_name = fn_shortest,
         model_id = model_id)


for (i in 2:length(fns)) {
    path <- fns[[i]]

    # Sometimes the filenames start with LCMCR_i_
    # where i is an integer for the model number
    # 
    # I named the first model LCMCR_blah instead of LCMCR_1_blah, though
    # so need to take that into account too.
    # 
    # we want to sort the plot by the things that come after that prefix
    # but we want to plot the numbers in the labels
    #
    # this could be done via regexes but oh well

    fn <- file_path_sans_ext(basename(path))
    fn_short <- substr(fn, start=7, stop=10000)
    starts_with_number <- suppressWarnings(!is.na(as.numeric(substr(fn_short, start=1, stop=1))))
    fn_shortest <- fn_short
    model_id <- 1
    if (starts_with_number) {
        fn_shortest <- substr(fn_short, start=3, stop=10000)
        model_id <- substr(fn_short, start=1, stop=1)
    }

    stan_fit <- readRDS(path)

    traces <- as.data.frame.table(stan_fit$draws()) %>%
      filter(variable %in% c("N") & chain %in% c(1, 2, 3, 4)) %>%
      mutate(iteration = as.numeric(iteration),
             Freq = as.numeric(Freq),
             chain = recode_factor(chain, `1`="Chain 1",`2`="Chain 2",`3`="Chain 3", `4`="Chain 4"),
             display_name = fn_short,
             sort_name = fn_shortest,
             model_id = model_id)

    traces_all <- rbind(traces_all, traces)

    print(fn_short)
    print(tail(traces_all))
}

#traces_all <- arrange(traces_all, sort_name, display_name, iteration) %>%
#    mutate(display_name = fct_reorder(display_name, sort_name))

write.csv(traces_all, here("write", "output", "diagnostics", "traces.csv"), row.names=FALSE)

ggplot(traces_all) +
    geom_point(aes(x=iteration,y=Freq), alpha=0.3) +
    facet_grid(sort_name + model_id ~ chain, scales="free") +
    labs(x="Iteration", y="Value")

ggsave(here("write", "output", "diagnostics", "traceplots.pdf"), height=2 * length(fns), width=7, limitsize=FALSE)
