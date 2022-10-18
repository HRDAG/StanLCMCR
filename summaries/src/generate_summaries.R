library(pacman)
pacman::p_load(here, LCMCR, cmdstanr, yaml, tidyverse)

summaries_spec <- read_yaml(here("summaries", "hand", "summaries.yaml"))
model_names <- summaries_spec$models
dataset_names <- summaries_spec$datasets

df_divergences <- data.frame(num.divergent=numeric(), prop.divergent=numeric(), model=character(), Dataset=character())
df_estimates <- data.frame(estimates=numeric(), model=character(), Dataset=character())

for (j in 1:length(dataset_names)) {
    dataset_name <- dataset_names[j]
    R_estimates <- readRDS(here("summaries", "input", paste("R_", dataset_name, "_estimates.rds", sep="")))
    df_estimates <- bind_rows(df_estimates, tibble(estimates=R_estimates, model="R", Dataset=dataset_name))

    for (i in 1:length(model_names)) {
        model_name <- model_names[i]

        fitted <- readRDS(here("summaries", "input", paste(model_name, "_", dataset_name, ".rds", sep="")))

        estimates_df <- as.data.frame(fitted$draws("N"))
        estimates <- apply(estimates_df, 1, base::sample, size = 1)
        num.divergent <- sum(fitted$diagnostic_summary(quiet=TRUE)$num_divergent)
        prop.divergent <- num.divergent / (dim(estimates_df)[1] * dim(estimates_df)[2])

        df_divergences <- bind_rows(df_divergences, tibble(num.divergent = num.divergent, prop.divergent = prop.divergent, model=model_name, Dataset=dataset_name))
        df_estimates <- bind_rows(df_estimates, tibble(estimates=estimates, model=model_name, Dataset=dataset_name))
    }
}

df_summaries <- df_estimates %>% group_by(model, Dataset) %>%
    summarize(
      q025 = quantile(estimates, 0.025),
      q500 = quantile(estimates, 0.5),
      q975 = quantile(estimates, 0.975),
      ci_95_length = q975 - q025,
      mean = mean(estimates),
      sd = sd(estimates),
    ) %>% merge(df_divergences, by=c("Dataset", "model"), all=TRUE)

write.csv(df_estimates, here("summaries", "output", "estimates.csv"), row.names=FALSE)
write.csv(df_summaries, here("summaries", "output", "summaries.csv"), row.names=FALSE)