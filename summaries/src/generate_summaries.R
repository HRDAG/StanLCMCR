library(pacman)
pacman::p_load(here, LCMCR, cmdstanr, yaml, tidyverse,
               fpeek # for wordcount on systems without wc
)

summaries_spec <- read_yaml(here("summaries", "hand", "summaries.yaml"))
dataset_names <- summaries_spec$datasets

###############################################
# Estimates and summaries for main models/datasets
###############################################

df_divergences <- data.frame(num.divergent=numeric(), prop.divergent=numeric(), model=character(), Dataset=character())
df_estimates <- data.frame(estimates=numeric(), model=character(), Dataset=character(), expfacs=numeric())

for (j in 1:length(dataset_names)) {
    dataset <- dataset_names[[j]]
    dataset_name <- dataset$name

    # We're just going to count the lines on the original dataset, and subtract one for the header, rather than read the whole thing in again
    observed <- peek_count_lines(here("summaries", "input", "import", paste(dataset_name, ".csv", sep=""))) - 1

    R_estimates <- readRDS(here("summaries", "input", "fit", paste("R_", dataset_name, "_estimates.rds", sep="")))
    expfacs <- R_estimates / observed
    df_estimates <- bind_rows(df_estimates, tibble(estimates=R_estimates, model="R", Dataset=dataset_name, expfacs=expfacs))

    model_names <- dataset$models

    for (i in 1:length(model_names)) {
        model_name <- model_names[i]

        fitted <- readRDS(here("summaries", "input", "fit", paste(model_name, "_", dataset_name, ".rds", sep="")))

        estimates_df <- as.data.frame(fitted$draws("N"))
        estimates <- apply(estimates_df, 1, base::sample, size = 1)
        expfacs <- estimates / observed

        num.divergent <- sum(fitted$diagnostic_summary(quiet=TRUE)$num_divergent)
        prop.divergent <- num.divergent / (dim(estimates_df)[1] * dim(estimates_df)[2])

        df_divergences <- bind_rows(df_divergences, tibble(num.divergent = num.divergent, prop.divergent = prop.divergent, model=model_name, Dataset=dataset_name))
        df_estimates <- bind_rows(df_estimates, tibble(estimates=estimates, model=model_name, Dataset=dataset_name, expfacs=expfacs))
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
      q025_expfac = quantile(expfacs, 0.025),
      q500_expfac = quantile(expfacs, 0.5),
      q975_expfac = quantile(expfacs, 0.975),
      mean_expfac = mean(expfacs),
    ) %>% merge(df_divergences, by=c("Dataset", "model"), all=TRUE)

write.csv(df_estimates, here("summaries", "output", "estimates.csv"), row.names=FALSE)
write.csv(df_summaries, here("summaries", "output", "summaries.csv"), row.names=FALSE)

###############################################
# Estimates and summaries for alpha sweeps
###############################################

alpha_sweep_model <- summaries_spec$alpha_sweep$model
alpha_sweep_dataset <- summaries_spec$alpha_sweep$dataset
alphas <- summaries_spec$alpha_sweep$alphas

df_divergences_alpha <- data.frame(num.divergent=numeric(), prop.divergent=numeric(), alpha=numeric())
df_estimates_alpha <- data.frame(estimates=numeric(), alpha=numeric())

for (i in 1:length(alphas)) {
    alpha <- alphas[[i]]
    fitted <- readRDS(here("summaries", "input", "fit", paste(alpha_sweep_model, "_", alpha_sweep_dataset, "_", alpha, ".rds", sep="")))

    estimates_df <- as.data.frame(fitted$draws("N"))
    estimates <- apply(estimates_df, 1, base::sample, size = 1)
    num.divergent <- sum(fitted$diagnostic_summary(quiet=TRUE)$num_divergent)
    prop.divergent <- num.divergent / (dim(estimates_df)[1] * dim(estimates_df)[2])

    df_divergences_alpha <- bind_rows(df_divergences_alpha, tibble(num.divergent = num.divergent, prop.divergent = prop.divergent, alpha=alpha))
    df_estimates_alpha <- bind_rows(df_estimates_alpha, tibble(estimates=estimates, alpha=alpha))
}


df_summaries_alpha <- df_estimates_alpha %>% group_by(alpha) %>%
    summarize(
      q025 = quantile(estimates, 0.025),
      q500 = quantile(estimates, 0.5),
      q975 = quantile(estimates, 0.975),
      ci_95_length = q975 - q025,
      mean = mean(estimates),
      sd = sd(estimates),
    ) %>% merge(df_divergences_alpha, by=c("alpha"), all=TRUE)

write.csv(df_estimates_alpha, here("summaries", "output", "estimates_alphas.csv"), row.names=FALSE)
write.csv(df_summaries_alpha, here("summaries", "output", "summaries_alphas.csv"), row.names=FALSE)
