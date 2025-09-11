library(pacman)
pacman::p_load(here, LCMCR, cmdstanr, yaml, dplyr,
               R.utils # for wordcount on systems without wc
)

summaries_spec <- read_yaml(here("summaries", "hand", "summaries.yaml"))

dataset_names <- summaries_spec$datasets
global_models <- summaries_spec$models
R_priors <- summaries_spec$R_priors
iterations <- as.numeric(summaries_spec$iterations)

###############################################
# Estimates and summaries for main models/datasets
###############################################

df_divergences <- data.frame(num.divergent=numeric(), prop.divergent=numeric(), model=character(), Dataset=character(), family=character())
df_estimates <- data.frame(estimates=numeric(), model=character(), priors=character(), Dataset=character(), expfacs=numeric(), family=character(), i=numeric(), truth=numeric(), observed=numeric())

for (j in 1:length(dataset_names)) {
    dataset <- dataset_names[[j]]
    dataset_name <- dataset$name
    truth <- dataset$ground_truth
    family <- ifelse(is.null(dataset$family), NA, dataset$family)

    # We're just going to count the lines on the original dataset, and subtract one for the header, rather than read the whole thing in again
    observed <- countLines(here("summaries", "input", "import", paste(dataset_name, ".csv", sep=""))) - 1

    print(c(dataset_name, family, observed))
    
    R_estimate_fn <- here("summaries", "input", "fit", paste("R__", dataset_name, "_estimates.rds", sep=""))

    if (file.exists(R_estimate_fn)) {
        R_estimates <- readRDS(R_estimate_fn)
        expfacs <- R_estimates / observed
        est_df <- tibble(estimates=R_estimates, model="R", priors="Uniform", Dataset=dataset_name, expfacs=expfacs, family=family, observed=observed, truth=truth) |> mutate(i=row_number())
        df_estimates <- bind_rows(df_estimates, est_df)
    }

    for (i in 1:length(R_priors)) {
        prior = R_priors[i]
        model_name <- paste("R", prior, sep="_")
        fn <- here("summaries", "input", "fit", paste(model_name, "_", dataset_name, "_estimates.rds", sep=""))

        if (!file.exists(fn)) {
            print(paste("Warning:", fn, "does not exist", sep=" "))
            next
        }

        R_estimates <- readRDS(fn)

        expfacs <- R_estimates / observed
        est_df <- tibble(estimates=R_estimates, model="R", priors=paste("(1.1, ", prior, ")", sep=""), Dataset=dataset_name, expfacs=expfacs, family=family, observed=observed, truth=truth) |> mutate(i=row_number())
        df_estimates <- bind_rows(df_estimates, est_df)
    }

    model_names <- dataset$models

    if (is.null(model_names)) {
        model_names <- global_models
    }

    for (i in 1:length(model_names)) {
        model_name <- model_names[i]
        priors = "Uniform"
        if (startsWith(model_name, "LCMCR_7")) {
            priors = paste("(1.1, ", substr(model_name, 9, 100), ")", sep="")
        }

        fitted <- readRDS(here("summaries", "input", "fit", paste(model_name, "_", dataset_name, ".rds", sep="")))

        estimates_df <- as.data.frame(fitted$draws("N"))
        estimates <- apply(estimates_df, 1, base::sample, size = 1)
        expfacs <- estimates / observed

        num.divergent <- sum(fitted$diagnostic_summary(quiet=TRUE)$num_divergent)
        prop.divergent <- num.divergent / (dim(estimates_df)[1] * dim(estimates_df)[2])

        est_df <- tibble(estimates=estimates, model="Stan", priors=priors, Dataset=dataset_name, expfacs=expfacs, family=family, truth=truth, observed=observed) |> mutate(i=row_number())
        df_divergences <- bind_rows(df_divergences, tibble(num.divergent = num.divergent, prop.divergent = prop.divergent, model="Stan", priors=priors, Dataset=dataset_name, family=family))
        df_estimates <- bind_rows(df_estimates, est_df)
    }
}

#df_estimates_non_family <- df_estimates |> filter(is.na(family)) |> 
#    select(-c(family))

df_estimates_non_family <- df_estimates |>
    select(-c(family))

print(colnames(df_estimates_non_family))

df_estimates_family <- df_estimates |> filter(!is.na(family)) |>
    select(-c(Dataset)) |>
    rename(Dataset=family) |>
    group_by(Dataset, model, priors, i) |>
    summarize(estimates = sum(estimates),
             truth = sum(truth),
             observed = sum(observed),
             expfacs = estimates / observed)

print(colnames(df_estimates_family))

df_estimates <- rbind(df_estimates_non_family, df_estimates_family)

df_divergences_family <- df_divergences |> filter(is.na(family)) |> select(-family)
print(colnames(df_divergences_family))

df_divergences_non_family <- df_divergences |> filter(!is.na(family)) |>
                                        select(-c(Dataset)) |>
                                        rename(Dataset = family) |>
                                        group_by(model, priors, Dataset) |>
                                        summarize(num.divergent = sum(num.divergent),
                                                  prop.divergent = num.divergent / iterations)
print(colnames(df_divergences_non_family))

df_divergences_cleaned <- rbind(df_divergences_family, df_divergences_non_family)
                                        
df_summaries <- df_estimates |> group_by(model, priors, Dataset) |>
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
      truth = mean(truth),
      observed = mean(observed),
      expfac_truth = truth / observed
      ) |> 
           merge(df_divergences_cleaned, by=c("Dataset", "model", "priors"), all=TRUE)


write.csv(df_estimates, here("summaries", "output", "estimates.csv"), row.names=FALSE)
write.csv(df_summaries, here("summaries", "output", "summaries.csv"), row.names=FALSE)

###############################################
# Estimates and summaries for alpha sweeps
###############################################
#alpha_sweep_model <- summaries_spec$alpha_sweep$model
#alpha_sweep_dataset <- summaries_spec$alpha_sweep$dataset
#alphas <- summaries_spec$alpha_sweep$alphas
#
#df_divergences_alpha <- data.frame(num.divergent=numeric(), prop.divergent=numeric(), alpha=numeric())
#df_estimates_alpha <- data.frame(estimates=numeric(), alpha=numeric())
#
#for (i in 1:length(alphas)) {
#    alpha <- alphas[[i]]
#
#    fitted <- readRDS(here("summaries", "input", "fit", paste(alpha_sweep_model, "_", alpha_sweep_dataset, "_", alpha, ".rds", sep="")))
#
#    estimates_df <- as.data.frame(fitted$draws("N"))
#    estimates <- apply(estimates_df, 1, base::sample, size = 1)
#    num.divergent <- sum(fitted$diagnostic_summary(quiet=TRUE)$num_divergent)
#    prop.divergent <- num.divergent / (dim(estimates_df)[1] * dim(estimates_df)[2])
#
#    df_divergences_alpha <- bind_rows(df_divergences_alpha, tibble(num.divergent = num.divergent, prop.divergent = prop.divergent, alpha=alpha))
#    df_estimates_alpha <- bind_rows(df_estimates_alpha, tibble(estimates=estimates, alpha=alpha))
#}
#
#
#df_summaries_alpha <- df_estimates_alpha |> group_by(alpha) |>
#    summarize(
#      q025 = quantile(estimates, 0.025),
#      q500 = quantile(estimates, 0.5),
#      q975 = quantile(estimates, 0.975),
#      ci_95_length = q975 - q025,
#      mean = mean(estimates),
#      sd = sd(estimates),
#    ) |> merge(df_divergences_alpha, by=c("alpha"), all=TRUE)
#
#write.csv(df_estimates_alpha, here("summaries", "output", "estimates_alphas.csv"), row.names=FALSE)
#write.csv(df_summaries_alpha, here("summaries", "output", "summaries_alphas.csv"), row.names=FALSE)
