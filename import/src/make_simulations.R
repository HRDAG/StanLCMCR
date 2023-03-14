library(pacman)
pacman::p_load(here, yaml, dplyr, magrittr, LCMCR)

import_mse_dataset <- function(N, list_capture_probs) {
  membership_in_lists <- data.frame(list(V1=as.numeric(runif(N) < list_capture_probs[1]) ))
  J <- length(list_capture_probs)
  
  for (j in 2:J) {
    membership_in_lists[,j] = as.numeric(runif(N) < list_capture_probs[j])
  }
  
  membership_in_lists
}

# Configuration file for simulations
simulations <- read_yaml(here("import", "hand", "simulations.yaml"))

# Random seed
set.seed(simulations$seed)

datasets <- simulations$data

for (i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    row <- dataset$rows[[1]]
    rows <- as.data.frame(import_mse_dataset(row$size, row$probs))

    if (length(dataset$rows) > 1) {
        for (j in 2:length(dataset$rows))
        {
            row <- dataset$rows[[j]]
            rows <- rbind(rows, import_mse_dataset(row$size, row$probs))
        }
    }

    df <- rbind(rows)

    # Remove all of the rows unobserved by any of the lists
    df <- df %>% filter(rowSums(across(everything())) > 0)

    # If we are growing columns
    cols_to_grow <- dataset$grow_columns

    if (is.null(cols_to_grow)) {
        write.csv(df, here("import", "output", paste(dataset$name, ".csv", sep="")), row.names=FALSE)

        # If we are breaking into substrata:

        substrata_sizes <- dataset$number_of_substrata
        for (n in substrata_sizes) {
            nr <- nrow(df)
            subdfs <- split(df, rep(1:n, each=ceiling(nr/n), length.out=nr))
            for (j in 1:length(subdfs)) {
                subdf <- subdfs[[j]]
                write.csv(subdf, here("import", "output", paste(dataset$name, "-", n, "-", j, ".csv", sep="")), row.names=FALSE)
            }
        }
    }
    else {
        for (n in cols_to_grow) {
            df_subset <- df[1:n] %>% filter(rowSums(across(everything())) > 0)
            write.csv(df_subset, here("import", "output", paste(dataset$name, "-", n, ".csv", sep="")), row.names=FALSE)
        }
    }
}

# From the LCMCR dataset
data("kosovo_aggregate")
kosovo <- as_tibble(sapply(kosovo_aggregate, as.numeric) - 1) 
write.csv(kosovo, here("import", "output", "kosovo.csv"), row.names=FALSE)
