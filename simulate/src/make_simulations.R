library(pacman)
pacman::p_load(here, yaml, dplyr, magrittr, LCMCR)

simulate_mse_dataset <- function(N, list_capture_probs) {
  membership_in_lists <- data.frame(list(V1=as.numeric(runif(N) < list_capture_probs[1]) ))
  J <- length(list_capture_probs)
  
  for (j in 2:J) {
    membership_in_lists[,j] = as.numeric(runif(N) < list_capture_probs[j])
  }
  
  membership_in_lists
}

# Configuration file for simulations
simulations <- read_yaml(here("simulate", "hand", "simulations.yaml"))

# Random seed
set.seed(simulations$seed)

datasets <- simulations$data

for (i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    row <- dataset$rows[[1]]
    rows <- as.data.frame(simulate_mse_dataset(row$size, row$probs))
    for (j in 2:length(dataset$rows))
    {
        row <- dataset$rows[[j]]
        rows <- rbind(rows, simulate_mse_dataset(row$size, row$probs))
    }

    df <- rbind(rows)

    # Remove all of the rows unobserved by any of the lists
    df <- df %>% filter(rowSums(across(everything())) > 0)

    write.csv(df, here("simulate", "output", paste(dataset$name, ".csv", sep="")), row.names=FALSE)
}

# From the LCMCR dataset
data("kosovo_aggregate")
kosovo <- as_tibble(sapply(kosovo_aggregate, as.numeric) - 1) 
write.csv(kosovo, here("simulate", "output", "kosovo.csv"), row.names=FALSE)
