library(pacman)
pacman::p_load(here, yaml, dplyr, magrittr)

dataset_yaml_path <- here("import", "hand", "datasets.yaml")

if (!file.exists(dataset_yaml_path)) {
    stop("hand/datasets.yaml does not exist; no datasets to process")
}

dataset_spec <- read_yaml(dataset_yaml_path)
datasets <- dataset_spec$data

for (i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    df <- read.delim(here("import", "input", dataset$filename), sep=dataset$delimiter)
    df %>% select(dataset$list_names) %>%
        select_if(function(col) length(unique(col)) > 1) %>%
        write.csv(here("import", "output", paste(dataset$name, ".csv", sep="")), row.names=FALSE)
}
