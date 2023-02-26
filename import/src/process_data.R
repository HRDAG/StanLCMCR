library(pacman)
pacman::p_load(here, yaml, dplyr, magrittr)

dataset_yaml_path <- here("import", "hand", "datasets.yaml")

if (!file.exists(dataset_yaml_path)) {
    stop("hand/datasets.yaml does not exist; no datasets to process")
}

dataset_spec <- read_yaml(dataset_yaml_path)
families <- dataset_spec$families

for (i in 1:length(families)) {
    family <- families[[i]]

    datasets <- family$data

    for (i in 1:length(datasets)) {
        dataset <- datasets[[i]]
        df <- read.delim(here("import", "input", dataset$filename), sep=dataset$delimiter)
        df %>% select(family$list_names) %>%
            select_if(function(col) length(unique(col)) > 1) %>%
            mutate(across(where(is.character), function(x) {as.numeric(as.logical(x))})) %>%
            write.csv(here("import", "output", paste(dataset$name, ".csv", sep="")), row.names=FALSE)
    }
}

