library(pacman)
pacman::p_load(here, yaml, tidyverse)

co_yaml_path <- here("import", "hand", "CO.yaml")
co_raw_path <- here("import", "input", "mr-homicidio-R1.csv")
co_dept_groups_path <- here("import", "input", "dept-groups.csv")
co_regions_path <- here("import", "input", "regiones_onic.csv")

if (!file.exists(co_raw_path)) {
    stop("input/mr-homicidio-R1.csv does not exist")
}
if (!file.exists(co_dept_groups_path)) {
    stop("input/dept-groups.csv does not exist")
}
if (!file.exists(co_regions_path)) {
    stop("input/regiones_onic.csv does not exist")
}

strata_spec <- read_yaml(co_yaml_path)
co_dept_groups <- read.delim(co_dept_groups_path, sep="|")
co_raw <- read_csv(co_raw_path)
co_regions <- read.delim(co_regions_path, sep=";") |>
    select(dept_code, muni_code, region_onic) |>
    rename(dept_code_hecho=dept_code, muni_code_hecho=muni_code)

print("Merging inputs...")

co <- co_raw |> filter(is_conflict == TRUE) |> 
    merge(co_dept_groups, by="dept_code_hecho") |> 
    merge(co_regions, by=c("dept_code_hecho", "muni_code_hecho")) |>
    mutate(pert_etnica = case_when(etnia == "ROM" ~ "pueblo_etnico",
                                   etnia == "NARP" ~ "pueblo_etnico",
                                   etnia == "INDIGENA" ~ "pueblo_etnico",
                                   etnia == "MESTIZO" ~ "mestizo", TRUE ~ as.character(etnia))) |>
    mutate(edad_cat = if_else(edad < 18, "MENOR", "ADULTO"))

print("Forming strata...")

strata <- strata_spec$strata
all_strata <- list()

for (i in 1:length(strata)) {
    stratum <- strata[[i]]
    stratum_num <- stratum$stratum
    criteria <- stratum$criteria

    cols <- names(criteria)
    cols <- cols[! cols %in% c("yy_hecho_start", "yy_hecho_end") ]

    yy_hecho_start <- criteria$yy_hecho_start
    yy_hecho_end <- criteria$yy_hecho_end

    co_stratum <- filter(co, yy_hecho >= yy_hecho_start & yy_hecho <= yy_hecho_end)
    for (j in 1:length(cols)) {
        colname <- cols[[j]]
        co_stratum <- co_stratum[co_stratum[[colname]] == criteria[[colname]],]
    }

    all_strata[[stratum_num]] <- co_stratum

    write.csv(co_stratum, file=here("import", "output", paste("CO_strata_", stratum_num, ".csv", sep="")))
    print(paste(stratum_num, nrow(co_stratum)))
}

superstrata <- strata_spec$superstrata
for (superstratum in superstrata) {
    superstratum_name <- superstratum$name
    superstratum_strata <- superstratum$strata

    superstratum_df <- all_strata[[superstratum_strata[[1]]]]

    for (j in 2:length(superstratum_strata)) {
        s <- superstratum_strata[[j]]
        superstratum_df <- rbind(superstratum_df, all_strata[[j]])
    }

    write.csv(superstratum_df, file=here("import", "output", paste("CO_superstrata_", superstratum_name, ".csv", sep="")))
    print(paste(superstratum_name, nrow(superstratum_df)))
}
