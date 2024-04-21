#### ATM #######################################################################
#' 
#' @title 01_clean_denue.R
#' 
#' @description This script cleans the DENUE data for the ATM project.
#' 
#' @author Esteban Degetau, José Ángel Alonso
#' 
#' @created 2024-04-21
#' 
#### Clean DENUE ################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, sf, patchwork, here)

#---- Functions ----------------------------------------------------------------
# file <- files$file[1]

read_denue <- function(file) {
    path <- here("data/01_raw/denue", file, "conjunto_de_datos/denue_inegi_52_.shp")

    raw_st <- st_read(path, options = "ENCODING=ISO-8859-1")

    raw_st |>
        filter(codigo_act == "522110") |>
        mutate(
            full_branch = !str_detect(
                string = (nom_estab),
                pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
            ) &
                !str_detect(
                    string = (nom_estab),
                    pattern = regex("BANCO", ignore_case = TRUE)
                ),
            program = str_detect(
                string = tolower(nom_estab),
                pattern = regex("banco del bienestar|bansefi", ignore_case = TRUE)
            )
        ) |>
        select(!any_of("clee"))
}

#---- Files --------------------------------------------------------------------

files <- tibble(
    file = list.dirs(here("data/01_raw/denue"),
        full.names = F, recursive = F
    )
) |>
    filter(str_detect(file, regex("52", ignore_case = TRUE))) |>
    mutate(year = c(2020, 2021, 2022, 2023))


#---- Read data ---------------------------------------------------------------- 

denue <- files |>
    mutate(data = map(file, read_denue, .progress = T) ) 

#---- Save data ----------------------------------------------------------------

banks <- denue |>
    select(year, data) |>
    unnest(data)

banks |> save(file = here("data/03_working/banks.RData"))

# banks |> sample_n(1000) |> view()

# denue |>
#     mutate(
#         names = map(data, names)
#     ) |>
#     select(year, names) |>
#     unnest(names) |>
#     group_by(names) |>
#     tally() |>
#     filter(n != 4)

