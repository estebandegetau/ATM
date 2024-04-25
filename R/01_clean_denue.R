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

pacman::p_load(tidyverse, sf, patchwork, here, labelled)

#---- Functions ----------------------------------------------------------------
# file <- files$file[1]

read_denue <- function(file) {
  path <-
    here("data/01_raw/denue",
         file,
         "conjunto_de_datos/denue_inegi_52_.shp")

  raw_st <- read_sf(path, options = "ENCODING=ISO-8859-1")

  raw_st |>
    filter(codigo_act %in% c("522110", "522210")) |>
    filter(!str_detect(
      nom_estab,
      regex("prenda|empeño|empeñar", ignore_case = TRUE)
    )) |>
    mutate(
      atm = str_detect(
        string = (nom_estab),
        pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
      ),
      full_branch = !atm,
      program = str_detect(
        string = tolower(nom_estab),
        pattern = regex("banco del bienestar|bansefi", ignore_case = TRUE)
      ),
      private = !program,
      entidad = str_to_title(entidad)
    ) |>
    select(!any_of("clee"))
}

#---- Files --------------------------------------------------------------------

files <- tibble(file = list.dirs(
  here("data/01_raw/denue"),
  full.names = F,
  recursive = F
)) |>
  filter(str_detect(file, regex("52", ignore_case = TRUE))) |>
  mutate(year = c(2020, 2021, 2022, 2023))


#---- Read data ----------------------------------------------------------------

denue <- files |>
  mutate(data = map(file, read_denue, .progress = T))

branches <- denue |>
  select(year, data) |>
  unnest(data) |>
  arrange(year, cve_ent, id) |>
  st_sf() |>
  set_variable_labels(
    year = "Year",
    fecha_alta = "Date of establishment",
    tipo_asent = "Type of settlement",
    per_ocu = "Number of employees",
    nom_estab = "Establishment name",
    atm = "ATM",
    full_branch = "Full branch",
    program = "Belongs to welfare program",
    private = "Private"
  )


mexico_map <- st_read(
  here(
    "data/01_raw/denue/Marco Geo/mg_2023_integrado/conjunto_de_datos/00ent.shp"
  ),
  options = "ENCODING=ISO-8859-1"
)

#---- Check data ---------------------------------------------------------------

# set.seed(20240422)
#
# banks_sample <- branches |>
#   sample_n(1000)
#
# mexico_map |>
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = banks_sample, aes(color = program))
#
# class(banks_sample)
#
# class(mexico_map)
#
# banks_sample |>
#   filter(cve_ent == "09") |>
#   ggplot() +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = mexico_map, fill = "darkgreen", alpha = 0.05)

#---- Save data ----------------------------------------------------------------

save(branches,
    mexico_map,
    file = here("data/03_working/branches.RData")
)

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

