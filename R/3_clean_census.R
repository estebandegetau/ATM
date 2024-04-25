#### ATM #######################################################################
#'
#' @name 03_clean_census.R
#'
#' @description
#' Clean 2020 census data at ITER level and assign geo reference.
#'
#' @author Esteban Degetau
#'
#' @created 2024-04-24
#'
#### Clean census ##############################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, sf, patchwork, here, labelled)

#---- Read data ----------------------------------------------------------------

censo_l <-
  read_csv(
    here(
      "data/01_raw/censo/iter_00_cpv2020_csv/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv"
    )
  )

# censo_l <- censo_l |>
#   filter(ENTIDAD != "00", MUN != "000", LOC != "0000")


localidades <-
  st_read(here(
    "data/01_raw/censo/Marco Geo/MG_2020_Integrado/conjunto_de_datos/00l.shp"
  ))

agebs <-
  st_read(here(
    "data/01_raw/censo/Marco Geo/MG_2020_Integrado/conjunto_de_datos/00a.shp"
  ))

municipios <-
  st_read(
    here(
      "data/01_raw/censo/Marco Geo/MG_2020_Integrado/conjunto_de_datos/00mun.shp"
    )
  )

entidades <-
  st_read(
    here(
      "data/01_raw/censo/Marco Geo/MG_2020_Integrado/conjunto_de_datos/00ent.shp"
    )
  )


#---- Save data ----------------------------------------------------------------

save(
  censo_l,
  localidades,
  agebs,
  municipios,
  entidades,
  file = here("data/03_working/census.RData")
)
