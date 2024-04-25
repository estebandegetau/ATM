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


load(here("data/03_working/branches.RData"))

#---- Clean data ---------------------------------------------------------------

branches_m <- branches |>
  as_tibble() |>
  group_by(year, cve_ent, cve_mun) |>
  summarise(
    program = sum(program),
    private = sum(private)
  ) |>
  ungroup() |>
  left_join(municipios, by = c("cve_ent" = "CVE_ENT", "cve_mun" = "CVE_MUN")) |>
  st_sf()

years <- branches_m$year |> unique()

censo_m <- censo_l |>
  filter(LOC == "0000", MUN != "000", ENTIDAD != "00") |>
  full_join(municipios, by = c("ENTIDAD" = "CVE_ENT", "MUN" = "CVE_MUN")) |>
  mutate(year = list(years)) |>
  unnest(year) |>
  left_join(branches_m, by = c("ENTIDAD" = "cve_ent", "MUN" = "cve_mun", "year"),
            relationship = "one-to-many"
  ) |>
  mutate(
    branches_per_100k = (program + private) / POBTOT * 100000,
    program = ifelse(is.na(program), 0, program),
    private = ifelse(is.na(private), 0, private)
  ) |>
  st_sf()


#---- Save data ----------------------------------------------------------------

save(
  censo_l,
  file = here("data/03_working/census.RData")
)

save(
  localidades,
  agebs,
  municipios,
  entidades,
  file = here("data/03_working/shapes.RData")
)


save(
  branches_m,
  censo_m,
  file = here("data/03_working/municipios.RData"))
