#### ATM #######################################################################
#'
#' @name 04_explore_oaxaca.R
#'
#' @description
#' Derive descriptive statistics from the 2020 census data for Oaxaca.
#'
#' @author Esteban Degetau
#'
#' @created 2024-04-24
#'
#### Explore Oaxaca ############################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, sf, patchwork, here, labelled)

#---- Load data ----------------------------------------------------------------

load(here("data/03_working/census.RData"))

load(here("data/03_working/branches.RData"))

#---- Explore Oaxaca ------------------------------------------------------------

branches_m <- branches |>
  as_tibble() |>
  group_by(year, cve_ent, cve_mun) |>
  summarise(
    program = sum(program),
    private = sum(private)
  )

censo_m <- censo_l |>
  filter(LOC == "0000", MUN != "000", ENTIDAD != "00") |>
  full_join(municipios, by = c("ENTIDAD" = "CVE_ENT", "MUN" = "CVE_MUN")) |>
  st_sf()



ggplot() +
  geom_sf(data = censo_m |>
            filter(ENTIDAD == "20"),
          aes(fill = log(POBTOT))) +
  geom_sf(data = branches |> filter(cve_ent == "20"),
          aes(color = program, geometry = geometry)) +
  facet_wrap(~year)

agebs |> names()

censo_l |> names()

censo_l |>
  filter(ENTIDAD == "20") |>
  distinct(MUN, LOC)

localidades |>
  as_tibble() |>
  filter(CVE_ENT == "20") |>
  distinct(CVE_MUN, CVE_LOC)

