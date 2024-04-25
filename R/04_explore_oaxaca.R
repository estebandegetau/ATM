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

pacman::p_load(tidyverse, sf, patchwork, here, labelled, fixest)

#---- Load data ----------------------------------------------------------------

load(here("data/03_working/municipios.RData"))

load(here("data/03_working/shapes.RData"))

load(here("data/03_working/branches.RData"))

#---- Explore Oaxaca ------------------------------------------------------------



ggplot() +
  geom_sf(data = censo_m |>
            filter(ENTIDAD == "20"),
          aes(fill = log(POBTOT))) +
  geom_sf(data = branches |>
            filter(cve_ent == "20", program == T),
          aes(geometry = geometry),
          alpha = 0.8,
          size = 0.8,
          color = "white"
          ) +
  facet_wrap( ~ year) +
  scale_fill_viridis_c(
    labels = ~ exp(.x) |> scales::comma(),
    breaks = log(c(1000, 10000, 100000, 1000000)))


ggplot() +
  geom_sf(
    data = municipios |>
      filter(CVE_ENT == "20")
  )  +
  geom_sf(
    data = branches_m |>
      filter(cve_ent == "20"),
    aes(fill = program + private)
  ) +
  scale_fill_viridis_c() +
  facet_wrap( ~ year)


ggplot() +
  geom_sf(
    data = censo_m |>
      filter(ENTIDAD == "20"),
    aes(fill = asinh(branches_per_100k))
  ) +
  facet_wrap( ~ year) +
  scale_fill_viridis_c(
    labels = ~ .x |> sinh(),
    breaks = asinh(c(1, 10, 30, 100, 300))
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Branches per 100k inhabitants",
    fill = "Branches per 100k inhabitants"
  )

#---- Regressions --------------------------------------------------------------

censo_m |>
  feols(
    program ~ log(POBTOT) | MUN + year

  )


