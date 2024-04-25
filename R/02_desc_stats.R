#### ATM #######################################################################
#'
#' @name 02_desc_stats.R
#'
#' @description Descriptive statistics for the branch data
#'
#' @details Are private banks updated in the data?
#'
#' @author Esteban Degetau
#'
#' @created 2024-04-21
#'
#### Desc Stats #################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, sf, patchwork, here, gtsummary, fixest)

#---- Load data ----------------------------------------------------------------

load(here("data/03_working/branches.RData"))

#---- Descriptive statistics ---------------------------------------------------

branches |>
  as_tibble() |>
    select(
        year, program, private, full_branch, atm
    ) |>
    tbl_summary(
        by = year
    )

branches |>
  as_tibble() |>
  select(year, program, full_branch, atm) |>
  tbl_summary(
    by = program
  )

branches |>
  as_tibble() |>
  group_by(year, entidad) |>
  summarise(
    program = sum(program),
    private = sum(private)
  ) |>
  ungroup() |>
  pivot_longer(!c(entidad, year)) |>
  ggplot(aes(factor(year), value, color = name, group = name)) +
  geom_line() +
  facet_wrap(~entidad, scales = "free_y")

#' Regardless of whether the data for private branches have been updated, it is
#' still too early to see a response from private banks to the government's
#' program. There is no reading after the first showing of the program in 2023.

#---- Maps ---------------------------------------------------------------------



branches |>
  as_tibble() |>
  distinct(cve_ent, entidad) |>
  arrange(cve_ent) |>
  print(n = 32)

mexico_map |>
  ggplot() +
  geom_sf()

branches |>
  filter(cve_ent == "20") |>
  ggplot() +
  geom_sf(data = mexico_map %>% filter(CVE_ENT == "20")) +
  geom_sf(aes(color = program, geometry = geometry)) +
  facet_wrap( ~ year)

#---- Program expansion --------------------------------------------------------

branches |>
  as_tibble() |>
  group_by(entidad, year) |>
  summarise(
    n = n(),
    program = sum(program)
  ) |>
  ungroup() |>
  ggplot() +
  geom_col(aes(program, entidad, fill = as_factor(year)), position = "dodge") +
  facet_wrap(~year)
