#### ATM #######################################################################
#'
#' @name 03_distnaces.R
#'
#' @description Calculate distances between branches
#'
#' @author Esteban Degetau
#'
#' @created 2024-04-22
#'
#### Distances #################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, sf, patchwork, here, geosphere)

#---- Load data ----------------------------------------------------------------

load(here("data/03_working/branches.RData"))

#---- Calculate distances ------------------------------------------------------

# a <- branches |>
#   filter(year == "2023" & cve_ent == "01") |>
#   st_distance(by_element = F)
#
# a_long <- a |>
#   as_tibble() |>
#   rowid_to_column() |>
#   pivot_longer(cols = !rowid) |>
#   mutate(name = str_remove(name, "V") |> as.integer())
#
# a_long |>
#   filter(rowid != name) |>
#   group_by(rowid) |>
#   summarise(mean_dist = mean(value, na.rm = T),
#             min_dist = min(value, na.rm = T),
#             max_dist = max(value, na.rm = T),
#             sd_dist = sd(value, na.rm = T))

distances <- branches |>
  nest(.by = c(year, entidad)) |>
  mutate(dist_matrix = map(data,
                           \(x) st_distance(x, by_element = F) |>
                             as_tibble(),
                           .progress = T))

# save(distances, file = here("data/03_working/distances.RData")
# )

#---- Compute statistics -------------------------------------------------------



stats <- distances |>
  mutate(
    dist_long = map(dist_matrix,\(x) x |>
                      as_tibble() |>
                      rowid_to_column() |>
                      pivot_longer(cols = !rowid) |>
                      mutate(name = str_remove(name, "V") |> as.integer()), .progress = T),
    stats = map(dist_long,
                \(x) x |>
                  filter(rowid != name) |>
                  group_by(rowid) |>
                  summarise(mean_dist = mean(value, na.rm = T),
                            min_dist = min(value, na.rm = T),
                            max_dist = max(value, na.rm = T),
                            sd_dist = sd(value, na.rm = T)), .progress = T)
  )
