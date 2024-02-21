#### ATM #######################################################################
#'
#' @name 01_open_denue.R
#'
#' @description Open DENUE shape files from .zip raw data.
#'
#' @author Esteban Degetau
#'
#' @created 2024-02-20
#'
#### Open DENUE ################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, here, lubridate)

#---- Paths --------------------------------------------------------------------

denue_dir <- here::here("data", "01_raw", "denue")

denue_files <- tibble(
  path = list.files(denue_dir, pattern = ".zip", full.names = TRUE),
  file = list.files(denue_dir, pattern = ".zip", full.names = FALSE)
) |>
  mutate(
    date = str_sub(file, 13, 16) |> my(),
    date = case_when(
      is.na(date) ~ my(1123, quiet = T),
      T ~ date
    )
  ) |>
  filter(month(date) == 11) |>
  mutate(year = year(date))

#---- Open DENUE ---------------------------------------------------------------

for (i in 1:nrow(denue_files)) {
  year <- denue_files$year[i]
  exdir_i <- here::here("data", "02_temp", "denue", year)
  unzip(denue_files$path[i], exdir = exdir_i)
  cat("Unzipped DENUE ", year, "\n")
}
