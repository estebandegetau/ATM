rm(list = ls())
gc()

pacman::p_load(tidyverse, sf, patchwork, here)

denue_dir <- here::here("data", "01_raw", "denue")

#---- Read data ----------------------------------------------------------------

# Lee el archivo KML
pf <- c("Sucursales del Banco del Bienestar.kml")
st_layers(glue::glue("{denue_dir}/{pf}"))

centro <-
  st_read(glue::glue("{denue_dir}/{pf}"), layer = "Centro (CDMX, Edomex, Gro., Hgo., Mor., Pue. y Tlax.)")
noreste <-
  st_read(glue::glue("{denue_dir}/{pf}"), layer = "Noreste (Coah, Dur., NL, SLP y Tamps.)")
noroeste <-
  st_read(glue::glue("{denue_dir}/{pf}"), layer = "Noroeste (BC, BCS, Chih., Sin. y Son.)")
occidente <-
  st_read(glue::glue("{denue_dir}/{pf}"), layer = "Occidente (Ags., Col., Gto., Jal., Mich., Nay., Qro. y Zac.)")
sureste <-
  st_read(glue::glue("{denue_dir}/{pf}"), layer = "Sureste (Camp., Chia., Oax., QR, Tab., Ver. y Yuc.)")

## dde
glue::glue("{denue_dir}/{pf}")

MX <- glue::glue("{denue_dir}/Marco Geo/mg_2023_integrado/conjunto_de_datos/")
D23 <- glue::glue("{denue_dir}/denue_00_52_shp/conjunto_de_datos/")
mexico_map <- st_read(glue::glue("{MX}00ent.shp"))

# Sys.setenv(SHAPE_RESTORE_SHX="YES")
# no lee si no va el completo
s52 <- st_read(glue::glue("{D23}denue_inegi_52_.shp"), options = "ENCODING=ISO-8859-1")

#---- Clean data ----------------------------------------------------------------



ATMS <- s52 %>%
  filter(codigo_act == "522110") %>%
  filter(str_detect(
    string = (nom_estab),
    pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
  ) &
    !str_detect(
      string = (nom_estab),
      pattern = regex("BANCO", ignore_case = TRUE)
    ))

Suc <- s52 %>%
  filter(codigo_act == "522110") %>%
  filter(!str_detect(
    string = (nom_estab),
    pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
  ) &
    str_detect(
      string = (nom_estab),
      pattern = regex("BANCO", ignore_case = TRUE)
    ))

# bases
D22 <- glue::glue("{denue_dir}/denue_00_52_1122_shp/conjunto_de_datos/")
D21 <- glue::glue("{denue_dir}/denue_00_52_1121_shp/conjunto_de_datos/")
D20 <- glue::glue("{denue_dir}/denue_00_52_1120_shp/conjunto_de_datos/")


# 2022
bbdI22 <- st_read(glue::glue("{D22}denue_inegi_52_.shp"), options = "ENCODING=ISO-8859-1") %>%
  filter(codigo_act == "522210")

bbdI22 <- bbdI22 %>% filter(str_detect(
  string = tolower(nom_estab),
  pattern = regex("banco del bienestar|bansefi", ignore_case = TRUE)
))

# 2021
bbdI21 <- st_read(glue::glue("{D21}denue_inegi_52_.shp"),
  options = "ENCODING=ISO-8859-1"
)

bbdI21i <- bbdI21 %>%
  filter(codigo_act == "522210") %>%
  filter(str_detect(
    string = tolower(nom_estab),
    pattern = regex("banco del bienestar|bansefi", ignore_case = TRUE)
  ))

bbdI21ii <- bbdI21 %>%
  filter(codigo_act == "522110") %>%
  filter(!str_detect(
    string = (nom_estab),
    pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
  ) &
    str_detect(
      string = (nom_estab),
      pattern = regex("BANCO", ignore_case = TRUE)
    ))

# 2020
bbdI20 <- st_read(glue::glue("{D20}denue_inegi_52_.shp"),
  options = "ENCODING=ISO-8859-1"
)

bbdI20i <- bbdI20 %>%
  filter(codigo_act == "522210") %>%
  filter(str_detect(
    string = tolower(nom_estab),
    pattern = regex("banco del bienestar|bansefi", ignore_case = TRUE)
  ))

bbdI20ii <- bbdI20 %>%
  filter(codigo_act == "522110") %>%
  filter(!str_detect(
    string = (nom_estab),
    pattern = regex("atm|cajero|AUTOM[ÁA]TICO", ignore_case = TRUE)
  ) &
    str_detect(
      string = (nom_estab),
      pattern = regex("BANCO", ignore_case = TRUE)
    ))


#---- Plots --------------------------------------------------------------------

t1 <- ggplot(data = mexico_map) +
  geom_sf(fill = "#699672", color = "white") +
  geom_sf(data = Suc, color = "#091736") +
  geom_sf(data = centro, color = "#C83942") +
  geom_sf(data = noreste, color = "#C83942") +
  geom_sf(data = noroeste, color = "#C83942") +
  geom_sf(data = occidente, color = "#C83942") +
  geom_sf(data = sureste, color = "#C83942") +
  ggtitle("BB & BM / sucursales - 2023") +
  labs(caption = "BB= Banco del Bienestar, BM= Banca Múltiple") +
  theme_minimal() +
  coord_sf()

t2 <- ggplot(data = mexico_map) +
  geom_sf(fill = "#699672", color = "white") + # Draw the Mexico shapefile as the base layer
  geom_sf(data = bbdI21ii, color = "#091736") +
  geom_sf(data = bbdI21i, color = "#C83942") +
  ggtitle("BB & BM / sucursales - 2021") +
  labs(caption = "BB= Banco del Bienestar, BM= Banca Múltiple") +
  theme_minimal() +
  coord_sf()

ct1 <- t1 + t2

ggsave("MXT.png", plot = ct1, width = 25, height = 20, units = "cm")


#### evolución BB Regiones Oaxaca - CDMX

M1 <- ggplot(data = mexico_map %>% filter(CVE_ENT == "09" | CVE_ENT == "15")) +
  geom_sf(fill = "#699672", color = "white") + # Draw the Mexico shapefile as the base layer
  geom_sf(
    data = centro %>%
      filter(str_detect(
        string = tolower(Description),
        pattern = regex("CIUDAD DE MEXICO|MEXICO", ignore_case = TRUE)
      )),
    color = "#C83942"
  ) + # Add Centro layer points
  geom_sf(
    data = Suc %>%
      filter(cve_ent == "09" | cve_ent == "15"),
    color = "#091736"
  ) +
  ggtitle("BB/CDMX-MEX sucursales - 2023") +
  labs(caption = "BB= Banco del Bienestar, BM= Banca Múltiple") +
  coord_sf() +
  theme_minimal()

M2 <- ggplot(data = mexico_map %>% filter(CVE_ENT == "09" | CVE_ENT == "15")) +
  geom_sf(fill = "#699672", color = "white") + # Draw the Mexico shapefile as the base layer
  geom_sf(
    data = bbdI21i %>%
      filter(cve_ent == "09" | cve_ent == "15"),
    color = "#C83942"
  ) +
  geom_sf(
    data = bbdI21ii %>%
      filter(cve_ent == "09" | cve_ent == "15"),
    color = "#091736"
  ) +
  ggtitle("BB/CDMX-MEX sucursales - 2021") +
  coord_sf() +
  theme_minimal()

combined_maps <- M1 + M2
ggsave("cdmx.png", plot = combined_maps, width = 25, height = 20, units = "cm")

## oaxaca
OX1 <- ggplot(data = mexico_map %>% filter(CVE_ENT == "20")) +
  geom_sf(fill = "#699672", color = "white") + # Draw the Mexico shapefile as the base layer
  geom_sf(
    data = sureste %>%
      filter(str_detect(
        string = tolower(Description),
        pattern = regex("OAXACA", ignore_case = TRUE)
      )),
    color = "#C83942"
  ) + # Add Centro layer points
  geom_sf(
    data = Suc %>%
      filter(cve_ent == "20"),
    color = "#091736"
  ) +
  ggtitle("BB/Oaxaca sucursales - 2023") +
  labs(caption = "BB= Banco del Bienestar, BM= Banca Múltiple") +
  coord_sf() +
  theme_minimal()

OX2 <- ggplot(data = mexico_map %>% filter(CVE_ENT == "20")) +
  geom_sf(fill = "#699672", color = "white") + # Draw the Mexico shapefile as the base layer
  geom_sf(
    data = bbdI21i %>%
      filter(cve_ent == "20"),
    color = "#C83942"
  ) +
  geom_sf(
    data = bbdI21ii %>%
      filter(cve_ent == "20"),
    color = "#091736"
  ) +
  ggtitle("BB/Oaxaca - 2021") +
  coord_sf() +
  theme_minimal()

ox1 <- OX1 + OX2
ggsave("Oacaxa.png", plot = ox1, width = 25, height = 20, units = "cm")
