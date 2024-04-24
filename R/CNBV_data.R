
rm(list = ls())
pacman::p_load( tidyverse,glue,openxlsx)
dir.create(glue::glue('{getwd()}/data/cnbv'))

dir_1 <- here::here("data","cnbv")
#ym1 <- as.character(2017:2023)


ym1 <- crossing(
  year = 2017:2024,
  month = sprintf("%02d", 1:12)) %>%mutate(yearmonth = paste0(year, month)) %>% pull(yearmonth) %>% .[. <= "202402"]

#operativa
op1 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_{x}.xlsx"),sheet = 2)

  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==31|cve_tipo_informacion==33|cve_tipo_informacion==34|cve_tipo_informacion==35|cve_tipo_informacion==37)%>%
    group_by(cve_periodo,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero) %>%
    summarise(num=sum(dat_num_total,na.rm = T)) %>%
    ungroup() %>% filter(num!=0) %>%  mutate(tipo="BM")

  print(x)
  return(df)
}
f1<-map(ym1, op1)
f1 <- bind_rows(f1)


f1<- f1 %>% mutate(year=str_sub(cve_periodo,1,4))%>%
  group_by(year,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero,tipo) %>%
  summarise( num = case_when(
    all(cve_tipo_informacion == 31|cve_tipo_informacion == 33|cve_tipo_informacion == 35) ~ mean(num,na.rm=T),
    all(cve_tipo_informacion == 34|cve_tipo_informacion == 37) ~ sum(num,na.rm=T),  TRUE ~ NA ))  %>%ungroup() %>%
  mutate(num=ceiling(num)) %>%
  select(-cve_tipo_informacion) %>%
  pivot_wider(names_from = dl_producto_financiero, values_from = num) %>% ungroup() %>% janitor::clean_names()


#operativa desarrollo
op2 <- function(x) {
  # Attempt to download and read the Excel file
  df <- tryCatch({
    df<- read.xlsx(glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BD_Operativa_{x}.xlsx"),
              sheet = 2)
  }, error = function(e) {
    message("Failed to download or read the file for ", x, ": ", e$message)
    return(NULL)  # Return NULL if there's an error
  })

  if (is.null(df)) {
    return(NULL)  # Skip further processing if file wasn't read
  }

  # Process the dataframe
  df <- df %>%
    mutate(c_mun = str_sub(cve_inegi, 4, 8)) %>%
    filter(cve_tipo_informacion %in% c(31, 33, 34,35,37)) %>%
    group_by(cve_periodo, c_mun, dl_estado, dl_municipio, cve_tipo_informacion, dl_producto_financiero, nombre_publicacion) %>%
    summarise(num = sum(dat_num_total, na.rm = TRUE), .groups = 'drop') %>%
    filter(num != 0, nombre_publicacion %in% c("Bansefi", "Banco del Bienestar")) %>%
    mutate(tipo = "BD")

  return(df)
}
f2<-map(ym1, op2)
f2 <- f2[-1] #
f2 <- bind_rows(f2)

f2<- f2 %>% mutate(year=str_sub(cve_periodo,1,4))%>%
  group_by(year,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero,tipo) %>%
  summarise( num = case_when(
    all(cve_tipo_informacion == 31|cve_tipo_informacion == 33|cve_tipo_informacion == 35) ~ mean(num,na.rm=T),
    all(cve_tipo_informacion == 34|cve_tipo_informacion == 37) ~ sum(num,na.rm=T),  TRUE ~ NA ))  %>% ungroup() %>%
  mutate(num=ceiling(num)) %>%
  select(-cve_tipo_informacion) %>%
  pivot_wider(names_from = dl_producto_financiero, values_from = num) %>% janitor::clean_names()



#captación multiple

c1 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Captaci%C3%B3n_{x}.xlsx"),sheet = 3)
  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==99)%>%
    group_by(cve_periodo,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero) %>%
    summarise(num=sum(dat_num_total,na.rm = T),
              liab=sum(dat_saldo_producto,na.rm = T)) %>% ungroup() %>% filter(num!=0) %>%  mutate(tipo="BM")
  print(x)
  return(df)
}

f3<-map(ym1, c1)
f3 <- bind_rows(f3)

f3<- f3 %>% mutate(year=str_sub(cve_periodo,1,4))%>%
  group_by(year,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero,tipo) %>%
  summarise( num=mean(num,na.rm = T),
             liab=mean(liab,na.rm = T))  %>%
  ungroup() %>%
  select(-cve_tipo_informacion) %>%
  janitor::clean_names()


# captación desarrollo

c2 <- function(x) {
  # Attempt to download and read the Excel file
  df <- tryCatch({
    read.xlsx(glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BD_Captación_{x}.xlsx"), sheet = 3)
  }, error = function(e) {
    message("Failed to download or read the file for ", x, ": ", e$message)
    return(NULL)  # Return NULL if there's an error
  })

  # Check if the data frame was successfully read
  if (is.null(df)) {
    return(NULL)  # Skip further processing if file wasn't read
  }

  # Process the dataframe if download/read was successful
  df <- df %>%
    mutate(c_mun = str_sub(cve_inegi, 4, 8)) %>%
    filter(cve_tipo_informacion == 99) %>%
    group_by(cve_periodo, c_mun, dl_estado, dl_municipio, cve_tipo_informacion, dl_producto_financiero, nombre_publicacion) %>%
    summarise(num = sum(dat_num_total, na.rm = TRUE),
              liab = sum(dat_saldo_producto, na.rm = TRUE), .groups = 'drop') %>%
    filter(num != 0) %>%
    filter(nombre_publicacion %in% c("Bansefi", "Banco del Bienestar")) %>%
    mutate(tipo = "BD")

  # Print the processed year-month identifier
  print(x)
  return(df)
}

f4<-map(ym1, c2)
f4 <- bind_rows(f4)

f4<- f4 %>% mutate(year=str_sub(cve_periodo,1,4))%>%
  group_by(year,c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero,tipo) %>%
  summarise( num=mean(num,na.rm = T),
             liab=mean(liab,na.rm = T))  %>%
  ungroup() %>%
  select(-cve_tipo_informacion) %>%
  janitor::clean_names()


salida <- list("bmop"=f1,
               "bdop"=f2,
               "salbm"=f3,
               "salbd"=f4)
openxlsx::write.xlsx(salida, file =glue('{dir_1}/cnvbdetalle.xlsx'), overwrite = TRUE)


#falta inegi/ deflactor /

