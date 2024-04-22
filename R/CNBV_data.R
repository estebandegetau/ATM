
rm(list = ls())
pacman::p_load( tidyverse,glue,openxlsx)

dir_1 <- here::here("data", "01_raw", "denue")

#operativa
op1 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_{x}12.xlsx"),sheet = 2)

  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==31|cve_tipo_informacion==33|cve_tipo_informacion==35)%>% group_by(c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero) %>%
    summarise(num=sum(dat_num_total,na.rm = T)) %>% ungroup() %>% filter(num!=0) %>%  mutate(y=x)

  print(x)
  return(df)
}
ym1 <- as.character(2017:2023)
f1<-map(ym1, op1)

all1 <- bind_rows(f1)
#operativa desarrollo
op2 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BD_Operativa_{x}12.xlsx"),sheet = 2)

  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==31|cve_tipo_informacion==33|cve_tipo_informacion==35)%>% group_by(c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero) %>%
    summarise(num=sum(dat_num_total,na.rm = T)) %>% ungroup() %>% filter(num!=0) %>%
    filter(nombre_publicacion=="Bansefi"|nombre_publicacion=="Banco del Bienestar")%>% mutate(y=x)

  print(x)
  return(df)
}
f2<-map(ym1, op2)


#captación multiple

c1 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Captaci%C3%B3n_{x}12.xlsx"),sheet = 3)
  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==13|cve_tipo_informacion==14|cve_tipo_informacion==98)%>%
    group_by(c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero) %>%
    summarise(num=sum(dat_num_total,na.rm = T),
              liab=sum(dat_saldo_productom,na.rm = T)) %>% ungroup() %>% filter(num!=0) %>%  mutate(y=x)
  print(x)
  return(df)
}

f3<-map(ym1, c1)

# captación desarrollo

c2 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BD_Captaci%C3%B3n_{x}12.xlsx"),sheet = 3)
  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==13|cve_tipo_informacion==14|cve_tipo_informacion==98)%>%
    group_by(c_mun,dl_estado,dl_municipio,cve_tipo_informacion,dl_producto_financiero,nombre_publicacion) %>%
    summarise(num=sum(dat_num_total,na.rm = T),
              liab=sum(dat_saldo_productom,na.rm = T)) %>% ungroup() %>% filter(num!=0) %>%
    filter(nombre_publicacion=="Bansefi"|nombre_publicacion=="Banco del Bienestar")%>%
    mutate(y=x)
  print(x)
  return(df)
}

f4<-map(ym1, c2)


#falta inegi/ deflactor /

