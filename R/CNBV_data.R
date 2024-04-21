
rm(list = ls())
pacman::p_load( tidyverse,glue,openxlsx)

dir_1 <- here::here("data", "01_raw", "denue")

op1 <- function(x) {
  df<- openxlsx::read.xlsx( glue("https://portafolioinfo.cnbv.gob.mx/_layouts/15/download.aspx?SourceUrl=https://portafolioinfo.cnbv.gob.mx/PortafolioInformacion/BM_Operativa_{x}12.xlsx"),sheet = 2)
  
  df <- df %>% mutate(c_mun=str_sub(cve_inegi,4,8))%>%
    filter(cve_tipo_informacion==31|cve_tipo_informacion==33|cve_tipo_informacion==35)%>% group_by(c_mun,dl_estado,dl_municipio,cve_tipo_informacion,nombre_publicacion) %>%
    summarise(num=sum(dat_num_total,na.rm = T)) %>% ungroup(dat_num_total=!0)%>% filter()%>%  mutate(y=x) 
  
  print(x)
  return(df)
}
ym1 <- as.character(2015:2023)
f1<-map(ym1, op1)

all1 <- bind_rows(f1)


