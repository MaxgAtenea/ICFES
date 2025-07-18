############################################################
#Calculo valor agregado
#Abril 2025 
#Autor:TBD

#Fuentes de los datos

#1.bd.csv: Base elaborada manualmente con la informacion de data icfes.  

#2. programas_bogota_nbc_cine.xlsx: https://hecaa.mineducacion.gov.co/consultaspublicas/programas
#NOTA: Es un listado con los codigos CINE F2013, NBC, SNIES de cada programa.

### La fuente 2 es el reemplazo de la fuente 3. 
#3. codigos_snies_cine_2023.csv: https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/
#Archivo csv: "Estudiantes matriculados 2023.csv"
#NOTA: El archivo viene sin duplicados de los codigos SNIES
############################################################

##################################
#Set working directory
##################################
setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")


##########################################
# Tablas para la presentacion de Juanita
##########################################


##########################################
# Tabla 2 del Anexo del ICFES
##########################################
library(openxlsx)

data_filtrado <- filtros_icfes(data, nivel_cine = "cine_especifico", anios = c(2022, 2023) )

#### conteo de estudiantes por periodo de presentacion ####
conteo_presentacion_saber11_2022 <- data_filtrado %>%  
  filter(anio_presentacion_sbpro==2022) %>%  
  group_by(anio_presentacion_sb11) %>%
  summarise(
      n_obs = n()
    )

conteo_presentacion_saber11_2023 <- data_filtrado %>%  
  filter(anio_presentacion_sbpro==2023) %>%  
  group_by(anio_presentacion_sb11) %>%
  summarise(
    n_obs = n()
  )

write.xlsx(conteo_presentacion_saber11_2022, "conteo_presentacion_saber11_2022.xlsx")
write.xlsx(conteo_presentacion_saber11_2023, "conteo_presentacion_saber11_2023.xlsx")

##########################################
# Tabla 4 del Anexo del ICFES
##########################################
#### conteo de instituciones y CINES y instituciones-programas ####
conteo_programas_snies_filtrados <- data_filtrado %>%  
  group_by(codigo_institucion,estu_snies_prgmacademico) %>%
  summarise(
    n_obs = n()
  )

conteo_programas_snies <- data %>%  filter(anio_presentacion_sbpro %in% c(2022,2023)) %>% 
  group_by(codigo_institucion,estu_snies_prgmacademico) %>%
  summarise(
    n_obs = n()
  )


##########################################
# Tabla 6 del Anexo del ICFES
##########################################
#### configuración estudiante por NBC ####

conteo_estudiantes_por_cine <- data %>% 
  filter(anio_presentacion_sbpro %in% c(2022,2023)) %>% 
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    n_obs = n()
  ) %>% 
  arrange(cine_f_2013_ac_campo_especific)

write.xlsx(conteo_estudiantes_por_cine_filtrados, "conteo_estudiantes_por_cine_filtrados.xlsx")

conteo_estudiantes_por_cine_filtrados <- data_filtrado %>%  
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    n_obs = n()
  ) %>% 
  arrange(cine_f_2013_ac_campo_especific)

write.xlsx(conteo_estudiantes_por_cine, "conteo_estudiantes_por_cine.xlsx")
write.xlsx(conteo_estudiantes_por_cine_filtrados, "conteo_estudiantes_por_cine_filtrados.xlsx")

  
##########################################
# Tabla Adicional - Numero IES por municipio
##########################################
  
conteo_ies_por_municipio_filtrado <- data_filtrado %>%  
  group_by(municipio_oferta_programa) %>%
  summarise(
    programas_distintos = n_distinct(estu_snies_prgmacademico),
    ies_distintas = n_distinct(codigo_institucion)
  ) 

write.xlsx(conteo_ies_por_municipio_filtrado, "conteo_ies_y_programas_por_municipio.xlsx")
