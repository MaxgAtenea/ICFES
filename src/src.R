############################################################
#Calculo valor agregado
#Abril 2025 
#Atenea

#Fuentes de los datos

#1.bd.csv: Base elaborada manualmente con la informacion de data icfes.  

#2. codigos_snies_cine_2023.csv: https://snies.mineducacion.gov.co/portal/ESTADISTICAS/Bases-consolidadas/
#Archivo csv: "Estudiantes matriculados 2023"
#El archivo viene sin duplicados de codigos snies
############################################################


#Cargar las librerias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


##################################
#Lectura de los datos
##################################

#Leer la base consolidada i,e., Saber Pro cruzado con Saber 11
icfes <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)


#Leer base con codigos snies y cine de los programas
cine_snies <- read_delim("ICFES/data/SNIES_CINE/codigos_snies_cine_2023.csv",
                         escape_double = FALSE,
                         trim_ws = TRUE,
                         delim=";",
                         locale = locale(encoding = "Latin1"))

#Verificacion de duplicados para cine_snies
keys <- c(
  "PROGRAMA ACADÉMICO",
  "PROGRAMA ACREDITADO",
  "CÓDIGO DE LA INSTITUCIÓN",
  "CÓDIGO DEL MUNICIPIO (PROGRAMA)",
  "ID MODALIDAD"
  #"ID SEXO",
  #"SEMESTRE",
  #"MATRICULADOS"
  )

#Si bien el codigo SNIES es unico, el nombre del Programa Académico tiene duplicados
#Estos estan en funcion de:
#1. Programa academico
#2. Programa acreditado
#3. Codigo de la institucion
#4. CÓDIGO DEL MUNICIPIO (PROGRAMA)
#5. Id modalidad 
#6. Id sexo #no es relevante para el programa
#7. Semestre #no es relevante para el programa
#8. Matriculados #no es relevante para el programa
#Existen 73 duplicados con las llaves del 1-5 y 14 duplicados del 1-8

# #Mirar cuales son las filas duplicadas
# duplicated_rows <- cine_snies %>%
#   filter(duplicated(select(., all_of(keys))))

#Quedarse con las filas no duplicadas
cine_snies <- cine_snies %>%
  distinct(across(all_of(keys)), .keep_all = TRUE)

#Filtrar por los programas que se ofrecen en Bogota
#Tener en cuenta que existe otro potencial filtro: `CÓDIGO DEL MUNICIPIO IES`
#Discutirlo con el equipo

cine_snies <- cine_snies %>%
  filter(`CÓDIGO DEL MUNICIPIO (PROGRAMA)` == 11001)

############################################
#Mirar los programas cuyos snies no cruzan
############################################

#Miramos codigos snies de los programas de la BD cine_snies
programas_bdcine_snies <- cine_snies %>%
  select(
    codigo_snies = `CÓDIGO SNIES DEL PROGRAMA`,
    nombre_programa_bdsnies = `PROGRAMA ACADÉMICO`
  ) %>%
  distinct() %>%
  mutate(from_cine_snies = TRUE) #agrega identificador de la BD donde vienen los datos

#Miramos codigos snies de los programas de la BD icfes
programas_bdicfes <- icfes %>%
  select(
    codigo_snies = estu_snies_prgmacademico,
    nombre_programa_bdicfes = estu_prgm_academico
  ) %>%
  distinct() %>%
  mutate(from_icfes = TRUE) #agrega identificador de la BD donde vienen los datos

#Hacer outer join para mirar los programas que coincidieron y no coincidieron en las BDs 
programas <- full_join(programas_bdcine_snies, programas_bdicfes, by = c("codigo_snies"))

#Omitir codigos repetidos
programas <- programas %>%
  distinct(codigo_snies, .keep_all = TRUE)

#Liberar memoria
remove(programas_bdcine_snies)
remove(programas_bdicfes)

#Programas que cruzaron
programas_non_matching <- programas %>%
  filter(is.na(from_cine_snies) | is.na(from_icfes))

#Programas que no cruzaron
programas_matching <- programas %>%
  filter(!is.na(from_cine_snies) & !is.na(from_icfes))

############################################
#Merge de la data del icfes con la del snies
############################################

#Hacer inner join con la base icfes y cine_snies
data <- inner_join(
  icfes,
  cine_snies,
  by = join_by(estu_snies_prgmacademico==`CÓDIGO SNIES DEL PROGRAMA`)
)

#liberamos memoria
remove(cine_snies)
remove(icfes)

#Resumen de los datos por tipo de dato y Nans
data_summary <- data %>%
  summarise(across(everything(), list(
    class = ~ class(.)[1],
    NA_count = ~ sum(is.na(.)),
    NaN_count = ~ sum(is.nan(.))
  ), .names = "{.col}___{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("column", ".value"),
    names_sep = "___"
  )

#Mirar la frecuencia 
programas_freq <- data %>%
  count(estu_snies_prgmacademico, estu_prgm_academico, name = "frecuencia") %>%
  mutate(estu_prgm_academico = tolower(estu_prgm_academico)) %>%
  mutate(programa_label = paste0(estu_snies_prgmacademico, " - ", estu_prgm_academico))


top_10 <- programas_freq %>% slice_max(frecuencia, n = 10)

# Top 10
ggplot(top_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Programas", x = "Programa", y = "Frecuencia") +
  theme_minimal()

# bottom_10 <- programas_freq %>% slice_min(frecuencia, n = 10)
# # Bottom 10
# ggplot(bottom_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
#   geom_bar(stat = "identity", fill = "firebrick") +
#   coord_flip() +
#   labs(title = "Bottom 10 Programas", x = "Programa", y = "Frecuencia") +
#   theme_minimal()


ggplot(programas_freq, aes(x = frecuencia)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    x = "Frecuencia de aparición del programa",
    y = "Densidad estimada",
    title = "Densidad de frecuencias de programas"
  ) +
  theme_minimal()

#Regresiones (calculo de VA)
############################################

#Definimos la BD con la que vamos a trabajar para no llamarla con cada variable
attach(data) 

#Como primer ejercicio filtramos las observaciones con programas distintos a 
#Medicina y cuya diferencia entre el saber pro y saber 11 esté en [4,8] años
# data <- data %>%filter(
#   dif_periodos>=40 &
#   dif_periodos<=80 &
#   estu_nucleo_pregrado!="MEDICINA"
# )

# La convertimos a factor
estu_snies_prgmacademico<-factor(estu_snies_prgmacademico)  


#Filtrar por las filas que tienen observaciones para punt_global_icfes
df_clean <- data %>%
  filter(!is.na(punt_global_icfes))

#Filtrar para un porgrama academico especifico
base_programa <- subset(df_clean, estu_snies_prgmacademico=="4753")
attach(base_programa)


mco_prog_basico<-lm (punt_global_pro ~ punt_global_icfes)
summary(residuals(mco_prog_basico)) 







