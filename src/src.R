############################################################
#Calculo valor agregado
############################################################

#Cargar las librerias
library(readr)
library(dplyr)
library(tidyr)

#Leer la base consolidada i,e., Saber Pro cruzado con Saber 11
data <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)
attach(data) #Definimos la BD con la que vamos a trabajar para no llamarla con cada variable

#Como primer ejercicio filtramos las observaciones con programas distintos a 
#Medicina y cuya diferencia entre el saber pro y saber 11 esté en [4,8] años
data <- data %>%filter(
  dif_periodos>=40 &
  dif_periodos<=80 &
  estu_nucleo_pregrado!="MEDICINA"
)

estu_snies_prgmacademico<-factor(estu_snies_prgmacademico)  # La convertimos a factor

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

#Filtrar por las filas que tienen observaciones para punt_global_icfes
df_clean <- data %>%
  filter(!is.na(punt_global_icfes))

#Filtrar para un porgrama academico especifico
base_programa <- subset(df_clean, estu_snies_prgmacademico=="4753")
attach(base_programa)


mco_prog_basico<-lm (punt_global_pro ~ punt_global_icfes)
summary(residuals(mco_prog_basico)) 

