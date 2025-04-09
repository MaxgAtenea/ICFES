#=====================================================================================================#
#                      CURSO PRÁCTICO DE VALOR AGREGADO Y APORTE RELATIVO                             # 
#=====================================================================================================#

#Fecha de creación: Agosto 1 de 2022
#Autor: Alexander Villegas
#Descripción: Contiene actividades introductorias para la estimación de modelos de
#             valor agregado en la educación superior

#Contenido: 0. Preparación
#           1. Importación
#           2. Unión de BD
#           3. Estimación VA
#           3.1 Modelo de Regresión Lineal - MCO
#           3.2 Modelo Multinivel - Efectos Aleatorios

#Fechas de modificación: Noviembre 29 de 2023

#=====================================================================================================#
                                  #  0.PREPARACION     #
#=====================================================================================================#

# Seleccionar el directorio donde tenemos nuestros archivos #
getwd () ## Para ver donde se están guardando los archivos

#Especifíco el directorio que me interesa. 
#NOTA: R lee / (slash o división) y no el de Windows \
#setwd ("C:/Users/axle9/OneDrive - icfes.gov.co/01. Valor agregado/Curso Práctico/Archivos Taller 2023")


#Cargamos la librería con los paquetes que vamos a emplear
install.packages("readr")
library(readr) # activa el paquete ya presente en la librería, para importar .txt & .csv

install.packages("plyr") #descargo el paquete que contiene el comando para unir las bases
library(plyr)  #activamos el paquete

library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel


#=====================================================================================================#
                                    #  1. IMPORTACION    #
#=====================================================================================================#

##  Importo los datos de Saber Pro
sbpro_2022 <- read_delim("R-y-DataICFES/SaberPro_2022_Taller.txt",
                         delim = "¬", escape_double = FALSE, trim_ws = TRUE)

View(sbpro_2022) #Abre la base de datos

sbpro_2021 <- read_delim("R-y-DataICFES/SaberPro_2021_Taller.txt", 
                         delim = "¬", escape_double = FALSE, trim_ws = TRUE)

View(sbpro_2021) #ver el periodo diferente 

## Importo los datos de Saber 11
sb11_14_16 <- read_delim("R-y-DataICFES/Saber11_14-16_Taller.txt", 
                         delim = "¬", escape_double = FALSE, trim_ws = TRUE)

sb11_17_18 <- read_delim("R-y-DataICFES/Saber11_17-18_Taller.txt", 
                         delim = "¬", escape_double = FALSE, trim_ws = TRUE)

                
llave <- read_delim("R-y-DataICFES/Llave_Saber11_SaberPro_Taller.txt", 
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
names (llave)=tolower(names(llave))
View(llave) #nombres de los consecutivos

#=====================================================================================================#
                                  #  2.UNIÓN DE BD     #
#=====================================================================================================#

#Uno las bases de Saber Pro en una sola y borro las anteriores
sbpro <- rbind.fill(sbpro_2021, sbpro_2022) 
names (sbpro)=tolower(names(sbpro))
View(sbpro)
remove(sbpro_2021, sbpro_2022) ## elimino las BD que no voy a emplear
table (sbpro$periodo)

#Hago lo mismo con Saber 11
sb11 <- rbind.fill(sb11_14_16, sb11_17_18)
names (sb11)=tolower(names(sb11))
View(sb11)
remove(sb11_14_16, sb11_17_18)  ## elimino las BD que no voy a emplear
table (sb11$periodo)
names(sb11)

#Cambio el nombre al puntaje global para diferenciarlo del de SPro
colnames(sb11)[which(names(sb11) == "punt_global")] <- "punt_global_11"


##Uno la base de Saber Pro con la llave
sbpro <- merge(sbpro, llave, by.x = "estu_consecutivo", by.y = "id_master")
names(sbpro) ##ver que al final se agregó la columna con el consecutivo de saber 11 (id_using)

##Uno la base resultante con Saber 11
base_va <- merge(sbpro, sb11, by.x = "id_using", by.y="consecutivo_saber11")

# Verificamos que la base se unieron correctamente
names(base_va)
View(base_va)

# Borro las bases que no voy a emplear
remove(llave, sb11, sbpro)

#=====================================================================================================#
                                     #  3. ESTIMACIÓN VA  #  
#=====================================================================================================#


#=====================================================================================================#
                          #  3.1 MODELO DE REGRESIÓN LINEAL - MCO (UN PROGRAMA) #
#=====================================================================================================#

#Inspeccionamos la cantidad de observaciones por programas
table(base_va$estu_snies_prgmacademico)

#Creo una sub-base, filtrando solo las observaciones que me interesan
base_programa <- subset(base_va, estu_snies_prgmacademico=="4753")

attach(base_programa) #Definimos la BD con la que vamos a trabajar para no llamarla con cada variable

###Estimo un modelo de regresi?n simple para el programa
mco_prog_basico<-lm (punt_global ~ punt_global_11)

summary(mco_prog_basico) # Vemos un resumen de la estimación
summary(residuals(mco_prog_basico))  ## Como es de esperarse, la media de los residuos (VA) es cero.

#Probemos agregando variables adicionales como el género
#Primero hay que convertir el género en una variable tipo factor
table(estu_genero)
class(estu_genero)  # Examinamos el tipo de la variable
estu_genero<-factor(estu_genero, levels = c("F","M"))  # La convertimos a factor

mco_prog_aumentado<-lm (punt_global ~ punt_global_11 + estu_genero)
summary(mco_prog_aumentado) # Vemos un resumen de la estimación: coeficiente no significativo. 
                            #Quizá, recogidos por el puntaje global


###Para un modulo específico de Saber Pro
mco_prog_compciu<-lm (mod_competen_ciudada_punt ~ punt_lectura_critica + punt_sociales_ciudadanas + estu_genero)
summary (mco_prog_compciu)



#=====================================================================================================#
                #  3.1 MODELO DE REGRESIÓN LINEAL - MCO (CON DIVERSOS PROGRAMAS DE UN NBC) #
#=====================================================================================================#
detach(base_programa) #Desfijamos la base anterior
remove(estu_genero) #Elimino la definición anterior, dado que en ambas bases la variable tiene el mismo nombre

attach(base_va) #Definimos la BD con la que vamos a trabajar para no llamarla con cada variable

estu_snies_prgmacademico<-factor(estu_snies_prgmacademico)  # La convertimos a factor

#Empecemos con un módelo básico donde el puntaje global de Saber Pro solamente depende del puntaje global de Saber 11
mco_basico<-lm (punt_global ~ punt_global_11)

summary(mco_basico) # Vemos un resumen de la estimación

#Obtengo los residuos de la estimación para cada estudiante (VA)
resid_mco_basico <- residuals(mco_basico) 
summary(resid_mco_basico)  #Resumen descriptivo de los residuos

#Obtengo el promedio del valor agregado para cada programa. 
#El promedio del residuo de los estudiantes de cada institución es el valor agregado de la institución. 
#Será positivo, si en promedio los puntajes de los estudiantes están por encima de otros con similares 
#características.
by(data=resid_mco_basico, INDICES = estu_snies_prgmacademico, FUN = mean, na.rm =TRUE, na.action = na.pass) 

tapply(resid_mco_basico, INDEX =  estu_snies_prgmacademico,FUN=mean, na.rm =TRUE, na.action = na.pass)

####Agreguemos más variables
#Volvemos a declarar el género en una variable tipo factor
table(estu_genero)
class(estu_genero)  # Examinamos el tipo de la variable
estu_genero<-factor(estu_genero, levels = c("F","M"))  # La convertimos a factor

#Estimo el modelo
mco_aumentado<-lm (punt_global ~ punt_global_11 + estu_genero)

summary(mco_aumentado) # Vemos un resumen de la estimaci?n

#Obtengo los residuos de la estimación para cada estudiante (VA)
resid_mco_aumentado <- residuals(mco_aumentado) 
summary(resid_mco_aumentado)  #Resumen descriptivo de los residuos

#Obtengo el promedio del valor agregado para cada programa. 
by(data=resid_mco_aumentado, INDICES = estu_snies_prgmacademico, FUN = mean) 
tapply(resid_mco_aumentado, INDEX =  base_va$estu_snies_prgmacademico,FUN=mean, na.rm =TRUE, na.action = na.pass)

#=====================================================================================================#
                       #  3.2 MODELO MULTINIVEL - EFECTOS ALEATORIOS #
#=====================================================================================================#

# Verificamos cuántos INBC tiene nuestra base --> cuantas mediciones de VA vamos a obtener
length(unique(inbc))
inbc<-factor(inbc)  # La convertimos a factor

# Ahora procedemos con las estimaciones de VA y AR

# 1. Empezamos con un modelo básico donde el puntaje global de Saber Pro solamente depende del puntaje global de Saber 11
  
  multinivel_basico <- lme(punt_global ~ punt_global_11, random = ~ 1| inbc, data = base_va)
  
  # Vemos un resumen de la estimación
  summary(multinivel_basico)
  
  #Ahora extraemos el listado de efectos aleatorios (valor agregado)
  coeff_va <- ranef(multinivel_basico)
  View(coeff_va)
  
  multinivel_basico[["coefficients"]][["random"]] # para visualizar el VA más directamente
  
# 2. Ahora estimamos un modelo donde el puntaje global de Saber Pro depende del puntaje de cada una de las 
  #competencias de Saber 11
  
  multinivel_comp <- lme(punt_global ~ punt_matematicas + punt_ingles + 
                           punt_lectura_critica + punt_c_naturales + punt_sociales_ciudadanas, 
                         random = ~ 1| inbc, data = base_va)
  
  #Ahora extraemos el listado de efectos aleatorios (valor agregado)
  coeff_va_comp <- ranef(multinivel_comp)
  View(coeff_va_comp)
  
  multinivel_comp[["coefficients"]][["random"]] # para visualizar el VA más directamente
  
  
# 3. También podemos estimar el VA sobre el puntaje por competencia de Saber Pro 
  
  # Razonamiento cuantitativo
  multinivel_rc <- lme(mod_razona_cuantitat_punt ~ punt_matematicas + punt_ingles + 
                         punt_lectura_critica + punt_c_naturales + punt_sociales_ciudadanas, 
                        random = ~ 1| inbc, data = base_va)
  
  coeff_va_rc <- ranef(multinivel_rc)
  View(coeff_va_rc)
  
  multinivel_rc[["coefficients"]][["random"]] # para visualizar el VA m?s directamente
  
  
  # Lectura crítica
  multinivel_lc <- lme(mod_lectura_critica_punt ~ punt_matematicas + punt_ingles + 
                         punt_lectura_critica + punt_c_naturales + punt_sociales_ciudadanas, 
                         random = ~ 1| inbc, data = base_va)
  
  
  coeff_va_lc <- ranef(multinivel_lc)
  View(coeff_va_lc)
  
  multinivel_lc[["coefficients"]][["random"]] # para visualizar el VA más directamente
  
  
  
  
  
  