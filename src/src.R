library(readr) # activa el paquete ya presente en la librería, para importar .txt & .csv
library(plyr)  #activamos el paquete
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel

getwd()

cols_saberpro <- c(
  "PERIODO",
  "PUNT_GLOBAL",
  "ESTU_CONSECUTIVO",
  "ESTU_COD_MCPIO_PRESENTACION",
  "INST_COD_INSTITUCION",
  "INST_NOMBRE_INSTITUCION",
  "ESTU_PRGM_ACADEMICO",
  "ESTU_SNIES_PRGMACADEMICO",
  "ESTU_PRGM_CODMUNICIPIO",
  "ESTU_PRGM_MUNICIPIO",
  "ESTU_PRGM_DEPARTAMENTO",
  "ESTU_NIVEL_PRGM_ACADEMICO",  # UNIVERSITARIO, TECNOLOGIA, TECNICO PROFESIONAL, ESCUELA NORMAL SUPERIOR
  "ESTU_NUCLEO_PREGRADO",
  "ESTU_INST_CODMUNICIPIO",
  "ESTU_INST_MUNICIPIO",
  "ESTU_INST_DEPARTAMENTO",
  "INST_CARACTER_ACADEMICO"  # UNIVERSIDAD, INSTITUCION UNIVERSITARIA, INSTITUCION TECNOLOGICA, TECNICA PROFESIONAL..
)

# Define the directory containing the files
data_dir <- "ICFES/data/SABERPRO_raw_reduced/"

sbpro_20231 <- read_delim(paste0(data_dir,"saberpro_20231.txt"),
                         delim = "¬", escape_double = FALSE, col_select= cols_saberpro,trim_ws = TRUE)

sbpro_20221 <- read_delim(paste0(data_dir,"saberpro_20221.txt"),
                         delim = "¬", escape_double = FALSE, col_select= cols_saberpro, trim_ws = TRUE)

sbpro <- rbind.fill(sbpro_20231, sbpro_20221) 
names (sbpro)=tolower(names(sbpro))
remove(sbpro_20231, sbpro_20221) ## elimino las BD que no voy a emplear
table (sbpro$periodo)

# Save data frame as a CSV file
write.csv(sbpro, "ICFES/data/SABERPRO_cleaned/base_sbpro.csv", row.names = FALSE)  # row.names = FALSE excludes row numbers

data <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)

attach(data) #Definimos la BD con la que vamos a trabajar para no llamarla con cada variable

estu_snies_prgmacademico<-factor(estu_snies_prgmacademico)  # La convertimos a factor


mco_basico<-lm (punt_global_pro ~ punt_global_icfes)


