library(readr) # activa el paquete ya presente en la librería, para importar .txt & .csv
library(plyr)  #activamos el paquete
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel
library(dplyr)



#Mirar el directorio de trabajo
getwd()

#Definir las variables de interes del saber pro
cols_saberpro <- c(
  "periodo",
  "estu_genero",
  "estu_consecutivo",
  "punt_global",
  "inst_cod_institucion",
  "estu_cod_mcpio_presentacion",
  "inst_nombre_institucion",
  "estu_prgm_academico",
  "estu_snies_prgmacademico",
  "estu_prgm_codmunicipio",
  "estu_prgm_municipio",
  "estu_prgm_departamento",
  "estu_nivel_prgm_academico",
  "estu_nucleo_pregrado",
  "estu_inst_codmunicipio",
  "estu_inst_municipio",
  "estu_inst_departamento",
  "inst_caracter_academico",
  "mod_razona_cuantitat_punt",
  "mod_lectura_critica_punt",
  "mod_competen_ciudada_punt",
  "mod_ingles_punt",
  "mod_comuni_escrita_punt"
)

#Ruta del directorio con los archivos
data_dir <- "ICFES/data/SABERPRO_raw_reduced/"

########################################################################### 
#Lectura de los archivos con la informacion del saber pro desde el 2016 hasta el 2023-2"
###########################################################################

sbpro_20232 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_20232.txt"),
             delim = ";",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_20232.txt"),
               delim = ";",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)
  
sbpro_20231 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_20231.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_20231.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)


sbpro_20222 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_20222.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_20222.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_20221 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_20221.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_20221.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)



sbpro_2021 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_2021.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_2021.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_2020 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_gen_2020.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_gen_2020.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_2019 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_gen_2019.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_gen_2019.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_2018 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_gen_2018.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_gen_2018.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_2017 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_gen_2017.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_gen_2017.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

sbpro_2016 <- tryCatch(
  read_delim(paste0(data_dir, "saberpro_gen_2016.txt"),
             delim = "¬",
             escape_double = FALSE,
             col_select = all_of(cols_saberpro),
             trim_ws = TRUE),
  error = function(e) {
    read_delim(paste0(data_dir, "saberpro_gen_2016.txt"),
               delim = "¬",
               escape_double = FALSE,
               col_select = all_of(toupper(cols_saberpro)),
               trim_ws = TRUE)
  }
)

##############################
#Arreglos menores a los datos
##############################

#Pasamos a minuscula los nombres de las columnas de los dataframes
for (name in c("sbpro_20232", "sbpro_20231", "sbpro_20222", "sbpro_20221",
               "sbpro_2021", "sbpro_2020", "sbpro_2019", "sbpro_2018",
               "sbpro_2017", "sbpro_2016")) {
  df <- get(name)
  names(df) <- tolower(names(df))
  assign(name, df)
}

#Parseamos algunas columnas a tipo integer
for (name in c("sbpro_20232", "sbpro_20231", "sbpro_20222", "sbpro_20221",
               "sbpro_2021", "sbpro_2020", "sbpro_2019", "sbpro_2018",
               "sbpro_2017", "sbpro_2016")) {
  df <- get(name)
  df$inst_cod_institucion <- as.integer(df$inst_cod_institucion)
  df$estu_cod_mcpio_presentacion <- as.integer(df$estu_cod_mcpio_presentacion)
  df$estu_prgm_codmunicipio <- as.integer(df$estu_prgm_codmunicipio)
  df$estu_inst_codmunicipio <- as.integer(df$estu_inst_codmunicipio)
  assign(name, df)
}

#Liberamos memoria
remove(df)

#Concatenamos los dataframes de cada periodo del saber pro
sbpro <- bind_rows(
  sbpro_20232,
  sbpro_20231,
  sbpro_20222,
  sbpro_20221,
  sbpro_2021,
  sbpro_2020,
  sbpro_2019,
  sbpro_2018,
  sbpro_2017,
  sbpro_2016
  ) 

#filtrar por los programas que se ofrecen en bogota
temp_sbpro <- sbpro %>%filter(estu_prgm_codmunicipio == 11001)

#liberamos memoria
remove(
  sbpro_20232,
  sbpro_20231,
  sbpro_20222,
  sbpro_20221,
  sbpro_2021,
  sbpro_2020,
  sbpro_2019,
  sbpro_2018,
  sbpro_2017,
  sbpro_2016
) 

#Guardamos el dataframe como un archivo csv en la carpeta SABERPRO_cleaned
write.csv(temp_sbpro, "ICFES/data/SABERPRO_cleaned/base_sbpro.csv", row.names = FALSE)  # row.names = FALSE excludes row numbers

