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

for (name in c("sbpro_20232", "sbpro_20231", "sbpro_20222", "sbpro_20221",
               "sbpro_2021", "sbpro_2020", "sbpro_2019", "sbpro_2018",
               "sbpro_2017", "sbpro_2016")) {
  df <- get(name)
  names(df) <- tolower(names(df))
  assign(name, df)
}

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

remove(df)

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

names (sbpro)=tolower(names(sbpro))

#filtrar por los programas que se ofrecen en bogota
temp_sbpro <- sbpro %>%filter(estu_prgm_codmunicipio == 11001)

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


# Save data frame as a CSV file
write.csv(temp_sbpro, "ICFES/data/SABERPRO_cleaned/base_sbpro.csv", row.names = FALSE)  # row.names = FALSE excludes row numbers

data <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)

attach(data) #Definimos la BD con la que vamos a trabajar para no llamarla con cada variable

estu_snies_prgmacademico<-factor(estu_snies_prgmacademico)  # La convertimos a factor


mco_basico<-lm (punt_global_pro ~ punt_global_icfes)


