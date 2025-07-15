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
#Instalar librerias
#################################
# install.packages(c(
#   "readr",
#   "readxl",
#   "stringi",
#   "dplyr",
#   "tidyr",
#   "ggplot2",
#   "nlme",
#   "stringr",
#   "plotly",
#   "lme4"
# ))

##################################
#Librerias
##################################

#Cargar las librerias
library(readr) #lectura de archivos text
library(readxl) #lectura de archivos excel
library(stringi) #para ajustar nombres de las columnas de los data frames
library(dplyr) #manipulacion de dataframes
library(tidyr)
library(ggplot2) #para graficar
library(nlme) #activamos la librería/paquete que nos permite estimar el modelo multinivel
library(stringi) #facilitar la manipulacion de caracteres
library(stringr)#facilitar la manipulacion de caracteres
library(plotly) #para graficas dinamicas
library(lme4)#para la regresion de mixed models
library(fuzzyjoin)
library(reticulate)

##############################################
#install.packages("reticulate")
#Correr este bloque antes de cargar library(gt)
#install.packages("gt")
#install.packages("webshot2")  # better than webshot, works well
#webshot::install_phantomjs()
#webshot2::install_phantomjs()  # only needs to be run once
#library(gt) #para guardar tablas
##############################################


##################################
#Set working directory
##################################
setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")


##################################
#Constantes
##################################

#Los 55 NBC listados por el SNIES en su pagina web 
#Recuperado de: https://snies.mineducacion.gov.co/portal/DOCUMENTOS/Glosario/
listado_nbc_men <- c(
  "Administración",
  "Agronomía",
  "Antropología, Artes Liberales",
  "Arquitectura",
  "Artes Plásticas, Visuales y Afines Artes",
  "Artes Representativas",
  "Bacteriología",
  "Bibliotecología, Otros de Ciencias Sociales y Humanas",
  "Biología, Microbiología y Afines",
  "Ciencia Política, Relaciones Internacionales",
  "Comunicación Social, Periodismo y Afines",
  "Contaduría Pública",
  "Deportes, Educación Física y Recreación",
  "Derecho y Afines",
  "Diseño",
  "Economía",
  "Educación",
  "Enfermería",
  "Filosofía, Teología y Afines",
  "Formación Relacionada con el Campo Militar o Policial",
  "Física Geología y otros",
  "Geografía, Historia",
  "Ingeniería Administrativa y Afines",
  "Ingeniería Agroindustrial, Alimentos y Afines",
  "Ingeniería Agronómica, Pecuaria y Afines",
  "Ingeniería Agrícola, Forestal y Afines",
  "Ingeniería Ambiental, Sanitaria y Afines",
  "Ingeniería Biomédica y Afines",
  "Ingeniería Civil y Afines",
  "Ingeniería Electrónica, Telecomunicaciones y Afines",
  "Ingeniería Eléctrica y Afines",
  "Ingeniería Industrial y Afines",
  "Ingeniería Mecánica y Afines",
  "Ingeniería Química y Afines",
  "Ingeniería de Minas, Metalurgia y Afines",
  "Ingeniería de Sistemas, Telemática y Afines",
  "Instrumentación Quirúrgica",
  "Lenguas Modernas, Literatura, Lingüística y Afines",
  "Matemáticas, Estadística y Afines",
  "Medicina",
  "Medicina Veterinaria",
  "Nutrición y Dietética",
  "Odontología",
  "Optometría",
  "Otras Ingenierías",
  "Otros Programas Asociados a Bellas Artes",
  "Otros Programas de Ciencias de la Salud",
  "Programas de Ciencias Naturales",
  "Psicología",
  "Publicidad y Afines",
  "Química y Afines",
  "Salud Pública",
  "Sociología, Trabajo Social y Afines",
  "Terapias",
  "Zootecnia"
)

#Variables del excel del snies que viene con los id de cada cine
variables_codigo_cine <- c(
  "codigo_snies_del_programa",
  "programa_academico",
  #"nucleo_basico_del_conocimiento_nbc",
  "id_cine_campo_amplio",
  "id_cine_campo_especifico",
  "id_cine_campo_detallado",
  "codigo_del_municipio_programa",
  "codigo_de_la_institucion",
  "institucion_de_educacion_superior_ies",
  "ies_acreditada",
  "caracter_ies"
)

#variables del excel de snies que viene sin los codigos cine pero si con los nombres
variables_nombre_cine <- c(
  "codigo_institucion",
  "codigo_institucion_padre",
  "nombre_institucion",
  "estado_institucion",
  "caracter_academico",
  "codigo_snies_del_programa",
  "nombre_del_programa",
  "titulo_otorgado",
  "estado_programa",
  "cine_f_2013_ac_campo_amplio",
  "cine_f_2013_ac_campo_especific",
  "cine_f_2013_ac_campo_detallado",
  "area_de_conocimiento",
  "nucleo_basico_del_conocimiento",
  "nivel_academico",
  "nivel_de_formacion",
  "modalidad",
  "numero_creditos",
  "numero_periodos_de_duracion",
  "periodicidad",
  "departamento_oferta_programa",
  "municipio_oferta_programa",
  "costo_matricula_estud_nuevos",
  "reconocimiento_del_ministerio"
)

#municipios que conforman bogota region según la camara de comercio
#https://www.ccb.org.co/es/informacion-especializada/observatorio/entorno-para-los-negocios/desarrollo-urbano-y-regional/en-bogota-region-viven-mas-de-10-millones-de-habitantes
bogota_region <- c(
  "arbelaez",
  "bogota_dc",
  "cabrera",
  "cajica",
  "carmen_de_carupa",
  "caqueza",
  "chia",
  "chipaque",
  "choachi",
  "choconta",
  "cogua",
  "cota",
  "cucunuba",
  "fusagasuga",
  "fomeque",
  "fosca",
  "fuquene",
  "gachala",
  "gachancipa",
  "gacheta",
  "gama",
  "granada",
  "guacheta",
  "guatavita",
  "guasca",
  "gutierrez",
  "junin",
  "la_calera",
  "lenguazaque",
  "macheta",
  "manta",
  "medina",
  "nemocon",
  "pandi",
  "pasca",
  "quetame",
  "san_bernardo",
  "sesquile",
  "sibate",
  "silvania",
  "simijaca",
  "soacha",
  "sopo",
  "suesca",
  "susa",
  "sutatausa",
  "tabio",
  "tausa",
  "tenjo",
  "tibacuy",
  "tibirita",
  "tocancipa",
  "ubala",
  "ubate",
  "ubaque",
  "une",
  "venecia",
  "villapinzon",
  "zipaquira"
)

#CINE campos amplios son 11 los reportados por el DANE
#CINE campos detallados son 121 (82+10 interdisciplinarios + 10 no clasificados en otra parte)
#CINES especificos segun documento DANE
#Son 39 (29 + 10 interdisciplinarios)
#https://www.sen.gov.co/sites/default/files/pagina-migraciones-files/2024-07/documento-de-la-clasificacion-internacional-normalizada-de-la-educacion-campos-de-educacion-y-formacion-adaptada-para-colombia-CINE-F-2013-A.C.pdf
cines_especificos = c(1,2,3,11,18,21,22,23,28,31,32,38,41,42,48,51,
                      52,53,54,58,61,68,71,72,73,78,81,82,83,84,88,
                      91,92,98,101,102,103,104,108)

##################################
#Funciones
##################################

# Función para rellenar con NA
rellenar_na <- function(df, max_filas) {
  n_faltantes <- max_filas - nrow(df)
  if (n_faltantes > 0) {
    df <- bind_rows(df, tibble::tibble(!!names(df)[1] := rep(NA_character_, n_faltantes)))
  }
  return(df)
}

# Resumir nans por cada variables
resumen_nans <- function(df) {
  df %>%
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
}

# Funcion para recalificar el puntaje global del saber 11
#Las series 2012-1 a 2014-1 nececesitan recalificacion del puntaje global
recalificar_puntaje_global_sb11 <- function(df) {
  round(
    5*(3 * df$recaf_punt_lectura_critica +
         3 * df$recaf_punt_matematicas +
         3 * df$recaf_punt_c_naturales +
         3 * df$recaf_punt_sociales_ciudadanas +
         df$recaf_punt_ingles) / 13
  )
}


#Funcion para validar como el ICFES calcula el puntaje global del saber 11
calcular_puntaje_global_sb11 <- function(df) {
  round(
    5*(3 * df$punt_lectura_critica +
         3 * df$punt_matematicas +
         3 * df$punt_c_naturales +
         3 * df$punt_sociales_ciudadanas +
         df$punt_ingles) / 13
  )
}


##################################
#LECTURA DE DATOS
##################################

#0. Municipios bogota region
municipios <- read_delim("data/Municipios_cleaned/municipios.csv", escape_double = FALSE, trim_ws = TRUE)

#1. ICFES
#Saber pro y saber 11

#Leer la base consolidada del Saber Pro cruzado con Saber 11
icfes <- read_delim("data/BD/saber11_nacional_saberpro_bogota_region.csv", escape_double = FALSE, trim_ws = TRUE)

#Limpiar la columna estu_nucleo_pregrado y actualizarla en el dataframe
#sutilezas en la redaccion generan campos duplicados
#estu_nucleo_pregrado representa el NBC
icfes$estu_nucleo_pregrado <- icfes$estu_nucleo_pregrado %>%
  str_to_lower() %>%                          # Convertir a minúsculas
  stri_trans_general("latin-ascii") %>%       # Eliminar tildes
  str_squish() %>%                            # Eliminar espacios extra
  str_trim() %>%                              # Eliminar espacios al principio y al final
  str_to_title()                              # Capitalizar la primera letra de cada palabra

#Limpiar el formato de la columna inst_nombre_institucion porque hay filas con este formato:
#"FUNDACION UNIVERSIDAD DE BOGOTA\"\"JORGE TADEO LOZANO\"\"-BOGOTÁ D.C." 
icfes$inst_nombre_institucion <- icfes$inst_nombre_institucion %>%
  str_replace_all('\"', "") %>%              # Eliminar comillas dobles
  str_replace_all("\\\\", "") %>%            # Eliminar backslashes
  str_squish() %>%                           # Eliminar espacios extra
  stri_trans_general("latin-ascii") %>%      # Eliminar tildes
  str_to_title() 

#Limpiar el formato de la columna estu_prgm_municipio:
icfes$estu_prgm_municipio <- icfes$estu_prgm_municipio %>%
  str_replace_all('\"', "") %>%              # Eliminar comillas dobles
  str_replace_all("\\.", "") %>%             # Eliminar puntos
  str_replace_all(",", "") %>%               # Eliminar comas (opcional)
  str_replace_all("\\\\", "") %>%            # Eliminar backslashes
  str_squish() %>%                           # Eliminar espacios extra
  stri_trans_general("latin-ascii") %>%      # Eliminar tildes
  str_to_lower() %>% 
  str_replace_all(" ", "_")                  # Reemplazar espacios por guión bajo

#Municipios donde se tiene informacion del icfes
unique(icfes$estu_prgm_municipio)

#Nota: El ICFES recalculó los puntajes del saber 11 para las bases 2012-1 a 2014-1 para que fueran
#comparables con las series del 2014-2 en adelante

#calcular el puntaje global (recalificado) para las observaciones con los puntajes recalificados
#Source: https://www.icfes.gov.co/wp-content/uploads/2024/11/FICHA_METODOLOGICA_OE.pdf
icfes$punt_global_recaf_bdsaber11 <- recalificar_puntaje_global_sb11(icfes)


#Asignamos a cada observacion el correspondiente puntaje global del saber 11 
icfes$punt_global_bdsaber11_conciliado <- ifelse(
  is.na(icfes$punt_global_bdsaber11),
  icfes$punt_global_recaf_bdsaber11,
  icfes$punt_global_bdsaber11
)


#Asignamos a cada observacion el correspondiente puntaje de mate del saber 11 
icfes$punt_mate_conciliado <- ifelse(
  is.na(icfes$punt_matematicas),
  icfes$recaf_punt_matematicas,
  icfes$punt_matematicas
)

#Asignamos a cada observacion el correspondiente puntaje de lectura critica del saber 11 
icfes$punt_lectura_critica_conciliado <- ifelse(
  is.na(icfes$punt_lectura_critica),
  icfes$recaf_punt_lectura_critica,
  icfes$punt_lectura_critica
)

#Asignamos a cada observacion el correspondiente puntaje de ciencias nat. del saber 11 
icfes$punt_c_naturales_conciliado <- ifelse(
  is.na(icfes$punt_c_naturales),
  icfes$recaf_punt_c_naturales,
  icfes$punt_c_naturales
)

#Asignamos a cada observacion el correspondiente puntaje de ingles del saber 11 
icfes$punt_ingles_conciliado <- ifelse(
  is.na(icfes$punt_ingles),
  icfes$recaf_punt_ingles,
  icfes$punt_ingles
)

#Asignamos a cada observacion el correspondiente puntaje de sociales del saber 11 
icfes$punt_sociales_conciliado <- ifelse(
  is.na(icfes$punt_sociales_ciudadanas),
  icfes$recaf_punt_sociales_ciudadanas,
  icfes$punt_sociales_ciudadanas
)

# Resumen de ambos puntajes globales
summary(icfes[
  ,
  c(
    "punt_global_recaf_bdsaber11",
    "punt_global_bdsaber11",
    "punt_global_bdsaber11_conciliado"
    )
  ]
)

#2. CINE

#2.1 CODIGOS CINE
#Leer base con codigos snies y cine de los programas
codigos_cine <- read_delim("data/SNIES_CINE_raw/codigos_snies_cine_2023.csv",
                           escape_double = FALSE,
                           trim_ws = TRUE,
                           delim=";",
                           locale = locale(encoding = "Latin1"))

#Limpiar el nombre de las columnas de cine_sines
names(codigos_cine) <- names(codigos_cine) %>%
  stri_trans_general("Latin-ASCII") %>%       # elimina tildes
  tolower() %>%                               # convierte a minúsculas
  gsub(" ", "_", .) %>%                       # reemplaza espacios con _
  gsub("\\(|\\)", "", .)                      #Elimina parentesis


#Seleccionar las columnas de interes
codigos_cine <- codigos_cine %>%
  select(all_of(variables_codigo_cine))

#2.2 NOMBRES CINE
#Leer base con codigos snies y nombre de la clasificacion cine
#Base a nivel nacional
cine_snies <- read_excel("data/SNIES_CINE_raw/programas_nivel_nacional.xlsx")

#Limpiar el nombre de las columnas de cine_sines
names(cine_snies) <- names(cine_snies) %>%
  stri_trans_general("Latin-ASCII") %>%       # elimina tildes
  tolower() %>%                               # convierte a minúsculas
  gsub(" ", "_", .) %>%                       # reemplaza espacios con _
  gsub("\\(|\\)", "", .)                      #Elimina parentesis

#Seleccionar las columnas de interes
cine_snies <- cine_snies %>%
  select(all_of(variables_nombre_cine))


#2.3 Unir los dataframes cine_snies y codigos_cine

#left join porque la base cine_snies es mas completa
base_cine <- left_join(
  cine_snies,
  codigos_cine,
  by = "codigo_snies_del_programa")


#Limpiar la columna estu_nucleo_pregrado:
#sutilesas en la redaccion generan campos duplicados
base_cine$municipio_oferta_programa  <- base_cine$municipio_oferta_programa  %>%
  str_to_lower() %>%                          # Convertir a minúsculas
  stri_trans_general("latin-ascii") %>%       # Eliminar tildes
  str_squish() %>%                            # Eliminar espacios extra
  str_trim() %>%                              # Eliminar espacios al principio y al final
  str_replace_all(" ", "_") %>%               # Reemplazar espacios por "_"
  str_remove_all("[\\.,]")                    # Eliminar puntos y comas

#Limpiar la columna nombre_insititucion:
base_cine$nombre_institucion <- base_cine$nombre_institucion %>%
  str_replace_all('\"', "") %>%              # Eliminar comillas dobles
  str_replace_all("\\\\", "") %>%            # Eliminar backslashes
  str_squish() %>%                           # Eliminar espacios extra
  stri_trans_general("latin-ascii") %>%      # Eliminar tildes
  str_to_title()

#Limpiar la columna nombre_programa:
base_cine$nombre_del_programa <- base_cine$nombre_del_programa %>%
  str_replace_all('\"', "") %>%              # Eliminar comillas dobles
  str_replace_all("\\\\", "") %>%            # Eliminar backslashes
  str_squish() %>%                           # Eliminar espacios extra
  stri_trans_general("latin-ascii") %>%      # Eliminar tildes
  str_to_title() 

write_csv(base_cine, "data/SNIES_CINE_cleaned/base_cine.csv")


#Filtrar por los programas que:
#0. Esten en bogotá region
#1. Esten activos
#2. Programas universitarios
#3. Programas de pregrado


# Convierte bogota_region en data frame para hacer el join
bogota_region_df <- data.frame(
  municipio = bogota_region,
  stringsAsFactors = FALSE
)

#hacer filtro de bogota region con un fuzzy join
base_cine_filtrada <- stringdist_inner_join(
  base_cine,
  bogota_region_df,
  by = c("municipio_oferta_programa" = "municipio"),
  method = "jw",         # Jaro-Winkler
  max_dist = 0.05        # Ajusta este umbral según tus datos
) %>%
  filter(
    estado_programa == "Activo",
    nivel_de_formacion == "Universitario",
    nivel_academico == "Pregrado"
  )

#municipios en la base_cine_filtrada
sort(unique(base_cine_filtrada$municipio_oferta_programa))

#3. Cruzar informacion del icfes con la informacion del CINE
#Hacer left join con la base icfes y base_cine
#Nota: 31 codigos snies en la bd icfes no encontraron match en la bd del snies
data <- left_join(
  icfes,
  base_cine_filtrada,
  by = join_by(estu_snies_prgmacademico==codigo_snies_del_programa)
)


#4.Limpieza de inconsistencias de la data 

#Remover las observaciones donde no coinciden los codigos de la IES
data <- data %>%
  filter(inst_cod_institucion == codigo_institucion)

#5.Definicion de la variable ICINE

#Crear la variable ICINE que pretende ser el analogo al INBC
data <- data %>%
  mutate(icine = paste(codigo_institucion, cine_f_2013_ac_campo_especific, sep = "_"),
         icine_detall = paste(codigo_institucion, cine_f_2013_ac_campo_detallado, sep = "_"),
         icine_amplio = paste(codigo_institucion, cine_f_2013_ac_campo_amplio, sep = "_"),
         inbc = paste(codigo_institucion, nucleo_basico_del_conocimiento , sep = "_")
  )

#6. Resumen de Nans

#Resumen de los datos por tipo de dato y Nans
data_summary <- resumen_nans(data)

#Guardar DataFrame
#Especificacion del dataframe:
#1.Pruebas saber 11 y pro correspondientes a bogota
#2.Nombres y codigos cine de programas universitarios de pregrado activos en bogota
#3.Variable ICINE (analogo al INBC)

write_csv(data, "data/BD/icfes_cine_15072025.csv")

