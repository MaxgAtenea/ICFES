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
library(plotly)

##################################
#Constantes
##################################

# variables_cine_snies <- c(
#   "codigo_snies_del_programa",
#   "programa_academico",
#   "nucleo_basico_del_conocimiento_nbc",
#   "id_cine_campo_amplio",
#   "id_cine_campo_especifico",
#   "id_cine_campo_detallado",
#   "codigo_del_municipio_programa",
#   "codigo_de_la_institucion",
#   "institucion_de_educacion_superior_ies",
#   "ies_acreditada",
#   "caracter_ies"
# )

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

variables_cine_snies <- c(
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
  "costo_matricula_estud_nuevos"
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

##################################
#Lectura de los datos
##################################

########
#ICFES
########

#Leer la base consolidada del Saber Pro cruzado con Saber 11
icfes <- read_delim("ICFES/data/BD/bd.csv", escape_double = FALSE, trim_ws = TRUE)

#Resumen de los datos por tipo de dato y Nans
icfes_summary <- icfes %>%
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

#Limpiar la columna estu_nucleo_pregrado y actualizarla en el dataframe
#toda vez que sutilesas en la redaccion generan campos duplicados
#estu_nucleo_pregrado representa el NBC
icfes$estu_nucleo_pregrado <- icfes$estu_nucleo_pregrado %>%
  str_to_lower() %>%                          # Convertir a minúsculas
  stri_trans_general("latin-ascii") %>%       # Eliminar tildes
  str_squish() %>%                            # Eliminar espacios extra
  str_trim() %>%                              # Eliminar espacios al principio y al final
  str_to_title()                              # Capitalizar la primera letra de cada palabra



############
#NBC y CINE
############
# #Leer base con codigos snies y cine de los programas
# cine_snies <- read_delim("ICFES/data/SNIES_CINE/codigos_snies_cine_2023.csv",
#                          escape_double = FALSE,
#                          trim_ws = TRUE,
#                          delim=";",
#                          locale = locale(encoding = "Latin1"))

#Leer base con codigos snies y cine de los programas
#Base a nivel nacional
cine_snies <- read_excel("ICFES/data/SNIES_CINE/programas.xlsx")

#Limpiar el nombre de las columnas de cine_sines
names(cine_snies) <- names(cine_snies) %>%
  stri_trans_general("Latin-ASCII") %>%       # elimina tildes
  tolower() %>%                               # convierte a minúsculas
  gsub(" ", "_", .) %>%                       # reemplaza espacios con _
  gsub("\\(|\\)", "", .)                      #Elimina parentesis


#Seleccionar las columnas de interes
cine_snies <- cine_snies %>%
  select(all_of(variables_cine_snies))


#Mirar codigos CINE unicos por nivel
#CINE campos amplios son 11 los reportados por el DANE
#CINE campos detallados son 121 (82+10 interdisciplinarios + 10 no clasificados en otra parte)
#CINES especificos segun documento DANE
#Son 39 (29 + 10 interdisciplinarios)
#https://www.sen.gov.co/sites/default/files/pagina-migraciones-files/2024-07/documento-de-la-clasificacion-internacional-normalizada-de-la-educacion-campos-de-educacion-y-formacion-adaptada-para-colombia-CINE-F-2013-A.C.pdf
cine_snies %>%
  summarise(
    n_campo_amplio = n_distinct(cine_f_2013_ac_campo_amplio),
    n_campo_especifico = n_distinct(cine_f_2013_ac_campo_especific),
    n_campo_detallado = n_distinct(cine_f_2013_ac_campo_detallado)
  )

#Filtrar por los programas que se ofrecen en Bogota
# cine_snies <- cine_snies %>%
#   filter(codigo_del_municipio_programa == 11001)
cine_snies <- cine_snies %>%
  filter(municipio_oferta_programa == "Bogotá, D.C.")

#Resumen de los datos por tipo de dato y Nans
cine_snies_summary <- cine_snies %>%
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

#Verificamos que no hayan duplicados de codigos SNIES en el df cine_snies
sum(cine_snies[duplicated(cine_snies$codigo_snies_del_programa), ])


#Mirar codigos CINE y NBC unicos por nivel a nivel Bogota
cine_snies %>%
  summarise(
    n_campo_amplio = n_distinct(cine_f_2013_ac_campo_amplio),
    n_campo_especifico = n_distinct(cine_f_2013_ac_campo_especific),
    n_campo_detallado = n_distinct(cine_f_2013_ac_campo_detallado),
    n_nbc = n_distinct(nucleo_basico_del_conocimiento)
  )


#Mirar NBC unicos en el dataframe icfes
#58 valores unicos
icfes %>%
  summarise(valores_unicos = n_distinct(estu_nucleo_pregrado))

#Mirar NBC unicos en el dataframe cine_snies
#56 valores unicos. En teoria son 55 NBC pero con el valor "Sin clasificar" suma 56.
cine_snies %>%
  summarise(valores_unicos = n_distinct(nucleo_basico_del_conocimiento))


#Guardar los valores unicos de nbc en el df cine_snies
nbc_cine_snies <- cine_snies %>%
  distinct(nucleo_basico_del_conocimiento) %>%
  arrange(nucleo_basico_del_conocimiento)

#Guardar los valores unicos de nbc en el df icfes
nbc_icfes <- icfes %>%
  distinct(estu_nucleo_pregrado) %>%
  arrange(estu_nucleo_pregrado)

# Convertir a tibble
nbc_men <- tibble(nbc_men = listado_nbc_men)

# Calcular el máximo número de filas
max_filas <- max(nrow(nbc_icfes), nrow(nbc_cine_snies), nrow(nbc_men))

# Rellenar todos
nbc_icfes <- rellenar_na(nbc_icfes, max_filas)
nbc_cine_snies <- rellenar_na(nbc_cine_snies, max_filas)
nbc_men <- rellenar_na(nbc_men, max_filas)

#Dataframe con el listado de NBC de cada una de las fuentes.
#El df cine_snies y el listado de nbc del SNIES no tiene el nbc llamado "normales superiores"
#El listado de NBC del SNIES no tiene el campo "Sin clasificar"
listado_nbc <- bind_cols(nbc_icfes, nbc_cine_snies, nbc_men)

#Liberar memoria
remove(nbc_cine_snies,nbc_icfes,nbc_men,max_filas)

############################################
#Mirar los programas cuyos snies no cruzan
############################################

#Obtenemos los codigos SNIES de los programas en el dataframe cine_snies
#Tambien obtenemos el nombre del programa, el nivel academico y el nivel de formacion
programas_bdcine_snies <- cine_snies %>%
  select(
    codigo_snies = codigo_snies_del_programa,
    nombre_programa_bdsnies = nombre_del_programa,
    nivel_academico_bdsnies = nivel_academico,
    nivel_de_formacion_bdsnies = nivel_de_formacion
  ) %>%
  distinct() %>%
  mutate(from_cine_snies = TRUE) #agrega identificador de la BD donde vienen los datos


#Remover los programas que no son de pregrado
programas_bdcine_snies <- programas_bdcine_snies %>%
  filter(nivel_academico_bdsnies == "Pregrado")

#Remover los programas que no son de tipo universitario
#Los otros tipos son "Formación técnica profesional" y "Tecnológico"
programas_bdcine_snies <- programas_bdcine_snies %>%
  filter(nivel_de_formacion_bdsnies == "Universitario")

#Obtenemos codigos snies de los programas en el dataframe icfes
#Tambien obtenemos el nombre del programa
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


#Programas que si cruzaron
#cruzaron 1225
programas_matching <- programas %>%
  filter(!is.na(from_cine_snies) & !is.na(from_icfes))

#Programas que no cruzaron (Llave: codigo snies del programa)
#no cruzaron 921: 890 no cruzaron del cine_snies y 31 no cruzaron del icfes
programas_non_matching <- programas %>%
  filter(is.na(from_cine_snies) | is.na(from_icfes))

#Reemplazar NA por False en la columna from_icfes y from_cine_snies
programas_non_matching <- programas_non_matching %>%
  mutate(
    from_icfes = if_else(is.na(from_icfes), FALSE, from_icfes),
    from_cine_snies = if_else(is.na(from_cine_snies), FALSE, from_cine_snies)
  )

#Conteo de programas que no cruzaron por fuente de datos
summary_programas_non_matching <- data.frame(
  from_icfes_count = sum(programas_non_matching$from_icfes, na.rm = TRUE),
  from_cine_count = sum(programas_non_matching$from_cine_snies, na.rm = TRUE)
)

remove(programas)

#Nota:
#31 programas que se encontraron en la BD del ICFES no tienen la
#info correspondiente en la BD de los programas del SNIES


############################################
#Merge de la data del icfes con la del snies
############################################

#Hacer left join con la base icfes y cine_snies
#recuerde que 31 codigos snies en la bd icfes no encontraron match en la bd del snies 
#TO DO: Analizar personas duplicadas (sea porque hicieron 2 saber pro, 2 icfes, etc)
data <- left_join(
  icfes,
  cine_snies,
  by = join_by(estu_snies_prgmacademico==codigo_snies_del_programa)
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

# #Mirar cuantos nbc no tienen codigo cine asociado
# conteo_na_por_nbc <- data %>%
#   filter(is.na(id_cine_campo_especifico)) %>%
#   count(estu_nucleo_pregrado, name = "conteo_con_na_en_cine")

#Detellarar conteo NBC y codigos CINE unicos
#11 CINE amplio
#27 CINE especifico
#73 CINE detallado
# data %>%
#   summarise(
#     n_campo_amplio = n_distinct(id_cine_campo_amplio),
#     n_campo_especifico = n_distinct(id_cine_campo_especifico),
#     n_campo_detallado = n_distinct(id_cine_campo_detallado),
#     n_nbc = n_distinct(nucleo_basico_del_conocimiento_nbc)
#   )


#Verificar si existen codigos de IES que no coinciden 
#Existe Una (1) observacion donde no coinciden los codigos de la IES
sum(data$inst_cod_institucion != data$codigo_institucion, na.rm = TRUE)

#Remover la unica observacion donde no coinciden los codigos de la IES
data <- data %>%
  filter(inst_cod_institucion == codigo_institucion)

#Crear la variable ICINE que pretende ser el analogo al INBC
# data <- data %>%
#   mutate(icine = paste(codigo_de_la_institucion, id_cine_campo_especifico, sep = "_"))
data <- data %>%
  mutate(icine = paste(codigo_institucion, cine_f_2013_ac_campo_especific, sep = "_"))

############################################
#Estadisticas descriptivas
############################################

##################################
#Frecuencia SNIES
##################################

#Mirar la frecuencia de cada programa
#Nota: recordar que el programa depende de la institucion.
programas_freq <- data %>%
  count(estu_snies_prgmacademico, estu_prgm_academico, name = "frecuencia") %>%
  mutate(estu_prgm_academico = tolower(estu_prgm_academico)) %>%
  mutate(programa_label = paste0(estu_snies_prgmacademico, " - ", estu_prgm_academico)) %>%
  arrange(desc(frecuencia))

#Obtener los top 10 programas con mayor frecuencia
top_10 <- programas_freq %>% slice_max(frecuencia, n = 10)

# Graficar top 10
ggplot(top_10, aes(x = reorder(programa_label, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 10 Programas", x = "Programa", y = "Frecuencia") +
  theme_minimal()

#Graficar una densidad de la frecuencia de los programas
#La grafica se utiliza para tener un panorama general de las frecuencias
ggplot(programas_freq, aes(x = frecuencia)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    x = "Frecuencia de aparición del programa",
    y = "Densidad estimada",
    title = "Densidad de frecuencias de programas"
  ) +
  theme_minimal()

##################################
#Frecuencia CINE Especifico
##################################

#Mirar la frecuencia de cada cine especifico
cine_freq <- data %>%
  count(cine_f_2013_ac_campo_especific, name = "frecuencia") %>% 
  arrange(desc(frecuencia))

#Mirar los codigos CINE especificos faltantes
# cines_especificos_faltantes <- setdiff(cines_especificos, cine_freq$id_cine_campo_especifico)
# cines_especificos_faltantes

#Obtener los top 10 cines especificos con mayor frecuencia
top_10_cine <- cine_freq %>% slice_max(frecuencia, n = 10)

ggplot(top_10_cine, aes(x = reorder(id_cine_campo_especifico, frecuencia), y = frecuencia)) +
  geom_col(fill = "#69b3a2") +
  coord_flip() +
  geom_text(aes(label = frecuencia), hjust = -0.1, size = 4) +
  labs(title = "Top 10: Códigos CINE con mayor frecuencia",
       x = "ID CINE Campo Específico",
       y = "Frecuencia") +
  theme_minimal()


#Graficar una densidad de la frecuencia de los cines especificos
#La grafica se utiliza para tener un panorama general de las frecuencias
ggplot(cine_freq, aes(x = frecuencia)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    x = "Frecuencia de aparición del programa",
    y = "Densidad estimada",
    title = "Densidad de frecuencias de programas"
  ) +
  theme_minimal()


##################################
#Frecuencia ICINE
##################################

#Conteo de observaciones por ICINE
#Para observaciones cuyo saber 11 se presento minimo en el 2014-2
#Esto porque la var puntaje_global tiene valores para las observaciones del 2014-2 en adelante
conteo_icine <- data %>%
  filter(!is.na(punt_global_bdsaber11)) %>%
  group_by(icine) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

############################################
#Data para el calcaculo del VA
############################################

#Seleccionar columnas de interes
# columnas_regresion <- c(
#   "estu_consecutivo_bdsaber11",
#   "estu_consecutivo_bdsaberpro",
#   "icine",
#   "institucion_de_educacion_superior_ies",
#   "inst_nombre_institucion",
#   "estu_nucleo_pregrado",
#   "estu_snies_prgmacademico",
#   "nucleo_basico_del_conocimiento_nbc",
#   "id_cine_campo_amplio",
#   "id_cine_campo_especifico",
#   "id_cine_campo_detallado",
#   "punt_global_bdsaberpro",
#   "punt_global_bdsaber11",
#   "periodo_bdsaber11",
#   "periodo_bdsaberpro",
#   "dif_periodos"
# )

columnas_regresion <- c(
  "estu_consecutivo_bdsaber11",
  "estu_consecutivo_bdsaberpro",
  "icine",
  "codigo_institucion",
  "inst_nombre_institucion",
  "estu_nucleo_pregrado",
  "estu_snies_prgmacademico",
  "nucleo_basico_del_conocimiento",
  "cine_f_2013_ac_campo_amplio",
  "cine_f_2013_ac_campo_especific",
  "cine_f_2013_ac_campo_detallado",
  "punt_global_bdsaberpro",
  "punt_global_bdsaber11",
  "periodo_bdsaber11",
  "periodo_bdsaberpro",
  "dif_periodos"
)


#Seleccionar las  variables en columnas_regresion y quedarnos con las filas que no tienen NA en
#punt_global_icfes y en cine_f_2013_ac_campo_especific.
data_filtrado <- data %>%
  select(all_of(columnas_regresion)) %>%
  filter(
    !is.na(punt_global_bdsaber11),
    !is.na(cine_f_2013_ac_campo_especific),
    (
      (cine_f_2013_ac_campo_detallado == "Medicina" & dif_periodos >= 40 & dif_periodos <= 90) |
      (cine_f_2013_ac_campo_detallado != "Medicina" & dif_periodos >= 40 & dif_periodos <= 80) #filtro que hace el ICFES en su metodologia
    )
  )


#Existen 56 NBC unicos
#Recordar que hay una categoria demas llamada "sin clasificar".
#El problema con el campo NBC es que no es un id sino un string, entonces no es fiable
length(unique(data_filtrado$nucleo_basico_del_conocimiento))
length(unique(data_filtrado$estu_nucleo_pregrado))

#Existen 27 CINE especificos unicos para este ejercicio donde se excluye medicina
length(unique(data_filtrado$cine_f_2013_ac_campo_especific))

############################################
#Data para el calcaculo del VA
############################################

#Liberar memoria
remove(data,
       data_summary,
       programas,
       programas_freq,
       programas_matching,
       programas_non_matching,
       top_10
)

#Fijamos la BD con la que vamos a trabajar para no llamarla con cada variable
attach(data_filtrado)

#Resumen de los datos por tipo de dato y Nans
data_filtrado %>%
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

#Tabla con frecuencia por ICINE
conteo_icine <- data_filtrado %>%
  count(icine, name = "n_observaciones") %>%
  arrange(desc(n_observaciones))  #para ver primero los más frecuentes

#Número total de valores únicos de ICINE
n_distinct(data_filtrado$icine)

#estimamos la regresion multinivel con los codigos CINE
multinivel_basico <- lme(
  punt_global_bdsaberpro ~ punt_global_bdsaber11, random = ~ 1| icine, data = data_filtrado
  )

#extraemos el listado de efectos aleatorios (valor agregado)
summary(multinivel_basico)
coeff_va <- ranef(multinivel_basico)

#visualizar el VA más directamente
multinivel_basico[["coefficients"]][["random"]] 

# Convertimos a un data frame con las etiquetas como columna
coefs_df <- data.frame(
  variable = rownames(coeff_va),
  coeficiente = coeff_va[[1]]
)

####################################################
#Graficas
###################################################

#Scatter plot Saber 11 vs Saber pro a nivel individual
ggplot(data_filtrado, aes(x = punt_global_bdsaber11, y = punt_global_bdsaberpro)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(
    title = "Saber 11 y Saber Pro",
    x = "Puntaje Global Saber 11",
    y = "Puntaje Global Saber Pro"
  ) +
  theme_minimal()

# Guardar la gráfica
ggsave(
  filename = "ICFES/output/scatter_saber11_saberpro.jpg",
  plot = last_plot(),   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

#Scatter plot Saber 11 vs Saber pro de los estudiantes en Uniandes
ggplot(
  data_filtrado %>% filter(codigo_institucion == 1813),
  aes(x = punt_global_bdsaber11, y = punt_global_bdsaberpro)
) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Saber 11 vs Saber Pro (Uniandes)",
    x = "Puntaje Saber 11",
    y = "Puntaje Saber Pro",
    caption = paste("Número de observaciones:", 
                    nrow(data_filtrado %>% filter(codigo_institucion == 1813)))
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))


#Scatter plot Saber 11 vs Saber pro de los estudiantes en CESA
ggplot(
  data_filtrado %>% filter(codigo_institucion == 2704),
  aes(x = punt_global_bdsaber11, y = punt_global_bdsaberpro)
) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Saber 11 vs Saber Pro (CESA)",
    x = "Puntaje Saber 11",
    y = "Puntaje Saber Pro",
    caption = paste("Número de observaciones:", 
                    nrow(data_filtrado %>% filter(codigo_institucion == 2704)))
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))


#Scatter plot Saber 11 vs Saber pro a nivel ICINE
data_filtrado %>%
  group_by(icine) %>%
  summarise(
    promedio_saber11 = mean(punt_global_bdsaber11, na.rm = TRUE),
    promedio_saberpro = mean(punt_global_bdsaberpro, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = promedio_saber11, y = promedio_saberpro)) +
  geom_point(color = "steelblue", size = 2.5) +
  labs(
    title = "Promedio Saber 11 vs Saber Pro por Campo ICINE",
    x = "Promedio Puntaje Saber 11",
    y = "Promedio Puntaje Saber Pro"
  ) +
  theme_minimal()

# Guardar la gráfica
ggsave(
  filename = "ICFES/output/scatter_saber11_saberpro_icine.jpg",
  plot = last_plot(),   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

##############
#Graficar los top y bottom 10 valores agregados 
##############

# Ordenamos y seleccionamos top y bottom 10 valores agregados
top_bottom_coefs <- coefs_df %>%
  slice_max(order_by = coeficiente, n = 10) %>%
  bind_rows(slice_min(coefs_df, order_by = coeficiente, n = 10))

#Graficar top y bottom 10 valores agregados
ggplot(top_bottom_coefs, aes(x = reorder(variable, coeficiente), y = coeficiente)) +
  geom_col(fill = "tomato") +
  geom_text(
    aes(label = round(coeficiente, 2), hjust = ifelse(coeficiente > 0, -0.1, 1.1)),
    size = 3
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dashed") +
  labs(
    title = "Top y Bottom 10 Valores Agregados",
    x = NULL,
    y = "Coeficiente"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centrar título
    plot.title.position = "plot"             # Asegura que el título se trate como parte del plot
  )

# Guardar la gráfica
ggsave(
  filename = "ICFES/output/top_bottom_10_valores_agregados.jpg",
  plot = last_plot(),   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)


##############
#Graficar todos los valores agregados
##############

# Asegúrate de que coefs_df está ordenado
coefs_df <- coefs_df %>%
  arrange(coeficiente)

# Plot interactivo
p <- plot_ly(
  data = coefs_df,
  x = ~coeficiente,
  y = ~reorder(variable, coeficiente),
  type = "bar",
  orientation = "h",
  marker = list(color = ifelse(coefs_df$coeficiente >= 0, "tomato", "steelblue"))
) %>%
  layout(
    title = list(text = "Valor Agregado de los programas de pregrado en Bogotá", x = 0.5),
    xaxis = list(title = "Valor Agregado", zeroline = TRUE),
    yaxis = list(title = "", tickfont = list(size = 8)),
    margin = list(l = 150)  # Aumenta si los nombres de variable son muy largos
  )

#Guardar la grafica como un html
htmlwidgets::saveWidget(p, file = "ICFES/output/valor_agregado_general.html")

