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
##############################################
#Correr este bloque antes de cargar library(gt)
#install.packages("gt")
#install.packages("webshot2")  # better than webshot, works well
#webshot::install_phantomjs()
#webshot2::install_phantomjs()  # only needs to be run once
#library(gt) #para guardar tablas
##############################################

#Lectura de los valores agregados por icine
#Cada observación es un ICINE 
data_icine_unico <- read_delim("data/BD/va.csv", escape_double = FALSE, trim_ws = TRUE)

##########################################
#1 BOX PLOTS UNIVERSIDADES
##########################################


##########################################
#1.1 TOP Y BOTTOM UNIVERSIDADES
#SEGUN PROMEDIO DE VA
##########################################

#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(avg_coef = mean(coeficiente, na.rm = TRUE)) %>%
  arrange(desc(avg_coef))

# Top y bottom 10 avg_coef por unviersidad
va_top_bottom <- bind_rows(
  slice_max(va_universidades, avg_coef, n = 10),
  slice_min(va_universidades, avg_coef, n = 10)
)

#BOX PLOT
#Top y bottom 10 IES:

# Filtrar el data_icine_unico para solo incluir universidades del top y bottom 10
data_boxplot <- data_icine_unico %>%
  filter(codigo_institucion %in% va_top_bottom$codigo_institucion) %>%
  left_join(select(va_top_bottom, codigo_institucion, avg_coef), by = "codigo_institucion") %>%
  mutate(color = ifelse(avg_coef > 0, "orange", "steelblue")) %>%
  mutate(inst_nombre_institucion = str_to_sentence(tolower(inst_nombre_institucion)))

# Calcular el número de observaciones por universidad
obs_count <- data_boxplot %>%
  group_by(codigo_institucion) %>%
  summarise(n_obs = n(), .groups = "drop")

#Unir el conteo al dataframe para graficar
data_boxplot <- left_join(data_boxplot, obs_count, by = "codigo_institucion")

#Grafica los boxplots del VA de las top y bottom 10 universidades segun promedio de VA
p_boxplot <- ggplot(data_boxplot, aes(x = reorder(inst_nombre_institucion, avg_coef), y = coeficiente, fill = color)) +
  geom_boxplot() +
  # Etiqueta del número de observaciones
  geom_text(
    data = distinct(data_boxplot, inst_nombre_institucion, n_obs),
    aes(x = inst_nombre_institucion, y = max(data_boxplot$avg_coef, na.rm = TRUE) + 0.2, label = paste0("n = ", n_obs)),
    inherit.aes = FALSE,
    size = 3,
    hjust = 0
  ) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado por Universidad (Top y Bottom 10)",
    x = "Universidad",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


p_boxplot

# Guardar la gráfica
ggsave(
  filename = "output/boxplot_VA_top_bottom_universidades.png",
  plot = p_boxplot,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

##########################################
#1.2 TODAS LAS UNIVERSIDADES
##########################################

#1. Preparar los datos para todas las universidades
# Promedio del coeficiente por universidad
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(avg_coef = mean(coeficiente, na.rm = TRUE), .groups = "drop")

# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_universidades, by = "codigo_institucion") %>%
  mutate(inst_nombre_institucion = str_to_sentence(tolower(inst_nombre_institucion)))

#2. Crear el boxplot interactivo con plotly

# Reordenar universidades por promedio para el eje
data_boxplot$inst_nombre_institucion <- reorder(data_boxplot$inst_nombre_institucion, data_boxplot$avg_coef)

# Gráfico plotly
p_boxplot <- plot_ly(data_boxplot, x = ~coeficiente, y = ~inst_nombre_institucion, type = "box", color = ~inst_nombre_institucion,
                     boxpoints = "outliers", orientation = "h") %>%
  layout(
    title = "Distribución del Valor Agregado por Universidad",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = "Universidad"),
    showlegend = FALSE
  )

#3. Guardar el boxplot
htmlwidgets::saveWidget(p_boxplot, file = "output/boxplots_va_universidades.html")


