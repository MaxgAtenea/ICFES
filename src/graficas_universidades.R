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
data_icine_unico <- read_delim("data/Resultados/va_icine.csv", escape_double = FALSE, trim_ws = TRUE)


##########################################
#1 BOX PLOTS UNIVERSIDADES
##########################################

##########################################
#1.1.1 TOP Y BOTTOM UNIVERSIDADES
#SEGUN PROMEDIO DE VA Puntaje Global
##########################################

#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_PG, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))

#Top y bottom 10 avg_coef por universidad que tenga mas de 2 icine
va_top_bottom_ies <- va_universidades %>%
  filter(n_obs > 2) %>%
  {
    bind_rows(
      slice_max(., avg_coef, n = 10),
      slice_min(., avg_coef, n = 10)
    )
  }

#BOX PLOT
#Top y bottom 10 IES:

# Filtrar el data_icine_unico para solo incluir universidades del top y bottom 10
data_boxplot <- data_icine_unico %>%
  filter(codigo_institucion %in% va_top_bottom_ies$codigo_institucion) %>%
  left_join(select(va_top_bottom_ies, codigo_institucion, avg_coef, n_obs), by = "codigo_institucion") %>%
  mutate(
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
    nombre_institucion = str_to_sentence(tolower(nombre_institucion))
  )

# Crear etiquetas personalizadas para el eje Y con número de observaciones
etiquetas_eje_y <- data_boxplot %>%
  distinct(nombre_institucion, avg_coef, n_obs) %>%
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>%
  select(nombre_institucion, etiqueta)

# Unir al dataset principal
data_boxplot <- data_boxplot %>%
  left_join(etiquetas_eje_y, by = "nombre_institucion")

# Gráfico
p_boxplot <- ggplot(data_boxplot, aes(x = reorder(etiqueta, avg_coef), y = coeficiente_PG, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Puntaje Global) por Universidad.\nTop y Bottom 10 según promedio VA.\n (n>=3)",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

p_boxplot

# Guardar la gráfica
ggsave(
  filename = "output/Universidades/boxplot_va_pg_top_bottom_ies.png",
  plot = p_boxplot,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

##########################################
#1.1.2 TOP Y BOTTOM UNIVERSIDADES
#SEGUN PROMEDIO DE VA Puntaje Lect. Critic.
##########################################

#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_RC, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))

#Top y bottom 10 avg_coef por universidad que tenga mas de 2 icine
va_top_bottom_ies <- va_universidades %>%
  filter(n_obs > 2) %>%
  {
    bind_rows(
      slice_max(., avg_coef, n = 10),
      slice_min(., avg_coef, n = 10)
    )
  }

#BOX PLOT
#Top y bottom 10 IES:

# Filtrar el data_icine_unico para solo incluir universidades del top y bottom 10
data_boxplot <- data_icine_unico %>%
  filter(codigo_institucion %in% va_top_bottom_ies$codigo_institucion) %>%
  left_join(select(va_top_bottom_ies, codigo_institucion, avg_coef, n_obs), by = "codigo_institucion") %>%
  mutate(
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
    nombre_institucion = str_to_sentence(tolower(nombre_institucion))
  )

# Crear etiquetas personalizadas para el eje Y con número de observaciones
etiquetas_eje_y <- data_boxplot %>%
  distinct(nombre_institucion, avg_coef, n_obs) %>%
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>%
  select(nombre_institucion, etiqueta)

# Unir al dataset principal
data_boxplot <- data_boxplot %>%
  left_join(etiquetas_eje_y, by = "nombre_institucion")

# Gráfico
p_boxplot <- ggplot(data_boxplot, aes(x = reorder(etiqueta, avg_coef), y = coeficiente_RC, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Lectura Crítica) por Universidad.\nTop y Bottom 10 según promedio VA.\n (n>=3)",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

p_boxplot

# Guardar la gráfica
ggsave(
  filename = "output/Universidades/boxplot_va_lc_top_bottom_ies.png",
  plot = p_boxplot,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

##########################################
#1.1.3 TOP Y BOTTOM UNIVERSIDADES
#SEGUN PROMEDIO DE VA Puntaje Raz. Cuant.
##########################################

#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_LC, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))

#Top y bottom 10 avg_coef por universidad que tenga mas de 2 icine
va_top_bottom_ies <- va_universidades %>%
  filter(n_obs > 2) %>%
  {
    bind_rows(
      slice_max(., avg_coef, n = 10),
      slice_min(., avg_coef, n = 10)
    )
  }

#BOX PLOT
#Top y bottom 10 IES:

# Filtrar el data_icine_unico para solo incluir universidades del top y bottom 10
data_boxplot <- data_icine_unico %>%
  filter(codigo_institucion %in% va_top_bottom_ies$codigo_institucion) %>%
  left_join(select(va_top_bottom_ies, codigo_institucion, avg_coef, n_obs), by = "codigo_institucion") %>%
  mutate(
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
    nombre_institucion = str_to_sentence(tolower(nombre_institucion))
  )

# Crear etiquetas personalizadas para el eje Y con número de observaciones
etiquetas_eje_y <- data_boxplot %>%
  distinct(nombre_institucion, avg_coef, n_obs) %>%
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>%
  select(nombre_institucion, etiqueta)

# Unir al dataset principal
data_boxplot <- data_boxplot %>%
  left_join(etiquetas_eje_y, by = "nombre_institucion")

# Gráfico
p_boxplot <- ggplot(data_boxplot, aes(x = reorder(etiqueta, avg_coef), y = coeficiente_LC, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Razonamiento Cuantitativo) por Universidad.\nTop y Bottom 10 según promedio VA.\n (n>=3)",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

p_boxplot

# Guardar la gráfica
ggsave(
  filename = "output/Universidades/boxplot_va_rc_top_bottom_ies.png",
  plot = p_boxplot,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)



##########################################
#1.2 TODAS LAS UNIVERSIDADES
##########################################

##########################################
#1.2.1 Puntaje Global
##########################################

#1. Preparar los datos para todas las universidades
#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_PG, na.rm = TRUE),
    n_obs =n()  
  )

# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_universidades, by = "codigo_institucion") %>% 
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>% 
  arrange(desc(avg_coef)) 

#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot
p_boxplot <- plot_ly(
  data_boxplot,
  x = ~coeficiente_PG,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 3000  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Puntaje Global) por Universidad",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

#visualizar boxplot
p_boxplot

#3. Guardar el boxplot
htmlwidgets::saveWidget(p_boxplot, file = "output/Universidades/boxplots_va_pg_universidades.html")


##########################################
#1.2.2 Lectura Critica
##########################################
#1. Preparar los datos para todas las universidades
#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_LC, na.rm = TRUE),
    n_obs =n()  
  )

# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_universidades, by = "codigo_institucion") %>% 
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>% 
  arrange(desc(avg_coef)) 

#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot
p_boxplot <- plot_ly(
  data_boxplot,
  x = ~coeficiente_LC,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 3000  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Lectura Crítica) por Universidad",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

#visualizar boxplot
p_boxplot

#3. Guardar el boxplot
htmlwidgets::saveWidget(p_boxplot, file = "output/Universidades/boxplots_va_lc_universidades.html")



##########################################
#1.2.3 Razonamiento Cuantitativo
##########################################
#1. Preparar los datos para todas las universidades
#Promedio de los valores agregados por universidades
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    avg_coef = mean(coeficiente_RC, na.rm = TRUE),
    n_obs =n()  
  )

# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_universidades, by = "codigo_institucion") %>% 
  mutate(etiqueta = paste0(nombre_institucion, " (n = ", n_obs, ")")) %>% 
  arrange(desc(avg_coef)) 

#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot
p_boxplot <- plot_ly(
  data_boxplot,
  x = ~coeficiente_RC,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 3000  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Razonamiento Cuantitativo) por Universidad",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

#visualizar boxplot
p_boxplot

#3. Guardar el boxplot
htmlwidgets::saveWidget(p_boxplot, file = "output/Universidades/boxplots_va_rc_universidades.html")

