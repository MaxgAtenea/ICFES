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
library(ggrepel)
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
data_icine_unico <- read_delim("data/Resultados/va_icine_bogota_region.csv", escape_double = FALSE, trim_ws = TRUE)

##########################################
#0 SCATTER PLOT LC VS RC
##########################################

#Promedio de los valores agregados por cine especifico
va_universidades <- data_icine_unico %>%
  group_by(codigo_institucion) %>%
  summarise(
    nombre_institucion = first(nombre_institucion),  # Agrega el nombre
    avg_va_pg = median(coeficiente_PG, na.rm = TRUE),
    avg_va_lc = median(coeficiente_LC, na.rm = TRUE),
    avg_va_rc = median(coeficiente_RC, na.rm = TRUE),
    n_obs = n()
  ) %>%
  arrange(desc(avg_va_pg))

#calcular distancias al origen
va_universidades <- va_universidades %>%
  mutate(distancia_origen = sqrt(avg_va_lc^2 + avg_va_rc^2))

#encontrar los puntos extremos en cada cuadrante
extremos <- va_universidades %>%
  filter(n_obs >= 3) %>%
  mutate(cuadrante = case_when(
    avg_va_lc >= 0 & avg_va_rc >= 0 ~ "Q1",
    avg_va_lc <  0 & avg_va_rc >= 0 ~ "Q2",
    avg_va_lc <  0 & avg_va_rc <  0 ~ "Q3",
    avg_va_lc >= 0 & avg_va_rc <  0 ~ "Q4"
  )) %>%
  group_by(cuadrante) %>%
  slice_max(order_by = distancia_origen, n = 3) %>%
  ungroup()

extremos <- extremos %>%
  mutate(color = "Puntos Extremos")

#ajustar ejes para que tengan el mismo rango
lim <- max(abs(c(va_universidades$avg_va_lc, va_universidades$avg_va_rc)), na.rm = TRUE)

#ajustar posicion de etiquetas
va_universidades <- va_universidades %>%
  mutate(destacar = avg_va_lc > 0.1 | avg_va_lc < -0.1 | avg_va_rc > 0.1 | avg_va_rc < -0.1)


# Añadir una columna para los puntos extremos
extremos <- extremos %>%
  mutate(color = "Puntos Extremos")

# Graficar
p <- ggplot(va_universidades, aes(x = avg_va_lc, y = avg_va_rc)) +
  # Q1: x > 0, y > 0 → azul claro
  geom_rect(aes(xmin = 0, xmax = lim, ymin = 0, ymax = lim),
            fill = "palegreen", alpha = 0.2) +
  # Q3: x < 0, y < 0 → rojo claro
  geom_rect(aes(xmin = -lim, xmax = 0, ymin = -lim, ymax = 0),
            fill = "khaki", alpha = 0.2) +
  geom_point(color = "steelblue", size = 3) +  # Puntos normales
  geom_point(data = extremos, aes(color = color), size = 3) +  # Puntos extremos con color mapeado
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  ggrepel::geom_text_repel(
    data = extremos,
    aes(label = paste0(nombre_institucion, " (n=", n_obs, ")")),
    size = 3,
    color = "black"
  ) +
  annotate("text", x = 0, y = 0, label = "(0, 0)", hjust = -0.2, vjust = -0.5, size = 3) +
  labs(
    title = "Mediana del Valor Agregado\nLectura Crítica vs Razonamiento Cuantitativo\nIES",
    subtitle = "Los puntos extremos se consideran si tienen más de 3 observaciones.",
    x = "Lectura Crítica",
    y = "Razonamiento Cuantitativo",
    color = ""  # Añadir una leyenda para los puntos extremos
  ) +
  scale_color_manual(values = c("Puntos Extremos" = "tomato")) +  # Mapeo del color "Tomato"
  xlim(-lim, lim) +
  ylim(-lim, lim) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"  # Coloca la leyenda en la parte superior
  )

show(p)

#guardar el ggplot
ggsave(
  filename = "output/BogotaRegion/Universidades/scatter_va_median_rc_vs_lc_universidades_extremas.png",
  plot = p,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)
  

#grafica 2: etiqueta todos los puntos

p <- ggplot(va_universidades, aes(x = avg_va_lc, y = avg_va_rc)) +
  geom_point(color = "steelblue", size = 3) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  ggrepel::geom_text_repel(aes(label = ifelse(destacar, nombre_institucion, "")), size = 3) +
  annotate("text", x = 0, y = 0, label = "(0, 0)", hjust = -0.2, vjust = -0.5, size = 3) +  # Etiqueta del origen
  labs(
    title = "Mediana del Valor Agregado\nLectura Crítica vs Razonamiento Cuantitativo\nIES",
    x = "Lectura Crítica",
    y = "Razonamiento Cuantitativo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centra el título
  )

#ajusta los rangos de ambos ejes
p<-p + xlim(-lim, lim) + ylim(-lim, lim)

show(p)

#guardar el ggplot
ggsave(
  filename = "output/BogotaRegion/Universidades/scatter_va_median_rc_vs_lc_universidades.png",
  plot = p,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 20,
  dpi = 300,
  bg = "white" 
)


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
  filename = "output/BogotaRegion/Universidades/boxplot_va_pg_top_bottom_ies.png",
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
  filename = "output/BogotaRegion/Universidades/boxplot_va_lc_top_bottom_ies.png",
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
  filename = "output/BogotaRegion/Universidades/boxplot_va_rc_top_bottom_ies.png",
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
htmlwidgets::saveWidget(p_boxplot, file = "output/BogotaRegion/Universidades/boxplots_va_pg_universidades.html")


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
htmlwidgets::saveWidget(p_boxplot, file = "output/BogotaRegion/Universidades/boxplots_va_lc_universidades.html")



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
htmlwidgets::saveWidget(p_boxplot, file = "output/BogotaRegion/Universidades/boxplots_va_rc_universidades.html")

