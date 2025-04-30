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
#2 BOX PLOTS CINE ESPECIFICO
##########################################

##########################################
#2.1 TODOS LOS CINE ESPECIFICO
##########################################

##########################################
#2.1.1 PUNTAJE GLOBAL
##########################################

#Promedio de los valores agregados por cine especifico
va_cine_especifico <- data_icine_unico %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    avg_coef = mean(coeficiente_PG, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))


# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_cine_especifico, by = "cine_f_2013_ac_campo_especific") %>% 
  mutate(
    etiqueta = paste0(cine_f_2013_ac_campo_especific, " (n = ", n_obs, ")"),
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
  ) %>% 
  arrange(desc(avg_coef)) 


#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje y
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot en plotly
p_boxplot_plotly <- plot_ly(
  data_boxplot,
  x = ~coeficiente_PG,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 300  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Puntaje Global) por CINE específico",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

# Gráfico en ggplot
p_boxplot_gg <- ggplot(data_boxplot, aes(x = etiqueta, y = coeficiente_PG, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Puntaje Global) por CINE específico",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

#visualizar boxplot
p_boxplot_plotly

p_boxplot_gg

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_boxplot_plotly, file = "output/CINE_especifico/boxplots_va_pg_cine_especifico.html")

#guardar el ggplot
ggsave(
  filename = "output/CINE_especifico/boxplots_va_pg_cine_especifico.png",
  plot = p_boxplot_gg,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)

##########################################
#2.1.2 LECTURA CRITICA
##########################################

#Promedio de los valores agregados por cine especifico
va_cine_especifico <- data_icine_unico %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    avg_coef = mean(coeficiente_LC, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))


# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_cine_especifico, by = "cine_f_2013_ac_campo_especific") %>% 
  mutate(
    etiqueta = paste0(cine_f_2013_ac_campo_especific, " (n = ", n_obs, ")"),
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
  ) %>% 
  arrange(desc(avg_coef)) 


#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje y
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot en plotly
p_boxplot_plotly <- plot_ly(
  data_boxplot,
  x = ~coeficiente_LC,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 300  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Lectura Crítica) por CINE específico",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

# Gráfico en ggplot
p_boxplot_gg <- ggplot(data_boxplot, aes(x = etiqueta, y = coeficiente_LC, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Lectura Crítica) por CINE específico",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

#visualizar boxplot
p_boxplot_plotly

p_boxplot_gg

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_boxplot_plotly, file = "output/CINE_especifico/boxplots_va_lc_cine_especifico.html")

#guardar el ggplot
ggsave(
  filename = "output/CINE_especifico/boxplots_va_lc_cine_especifico.png",
  plot = p_boxplot_gg,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)




##########################################
#2.1.3 RAZONAMIENTO CUANTITATIVO
##########################################

#Promedio de los valores agregados por cine especifico
va_cine_especifico <- data_icine_unico %>%
  group_by(cine_f_2013_ac_campo_especific) %>%
  summarise(
    avg_coef = mean(coeficiente_RC, na.rm = TRUE),
    n_obs =n()  
  ) %>%
  arrange(desc(avg_coef))


# Agregar promedio a los datos
data_boxplot <- data_icine_unico %>%
  left_join(va_cine_especifico, by = "cine_f_2013_ac_campo_especific") %>% 
  mutate(
    etiqueta = paste0(cine_f_2013_ac_campo_especific, " (n = ", n_obs, ")"),
    color = ifelse(avg_coef > 0, "steelblue", "tomato"),
  ) %>% 
  arrange(desc(avg_coef)) 


#2. Crear el boxplot interactivo con plotly
# Reordenar universidades por promedio para el eje y
data_boxplot$etiqueta <- reorder(data_boxplot$etiqueta, data_boxplot$avg_coef)

#crear box plot en plotly
p_boxplot_plotly <- plot_ly(
  data_boxplot,
  x = ~coeficiente_RC,
  y = ~etiqueta,
  type = "box",
  boxpoints = "outliers",
  orientation = "h",
  height = 300  # Aumenta la altura para dar espacio vertical
) %>%
  layout(
    title = "Distribución del Valor Agregado (Razonamiento Cuantitativo) por CINE específico",
    xaxis = list(title = "Valor Agregado"),
    yaxis = list(title = ""),
    showlegend = FALSE
  )

# Gráfico en ggplot
p_boxplot_gg <- ggplot(data_boxplot, aes(x = etiqueta, y = coeficiente_RC, fill = color)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", size = 0.5) +
  coord_flip() +
  scale_fill_identity() +
  labs(
    title = "Distribución del Valor Agregado (Razonamiento Cuantitativo) por CINE específico",
    x = "",
    y = "Valor Agregado"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

#visualizar boxplot
p_boxplot_plotly

p_boxplot_gg

#3. Guardar el boxplot
#guardar el plotly
htmlwidgets::saveWidget(p_boxplot_plotly, file = "output/CINE_especifico/boxplots_va_rc_cine_especifico.html")

#guardar el ggplot
ggsave(
  filename = "output/CINE_especifico/boxplots_va_rc_cine_especifico.png",
  plot = p_boxplot_gg,   # o puedes nombrar tu plot si lo guardaste como objeto
  width = 20,
  height = 6,
  dpi = 300,
  bg = "white" 
)