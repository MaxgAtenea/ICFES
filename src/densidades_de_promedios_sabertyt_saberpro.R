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

setwd("/home/alejandro/Documentos/ATENEA/Despacho/ICFES")

promedios_saber_pro = read_delim("data/Promedios/Saberpro/promedios_programa_2023.csv",escape_double = FALSE, trim_ws = TRUE)
promedios_saber_pro <- promedios_saber_pro %>%
  mutate(puntaje_redondeado = round(promedio_punt_saberpro))

promedios_tyt = read_delim("data/Promedios/TYT/Consolidados/Programas/promedios_programa_consolidado.csv",escape_double = FALSE, trim_ws = TRUE)
promedios_tyt <- promedios_tyt %>%  filter(periodo == 2023)


# Redondear puntajes
promedios_saber_pro <- promedios_saber_pro %>%
  mutate(puntaje_redondeado = round(promedio_punt_saberpro))

# Calcular la promedio
promedio <- mean(promedios_saber_pro$promedio_punt_saberpro)

# Calcular cuántos programas están por encima y por debajo o igual
n_arriba <- sum(promedios_saber_pro$promedio_punt_saberpro > promedio)
n_abajo <- sum(promedios_saber_pro$promedio_punt_saberpro <= promedio)

# Subtítulo dinámico
subtitulo <- paste0(
  n_arriba, " programas por encima del promedio, ",
  n_abajo, " por debajo o igual."
)

# Gráfico
library(patchwork)

p <- ggplot(promedios_saber_pro, aes(x = puntaje_redondeado)) +
  geom_bar(fill = "#ADD8E6", color = "black", width = 0.8, alpha = 0.6) +
  stat_count(geom = "point", color = "#4682B4", size = 2) +
  geom_density(
    aes(
      x = promedio_punt_saberpro,
      y = after_stat(scaled) * max(table(round(promedios_saber_pro$puntaje_redondeado)))
    ),
    inherit.aes = FALSE,
    color = "red",
    fill = "red",
    alpha = 0.05,
    linewidth = 0.5
  ) +
  geom_vline(xintercept = promedio, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Promedio Saber Pro (2023)",
    x = "Puntaje promedio Saber Pro",
    y = "Número de programas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  plot_annotation(
    caption = subtitulo,
    theme = theme(
      plot.caption = element_text(
        size = 9,
        hjust = 0.55,
        lineheight = 1.2
      )
    )
  )

print(p)

###########################
library(patchwork)


# Redondear y preparar datos
promedios_tyt <- promedios_tyt %>%
  mutate(puntaje_redondeado = round(promedio_punt_sabertyt))

# Calcular promedios por grupo
promedios_por_grupo <- promedios_tyt %>%
  group_by(nivel_de_formacion) %>%
  summarise(promedio = mean(promedio_punt_sabertyt))

# Calcular conteos por grupo (por encima y por debajo o igual al promedio)
resumen_subtitulo <- promedios_tyt %>%
  left_join(promedios_por_grupo, by = "nivel_de_formacion") %>%
  group_by(nivel_de_formacion) %>%
  summarise(
    arriba = sum(promedio_punt_sabertyt > promedio),
    abajo = sum(promedio_punt_sabertyt <= promedio)
  ) %>%
  mutate(texto = paste0(
    nivel_de_formacion, ": ", arriba, " programas por encima del promedio, ", 
    abajo, " programas por debajo o igual al promedio"
  ))

# Extraer variables para el texto inferior
arriba <- resumen_subtitulo$arriba[resumen_subtitulo$nivel_de_formacion == "Tecnologico"]
abajo <- resumen_subtitulo$abajo[resumen_subtitulo$nivel_de_formacion == "Tecnologico"]

arriba2 <- resumen_subtitulo$arriba[resumen_subtitulo$nivel_de_formacion == "Formacion Tecnica Profesional"]
abajo2 <- resumen_subtitulo$abajo[resumen_subtitulo$nivel_de_formacion == "Formacion Tecnica Profesional"]

# Texto dividido en dos líneas y tamaño pequeño para mostrar debajo del gráfico
texto_inferior <- paste0(
  "Tecnologico: ", arriba, " programas por encima del promedio, ",
  abajo, " programas por debajo o igual al promedio"
)

texto_inferior2 <- paste0(
  "Formacion Tecnica Profesional: ", arriba2, " programas por encima del promedio, ",
  abajo2, " programas por debajo o igual al promedio"
)

caption_text <- paste(texto_inferior, texto_inferior2, sep = "\n")

# Crear gráfico
p <- ggplot(promedios_tyt, aes(x = puntaje_redondeado, fill = nivel_de_formacion)) +
  geom_bar(position = "identity", alpha = 0.4, color = "black", width = 0.9) +
  stat_count(
    aes(color = nivel_de_formacion),
    geom = "point",
    position = position_identity(),
    size = 2
  ) +
  geom_density(
    aes(
      x = promedio_punt_sabertyt,
      y = after_stat(scaled) * max(table(round(promedios_tyt$puntaje_redondeado))),
      color = nivel_de_formacion,
      fill = nivel_de_formacion
    ),
    inherit.aes = FALSE,
    alpha = 0.2,
    linewidth = 1
  ) +
  geom_vline(data = promedios_por_grupo, aes(xintercept = promedio, color = nivel_de_formacion),
             linetype = "dashed", linewidth = 0.5) +
  labs(
    title = "Promedio Saber TyT (2023)",
    x = "Puntaje promedio Saber TyT",
    y = "Número de programas",
    color = "Nivel de Formacion",
    fill = "Nivel de Formacion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.25, lineheight = 1.2)
  ) + 
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_text(
        size = 9,
        hjust = 0.25,
        lineheight = 1.2
      )
    )
  )

p



