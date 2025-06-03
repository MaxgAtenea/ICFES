library(dplyr)
library(ggplot2)
library(patchwork)
library(dplyr) #manipulacion de dataframes
################################################################################
#SABER PRO

################################################################################
promedios_saber_pro = read_delim("data/Promedios/Saberpro/promedios_programa_2023.csv",escape_double = FALSE, trim_ws = TRUE)

# Redondear puntajes
promedios_saber_pro <- promedios_saber_pro %>%
  mutate(puntaje_redondeado = round(promedio_punt_saberpro))

# Agrupar por puntaje redondeado y sumar n_estudiantes
datos_agrupados <- promedios_saber_pro %>%
  group_by(puntaje_redondeado) %>%
  summarise(total_estudiantes = sum(n_estudiantes_matriculados), .groups = "drop")

# Calcular promedio general
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
p <- ggplot(datos_agrupados, aes(x = puntaje_redondeado, y = total_estudiantes)) +
  geom_col(fill = "#ADD8E6", color = "black", width = 0.8, alpha = 0.6) +
  geom_density(
    data = promedios_saber_pro,
    aes(
      x = promedio_punt_saberpro,
      y = after_stat(scaled) * max(datos_agrupados$total_estudiantes)
    ),
    inherit.aes = FALSE,
    color = "red",
    fill = "red",
    alpha = 0.05,
    linewidth = 0.5
  ) +
  geom_vline(xintercept = promedio, color = "darkred", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Desempeño prueba Saber Pro vs Matrícula (2023)",
    x = "Puntaje promedio Saber Pro",
    y = "Número de estudiantes matriculados"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(p)

################################################################################
#SABER TYT

################################################################################

promedios_saber_tyt = read_delim("data/Promedios/TYT/Desglosado por años/2023/promedios_programa_tyt_2023.csv",escape_double = FALSE, trim_ws = TRUE)

# Redondear y preparar datos
promedios_saber_tyt <- promedios_saber_tyt %>%
  mutate(puntaje_redondeado = round(promedio_punt_sabertyt))

# Calcular promedios por grupo
promedios_por_grupo <- promedios_saber_tyt %>%
  group_by(nivel_de_formacion) %>%
  summarise(promedio = mean(promedio_punt_sabertyt))

datos_agrupados <- promedios_saber_tyt %>%
  group_by(nivel_de_formacion,puntaje_redondeado) %>%
  summarise(total_estudiantes = sum(n_estudiantes_matriculados), .groups = "drop")


############
#Subtitulo
############
# Calcular conteos por grupo (por encima y por debajo o igual al promedio)
resumen_subtitulo <- promedios_saber_tyt %>%
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


















###########
#Grafica
##########

# Gráfico con barras + densidad
p <- ggplot(datos_agrupados, aes(x = puntaje_redondeado, y = total_estudiantes, fill = nivel_de_formacion)) +
  geom_col(position = "identity", alpha = 0.5, color = "black", width = 0.9) +
  
  # Curvas de densidad (sobre todos los programas)
  geom_density(
    data = promedios_saber_tyt,
    aes(
      x = puntaje_redondeado,
      y = after_stat(scaled) * max(datos_agrupados$total_estudiantes),
      fill = nivel_de_formacion,
      color = nivel_de_formacion
    ),
    inherit.aes = FALSE,
    alpha = 0.2,
    linewidth = 1
  ) +
  
  # Líneas verticales con promedios por grupo
  geom_vline(
    data = promedios_por_grupo,
    aes(xintercept = promedio, color = nivel_de_formacion),
    linetype = "dashed",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Desempeño en pruebas saber TyT vs Matrícula (2023)",
    x = "Puntaje promedio Saber TyT",
    y = "Número de estudiantes matriculados",
    fill = "Nivel de formación",
    color = "Nivel de formación"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(p)


