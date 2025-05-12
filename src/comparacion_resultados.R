resultados_propios <- read_delim("data/Resultados/va_icine_bogota_region.csv", escape_double = FALSE, trim_ws = TRUE)

resultados_icfes <- read_excel("data/Resultados_ICFES/VA_INBCs_Bogota.xlsx")


comparaciones <- resultados_propios %>%
  mutate(codigo_institucion = as.integer(codigo_institucion)) %>%
  left_join(
    resultados_icfes %>%
      mutate(
        inst_cod_institucion = as.integer(inst_cod_institucion),
        nbc = str_to_sentence(nbc),
        nbc = case_when(
          nbc == "Derecho y afines" ~ "Derecho",  # Cambiar "Derecho y afines" a "Derecho"
          TRUE ~ nbc),
        nbc = case_when(
          nbc == "Matemáticas, estadística y afines" ~ "Matemáticas y estadística",
          TRUE ~ nbc),
        ),
    by = c("codigo_institucion" = "inst_cod_institucion", "cine" = "nbc")
  ) %>% 
  select(nombre_institucion, cine, coeficiente_LC, va_lectura_critica, coeficiente_RC, va_razona_cuantitat, everything())
