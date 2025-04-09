library(readr) # activa el paquete ya presente en la librería, para importar .txt & .csv

sbpro_2022 <- read_delim("ICFES/Examen_Saber_11_20242.txt",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
