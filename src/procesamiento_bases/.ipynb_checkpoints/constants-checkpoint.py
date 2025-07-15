cols_saberpro_upper = [
    "ESTU_CONSECUTIVO",
    "ESTU_GENERO",
    "PERIODO",
    "PUNT_GLOBAL",
    "ESTU_SNIES_PRGMACADEMICO",
    "ESTU_NUCLEO_PREGRADO",
    "ESTU_PRGM_ACADEMICO",
    "ESTU_NIVEL_PRGM_ACADEMICO", # UNIVERSITARIO, TECNOLOGIA, TECNICO PROFESIONAL, ESCUELA NORMAL SUPERIOR
    "ESTU_PRGM_CODMUNICIPIO",
    "ESTU_PRGM_MUNICIPIO",
    "ESTU_PRGM_DEPARTAMENTO",
    "INST_COD_INSTITUCION",
    "INST_NOMBRE_INSTITUCION",
    "INST_CARACTER_ACADEMICO",  #UNIVERSIDAD, INSTITUCION UNIVERSITARIA, INSTITUCION TECNOLOGICA, TECNICA PROFESIONAL..
    "ESTU_INST_CODMUNICIPIO",
    "ESTU_INST_MUNICIPIO",
    "ESTU_INST_DEPARTAMENTO",
    "ESTU_COD_MCPIO_PRESENTACION",
    "MOD_RAZONA_CUANTITAT_PUNT",
    "MOD_LECTURA_CRITICA_PUNT",
    "MOD_COMPETEN_CIUDADA_PUNT",
    "MOD_INGLES_PUNT",
    "MOD_COMUNI_ESCRITA_PUNT"
]


cols_saberpro_lower = [
    "estu_consecutivo",
    "estu_genero",
    "periodo",
    "punt_global",
    "estu_snies_prgmacademico",
    "estu_nucleo_pregrado",
    "estu_prgm_academico",
    "estu_nivel_prgm_academico", # UNIVERSITARIO, TECNOLOGIA, TECNICO PROFESIONAL, ESCUELA NORMAL SUPERIOR
    "estu_prgm_codmunicipio", #Código del municipio donde se ofrece el programa académico
    "estu_prgm_municipio",
    "estu_prgm_departamento",
    "inst_cod_institucion",
    "inst_nombre_institucion",
    "inst_caracter_academico", #UNIVERSIDAD, INSTITUCION UNIVERSITARIA, INSTITUCION TECNOLOGICA, TECNICA PROFESIONAL..
    "estu_inst_codmunicipio",
    "estu_inst_municipio",
    "estu_inst_departamento",
    "estu_cod_mcpio_presentacion",
    "mod_razona_cuantitat_punt",
    "mod_lectura_critica_punt",
    "mod_competen_ciudada_punt",
    "mod_ingles_punt",
    "mod_comuni_escrita_punt"
]

cols_sb11before_20142 = [
    "estu_consecutivo",
    "periodo",
    "cole_cod_depto_ubicacion", #Código Dane del departamento de la Sede
    "cole_cod_mcpio_ubicacion", #Código Dane del municipios de la Sede
    "cole_depto_ubicacion",
    "cole_naturaleza",
    "recaf_punt_sociales_ciudadanas",
    "recaf_punt_ingles",
    "recaf_punt_lectura_critica",
    "recaf_punt_matematicas",
    "recaf_punt_c_naturales",
    "cole_mcpio_ubicacion",
    "cole_cod_dane_establecimiento",
    "cole_cod_dane_sede",
    "cole_codigo_icfes",
    "cole_nombre_establecimiento",
    "cole_nombre_sede",
    "cole_sede_principal",
    "estu_inse_individual",#
    "estu_nse_individual",
    "estu_nse_establecimiento",
    "fami_estratovivienda",
    "fami_personashogar",
    "fami_cuartoshogar",
    "fami_educacionmadre",
    "fami_educacionpadre",
    "estu_horassemanatrabaja",
    "estu_genero",
    "estu_etnia",
    "estu_discapacidad",
    "estu_dedicacionlecturadiaria"
]


cols_sb11after_20142 = [
    "estu_consecutivo",
    "periodo",
    "punt_global",
    "cole_cod_depto_ubicacion", #Código Dane del departamento de la Sede
    "cole_cod_mcpio_ubicacion", #Código Dane del municipios de la Sede
    "cole_depto_ubicacion",
    "cole_naturaleza",
    "punt_sociales_ciudadanas",
    "punt_ingles",
    "punt_lectura_critica",
    "punt_matematicas",
    "punt_c_naturales",
    "percentil_c_naturales",
    "percentil_ingles",
    "percentil_lectura_critica",
    "percentil_matematicas",
    "percentil_sociales_ciudadanas",
    "percentil_global",
    "cole_mcpio_ubicacion",
    "cole_cod_dane_establecimiento",
    "cole_cod_dane_sede",
    "cole_codigo_icfes",
    "cole_nombre_establecimiento",
    "cole_nombre_sede",
    "cole_sede_principal",
    "estu_inse_individual",#
    "estu_nse_individual",
    "estu_nse_establecimiento",
    "fami_estratovivienda",
    "fami_personashogar",
    "fami_cuartoshogar",
    "fami_educacionmadre",
    "fami_educacionpadre",
    "estu_horassemanatrabaja",
    "estu_genero",
    "estu_etnia",
    "estu_discapacidad",
    "estu_dedicacionlecturadiaria"
]

cols_sb11 = [
    "estu_consecutivo", #variables identificadoras
    "llave_saber_11",
    "llave_saber_pro",
    "periodo",
    "punt_global",
    "cole_cod_depto_ubicacion", #variables de localizacion
    "cole_cod_mcpio_ubicacion",
    "cole_depto_ubicacion",
    "recaf_punt_sociales_ciudadanas", #variables de puntaje
    "recaf_punt_ingles",
    "recaf_punt_lectura_critica",
    "recaf_punt_matematicas",
    "recaf_punt_c_naturales",
    "punt_sociales_ciudadanas",
    "punt_ingles",
    "punt_lectura_critica",
    "punt_matematicas",
    "punt_c_naturales",
    "percentil_c_naturales",
    "percentil_ingles",
    "percentil_lectura_critica",
    "percentil_matematicas",
    "percentil_sociales_ciudadanas",
    "percentil_global",
    "cole_naturaleza", #variables del colegio
    "cole_mcpio_ubicacion",
    "cole_cod_dane_establecimiento",
    "cole_cod_dane_sede",
    "cole_codigo_icfes",
    "cole_nombre_establecimiento",
    "cole_nombre_sede",
    "cole_sede_principal",
    "estu_inse_individual",# variables socioeconomcias
    "estu_nse_individual",
    "estu_nse_establecimiento", 
    "fami_estratovivienda",
    "fami_personashogar",
    "fami_cuartoshogar",
    "fami_educacionmadre",
    "fami_educacionpadre",
    "estu_horassemanatrabaja",
    "estu_genero",
    "estu_etnia",
    "estu_discapacidad",
    "estu_dedicacionlecturadiaria"
]

dtype_mapping_saber11 = {
    "estu_consecutivo": "string",
    "llave_saber_11": "string",
    "llave_saber_pro": "string",
    "punt_global": "Int64",
    "cole_cod_depto_ubicacion": "Int64",
    "cole_cod_mcpio_ubicacion": "Int64",
    "cole_depto_ubicacion": "string",
    "recaf_punt_sociales_ciudadanas": "Int64",
    "recaf_punt_ingles": "Int64",
    "recaf_punt_lectura_critica": "Int64",
    "recaf_punt_matematicas": "Int64",
    "recaf_punt_c_naturales": "Int64",
    "punt_sociales_ciudadanas": "Int64",
    "punt_ingles": "Int64",
    "punt_lectura_critica": "Int64",
    "punt_matematicas": "Int64",
    "punt_c_naturales": "Int64",
    "cole_naturaleza": "str",
    "cole_mcpio_ubicacion": "str",
    "cole_cod_dane_establecimiento": "Int64",
    "cole_cod_dane_sede": "Int64",
    "cole_codigo_icfes": "Int64",
    "cole_nombre_establecimiento": "str",
    "cole_nombre_sede": "str",
    "cole_sede_principal": "str",
    "estu_inse_individual": "float",#
    "estu_nse_individual": "str",
    "estu_nse_establecimiento": "float",
    "fami_estratovivienda": "str",
    "fami_personashogar": "str",
    "fami_cuartoshogar": "str" ,
    "fami_educacionmadre": "str",
    "fami_educacionpadre": "str",
    "estu_horassemanatrabaja" :"str",
    "estu_genero": "str",
    "estu_etnia": "str",
    "estu_discapacidad": "str",
    "estu_dedicacionlecturadiaria" : "str,
}

dtype_mapping_saberpro = {
    "estu_consecutivo": "string",
    "estu_genero": "string",
    "periodo": "int64",  # ya es int64
    "punt_global": "int64",
    "estu_snies_prgmacademico": "Int64",
    "estu_nucleo_pregrado": "string",
    "estu_prgm_academico": "string",
    "estu_nivel_prgm_academico": "string",
    "estu_prgm_codmunicipio": "int64",
    "estu_prgm_municipio": "string",
    "estu_prgm_departamento": "string",
    "inst_cod_institucion": "int64",
    "inst_nombre_institucion": "string",
    "inst_caracter_academico": "string",
    "estu_inst_codmunicipio": "int64",
    "estu_inst_municipio": "string",
    "estu_inst_departamento": "string",
    "estu_cod_mcpio_presentacion": "Int64",  # ya es Int64
    "mod_razona_cuantitat_punt": "int64",
    "mod_lectura_critica_punt": "int64",
    "mod_competen_ciudada_punt": "int64",
    "mod_ingles_punt": "Int64",
    "mod_comuni_escrita_punt": "Int64"
}

#llave:nombre archivo
#valor: separador
nombre_archivos_sabertyt = {
    "SaberTyT_2020-1.txt":"¬",
    "SaberTyT_2020-2.txt":"¬",
    "SaberTyT_2021-1.txt":"¬",
    "SaberTyT_2021-2.txt":"¬",
    "SaberTyT_2022-1.txt":"¬",
    "SaberTyT_2022-2.txt":"¬",
    "SaberTyT_2023-1.txt":";",
    "SaberTyT_2023-2.txt":";"
} 

cols_saber_tyt_lower = [
    "periodo", #periodo de presentacion de la prueba
    "estu_consecutivo", #id publico del estudiante
    "mod_razona_cuantitat_punt", #puntaje en razonamiento cuantitativo
    "mod_lectura_critica_punt", #puntaje en lectura critica
    "punt_global", #puntaje global 
    "inst_cod_institucion", #Codigo de la Institución de  Educación Superior
    "inst_nombre_institucion", #Nombre de la Institución de  Educación Superior
    "estu_prgm_academico", # Nombre del programa académico  que estudia
    "estu_snies_prgmacademico", #Código SNIES del programa  académico que estudia
    "estu_prgm_codmunicipio", #codigo del municipio donde se ofrece el programa academico 
    "estu_prgm_municipio", #Nombre del municipio donde se ofrece el programa academico
    "estu_nucleo_pregrado",  # Nombre del nucleo de pregrado  al que pertenece el programa  academico 
    "estu_nivel_prgm_academico" #tecnologia, tecnico profesional, universitario
]

dtype_mapping_sabertyt =  {
    #"periodo" : "Int64", 
    "estu_consecutivo":"string" ,
    "mod_razona_cuantitat_punt": float,
    "mod_lectura_critica_punt": float,
    "punt_global":float, 
    #"inst_cod_institucion":"Int64",
    "inst_nombre_institucion" :"string", 
    "estu_prgm_academico": "string",
    #"estu_snies_prgmacademico":"Int64",
    #"estu_prgm_codmunicipio": "Int64",
    "estu_prgm_municipio": "string",
    "estu_nucleo_pregrado": "string",
    "estu_nivel_prgm_academico": "string"
}
 


#municipios de bogota region
BOGOTA_REGION_NOMBRES = [
    "bogota_dc",
    "arbelaez",
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
]

#variables del excel descargable en https://hecaa.mineducacion.gov.co/consultaspublicas/programas
columnas_base_snies =[
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
]