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
    "recaf_punt_sociales_ciudadanas",
    "recaf_punt_ingles",
    "recaf_punt_lectura_critica",
    "recaf_punt_matematicas",
    "recaf_punt_c_naturales"
]


cols_sb11after_20142 = [
    "estu_consecutivo",
    "periodo",
    "punt_global",
    "cole_cod_depto_ubicacion", #Código Dane del departamento de la Sede
    "cole_cod_mcpio_ubicacion", #Código Dane del municipios de la Sede
    "cole_depto_ubicacion",
    "punt_sociales_ciudadanas",
    "punt_ingles",
    "punt_lectura_critica",
    "punt_matematicas",
    "punt_c_naturales"
]

cols_sb11 = [
    "estu_consecutivo",
    "llave_saber_11",
    "llave_saber_pro",
    "periodo",
    "punt_global",
    "cole_cod_depto_ubicacion",
    "cole_cod_mcpio_ubicacion",
    "cole_depto_ubicacion",
    "recaf_punt_sociales_ciudadanas",
    "recaf_punt_ingles",
    "recaf_punt_lectura_critica",
    "recaf_punt_matematicas",
    "recaf_punt_c_naturales",
    "punt_sociales_ciudadanas",
    "punt_ingles",
    "punt_lectura_critica",
    "punt_matematicas",
    "punt_c_naturales"
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
    "punt_c_naturales": "Int64"
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
