# Línea de base APROCAM 
# Proyecto ACCESOS RURAL
# Ministerio de Desarrollo Rural y Tierras - Fondo Internacional para el Desarrollo Agrícola

# Estandarización, limpieza y consistencia
# Compilación 14/02/2024
# Elaborado por ARIA SRL

rm(list = ls())

# Librerías
library(tidyverse)
library(haven)
library(labelled)
library(tidyr)

# Base de datos
db <- readRDS("data_in/2_db_renamed/db.rds")
db2 <- readRDS("data_in/2_db_renamed/db2.rds")
db4 <- readRDS("data_in/2_db_renamed/db4.rds")

# Funciones ####
extremos <- function(df, rubro, variable_analisis) {
  # Filtrar por rubro
  df_rubro <- df %>% filter(s4_3_1 == rubro)
  
  # Calcular estadísticas descriptivas
  media <- mean(df_rubro[[variable_analisis]], na.rm = TRUE)
  mediana <- median(df_rubro[[variable_analisis]], na.rm = TRUE)
  
  # Calcular percentiles 5 y 95
  percentil_5 <- quantile(df_rubro[[variable_analisis]], probs = 0.01, na.rm = TRUE)
  percentil_95 <- quantile(df_rubro[[variable_analisis]], probs = 0.99, na.rm = TRUE)
  
  # Filtrar valores extremos
  valores_extremos_arriba <- df_rubro %>% filter(df_rubro[[variable_analisis]] > percentil_95)
  valores_extremos_abajo <- df_rubro %>% filter(df_rubro[[variable_analisis]] < percentil_5)
  
  # Recortar el dataframe excluyendo percentiles 1 y 99
  df_rubro_trim <- df_rubro %>% filter(df_rubro[[variable_analisis]] >= percentil_5, df_rubro[[variable_analisis]] <= percentil_95)
  
  # Retornar una lista con los resultados
  return(list(
    media = media,
    mediana = mediana,
    valores_extremos_arriba = valores_extremos_arriba,
    valores_extremos_abajo = valores_extremos_abajo,
    df_rubro_trim = df_rubro_trim
  ))
}

encontrar_cercano <- function(nombre, lista_correcta, umbral) {
  # Si el nombre es NA, devolver NA y no hacer más cálculos
  if (is.na(nombre)) {
    return(NA)
  }
  
  # Calcular la distancia a cada rubro correcto
  distancias <- adist(tolower(nombre), tolower(lista_correcta))
  min_distancia <- min(distancias)
  
  # Si la distancia mínima está dentro del umbral, considerarla un error tipográfico y corregir
  if(min_distancia <= umbral) {
    nombre_cercano <- lista_correcta[which.min(distancias)]
  } else {
    nombre_cercano <- nombre  # Mantener el nombre como está
  }
  
  return(nombre_cercano)
}

label_sino <- function(data, var){
  data[[var]] <- factor(data[[var]], levels = c(1, 2), 
                        labels = c("Si", "No"))
  return(data)
}

calcular_percentiles <- function(data, variable) {
  percentiles <- seq(0.05, 0.95, by = 0.05)
  percentiles_valores <- quantile(data[[variable]], probs = percentiles, na.rm = TRUE)
  return(tibble(Percentil = names(percentiles_valores), Valor = percentiles_valores))
}


calcular_estadisticos <- function(df, variable) {
  estadisticos <- list(
    min = min(df[[variable]], na.rm = TRUE),
    max = max(df[[variable]], na.rm = TRUE),
    percentil_1 = quantile(df[[variable]], probs = 0.01, na.rm = TRUE),
    percentil_99 = quantile(df[[variable]], probs = 0.99, na.rm = TRUE),
    mediana = median(df[[variable]], na.rm = TRUE)
  )
  return(estadisticos)
}

# Colores ####
colors <- list(
  Base = "#008DB9",
  Complementary = "#FFA57A", # Placeholder
  DarkerNeutral = "#005970", # Placeholder
  LighterNeutral = "#F5EED3", # Placeholder
  Accent = "#E74646" # Placeholder
)

# AJUSTES DB ####
# Sección 0 ####

db$s0_num_ninos[db$s0_num_ninos > 8] <- NA

# Sección 2 ####
s2_lc1 <- c("1"="Jefe o jefa de hogar",
            "2"="Esposo(a), conviviente",
            "3"="Hijo(a) o entenado(a)",
            "4"="Yerno o nuera", 
            "5"="Hermano(a) o cuñado(a)",
            "6"="Padres",
            "7"="Suegros",
            "8"="Nieto(a)",
            "9"="Otro pariente",
            "10"="Otro NO pariente", 
            "11"="Empleado del hogar")

db2$s2_1c <- factor(db2$s2_1c, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), labels = s2_lc1)
labelled::var_label(db2$s2_1c) <- "Digame, ¿cuál es la relación de parentesco con el Jefe de hogar?"

#idioma
s2_3l <- c("1"="Quechua",
           "2"="Aymara", 
           "3"="Guaraní", 
           "4"="Español")

db2$s2_3 <- factor(db2$s2_3, levels = c("1", "2", "3", "4"), labels = s2_3l)
labelled::var_label(db2$s2_3) <- "¿Cuál es la lengua que habla principalmente en el hogar?"

#Sexo
s2_4l <- c("1"="Hombre",
           "2"="Mujer")

db2$s2_4 <- factor(db2$s2_4, levels = c("1","2"), labels = s2_4l)
labelled::var_label(db2$s2_4) <- "Sexo"

#Pueblo indigena

s2_5l <- c("1"="Quechua",
           "2"="Aymara", 
           "3"="Guaraní", 
           "4"="Chiquitano", 
           "5"="Mojeño",
           "6"="Otro nativo",
           "7"="Ninguno",
           "8"="No es boliviano")

db2$s2_5 <- factor(db2$s2_5, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), labels = s2_5l)
labelled::var_label(db2$s2_5) <- "¿A qué nación o pueblo indígena originario campesino o afroboliviano pertenece?"

#Estado cilvil

s2_7l <- c("1"="Soltero(a)",
           "2"="Casado(a)", 
           "3"="Conviviente o concubino(a)", 
           "4"="Separado/a", 
           "5"="Divorciado(a)",
           "6"="Viudo(a)",
           "999"="No responde")

db2$s2_7 <- factor(db2$s2_7, levels = c("1", "2", "3", "4", "5", "6", "999"), labels = s2_7l)
labelled::var_label(db2$s2_7) <- "¿Cuál es el estado civil o conyugal actual?"

s2_8l <- c("1"="Celular con acceso a internet",
           "2"="Celular sin acceso a internet (solo llamadas)", 
           "3"="No tiene celular")

db2$s2_8 <- factor(db2$s2_8, levels = c("1", "2", "3"), labels = s2_8l)
labelled::var_label(db2$s2_7) <- "¿Tiene celular?"

# Sección 3 ####
table(db$s3_3)
db$s3_3[db$`_index` == 386] <- NA
db$s3_3[db$`_index` == 1180] <- NA
db$s3_3[db$`_index` == 1994] <- NA
table(db$s3_3)

db$s3_4e[db$`_index` == 934] <- NA
db$s3_4[db$`_index` == 934] <- 4
db$s3_4e[db$`_index` == 935] <- NA
db$s3_4[db$`_index` == 935] <- 4
db$s3_4e[db$`_index` == 1574] <- NA
db$s3_4[db$`_index` == 1574] <- 4

db$s3_4e[db$`_index` == 334] <- NA
db$s3_4[db$`_index` == 334] <- 2
db$s3_4e[db$`_index` == 336] <- NA
db$s3_4[db$`_index` == 336] <- 2

table(db$s3_4)

db$s3_5e[db$`_index` == 405]<- NA

db <- db %>% 
  mutate(s3_5 = ifelse(is.na(s3_5e) & s3_5 == 9991, NA, s3_5))
db_checko <- db %>% 
  select(s3_5, s3_5e,`_index`)

db <- db %>% 
  mutate(s3_6 = ifelse(s3_6 == 999 & s3_6e == "Cerámica" ,3, s3_6)) %>% 
  mutate(s3_6e = ifelse(s3_6e == "Cerámica", NA, s3_6e))

db_checkk <- db %>% 
  
  select(s3_6, s3_6e,`_index`)

db$s3_6[db$`_index` == 1180] <- NA
db$s3_6e[db$`_index` == 1180]<- NA

db$s3_6[db$`_index` == 234] <- 4
db$s3_6e[db$`_index` == 234]<- NA

db$s3_6[db$`_index` == 384] <- 1
db$s3_6e[db$`_index` == 384]<- NA

# Modificar las columnas s3_7 y s3_7e
db <- db %>%
  mutate(
    s3_7e = case_when(
      grepl("Poso ciego|Pozo ciego|Poco ciego|Pozo septico|Pozo ciego ecologico|Poso|Pozo", s3_7e, ignore.case = TRUE) ~ "Pozo ciego",
      TRUE ~ s3_7e
    )
  )

db <- db %>%
  mutate(
    s3_7 = case_when(
      s3_7 == 999 & s3_7e == "Pozo ciego" ~ 7,
      TRUE ~ s3_7
    ),
    s3_7e = ifelse(s3_7 == 7, NA_character_, s3_7e)
  )

db_checkPC <- db %>% 
  select(s3_7, s3_7e,`_index`)

# Modificar las columnas s3_7 y s3_7e
db <- db %>%
  mutate(
    s3_7e = case_when(
      grepl("Poso ciego|Pozo ciego|Poco ciego|Pozo septico|Pozo ciego ecologico|Poso|Pozo", s3_7e, ignore.case = TRUE) ~ "Pozo ciego",
      TRUE ~ s3_7e
    )
  )

db <- db %>%
  mutate(
    s3_7 = case_when(
      s3_7 == 999 & s3_7e == "Pozo ciego" ~ 7,
      TRUE ~ s3_7
    ),
    s3_7e = ifelse(s3_7 == 7, NA_character_, s3_7e)
  )

db_checkPC <- db %>% 
  select(s3_7, s3_7e,`_index`)
table(db$s3_7)
#Modificar 3_9

db <- db %>%
  mutate(
    s3_9e = case_when(
      grepl("Velas|Velitas|Vela 🕯️", s3_9e, ignore.case = TRUE) ~ "Vela",
      TRUE ~ s3_9e
    )
  )

db <- db %>%
  mutate(
    s3_9 = case_when(
      s3_9 == 999 & s3_9e == "Vela" ~ 8,
      TRUE ~ s3_9
    ),
    s3_9e = ifelse(s3_9 == 8, NA_character_, s3_9e)
  )



db <- db %>%
  mutate(
    s3_9e = case_when(
      grepl("Panel solar
|Panel", s3_9e, ignore.case = TRUE) ~ "Panel solar",
      TRUE ~ s3_9e
    )
  )

db <- db %>%
  mutate(
    s3_9 = case_when(
      s3_9 == 999 & s3_9e == "Panel solar" ~ 9,
      TRUE ~ s3_9
    ),
    s3_9e = ifelse(s3_9 == 9, NA_character_, s3_9e)
  )

db_checkPC <- db %>% 
  select(s3_9, s3_9e,`_index`)

# Modificar s3_10      
db <- db %>%
  mutate(
    s3_7e = case_when(
      grepl("Poso ciego|Pozo ciego|Poco ciego|Pozo septico|Pozo ciego ecologico|Poso|Pozo", s3_7e, ignore.case = TRUE) ~ "Pozo ciego",
      TRUE ~ s3_7e
    )
  )

db <- db %>%
  mutate(
    s3_7 = case_when(
      s3_7 == 999 & s3_7e == "Pozo ciego" ~ 7,
      TRUE ~ s3_7
    ),
    s3_7e = ifelse(s3_7 == 7, NA_character_, s3_7e)
  )

db_checkPC <- db %>% 
  select(s3_7, s3_7e,`_index`)

#Modificar 3_10

db <- db %>%
  mutate(
    s3_10e = case_when(
      grepl("Gas licuado|Butano|gas domiciliario|Gas natural|gas licuado|Gas y leña recogida|Hay en el lugar", s3_10e, ignore.case = TRUE) ~ "Gas",
      TRUE ~ s3_10e
    )
  )

db <- db %>%
  mutate(
    s3_10 = case_when(
      s3_10 == 999 & s3_10e == "Gas" ~ 5,
      TRUE ~ s3_10
    ),
    s3_10e = ifelse(s3_10 == 5, NA_character_, s3_10e)
  )

db_checkGas <- db %>% 
  select(s3_10, s3_10e,`_index`)
#Agua
db <- db %>%
  mutate(
    s3_11e = case_when(
      grepl("Agua lluvia|Agua de lluvia|Agua de lluvia|En tanque (cosecha de agua)|Lluvia|Cosecha de agua por tanque
                |Lluvia, tanque de agua|Cosecha de agua tanque|Cosecha de agua en tanque|Recojido de lluvia del techo a un tanque
                |Reciben de la lluvia del techo en tanques|Recolecta agua de lluvia|Acopio de lluvia y rosio|Tanque (cosecha de agua)|Lluvia
                |Cosecha de agua con filtro|Cpsecha de agua|En tanque (cosecha de agua)|
                En tanque (cosecha de agua)|Cosecha de agua de lluvia en estanque|	
Tanque ( cosecha de agua)|Agua lluvia|", s3_11e, ignore.case = TRUE) ~ "Cosecha de Agua",
      TRUE ~ s3_11e
    )
  )
db_checkAgua <- db %>% 
  select(s3_11, s3_11e,`_index`)

s3_1l <- c("1"="Semipermanente",
           "2"="Estructura temporaria", 
           "3"="Estructura Permanente", 
           "999"="Otro (especificar)")

db$s3_1 <- factor(db$s3_1, levels = c("1", "2", "3","999"), labels = s3_1l)
labelled::var_label(db$s3_1) <- "¿En qué tipo de vivienda vive este hogar?"



s3_2l <- c("1"="Propia",
           "2"="Alquilada", 
           "3"="Anticretico", 
           "4"="Mixto",
           "5"="Cedida por servicios",
           "6"="Prestada por parientes o vecino", 
           "7"="Otra")

db$s3_2 <- factor(db$s3_2, levels = c("1", "2", "3","4", "5", "6","7"), labels = s3_2l)
labelled::var_label(db$s3_2) <- "¿Actualmente su vivienda es…?"


s3_4l <- c("1"="Barro y palos",
           "2"="Adobe", 
           "3"="Zinc/hierro/estaño", 
           "4"="Piedra/ladrillos/arcilla",
           "5"="Bloques de hormigón/cemento",
           "6"="Madera o tablones", 
           "7"="Postes/carrizo/bambú/hierba o tapete",
           "8"="Hoja de lona/plástico", 
           "999"="Otro")

db$s3_4 <- factor(db$s3_4, levels = c("1", "2", "3","4", "5", "6","7","8","999"), labels = s3_4l)
labelled::var_label(db$s3_4) <- "¿Cuál es el material más utilizado en el techo de su vivienda?"


s3_5l <- c("1"="Hormigón/cemento",
           "2"="Tejas", 
           "3"="Calamina", 
           "4"="Asbesto",
           "5"="Hojas de hierro/zinc/estaño",
           "6"="Hoja de lona/plastico", 
           "7"="Paja, hierba, bambu",
           "999_1"="Otro (especificar)")

db$s3_5 <- factor(db$s3_5, levels = c("1", "2", "3","4", "5", "6","7","999_1"), labels = s3_5l)
labelled::var_label(db$s3_5) <- "¿Cuál es el material más utilizado en las paredes de su vivienda?"


s3_6l <- c("1"="Tierra",
           "2"="Hormigón/Cemento", 
           "3"="Azulejos", 
           "4"="Madera / tablones",
           "5"="Piedra",
           "999"="Otro (especificar)")

db$s3_6 <- factor(db$s3_6, levels = c("1", "2", "3","4", "5","999"), labels = s3_6l)
labelled::var_label(db$s3_6) <- "¿Cuál es el material más utilizado en los pisos de su vivienda?"

s3_7l <- c("1"="Letrina/inodoro en la vivienda",
           "2"="Letrina comunal", 
           "3"="Defecación al aire libre", 
           "4"="Bolsa de plático",
           "5"="Letrina de balde",
           "6"="arbusto/campo",
           "999"="Otro")

db$s3_7 <- factor(db$s3_7, levels = c("1", "2", "3","4", "5","6","999"), labels = s3_7l)
labelled::var_label(db$s3_7) <- "¿Qué tipo de servicio sanitario o letrina utilizan normalmente los miembros del hogar?"

db <- label_sino(db, "s3_8")

s3_9l <- c("1"="Ninguna",
           "2"="Kerosene/parafina", 
           "3"="Velas", 
           "4"="Aceite de palma/lámpara",
           "5"="Lámpara china",
           "6"="Leña",
           "7"="Antorcha",
           "999"="Otro")

db$s3_9 <- factor(db$s3_9, levels = c("1", "2", "3","4", "5","6","7","999"), labels = s3_9l)
labelled::var_label(db$s3_9) <- "¿Cuál es su fuente principal de luz?"
table(s3_9l)

s3_10l <- c("1"="Leña recogida",
            "2"="Leña comprada", 
            "3"="Electricidad", 
            "4"="Kerosene/aceite",
            "5"="Gas",
            "6"="Carbón",
            "7"="Estufa ecológica",
            "999"="Otro")
db$s3_10 <- factor(db$s3_10, levels = c("1", "2", "3","4", "5","6","7","999"), labels = s3_10l)
labelled::var_label(db$s3_10) <- "¿Cuál es su fuente principal de combustible para cocinar?"

s3_11l <- c("1"="Cañeria de red dentro de la vivienda",
            "2"="Cañeria de red fuera de la vivienda, pero dentro del lote o terreno", 
            "3"="Pileta pública", 
            "4"="Carro repartidor (aguatero)",
            "5"="Pozo",
            "6"="Rio, arroyo, estanque",
            "999"="Otro")

db$s3_11 <- factor(db$s3_11, levels = c("1", "2", "3","4", "5","6","999"), labels = s3_11l)
labelled::var_label(db$s3_11) <- "¿El agua para beber proviene de...?"

db <- label_sino(db, "s3_12_1")
db <- label_sino(db, "s3_12_2")
db <- label_sino(db, "s3_12_3")
db <- label_sino(db, "s3_12_4")
db <- label_sino(db, "s3_12_5")
db <- label_sino(db, "s3_12_6")
db <- label_sino(db, "s3_12_7")
db <- label_sino(db, "s3_12_8")


# Sección 6 ####
db$s6_1_2[db$`_index` == 1729]<- 100
db$s6_1_2[db$`_index` == 804]<- 50
db$s6_1_2[db$`_index` == 1372]<- 40
db$s6_1_2[db$`_index` == 785]<- 20


db <- db %>%
  mutate(
    s6_1_2 = ifelse(s6_1_2 %in% c(0, 1, 2, 5), NA, s6_1_2),
    s6_1_1 = ifelse(is.na(s6_1_2), 2, s6_1_1)
  )
db_checkS <- db %>%
  select(s6_1_1, s6_1_2,`_index`)


db$s6_2_2[db$`_index` == 334]<- 100
db$s6_2_2[db$`_index` == 788]<- 80
db$s6_2_2[db$`_index` == 1613]<- 50

db <- label_sino(db, "s6_1_1")
db <- label_sino(db, "s6_2_1")
db <- label_sino(db, "s6_3_1")
db <- label_sino(db, "s6_4_1")
db <- label_sino(db, "s6_5_1")
db <- label_sino(db, "s6_1_3a")
db <- label_sino(db, "s6_1_3b")
db <- label_sino(db, "s6_1_3c")
db <- label_sino(db, "s6_1_4a")
db <- label_sino(db, "s6_1_4b")
db <- label_sino(db, "s6_1_4c")
db <- label_sino(db, "s6_1_5a")
db <- label_sino(db, "s6_1_5b")
db <- label_sino(db, "s6_1_5c")
db <- label_sino(db, "s6_2_3a")
db <- label_sino(db, "s6_2_3b")
db <- label_sino(db, "s6_2_3c")
db <- label_sino(db, "s6_2_4a")
db <- label_sino(db, "s6_2_4b")
db <- label_sino(db, "s6_2_4c")
db <- label_sino(db, "s6_3_3a")
db <- label_sino(db, "s6_3_3b")
db <- label_sino(db, "s6_3_3c")
db <- label_sino(db, "s6_5_3a")
db <- label_sino(db, "s6_5_3b")
db <- label_sino(db, "s6_5_3c")
db <- label_sino(db, "s6_5_5a")
db <- label_sino(db, "s6_5_5b")
db <- label_sino(db, "s6_5_5c")
db <- label_sino(db, "s6_5_4a")
db <- label_sino(db, "s6_5_4b")
db <- label_sino(db, "s6_5_4c")
db <- label_sino(db, "s6_5_6a")
table(db$s6_5_6a)
db <- label_sino(db, "s6_5_6b")
db <- label_sino(db, "s6_5_6c")
db <- label_sino(db, "s6_5_7a")
table(db$s6_5_7a)
db <- label_sino(db, "s6_5_7b")
db <- label_sino(db, "s6_5_7c")

db <- label_sino(db, "s6_6_1a")
db <- label_sino(db, "s6_6_1b")
db <- label_sino(db, "s6_6_1c")
db <- label_sino(db, "s6_6_1d")

db <- label_sino(db, "s6_6_2a")
db <- label_sino(db, "s6_6_2b")
db <- label_sino(db, "s6_6_2c")
db <- label_sino(db, "s6_6_2d")

s4_19l <- c("1"="La producción ha sido severamente afectada por plagas y enfermedades",
              "2"="La producción ha sido moderadamente afectada por plagas y enfermedades", 
              "3"="Plagas y enfermedades no han afectado, o han afectado mínimamente")

db$s4_19 <- factor(db$s4_19, levels = c("1", "2", "3"), labels = s4_19l)
labelled::var_label(db$s4_19) <- "En el últimos año, ¿en qué medida su producción ha sido afectada por plagas o enfermedades?"

# Sección 7 ####

db <- label_sino(db, "s7_1")
db <- label_sino(db, "s7_1a")
db <- label_sino(db, "s7_2")
db <- label_sino(db, "s7_2a")

s7_1bl <- c("1"="Muy Satisfecho",
            "2"="Algo satisfecho", 
            "3"="Algo Insatisfecho",
            "4"="Muy Insatisfecho")

db$s7_1b <- factor(db$s7_1b, levels = c("1", "2", "3","4"), labels = s7_1bl)
labelled::var_label(db$s7_1b) <- "¿Estuvieron satisfechos con estos talleres?" 


s7_2bl <- c("1"="Muy Satisfecho",
            "2"="Algo satisfecho", 
            "3"="Algo Insatisfecho",
            "4"="Muy Insatisfecho")

db$s7_2b <- factor(db$s7_2b, levels = c("1", "2", "3","4"), labels = s7_2bl)
labelled::var_label(db$s7_2b) <- "¿Estuvieron satisfechos con estos talleres?"

s7_4l <- c("Productores_agrícolas"="Organización/asociación de productores agrícolas",
           "Ganaderos"="Organización/asociación de ganaderos", 
           "Pescadores"="Organización/asociación de pescadores", 
           "comercial"="Organización/asociación comercial",
           "Regantes"="Junta/Asociación de regantes",
           "Padres"="Junta/Asociación de padres",
           "ninguno"="Ninguna",
           "otro"="Otras asociaciones")
db$s7_4 <- factor(db$s7_4, levels = c("Productores_agrícolas", "Ganaderos","Pescadores",
                                      "comercial","Regantes", "Padres","ninguno","otro"), labels = s7_4l)
labelled::var_label(db$s7_4) <- "¿Usted o alguien de su hogar es miembro de alguna organización?"

s7_5l <- c("1"="Jefe o jefa de hogar",
           "2"="Esposo(a), conviviente",
           "3"="Hijo(a) o entenado(a)",
           "4"="Yerno o nuera", 
           "5"="Hermano(a) o cuñado(a)",
           "6"="Padres",
           "7"="Suegros",
           "8"="Nieto(a)",
           "9"="Otro pariente",
           "10"="Otro NO pariente", 
           "11"="Empleado del hogar")

db$s7_5 <- factor(db$s7_5, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), labels = s7_5l)
labelled::var_label(db$s7_5) <- "¿Quién en el hogar es un miembro activo de la Asociación?"

s7_6l <- c("1"="Nada",
           "2"="Un poco", 
           "3"="Bastante",
           "4"="Mucho")

db$s7_6 <- factor(db$s7_6, levels = c("1", "2", "3","4"), labels = s7_6l)
labelled::var_label(db$s7_6) <- "¿En qué medida siente usted que puede 
                                        INCIDIR en las decisiones de la Asociación?"

s7_7l <- c("1"="Nada",
           "2"="Un poco", 
           "3"="Bastante",
           "4"="Mucho")

db$s7_7 <- factor(db$s7_7, levels = c("1", "2", "3","4"), labels = s7_7l)
labelled::var_label(db$s7_7) <- "¿En qué medida siente usted que Asociación
                                        puede incidir en la toma de decisiones de las 
                                        autoridades locales y los proveedores de servicios apoyados por el proyecto?"

# Sección 9 ####
s9_2l <- c("1"="Jefe o jefa de hogar",
           "2"="Esposo(a), conviviente",
           "3"="Hijo(a) o entenado(a)",
           "4"="Yerno o nuera", 
           "5"="Hermano(a) o cuñado(a)",
           "6"="Padres",
           "7"="Suegros",
           "8"="Nieto(a)",
           "9"="Otro pariente",
           "10"="Otro NO pariente", 
           "11"="Empleado del hogar")

db$s9_2 <- factor(db$s9_2, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), labels = s9_2l)
labelled::var_label(db$s9_2) <- "¿Quién es el dueño de la empresa?"

s9_5l <- c("1"="Procesamiento (Cultivo/Ganado)",
           "2"="Procesamiento (Productos pesqueros)",
           "3"="Pesca Comercial",
           "4"="Manufactura", 
           "5"="Proveedor de reparaciones/servicios",
           "6"="Construcción",
           "7"="Venta al por menor",
           "8"="Comercio de productos agrícolas",
           "9"="Comercio de productos NO agrícolas",
           "10"="Restaurante/Hostelería", 
           "11"="Transporte",
           "12"="Educación",
           "13"="Atención de Salud",
           "14"="Administrativos",
           "15"="Turismo")

db$s9_5 <- factor(db$s9_5, levels = c("1", "2", "3", "4", 
                                      "5", "6", "7", "8", 
                                      "9", "10", "11", "12", 
                                      "13", "14","15"), labels = s9_5l)
labelled::var_label(db$s9_5) <- "¿Cuáles son los tipos de negocios principales de la empresa?"


s9_6l <- c("1"="Jefe o jefa de hogar",
           "2"="Esposo(a), conviviente",
           "3"="Hijo(a) o entenado(a)",
           "4"="Yerno o nuera", 
           "5"="Hermano(a) o cuñado(a)",
           "6"="Padres",
           "7"="Suegros",
           "8"="Nieto(a)",
           "9"="Otro pariente",
           "10"="Otro NO pariente", 
           "11"="Empleado del hogar")

db$s9_6 <- factor(db$s9_6, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), labels = s9_6l)
labelled::var_label(db$s9_6) <- "¿Cuáles miembros del hogar trabajan
                                        de manera permanente (tiempo completo o periódico/estacional)"

s9_11l<- c("1"="Si, por su cuantía son el ingreso principal de la familia",
           "2"="Si, pero no son los ingresos principales de la familia", 
           "3"="No, son útiles qpero no son tan significativos")


# AJUSTES DB2 ####
# Lista de encuestados #
db_lista <- db %>% 
  select(`_index`, nombre, apellido, s1_3, s1_5) %>%
  arrange(s1_5, s1_3, apellido, nombre) 
  
table(db2$s2_6)

check_completitud_1 <- db2 %>% 
  filter(s2_1a == "0" | s2_1a == "O") %>% 
  select(`_index`)

db2 <- db2 %>% 
  filter(!((s2_1a == "0") | (s2_1a == "O")))

# Edad #
check_consistencia_2 <- db2 %>% 
  filter(s2_6 == 0) %>% 
  select(`_index`)

db2 <- db2 %>% 
  mutate(s2_6 = if_else(s2_6 == 0, NA_real_, s2_6))

check_consistencia_2 <- db2 %>% 
  filter(s2_6 >= 90) %>% 
  select(s2_6, `_index`)

db2$s2_6[db2$"_index" == 143] <- 36
db2$s2_6[db2$"_index" == 152] <- 49
db2$s2_6[db2$"_index" == 262] <- 10
db2$s2_6[db2$"_index" == 692] <- 10
db2$s2_6[db2$"_index" == 389] <- 14
db2$s2_6[db2$"_index" == 402] <- 55
db2$s2_6[db2$"_index" == 1003] <- 69
db2$s2_6[db2$"_index" == 1033] <- 78
db2$s2_6[db2$"_index" == 1168] <- 48
db2$s2_6[db2$"_index" == 1543] <- 50
db2$s2_6[db2$"_index" == 1682] <- 20
db2$s2_6[db2$"_index" == 1744] <- 47
db2$s2_6[db2$"_index" == 1823] <- 16
db2$s2_6[db2$"_index" == 2199] <- 61
db2$s2_6[db2$"_index" == 2388] <- 57
db2$s2_6[db2$"_index" == 2424] <- 30
db2$s2_6[db2$"_index" == 2622] <- 26
db2$s2_6[db2$"_index" == 2740] <- 64
db2$s2_6[db2$"_index" == 2830] <- 52
db2$s2_6[db2$"_index" == 2853] <- 62
db2$s2_6[db2$"_index" == 2854] <- 31
db2$s2_6[db2$"_index" == 2855] <- 32
db2$s2_6[db2$"_index" == 2925] <- 51
db2$s2_6[db2$"_index" == 2936] <- 45
db2$s2_6[db2$"_index" == 3337] <- 65
db2$s2_6[db2$"_index" == 1824] <- NA
db2$s2_6[db2$"_index" == 1825] <- NA

db2_check_grado <- db2 %>% 
  filter(s2_9_a == 11)

db2 <- db2 %>% 
  mutate(s2_9_b = ifelse(s2_9_a == 11, NA, s2_9_b))

db2_básico <- db2 %>% 
  select(s2_9_b)

db2 <- db2 %>%
  mutate(s2_9_b_original = s2_9_b, # Guardar el original por si acaso
         s2_9_b = case_when(
           str_detect(str_to_lower(s2_9_b), "primero|1ro|1er") ~ "1",
           str_detect(str_to_lower(s2_9_b), "segundo|2do|2nd") ~ "2",
           str_detect(str_to_lower(s2_9_b), "tercero|tercer|tecero|3ro|3er") ~ "3",
           str_detect(str_to_lower(s2_9_b), "cuarto|4to|4th") ~ "4",
           str_detect(str_to_lower(s2_9_b), "quinto|5to|5th") ~ "5",
           str_detect(str_to_lower(s2_9_b), "sexto|6to|6th") ~ "6",
           str_detect(str_to_lower(s2_9_b), "séptimo|septimo|7mo") ~ "7",
           str_detect(str_to_lower(s2_9_b), "octavo|8vo") ~ "8",
           str_detect(str_to_lower(s2_9_b), "noveno|9no") ~ "9",
           str_detect(str_to_lower(s2_9_b), "décimo|decimo|10mo") ~ "10",
           # Extraer y convertir números de cadenas mixtas
           str_detect(s2_9_b, "\\d") ~ as.character(str_extract(s2_9_b, "\\d+")),
           # Mantener el valor original para cualquier otro caso
           TRUE ~ s2_9_b
         ))

table(db2$s2_9_b)


db2_check_grado_2 <- db2 %>% 
  select(s2_9_b, s2_9_b)

curso_correcto <- c("Universitario")

# Usar función "encontrar cercano"

umbral <- 5 
# db4$s4_3_1_n <- sapply(db4$s4_3_1, encontrar_cercano, lista_correcta = rubros_correctos, umbral = umbral)
db2$s2_9_b <- sapply(db2$s2_9_b, encontrar_cercano, lista_correcta = curso_correcto, umbral = umbral)

db2 <- db2 %>%
  mutate(s2_9_b = case_when(
    # Buscar variaciones de "técnico" y asignar "3"
    str_detect(str_to_lower(s2_9_b), "t[ée]cnic[oa]") ~ "3",
    # Agregar más condiciones según sea necesario
    # Mantener la lógica para extraer y convertir números de cadenas mixtas
    str_detect(s2_9_b, "\\d") ~ as.character(str_extract(s2_9_b, "\\d+")),
    # Mantener el valor original para cualquier otro caso
    TRUE ~ s2_9_b
  )) 

db2$s2_9_b[db2$s2_9_b == "Vachiller"] <- 6

table(db2$s2_9_b)
# db2_check_grado_3 <- db2 %>% 
#   select(s2_9_b, s2_9_b_XXX)

# AJUSTES DB4 ####
# Ajustes en número de parcela ####
db4$s4_2_n[db4$s4_2_n >= 6] <- 1

# Consistencia superficie ####
# Ajustes por error de código
db4$s4_3_2u[db4$"_index" == 221] <- 2
db4$s4_3_2u[db4$"_index" == 218] <- 2
db4$s4_3_2u[db4$"_index" == 567] <- 2
db4$s4_3_2u[db4$"_index" == 660] <- 2
db4$s4_3_2u[db4$"_index" == 672] <- 2
db4$s4_3_2u[db4$"_index" == 945] <- 2
db4$s4_3_2u[db4$"_index" == 709] <- 2
db4$s4_3_2u[db4$"_index" == 860] <- 2
db4$s4_3_2u[db4$"_index" == 887] <- 2
db4$s4_3_2u[db4$"_index" == 1037] <- 2
db4$s4_3_2u[db4$"_index" == 1059] <- 2
db4$s4_3_2u[db4$"_index" == 1317] <- 2
db4$s4_3_2u[db4$"_index" == 1367] <- 2
db4$s4_3_2u[db4$"_index" == 1396] <- 2
db4$s4_3_2u[db4$"_index" == 1542] <- 2
db4$s4_3_2u[db4$"_index" == 2553] <- 2
db4$s4_3_2u[db4$"_index" == 2558] <- 2
db4$s4_3_2u[db4$"_index" == 2636] <- 2
db4$s4_3_2u[db4$"_index" == 2830] <- 2
db4$s4_3_2u[db4$"_index" == 2932] <- 2
db4$s4_3_2u[db4$"_index" == 2963] <- 2
db4$s4_3_2u[db4$"_index" == 2964] <- 2
db4$s4_3_2u[db4$"_index" == 3151] <- 1
db4$s4_3_2[db4$"_index" == 126] <- 400
db4$s4_3_2[db4$"_index" == 521] <- 2
db4$s4_3_2[db4$"_index" == 321] <- 200
db4$s4_3_2[db4$"_index" == 331] <- 3000
db4$s4_3_2[db4$"_index" == 664] <- 5
db4$s4_3_2[db4$"_index" == 1674] <- 70
db4$s4_3_2[db4$"_index" == 987] <- 1
db4$s4_3_2[db4$"_index" == 771] <- 250
db4$s4_3_2[db4$"_index" == 769] <- 300
db4$s4_3_2[db4$"_index" == 777] <- 200
db4$s4_3_2[db4$"_index" == 784] <- 200
db4$s4_3_2[db4$"_index" == 802] <- 300
db4$s4_3_2[db4$"_index" == 795] <- 500
db4$s4_3_2[db4$"_index" == 991] <- 1000
db4$s4_3_2[db4$"_index" == 1034] <- 250
db4$s4_3_2[db4$"_index" == 1036] <- 200
db4$s4_3_2[db4$"_index" == 1129] <- 200
db4$s4_3_2[db4$"_index" == 1473] <- 2000
db4$s4_3_2[db4$"_index" == 1921] <- 300

# Recodificación en s4_2_a
s4_2_a_lab = c("1" = "Heredada", 
               "2" = "Comprada o arrendada", 
               "3" = "Asignada por el gobierno", 
               "4" = "Asignada por líder local", 
               "5" = "Alquilada",
               "6" = "Cultivo compartido",
               "7" = "Prestada/gratis",
               "8" = "Despejada y ocupada",
               "999" = "Otro")

db4$s4_2_a <- factor(db4$s4_2_a, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "999"), labels = s4_2_a_lab)
labelled::var_label(db4$s4_2_a) <- "¿Cómo adquirió su parcela?"

# AJustes s4_3_1
labelled::var_label(db4$s4_2_a) <- "Cultivo analizado"

# Estandarización superficie (hectáreas)
table(db4$s4_3_2e)
db4$s4_3_2e[db4$s4_3_2e == "Arrobada(900 m)"] <- "Arrobada (900m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada (1500 metros)"] <- "Arrobada (1500m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada(1800 m)"] <- "Arrobada (1800m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada(3625 metros)"] <- "Arrobada (3625m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada"] <- "Arrobada (3625m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobadas"] <- "Arrobada (3625m)"
db4$s4_3_2e[db4$s4_3_2e == "Arroba"] <- "Arrobada (3625m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobas"] <- "Arrobada (3625m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada (3600 metros)"] <- "Arrobada (3600m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada (3650 metros)"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada (3650metros)"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada 3650 metros"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada de 3650 metros"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobada(3650 metros cada uno)"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobadas de 3650 metros"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobadas de 3650 metros c/u"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Arrobadas de 3650 metros cada uno"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "3 y 1/2 arrobadas de 3650 metros"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Cajón(arrobada:3650 metros)"] <- "Arrobada (3650m)"
db4$s4_3_2e[db4$s4_3_2e == "Plantas"] <- "Arboles"
db4$s4_3_2e[db4$s4_3_2e == "Plantines"] <- "Arboles"
db4$s4_3_2e[db4$s4_3_2e == "Parcela"] <- "Arboles"
table(db4$s4_3_2e)

db4$s4_3_2[db4$"_index" == 191] <- 1
db4$s4_3_2[db4$"_index" == 209] <- 5
db4$s4_3_2[db4$"_index" == 227] <- 1
db4$s4_3_2[db4$"_index" == 264] <- 1
db4$s4_3_2[db4$"_index" == 337] <- 250
db4$s4_3_2[db4$"_index" == 340] <- 100
db4$s4_3_2[db4$"_index" == 492] <- 1000
db4$s4_3_2[db4$"_index" == 423] <- 0.25
db4$s4_3_2u[db4$"_index" == 423] <- 1
db4$s4_3_2[db4$"_index" == 863] <- 1
db4$s4_3_2[db4$"_index" == 1565] <- 3.5
db4$s4_3_2[db4$"_index" == 585] <- 200
db4$s4_3_2[db4$"_index" == 774] <- 100
db4$s4_3_2[db4$"_index" == 1039] <- 100
db4$s4_3_2[db4$"_index" == 2123] <- 1 
db4$s4_3_2u[db4$"_index" == 2123] <- 1
db4$s4_3_2e[db4$"_index" == 2123] <- NA
db4$s4_3_2[db4$"_index" == 2229] <- 0.5
db4$s4_3_2[db4$"_index" == 765] <- 900
db4$s4_3_2[db4$"_index" == 748] <- 6500
db4$s4_3_2[db4$"_index" == 333] <- 1250
db4$s4_3_2[db4$"_index" == 334] <- 1250
db4$s4_3_2[db4$"_index" == 1556] <- 2900
db4$s4_3_2[db4$"_index" == 1938] <- 400
db4$s4_3_2[db4$"_index" == 2023] <- 100
db4$s4_3_2[db4$"_index" == 2053] <- 100
db4$s4_3_2[db4$"_index" == 2067] <- 100
db4$s4_3_2[db4$"_index" == 1092] <- 100
db4$s4_3_2[db4$"_index" == 1367] <- 6000
db4$s4_3_2[db4$"_index" == 2113] <- 1
db4$s4_3_2[db4$"_index" == 2142] <- 80
db4$s4_3_2[db4$"_index" == 1677] <- 50
db4$s4_3_2[db4$"_index" == 1683] <- 60
db4$s4_3_2[db4$"_index" == 1689] <- 100
db4$s4_3_2[db4$"_index" == 1749] <- 60
db4$s4_3_2[db4$"_index" == 1772] <- 1813
db4$s4_3_2[db4$"_index" == 457] <- 350
db4$s4_3_2[db4$"_index" == 1805] <- 100
db4$s4_3_2[db4$"_index" == 1806] <- 100
db4$s4_3_2[db4$"_index" == 1813] <- 100
db4$s4_3_2[db4$"_index" == 1879] <- 50
db4$s4_3_2[db4$"_index" == 584] <- 1850
db4$s4_3_2[db4$"_index" == 2148] <- 900
db4$s4_3_2[db4$"_index" == 2154] <- 1000
db4$s4_3_2[db4$"_index" == 2481] <- 4000
db4$s4_3_2[db4$"_index" == 2685] <- 100
db4$s4_3_2[db4$"_index" == 2823] <- 300
db4$s4_3_2[db4$"_index" == 2831] <- 2500
db4$s4_3_2u[db4$"_index" == 2831] <- 2
db4$s4_3_2e[db4$"_index" == 3077] <- NA
db4$s4_3_2u[db4$"_index" == 3077] <- 1
db4$s4_3_2u[db4$"_index" == 2930] <- 1
db4$s4_3_2[db4$"_index" == 29] <- 0.5 
db4$s4_3_2u[db4$"_index" == 29] <- 1 
db4$s4_3_2[db4$"_index" == 32] <- 0.2 
db4$s4_3_2u[db4$"_index" == 32] <- 1 
db4$s4_3_2[db4$"_index" == 34] <- 0.5 
db4$s4_3_2u[db4$"_index" == 34] <- 1 
db4$s4_3_2[db4$"_index" == 34] <- 3 
db4$s4_3_2u[db4$"_index" == 34] <- 1
db4$s4_3_2u[db4$"_index" == 708] <- 1
db4$s4_3_2u[db4$"_index" == 1052] <- 1
db4$s4_3_2[db4$"_index" == 40] <- 7000 
db4$s4_3_2u[db4$"_index" == 40] <- 2 
db4$s4_3_2[db4$"_index" == 42] <- 1000 
db4$s4_3_2u[db4$"_index" == 42] <- 2 
db4$s4_3_2[db4$"_index" == 508] <- 1.5 
db4$s4_3_2u[db4$"_index" == 505] <- 1 
db4$s4_3_2u[db4$"_index" == 2113] <- 1

#Ajustes en pérdidas (por superficie)
db4$perdida[db4$"_index" == 1924] <- 100
db4$perdida[db4$"_index" == 34] <- 100
db4$perdida[db4$"_index" == 99] <- 100
db4$perdida[db4$"_index" == 174] <- 100
db4$perdida[db4$"_index" == 756] <- 100
db4$perdida[db4$"_index" == 767] <- 100
db4$perdida[db4$"_index" == 864] <- 100
db4$perdida[db4$"_index" == 1113] <- 100
db4$perdida[db4$"_index" == 1239] <- 100
db4$perdida[db4$"_index" == 1599] <- 100
db4$perdida[db4$"_index" == 2286] <- 100

# Ajustes a cantidades de peso
db4$s4n7[db4$"_index" == 100] <- 19000 
db4$s4n7[db4$"_index" == 333] <- 10
db4$s4n7[db4$"_index" == 664] <- 800
db4$s4n7[db4$"_index" == 972] <- 160
db4$s4n7[db4$"_index" == 989] <- 100
db4$s4n7[db4$"_index" == 1210] <- 20
db4$s4n7[db4$"_index" == 1223] <- 108
db4$s4n7[db4$"_index" == 1425] <- 0.5
db4$s4n7[db4$"_index" == 1426] <- 60
db4$s4n7[db4$"_index" == 1598] <- 8
db4$s4n7[db4$"_index" == 1569] <- 8
db4$s4n7[db4$"_index" == 2220] <- 200
db4$s4n7[db4$"_index" == 2723] <- 320 
db4$s4n7[db4$"_index" == 2236] <- 3

db4$s4n7_u[db4$"_index" == 508] <- 4
db4$s4n7_u[db4$"_index" == 989] <- 4
db4$s4n7_u[db4$"_index" == 664] <- 4
db4$s4n7_u[db4$"_index" == 1921] <- 2
db4$s4n7_u[db4$"_index" == 2236] <- 4
db4$s4n7_u[db4$"_index" == 533] <- 3
db4$s4n7_u[db4$"_index" == 3151] <- 4

# Estandarización de superficies ####
db4 <- db4 %>%
  mutate(
    s4_sup_std = case_when(
      s4_3_2u == 1 ~ as.numeric(s4_3_2), # Hectáreas <- default
      s4_3_2u == 2 ~ as.numeric(s4_3_2)/ 10000, # Metros cuadrados
      s4_3_2u == 3 ~ as.numeric(s4_3_2) * 2500 / 10000, # Tarea (2500m2)
      s4_3_2u == 99 & s4_3_2e == "Arrobada (900m)" ~ as.numeric(s4_3_2) * 900 / 10000, # Arrobada 900,
      s4_3_2u == 99 & s4_3_2e == "Arrobada (1500m)" ~ as.numeric(s4_3_2) * 1500 / 10000, # Arrobada 1500,
      s4_3_2u == 99 & s4_3_2e == "Arrobada (1800m)" ~ as.numeric(s4_3_2) * 1800 / 10000, # Arrobada 1800,
      s4_3_2u == 99 & s4_3_2e == "Arrobada (3600m)" ~ as.numeric(s4_3_2) * 3600 / 10000, # Arrobada 3600,
      s4_3_2u == 99 & s4_3_2e == "Arrobada (3625m)" ~ as.numeric(s4_3_2) * 3625 / 10000, # Arrobada 3625,
      s4_3_2u == 99 & s4_3_2e == "Arrobada (3650m)" ~ as.numeric(s4_3_2) * 3650 / 10000, # Arrobada 3650,
      s4_3_2u == 99 & s4_3_2e == "Arboles" ~ as.numeric(s4_3_2) * 90 / 10000, # Arrobada 3650,
      s4_3_2u == 99 ~ as.numeric(s4_3_2), # Otros, sin conversión específica
      TRUE ~ NA_real_ # Caso por defecto si ninguna condición anterior se cumple
    )
  )

table(db4$s4_sup_std)

# Revisión NA
table(db4$s4_3_2, useNA = "always")

db4_consistencia_sup <- db4 %>% 
  filter(is.na(s4_3_2) & (s4n7 == 0 | is.na(s4n7))) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)

db4 <- db4 %>%
  filter(!(is.na(s4_3_2) & (s4n7 == 0 | is.na(s4n7)))) 

table(db4$s4_3_2, useNA = "always")

db4_consistencia_sup3 <- db4 %>% 
  filter(is.na(s4_3_2)) %>% 
  select(s4_3_1, `_index`,s4_3_2, s4n7, perdida)

# Revisión 0's
db4_consistencia_sup4 <- db4 %>% 
  filter(s4_3_2 ==0 & (s4n7 == 0 | is.na(s4n7))) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)


# Estandarización de cantidades (kg)####
# Ajustes de "otros"
db4$s4n7_u[db4$s4n7_e == "Arroba"] <- 3
db4$s4n7_e[db4$s4n7_e == "Arroba"] <- NA
db4$s4n7_u[db4$s4n7_e == "anegas ( 1 quintal =1 anegar)"] <- 4
db4$s4n7_e[db4$s4n7_e == "anegas ( 1 quintal =1 anegar)"] <- NA
db4$s4n7_e[db4$s4n7_e == "Cargas de 8 @"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas (8 arrobas)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas de 8 arrobas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas( 8 arrobas c/u)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas( 8 arrobas cada uno)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas(8 arrobas c/u)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas(8 arrobas)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas(8@c/u)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsa de 8 arrobas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 8 arrobas cada uno"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bultos (8@)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bultos (8@)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas (8 @ la carga)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "28Cargas (bolsas c/u 8 @) cabeza\n28 Quepos (c/u 8. @) verde"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Sacañas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "10 Cargas x hectárea"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga de arrobas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 8 arrobas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 8@"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas 8@"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Fanegas ( cargas cada fanega)"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "1600@"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "6 bolsas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas"] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cajas de 2 arrobas"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Gangochos de 2 arrobas cada uno"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsa 2 arrobas"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "160 bolsas"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "bolsas"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsa de 2 arrobas"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Yutes"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Kepis"] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas( 3@= bolsa)"] <- "Carga (3 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cajas"] <- "Carga (3 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 3 arrobas"] <- "Carga (3 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas (4 @ carga)"] <- "Carga (4 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas de 4 arrobas"] <- "Carga (4 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Sacos"] <- "Carga (4 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Yutes de 5@"] <- "Carga (5 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 7 arrobas"] <- "Carga (7 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Camionada a granel de 40 bolsas de 7 arrobas"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Camionada"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cameonada"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Camión"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Directo de terreno a camión"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas de camión"] <- "Camionada (40x7arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 9 arrobas cada uno"] <- "Carga (9 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga(10 @)"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas 10 arrobas"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsa de 10 arrobas"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 10 arrobas"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga 10 arrobas"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga 40 libras"] <- "Carga (40 libras)"
db4$s4n7_e[db4$s4n7_e == "Carga de 10"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga de 10 arrobas"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsa de 10 @"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 10 @"] <- "Carga (10 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga de 12"] <- "Carga (12 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga 12 arrobas"] <- "Carga (12 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Carga de 12 arrobas"] <- "Carga (12 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas 12"] <- "Carga (12 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Cargas de 12 arrobas"] <- "Carga (12 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Bultos(13 arrobas)"] <- "Carga (13 arrobas)"
db4$s4n7_e[db4$s4n7_e == "Caja de 40 libras"] <- "Carga (40 libras)"
db4$s4n7_e[db4$s4n7_e == "Cargas(9@)"] <- "Carga (9 arrobas)"
db4$s4n7_e[db4$"_index" == 493] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$"_index" == 565] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$"_index" == 565] <- "Carga (8 arrobas)"
db4$s4n7_e[db4$"_index" == 1030] <- "Unidades de durazno"
db4$s4n7_e[db4$"_index" == 1032] <- "Unidades de durazno"
db4$s4n9_p[db4$"_index" == 1032] <- 0.80
db4$s4n7_e[db4$"_index" == 2544] <- "Unidades de durazno"
db4$s4n9_p[db4$"_index" == 2544] <- 0.99
db4$s4n7_e[db4$"_index" == 9] <- "Unidades de lechuga"
db4$s4n7_e[db4$"_index" == 25] <- "Unidades de cebolla"
db4$s4n7_e[db4$s4n7_e == "Bolsa de 150 unidades"] <- "Bolsa (150 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsas (100 a 150 unidafes)"] <- "Bolsa (150 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsas (100 a 150 unidades)"] <- "Bolsa (150 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsa (150 unidades) Choclo"] <- "Bolsa (150 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsas = 140 unid"] <- "Bolsa (140 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsa (140 unidades) Choclo"] <- "Bolsa (140 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Bolsas de 170 choclos"] <- "Bolsa (170 unidades) Choclo"
db4$s4n7_e[db4$s4n7_e == "Ch'ipas"] <- "Chipa (160u)"
db4$s4n7_e[db4$s4n7_e == "Chipa"] <- "Chipa (160u)"
db4$s4n7_e[db4$s4n7_e == "Chpa"] <- "Chipa (160u)"
db4$s4n7_e[db4$s4n7_e == "Chicas de 130"] <- "Chipa (160u)"
db4$s4n7_e[db4$s4n7_e == "Chipas 200"] <- "Chipa (200u)"
db4$s4n7_e[db4$"_index" == 2896] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$"_index" == 2893] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$"_index" == 2899] <- "Carga (2 arrobas)"
db4$s4n7_e[db4$"_index" == 2902] <- "Carga (2 arrobas)"

db4$s4n8_e[db4$"_index" == 2896] <- "Carga (2 arrobas)"
db4$s4n8_e[db4$"_index" == 2893] <- "Carga (2 arrobas)"

db4$s4n9_e[db4$"_index" == 2896] <- "Carga (2 arrobas)"
db4$s4n9_e[db4$"_index" == 2893] <- "Carga (2 arrobas)"
db4$s4n9_e[db4$"_index" == 2899] <- "Carga (2 arrobas)"
db4$s4n9_e[db4$"_index" == 2902] <- "Carga (2 arrobas)"

check_noq <- which(db4$s4n7_e == "Aun no esta en producción" | db4$s4n7_e == "Nada" |
                     db4$s4n7_e == "No se cosecho")
check_noq_id <- db4$`_index`[check_noq]

db4$s4n7_e[db4$"_index" == 184] <- NA
db4$s4n7_u[db4$"_index" == 184] <- NA



db4$s4n7_e[db4$"_index" == 1542] <- NA
db4$s4n7_u[db4$"_index" == 1542] <- NA
db4$perdida[db4$"_index" == 1542] <- 100

db4$s4n7_e[db4$"_index" == 1725] <- NA
db4$s4n7_u[db4$"_index" == 1725] <- NA

db4$s4n7_e[db4$"_index" == 1763] <- NA
db4$s4n7_u[db4$"_index" == 1763] <- NA

db4$s4n7_e[db4$"_index" == 2236] <- NA
db4$s4n7_u[db4$"_index" == 2236] <- NA

table(db4$s4n7_e)

table(db4$s4n7_u)
db4 <- db4 %>%
  mutate(
    s4_q_std = case_when(
      s4n7_u == 1 ~ as.numeric(s4n7) /1000, # kg 
      s4n7_u == 2 ~ as.numeric(s4n7) * 0.453592/1000, # libras
      s4n7_u == 3 ~ as.numeric(s4n7) * 11.5/1000, # arrobas
      s4n7_u == 4 ~ as.numeric(s4n7) * 46/1000, # quintales
      s4n7_u == 5 ~ as.numeric(s4n7), # toneladas <- default
      s4n7_u == 6 ~ as.numeric(s4n7) * 22/1000, # caja promedio,
      s4n7_u == 7 & s4n7_e == "Amarros" ~ as.numeric(s4n7) * 12/1000 , # Amarro 12 kg
      s4n7_u == 7 & s4n7_e == "Cuartillas" ~ as.numeric(s4n7) * 2.88/1000 , # Cuartilla 2.88 kg
      s4n7_u == 7 & s4n7_e == "Carga (2 arrobas)" ~ as.numeric(s4n7) * 2 * 11.5/1000, # Carga 2@
      s4n7_u == 7 & s4n7_e == "Carga (3 arrobas)" ~ as.numeric(s4n7) * 3 * 11.5/1000, # Carga 3@
      s4n7_u == 7 & s4n7_e == "Carga (4 arrobas)" ~ as.numeric(s4n7) * 4 * 11.5/1000, # Carga 4@
      s4n7_u == 7 & s4n7_e == "Carga (5 arrobas)" ~ as.numeric(s4n7) * 5 * 11.5/1000, # Carga 5@
      s4n7_u == 7 & s4n7_e == "Carga (7 arrobas)" ~ as.numeric(s4n7) * 7 * 11.5/1000, # Carga 7@
      s4n7_u == 7 & s4n7_e == "Carga (8 arrobas)" ~ as.numeric(s4n7) * 8 * 11.5/1000, # Carga 8@
      s4n7_u == 7 & s4n7_e == "Carga (9 arrobas)" ~ as.numeric(s4n7) * 9 * 11.5/1000, # Carga 9@
      s4n7_u == 7 & s4n7_e == "Carga (10 arrobas)" ~ as.numeric(s4n7) * 10 * 11.5/1000, # Carga 10@
      s4n7_u == 7 & s4n7_e == "Carga (13 arrobas)" ~ as.numeric(s4n7) * 13 * 11.5/1000, # Carga 13@
      s4n7_u == 7 & s4n7_e == "Carga (40 libras)" ~ as.numeric(s4n7) * 0.453592 * 40/1000, # Carga 40lb
      s4n7_u == 7 & s4n7_e == "Camionada (40x7arrobas)" ~ as.numeric(s4n7) * 40 * 7 * 11.5/1000, # Camionada 40 cargas de 7@
      s4n7_u == 7 & s4n7_e == "Bolsa (150 unidades) Choclo" ~ as.numeric(s4n7) * 150 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n7_u == 7 & s4n7_e == "Bolsa (140 unidades) Choclo" ~ as.numeric(s4n7) * 140 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n7_u == 7 & s4n7_e == "Bolsa (170 unidades) Choclo" ~ as.numeric(s4n7) * 170 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n7_u == 7 & s4n7_e == "Unidades de durazno" ~ as.numeric(s4n7) * 0.07/1000, # Duraznos 70[g]
      s4n7_u == 7 & s4n7_e == "Chipa (160u)" ~ as.numeric(s4n7) * 160 * 0.220/1000, # Cebollas 220[g]
      s4n7_u == 7 & s4n7_e == "Chipa (200u)" ~ as.numeric(s4n7) * 200 * 0.220/1000, # Cebollas 220[g]
      s4n7_u == 7 ~ as.numeric(s4n7), # Otros, sin conversión específica
      TRUE ~ NA_real_ # Caso por defecto si ninguna condición anterior se cumple
    )
  )

table(db4$s4n7_u, useNA = "always")
table(db4$s4n7_e, useNA = "always")
# Corrección superficie y cantidad (NA) #
# Corrección para papa; inclusión de valor promedio #
db4_consistencia_sup3 <- db4 %>% 
  filter(is.na(s4_3_2)) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)

 db4 <- db4 %>%
  mutate(s4_sup_std = case_when(
        s4_3_1 == "Papa" & is.na(s4_3_2) & s4_q_std != 0 & 
        (s4n7 != 0 | !is.na(s4n7)) & (perdida != 100 | is.na(perdida)) ~ (s4_q_std / 8 / ((1 - perdida / 100))),
      TRUE ~ s4_sup_std
    ))

db4_check_supq <- db4 %>% 
  filter(is.na(s4_sup_std)) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)

table(db4$s4_sup_std, useNA = "always")

 # Pérdidas ####

table(db4$perdida)

db4 <- db4 %>%
  mutate(perdida_10 = case_when(
    perdida >= 0 & perdida <= 5 ~ 0,  # Categoría especial de 0 a 5
    perdida > 5 & perdida <= 10 ~ 10, # De 6 a 10
    perdida > 10 & perdida <= 20 ~ 20, # De 11 a 20
    perdida > 20 & perdida <= 30 ~ 30, # De 21 a 30
    perdida > 30 & perdida <= 40 ~ 40, # De 31 a 40
    perdida > 40 & perdida <= 50 ~ 50, # De 41 a 50
    perdida > 50 & perdida <= 60 ~ 60, # De 51 a 60
    perdida > 60 & perdida <= 70 ~ 70, # De 61 a 70
    perdida > 70 & perdida <= 80 ~ 80, # De 71 a 80
    perdida > 80 & perdida <= 90 ~ 90, # De 81 a 90
    perdida > 90 & perdida <= 100 ~ 100, # De 91 a 100
    TRUE ~ NA_real_ # Para valores fuera de rango (como el 203)
   ))
 
table(db4$perdida_10)
hist(db4$perdida_10)
 
percentil_perdida <- calcular_percentiles(db4, "perdida")
print(percentil_perdida)
 
mean(db4$perdida, na.rm = TRUE) #PERDIDA PROMEDIO!!!!
 
db4_consistencia_perd <- db4 %>% 
  filter(s4n7 == 0 & perdida != 100) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)

db4 <- db4 %>% 
  mutate(perdida = ifelse((s4n7 == 0 & perdida == 0), 100, perdida))

db4_perdida_check <- db4 %>% 
   select(`_index`, s4_3_2, s4n7, perdida)

db4_consistencia_q <- db4 %>% 
   filter(s4_2_5 == 1 & is.na(s4n7)) %>% 
   select(`_index`,s4_3_2, s4n7, perdida)
 
db4 <- db4 %>%
   mutate(s4n7 = case_when(
     is.na(s4n7) & s4_2_5 == 1 & perdida == 100 ~ 0,
     TRUE ~ s4n7
   ))

# Factores de pérdidas
 # Cálculo de rendimientos DB4 ####
 db4 <- db4 %>% 
   mutate(rend = s4_q_std / s4_sup_std)

# revisión final rend
db4 <- db4 %>% 
  filter(!(`_index` == 359))

db4_checklow_nop <- db4 %>% 
  filter((s4_sup_std <= 0.001) & s4_2_5 == 2)%>% 
  select(`_index`, s4_2_5, s4_3_1, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, perdida)

indices_checklow_nop <- db4_checklow_nop$`_index`

db4 <- db4 %>%
  filter(!(`_index` %in% indices_checklow_nop))

db4_checklow <- db4 %>% 
  filter((s4_sup_std <= 0.001))%>% 
  select(`_index`, s4_3_1, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, perdida)


# db4$s4n7_u[db4$"_index" == 184] <- NA
# db4$s4n7_u[db4$"_index" == 184] <- NA

mean(db4$s4_sup_std, na.rm = TRUE)
median(db4$s4_sup_std, na.rm = TRUE)
max(db4$s4_sup_std, na.rm = TRUE)
min(db4$s4_sup_std, na.rm = TRUE)

db4_sup_checkfin2 <- db4 %>% 
  filter(s4_sup_std == 0) %>% 
  select(`_index`,s4_2_5, s4_3_1, s4_3_2, s4_3_2u, s4_3_2e, s4_sup_std, rend, s4n7,perdida) %>% 
  arrange()

# Completando pérdidas ####

d4_perd_check <- db4 %>% 
  filter(perdida > 100) %>% # Mayores a 100%
  select(`_index`, s4_3_1, s4_sup_std, s4_q_std, rend, perdida)

db4$perdida[db4$"_index" == 3272] <- 20 
db4$perdida[db4$"_index" == 2264] <- 50
db4$perdida[db4$"_index" == 1008] <- 50


# Revisión riego ####
db4 <- db4 %>%
  mutate(s4n5_e = case_when(
    grepl("Pozo|pozo|Agua de tanque", s4n5_e, ignore.case = TRUE) ~ "Pozo",
    grepl("Agua de rio|Estanco|Estanque con agua depocitada|Filtrante|Riego de rio|Riego del rio|Rio|Vertiendmtes|Vertiente|Vertiente estanque", s4n5_e, ignore.case = TRUE) ~ "Fuentes naturales cercanas",
    grepl("Bomba|Aspisora|Bomba ase agua|Bomba, Inundación|Bomba sumergible|Bomba en sequia|Bomba de riego|Bomba ase agua", s4n5_e, ignore.case = TRUE) ~ "Bomba",
    grepl("Acequia|Sequia|Sequía|Asequia", s4n5_e, ignore.case = TRUE) ~ "Acequia",
    grepl("Gravedad|Ibundacion|Inundacion|Inindacion|inundacion|Inundasion|Inundacio|Inundación|riego por inundación|Inundación por canal|Riego por inundación|Inundaciones", s4n5_e, ignore.case = TRUE) ~ "Gravedad",
    grepl("Cañeria|Microriego|Regado con manguera|Trmporal|Pileta|Con mangera|Cañerias|De la pila con manguera|Canal|canalización|Canalización|Canalizado|Canales|tuberías|Tuberías", s4n5_e, ignore.case = TRUE) ~ "Canales/Tuberías",
    TRUE ~ s4n5_e # Cambiar a "Otro" para los valores que no cumplen ninguna condición anterior
  ))

table(db4$s4n5_e)

# Ajustes#
indices_a_cambiar <- which(db4$s4n5_e %in% c("A secano", "Arroba", "Lluvia", "Agua de lluvia", 
                                             "Lluvia temporal", "Lluvia (temporal)", "Lluviaa", "Manual", "Mita", 
                                             "Propio", "Regado con manguera", "Comprado", 
                                             "Riega", "Riego", "Secano", "Sistema de riego", "Socio de riego", 
                                             "Temporal", "temporal", "Temporal lluvia", "tempral", 
                                             "Tradicional calaca"))

# Cambios en s4n5e
db4$s4n5[indices_a_cambiar] <- NA
db4$s4n5_e[indices_a_cambiar] <- NA
db4 <- db4 %>%
  mutate(s4n4 = if_else(s4n4 == 1 & is.na(s4n5), 2, s4n4))

table(db4$s4n5)
table(db4$s4n5_e)

indices_gravedad <- which(db4$s4n5_e == "Gravedad")
db4$s4n5_e[indices_gravedad] <- NA
db4$s4n5[indices_gravedad] <- ifelse(db4$s4n5[indices_gravedad] == 4, 1, db4$s4n5[indices_gravedad])

# Codificación
db4 <- label_sino (db4, "s4n4")
db4 <- db4 %>%
  mutate(s4n5 = case_when(
    s4n5 == 1 ~ "Gravedad",
    s4n5 == 2 ~ "Asperción",
    s4n5 == 3 ~ "Goteo",
    s4n5 == 4 ~ "Otro",
    TRUE ~ as.character(s4n5) # Para manejar cualquier otro valor no especificado
  ))


# Venta, consumo y ventas ####
#[código de análisis, ver ejemplo siguiente]

#Consumo
db4_check_consumo <- db4 %>% 
  filter(s4n8 == 0)

db4_check_consumo2 <- db4 %>% 
  filter(!(is.na(s4n8_e))) %>% 
  select(s4n8_e)

db4 <- db4 %>%
  mutate(s4n8_u = ifelse(s4n8 == 0, NA, s4n8_u)) %>% 
  mutate(s4n8_e = ifelse(s4n8 == 0 | grepl("qq", s4n8_e), NA, s4n8_e)) %>% 
  mutate(s4n8_e = ifelse(!(is.na(s4n8_e)), s4n7_e, s4n8_e))

table(db4$s4n8_u)
table(db4$s4n8_e)
  
db4 <- db4 %>%
  mutate(
    s4_q_cons_std = case_when(
      s4n8_u == 1 ~ as.numeric(s4n8)/1000, # kg 
      s4n8_u == 2 ~ as.numeric(s4n8) * 0.453592/1000, # libras
      s4n8_u == 3 ~ as.numeric(s4n8) * 11.5/1000, # arrobas
      s4n8_u == 4 ~ as.numeric(s4n8) * 46/1000, # quintales
      s4n8_u == 5 ~ as.numeric(s4n8) * 1000/1000, # toneladas <- default
      s4n8_u == 6 ~ as.numeric(s4n8) * 22/1000,
      s4n8_u == 7 & s4n8_e == "Amarros" ~ as.numeric(s4n8) * 12/1000 , # Amarro 12 kg
      s4n8_u == 7 & s4n8_e == "Cuartillas" ~ as.numeric(s4n8) * 2.88/1000 , # Cuartilla 2.88 kg
      s4n8_u == 7 & s4n8_e == "Carga (2 arrobas)" ~ as.numeric(s4n8) * 2 * 11.5/1000, # Carga 2@
      s4n8_u == 7 & s4n8_e == "Carga (3 arrobas)" ~ as.numeric(s4n8) * 3 * 11.5/1000, # Carga 3@
      s4n8_u == 7 & s4n8_e == "Carga (4 arrobas)" ~ as.numeric(s4n8) * 4 * 11.5/1000, # Carga 4@
      s4n8_u == 7 & s4n8_e == "Carga (5 arrobas)" ~ as.numeric(s4n8) * 5 * 11.5/1000, # Carga 5@
      s4n8_u == 7 & s4n8_e == "Carga (7 arrobas)" ~ as.numeric(s4n8) * 7 * 11.5/1000, # Carga 7@
      s4n8_u == 7 & s4n8_e == "Carga (8 arrobas)" ~ as.numeric(s4n8) * 8 * 11.5/1000, # Carga 8@
      s4n8_u == 7 & s4n8_e == "Carga (9 arrobas)" ~ as.numeric(s4n8) * 9 * 11.5/1000, # Carga 9@
      s4n8_u == 7 & s4n8_e == "Carga (10 arrobas)" ~ as.numeric(s4n8) * 10 * 11.5/1000, # Carga 10@
      s4n8_u == 7 & s4n8_e == "Carga (13 arrobas)" ~ as.numeric(s4n8) * 13 * 11.5/1000, # Carga 13@
      s4n8_u == 7 & s4n8_e == "Carga (40 libras)" ~ as.numeric(s4n8) * 0.453592 * 40/1000, # Carga 40lb
      s4n8_u == 7 & s4n8_e == "Camionada (40x7arrobas)" ~ as.numeric(s4n8) * 40 * 7 * 11.5/1000, # Camionada 40 cargas de 7@
      s4n8_u == 7 & s4n8_e == "Bolsa (150 unidades) Choclo" ~ as.numeric(s4n8) * 150 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n8_u == 7 & s4n8_e == "Bolsa (140 unidades) Choclo" ~ as.numeric(s4n8) * 140 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n8_u == 7 & s4n8_e == "Bolsa (170 unidades) Choclo" ~ as.numeric(s4n8) * 170 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n8_u == 7 & s4n8_e == "Unidades de durazno" ~ as.numeric(s4n8) * 0.07/1000, # Duraznos 70[g]
      s4n8_u == 7 & s4n8_e == "Chipa (160u)" ~ as.numeric(s4n8) * 160 * 0.220/1000, # Cebollas 220[g]
      s4n8_u == 7 & s4n8_e == "Chipa (200u)" ~ as.numeric(s4n8) * 200 * 0.220/1000, # Cebollas 220[g]
      s4n8_u == 7 ~ as.numeric(s4n8), # Otros, sin conversión específica
      TRUE ~ NA_real_ # Caso por defecto si ninguna condición anterior se cumple
    ))

table(db4$s4n8_u)
table(db4$s4n8_e)
table(db4$s4_q_cons_std)

mean(db4$s4_q_cons_std, na.rm = TRUE)
median(db4$s4_q_cons_std, na.rm = TRUE)

db4_check_consumo2 <- db4 %>% 
  select(s4_q_std, s4_q_cons_std)

# Ventas
db4_check_venta <- db4 %>% 
  filter(s4n9 == 0)


db4 <- db4 %>%
  mutate(s4n9_u = ifelse(s4n9 == 0, NA, s4n9_u)) %>% 
  mutate(s4n9_e = ifelse(s4n9 == 0, NA, s4n9_e)) %>% 
  mutate(s4n9_p = ifelse(s4n9 == 0, NA, s4n9_p)) %>% 
  mutate(s4n9_l = ifelse(s4n9 == 0, NA, s4n9_l))

table(db4$s4n9_u)
table(db4$s4n9_e)

# Ventas

db4_check_venta <- db4 %>%
  filter(!is.na(s4n9)) %>%
  select(`_index`,s4_2_5, s4_3_1, s4_3_2, s4_3_2u, s4_sup_std, rend, s4n7, s4n8, s4n9, perdida)

db4_check_venta2 <- db4 %>%
  filter(s4n9 == 0)

db4_check_venta3 <-db4 %>%
  filter(!is.na(s4n7_e) & !is.na(s4n9_e)) %>% 
  select(`_index`, s4n7_e, s4n9_e) #REVISAR 517

db4 <- db4 %>%
  mutate(s4n9_u = ifelse(s4n9 == 0, NA, s4n9_u)) %>%
  mutate(s4n9_e = ifelse(s4n9 == 0, NA, s4n9_e)) %>% 
  mutate(s4n9_e = ifelse(!(is.na(s4n9_e)), s4n7_e, s4n9_e))  

table(db4$s4n9_u)
table(db4$s4n9_e)

db4 <- db4 %>%
  mutate(
    s4_q_venta_std = case_when(
      s4n9_u == 1 ~ as.numeric(s4n9)/1000, # kg
      s4n9_u == 2 ~ as.numeric(s4n9) * 0.453592/1000, # libras
      s4n9_u == 3 ~ as.numeric(s4n9) * 11.5/1000, # arrobas
      s4n9_u == 4 ~ as.numeric(s4n9) * 46/1000, # quintales,
      s4n9_u == 5 ~ as.numeric(s4n9), # toneladas,
      s4n9_u == 6 ~ as.numeric(s4n9) * 22/1000,
      s4n9_u == 7 & s4n9_e == "Amarros" ~ as.numeric(s4n9) * 12/1000 , # Amarro 12 kg
      s4n9_u == 7 & s4n9_e == "Cuartillas" ~ as.numeric(s4n9) * 2.88/1000 , # Cuartilla 2.88 kg
      s4n9_u == 7 & s4n9_e == "Carga (2 arrobas)" ~ as.numeric(s4n9) * 2 * 11.5/1000, # Carga 2@
      s4n9_u == 7 & s4n9_e == "Carga (3 arrobas)" ~ as.numeric(s4n9) * 3 * 11.5/1000, # Carga 3@
      s4n9_u == 7 & s4n9_e == "Carga (4 arrobas)" ~ as.numeric(s4n9) * 4 * 11.5/1000, # Carga 4@
      s4n9_u == 7 & s4n9_e == "Carga (5 arrobas)" ~ as.numeric(s4n9) * 5 * 11.5/1000, # Carga 5@
      s4n9_u == 7 & s4n9_e == "Carga (7 arrobas)" ~ as.numeric(s4n9) * 7 * 11.5/1000, # Carga 7@
      s4n9_u == 7 & s4n9_e == "Carga (8 arrobas)" ~ as.numeric(s4n9) * 8 * 11.5/1000, # Carga 8@
      s4n9_u == 7 & s4n9_e == "Carga (9 arrobas)" ~ as.numeric(s4n9) * 9 * 11.5/1000, # Carga 9@
      s4n9_u == 7 & s4n9_e == "Carga (10 arrobas)" ~ as.numeric(s4n9) * 10 * 11.5/1000, # Carga 10@
      s4n9_u == 7 & s4n9_e == "Carga (13 arrobas)" ~ as.numeric(s4n9) * 13 * 11.5/1000, # Carga 13@
      s4n9_u == 7 & s4n9_e == "Carga (40 libras)" ~ as.numeric(s4n9) * 0.453592 * 40/1000, # Carga 40lb
      s4n9_u == 7 & s4n9_e == "Camionada (40x7arrobas)" ~ as.numeric(s4n9) * 40 * 7 * 11.5/1000, # Camionada 40 cargas de 7@
      s4n9_u == 7 & s4n9_e == "Bolsa (150 unidades) Choclo" ~ as.numeric(s4n9) * 150 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n9_u == 7 & s4n9_e == "Bolsa (140 unidades) Choclo" ~ as.numeric(s4n9) * 140 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n9_u == 7 & s4n9_e == "Bolsa (170 unidades) Choclo" ~ as.numeric(s4n9) * 170 * 0.2/1000, # Mazorcas con chala 200[g]
      s4n9_u == 7 & s4n9_e == "Unidades de durazno" ~ as.numeric(s4n9) * 0.07/1000, # Duraznos 70[g]
      s4n9_u == 7 & s4n9_e == "Chipa (160u)" ~ as.numeric(s4n9) * 160 * 0.220/1000, # Cebollas 220[g]
      s4n9_u == 7 & s4n9_e == "Chipa (200u)" ~ as.numeric(s4n9) * 200 * 0.220/1000, # Cebollas 220[g]
      s4n9_u == 7 ~ as.numeric(s4n9), # Otros, sin conversión específica
      TRUE ~ NA_real_ # Caso por defecto si ninguna condición anterior se cumple
    ))

table(db4$s4_q_venta_std)

db4_check_venta4 <-db4 %>%
  #filter(!is.na(s4n7_e) & !is.na(s4n9_e)) %>% 
  select(`_index`, s4_3_1, s4n7, s4n7_u, s4_q_std, s4n9, s4n9_u, s4_q_venta_std) #REVISAR 517

mean(db4$s4_q_venta_std, na.rm = TRUE)
median(db4$s4_q_venta_std, na.rm = TRUE)

calcular_estadisticos(db4, "s4_q_venta_std")

# Comercialización
table(db4$s4n9_l, useNA = "always")
#table(db4$s4n9_e_001)

db4 <- db4 %>%
  mutate(s4n9_e_001 = ifelse(s4n9 == 0, NA, s4n9_e_001)) %>%
  mutate(s4n9_u = ifelse(s4n9 == 0, NA, s4n9_u)) 

db4 <- db4 %>%
  mutate(
    s4n9_l = case_when(
      s4n9_e_001 %in% c("Empresa procesadora", "Camión a domicilio", "Vendida en terreno", 
                        "Camionadas a La Paz y Santa Cruz", "Camión directo", 
                        "Feria local e intermediario", "Ese precio lo vendio por cada Ramega en total son 5 Ramegas y cada uno es de 3600m y en total cada Ramega sacan 144@ por lo cual cada Ramega es de 1200bs",
                        "Ese es el precio de una Camionada, y Lo vendió en el Camión",
                        "De la tierra directo al camion", "Camión", "Camion",
                        "En su potrero") ~ 3L,
      
      s4n9_e_001 %in% c("Por raleo", 
                        "Ese precio lo vendio por cada Ramega en total son 5 Ramegas y cada uno es de 3600m y en total cada Ramega sacan 144@ por lo cual cada Ramega es de 1200bs",
                        "Ese es el precio de una Camionada, y Lo vendió en el Camión",
                        "Cuartilla", "Bs por 100", "No se vende", "No vendio", "No vendió") ~ NA_real_,
      
      s4n9_e_001 == "Propio Negocio" ~ as.double(99),
      TRUE ~ s4n9_l
    ),
    s4n9_e_001  = case_when(
      s4n9_e_001 %in% c("Empresa procesadora", "Camión a domicilio", "Vendida en terreno", 
                        "Camionadas a La Paz y Santa Cruz", "Camión directo", 
                        "Feria local e intermediario", "Ese precio lo vendio por cada Ramega en total son 5 Ramegas y cada uno es de 3600m y en total cada Ramega sacan 144@ por lo cual cada Ramega es de 1200bs",
                        "Ese es el precio de una Camionada, y Lo vendió en el Camión",
                        "De la tierra directo al camion", "Camión", "Camion",
                        "En su potrero","Por raleo","Ese precio lo vendio por cada Ramega en total son 5 Ramegas y cada uno es de 3600m y en total cada Ramega sacan 144@ por lo cual cada Ramega es de 1200bs",
                        "Ese es el precio de una Camionada, y Lo vendió en el Camión",
                        "Cuartilla", "Bs por 100", "No se vende", "No vendio", "No vendió") ~ NA_character_,
      TRUE ~ s4n9_e_001
      )
  )

db4_check_venta_X <- db4 %>% 
  filter(!is.na(s4n9_l) & !is.na(s4n9_e_001)) %>% 
  select(s4n9_l, s4n9_e_001)

db4 <- db4 %>%
  mutate(
    s4n9_l = case_when(
      s4n9_e_001 %in% c("No vendió", "No vendio", "No se vende") ~ s4n9_l,  # Mantener el valor original
      grepl("vendio|vendió|vende", s4n9_e_001, ignore.case = TRUE) ~ s4n9_l,  # Si contiene estas palabras, mantener el valor original
      grepl("la paz|lapaz", s4n9_e_001, ignore.case = TRUE) ~ 4,  # Para 'La Paz' en diferentes ortografías, asignar "4"
      grepl("cochabamba", s4n9_e_001, ignore.case = TRUE) ~ 4,  # Para 'Cochabamba', asignar "4"
      grepl("tarija", s4n9_e_001, ignore.case = TRUE) ~ 4,  # Para 'Tarija', asignar "4"
      grepl("el alto", s4n9_e_001, ignore.case = TRUE) ~ 4,  # Para 'El Alto', asignar "4"
      grepl("oruro|sucre|potosí|santa cruz|cbba|ciudad", s4n9_e_001, ignore.case = TRUE) ~ 4,  # Para otras ciudades importantes, asignar "4"
      !is.na(s4n9_e_001) ~ 4,  # Para cualquier otro texto no NA, asignar "4"
      TRUE ~ s4n9_l  # En cualquier otro caso, mantener el valor original
    )
  )
  
db4 <- db4 %>%
  mutate(s4n9_l = case_when(
    s4n9_l == 1 ~ "Asociación",
    s4n9_l == 2 ~ "Mercado Local",
    s4n9_l == 3 ~ "Intermediario",
    s4n9_l == 4 ~ "Directo a ciudad o ciudad intermedia",
    s4n9_l == 99 ~ "Otro",
    TRUE ~ as.character(s4n9_l) # Para manejar cualquier otro valor no especificado
  ))

# INGRESOS GENERAL ####
#CÁLCULOS PAPA ####
# Precio de consumo papa
db4_papa <- db4 %>% 
  filter(s4_3_1 == "Papa")
p_papa_prom <- db4_papa %>% 
  filter(s4n9_u == 4 & s4n9_p!=0) 
mean(p_papa_prom$s4n9_p, na.rm = TRUE)

pp_papa <- mean(p_papa_prom$s4n9_p, na.rm = TRUE)
pp_papa = (pp_papa/46)

# Precio de consumo maiz
db4_maiz <- db4 %>% 
  filter(s4_3_1 == "Maiz")
p_maiz_prom <- db4_maiz %>% 
  filter(s4n9_u == 4 & s4n9_p!=0) 
p_maiz_prom <- p_maiz_prom %>%
  filter(s4n9 > quantile(p_maiz_prom$s4n9, 0.15))
mean(p_maiz_prom$s4n9_p, na.rm = TRUE)

pp_maiz <- mean(p_maiz_prom$s4n9_p, na.rm = TRUE)
pp_maiz = (pp_maiz/46)

# Precio de consumo tomate
db4_tomate <- db4 %>% 
  filter(s4_3_1 == "Tomate")

table(db4_tomate$s4n9_u)

p_tomate_prom <- db4_tomate %>% 
  filter(s4n9_u == 6 & s4n9_p!=0) 
mean(p_tomate_prom$s4n9_p, na.rm = TRUE)

pp_tomate <- mean(p_tomate_prom$s4n9_p, na.rm = TRUE)
pp_tomate = (pp_tomate/22)


# Precio de consumo zanahoria
db4_zanahoria <- db4 %>% 
  filter(s4_3_1 == "Zanahoria")

table(db4_zanahoria$s4n9_u)

p_zanahoria_prom <- db4_zanahoria %>% 
  filter(s4n9_u == 4 & s4n9_p!=0) 
mean(p_zanahoria_prom$s4n9_p, na.rm = TRUE)

pp_zanahoria <- mean(p_zanahoria_prom$s4n9_p, na.rm = TRUE)
pp_zanahoria = (pp_zanahoria/46)

# Precio de consumo haba
db4_haba <- db4 %>% 
  filter(s4_3_1 == "Haba")

table(db4_haba$s4n9_u)

p_haba_prom <- db4_haba %>% 
  filter(s4n9_u == 4 & s4n9_p!=0) 
#p_haba_prom <- p_haba_prom %>%
  #filter(s4n9 > quantile(p_haba_prom$s4n9, 0.15))
mean(p_haba_prom$s4n9_p, na.rm = TRUE)

pp_haba <- mean(p_haba_prom$s4n9_p, na.rm = TRUE)
pp_haba = (pp_haba/46)

# Precio de consumo cebolla
db4_cebolla <- db4 %>% 
  filter(s4_3_1 == "Cebolla")

table(db4_cebolla$s4n9_u)

p_cebolla_prom <- db4_cebolla %>% 
  filter(s4n9_u == 4 & s4n9_p!=0) 
# p_cebolla_prom <- p_cebolla_prom %>%
#   filter(s4n9 > quantile(p_cebolla_prom$s4n9, 0.15))
mean(p_cebolla_prom$s4n9_p, na.rm = TRUE)

pp_cebolla <- mean(p_cebolla_prom$s4n9_p, na.rm = TRUE)
pp_cebolla = (pp_cebolla/46)

# Precio de consumo durazno
db4_durazno <- db4 %>% 
  filter(s4_3_1 == "Durazno")

table(db4_durazno$s4n9_u)

p_durazno_prom <- db4_durazno %>% 
  filter(s4n9_u == 6 & s4n9_p!=0) 
#p_durazno_prom <- p_durazno_prom %>%
  #filter(s4n9 > quantile(p_durazno_prom$s4n9, 0.15))
mean(p_durazno_prom$s4n9_p, na.rm = TRUE)

pp_durazno <- mean(p_durazno_prom$s4n9_p, na.rm = TRUE)
pp_durazno = (pp_durazno/18)

# Precio de consumo manzana
db4_manzana <- db4 %>% 
  filter(s4_3_1 == "Manzana")

table(db4_manzana$s4n9_u)

p_manzana_prom <- db4_manzana %>% 
  filter(s4n9_u == 6 & s4n9_p!=0) 
# p_manzana_prom <- p_manzana_prom %>%
#   filter(s4n9 > quantile(p_manzana_prom$s4n9, 0.15))
mean(p_manzana_prom$s4n9_p, na.rm = TRUE)

pp_manzana <- mean(p_manzana_prom$s4n9_p, na.rm = TRUE)
pp_manzana = (pp_manzana/18)

# Asignando precios promedio

db4 <- db4 %>%
  mutate(
    p_cons = case_when(
      s4_3_1 == "Papa" ~ pp_papa,
      s4_3_1 == "Maiz" ~ pp_maiz,
      s4_3_1 == "Tomate" ~ pp_tomate,
      s4_3_1 == "Zanahoria" ~ pp_zanahoria,
      s4_3_1 == "Haba" ~ pp_haba,
      s4_3_1 == "Cebolla" ~ pp_cebolla,
      s4_3_1 == "Durazno" ~ pp_durazno,
      s4_3_1 == "Manzana" ~ pp_manzana,
      TRUE ~ NA_real_ # En caso de no coincidir con ninguno de los anteriores
    )
  )

db4_pp_check_rubros <- db4 %>% 
  select(p_cons, s4_3_1)

#Costos por hectárea
cost_ha_papa_mo = 13500
cost_ha_papa_mo_perd100 = 8700
cost_ha_papa_fam = 11500
cost_ha_papa_fam_perd100 = 7700
cost_ha_papa_OAP = 18200


cost_ha_maiz_mo = 12672
cost_ha_maiz_mo_perd100 = 8835
cost_ha_maiz_fam = 8472
cost_ha_maiz_fam_perd100 = 5535
cost_ha_maiz_OAP = 4600

cost_ha_tomate_mo = 11972*10000/5000
cost_ha_tomate_mo_perd100 = 9210*10000/5000
cost_ha_tomate_fam = 7272*10000/5000
cost_ha_tomate_fam_perd100 = 5510*10000/5000
cost_ha_tomate_OAP = 26000

cost_ha_zanahoria_mo = 15208
cost_ha_zanahoria_mo_perd100 = 10798
cost_ha_zanahoria_fam = 12058
cost_ha_zanahoria_fam_perd100 = 8908
cost_ha_zanahoria_OAP = 21000

cost_ha_haba_mo = 10660
cost_ha_haba_mo_perd100 = 8020
cost_ha_haba_fam = 8960
cost_ha_haba_fam_perd100 = 6720
cost_ha_haba_OAP = 4850

cost_ha_cebolla_mo = 22041
cost_ha_cebolla_mo_perd100 = 16771
cost_ha_cebolla_fam = 7541
cost_ha_cebolla_fam_perd100 = 5271
cost_ha_cebolla_OAP = 8000

cost_ha_durazno_mo = 22397
cost_ha_durazno_mo_perd100 = 16405
cost_ha_durazno_fam = 11697
cost_ha_durazno_fam_perd100 = 7505
cost_ha_durazno_OAP = 18000

cost_ha_manzana_mo = 51172
cost_ha_manzana_mo_perd100 = 45460
cost_ha_manzana_fam = 10046
cost_ha_manzana_fam_perd100 = 5762
cost_ha_manzana_OAP = 10223

db4 <- db4 %>%
  mutate(
    costos_mo = case_when(
      s4_3_1 == "Papa" & perdida >= 80 ~ s4_sup_std * cost_ha_papa_mo_perd100,
      s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_mo,
      s4_3_1 == "Maiz" & perdida >= 80 ~ s4_sup_std * cost_ha_maiz_mo_perd100,
      s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_mo,
      s4_3_1 == "Tomate" & perdida >= 80 ~ s4_sup_std * cost_ha_tomate_mo_perd100,
      s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_mo,
      s4_3_1 == "Zanahoria" & perdida >= 80 ~ s4_sup_std * cost_ha_zanahoria_mo_perd100,
      s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_mo,
      s4_3_1 == "Haba" & perdida >= 80 ~ s4_sup_std * cost_ha_haba_mo_perd100,
      s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_mo,
      s4_3_1 == "Cebolla" & perdida >= 80 ~ s4_sup_std * cost_ha_cebolla_mo_perd100,
      s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_mo,
      s4_3_1 == "Durazno" & perdida >= 80 ~ s4_sup_std * cost_ha_durazno_mo_perd100,
      s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_mo,
      s4_3_1 == "Manzana" & perdida >= 80 ~ s4_sup_std * cost_ha_manzana_mo_perd100,
      s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_mo,
      TRUE ~ NA_real_
    ),
    costos_fam = case_when(
      s4_3_1 == "Papa" & perdida >= 80 ~ s4_sup_std * cost_ha_papa_fam_perd100,
      s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_fam,
      s4_3_1 == "Maiz" & perdida >= 80 ~ s4_sup_std * cost_ha_maiz_fam_perd100,
      s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_fam,
      s4_3_1 == "Tomate" & perdida >= 80 ~ s4_sup_std * cost_ha_tomate_fam_perd100,
      s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_fam,
      s4_3_1 == "Zanahoria" & perdida >= 80 ~ s4_sup_std * cost_ha_zanahoria_fam_perd100,
      s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_fam,
      s4_3_1 == "Haba" & perdida >= 80 ~ s4_sup_std * cost_ha_haba_fam_perd100,
      s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_fam,
      s4_3_1 == "Cebolla" & perdida >= 80 ~ s4_sup_std * cost_ha_cebolla_fam_perd100,
      s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_fam,
      s4_3_1 == "Durazno" & perdida >= 80 ~ s4_sup_std * cost_ha_durazno_fam_perd100,
      s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_fam,
      s4_3_1 == "Manzana" & perdida >= 80 ~ s4_sup_std * cost_ha_manzana_fam_perd100,
      s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_fam,
      TRUE ~ NA_real_
    ),
    costos_OAP = case_when(
      s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_OAP,
      s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_OAP,
      s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_OAP,
      s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_OAP,
      s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_OAP,
      s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_OAP,
      s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_OAP,
      s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_OAP,
      TRUE ~ NA_real_
    )
  )

check_NA_Costo <- db4 %>% 
  filter(s4_3_1 == "Durazno")

db4 <- db4 %>% 
  mutate(costos_fam = ifelse(s4_2_5 == 1, costos_fam, NA_real_))

summary(check_NA_Costo$costos_fam)

# db4 <- db4 %>%
#   mutate(
#     costos_mo = case_when(
#       s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_mo,
#       s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_mo,
#       s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_mo,
#       s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_mo,
#       s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_mo,
#       s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_mo,
#       s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_mo,
#       s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_mo,
#       TRUE ~ NA_real_ 
#     ),
#     costos_fam = case_when(
#       s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_fam,
#       s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_fam,
#       s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_fam,
#       s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_fam,
#       s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_fam,
#       s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_fam,
#       s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_fam,
#       s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_fam,
#       TRUE ~ NA_real_ 
#     ),
#     costos_OAP = case_when(
#       s4_3_1 == "Papa" ~ s4_sup_std * cost_ha_papa_OAP,
#       s4_3_1 == "Maiz" ~ s4_sup_std * cost_ha_maiz_OAP,
#       s4_3_1 == "Tomate" ~ s4_sup_std * cost_ha_tomate_OAP,
#       s4_3_1 == "Zanahoria" ~ s4_sup_std * cost_ha_zanahoria_OAP,
#       s4_3_1 == "Haba" ~ s4_sup_std * cost_ha_haba_OAP,
#       s4_3_1 == "Cebolla" ~ s4_sup_std * cost_ha_cebolla_OAP,
#       s4_3_1 == "Durazno" ~ s4_sup_std * cost_ha_durazno_OAP,
#       s4_3_1 == "Manzana" ~ s4_sup_std * cost_ha_manzana_OAP,
#       TRUE ~ NA_real_ 
#     )
#   )

check_costos <- db4 %>% 
  select(s4_3_1, rend, s4_sup_std, p_cons, s4n9_p, costos_mo, costos_fam, costos_OAP, perdida)

# Cálculos Ingreso
# db4 <- db4 %>%
#   mutate(ing_bruto_venta = coalesce(s4n9 * s4n9_p, 0), # Convierte NA en 0 para venta
#          ing_bruto_consumo = coalesce(s4_q_cons_std * p_cons, 0), # Convierte NA en 0 para consumo
#          ing_bruto_derivados = s4n12,
#          ing_bruto_ap = ing_bruto_venta + ing_bruto_consumo + ing_bruto_derivados)

# Cálculo Ingreso AP

db4 <- db4 %>%
  mutate(ing_bruto_venta = s4n9 * s4n9_p, 
         ing_bruto_consumo = s4_q_cons_std * p_cons, 
         ing_bruto_derivados = s4n12)

db4 <- db4 %>%
  mutate(ing_bruto_ap = pmap_dbl(list(ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados), 
                                 ~ sum(c(...), na.rm = TRUE)))

# Cálculo Ingreso AP_NOPRIORIZADO
db4 <- db4 %>%
  mutate(ing_bruto_nprio = s_4_17 * s_4_17_p)

db4_check_ingNP <- db4 %>% 
  select(s_4_17, s_4_17_p, ing_bruto_nprio)

db4_check_ing1 <- db4 %>% 
  select(`_index`, s4_3_1, rend, perdida, s4n9, s4_sup_std, s4_q_cons_std, ing_bruto_ap, costos_fam)

db4_check_ing2 <- db4 %>% 
  select(`_index`, s4_3_1, rend, perdida, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, ing_bruto_ap, ing_bruto_nprio, costos_fam)

db4_check_ing3 <- db4 %>% 
  select(`_index`, s4_3_1, ing_bruto_nprio, s_4_18)

db4_check_NAS <- db4 %>%
  filter(s4_3_1 == "Papa" | s4_3_1 == "Maiz" | s4_3_1 == "Tomate" | s4_3_1 == "Zanahoria" | 
           s4_3_1 == "Cebolla"  | s4_3_1 == "Haba" | s4_3_1 == "Durazno" | s4_3_1 == "Manzana") %>% 
  filter(is.na(perdida) & is.na(ing_bruto_venta) & is.na(ing_bruto_consumo) & is.na(ing_bruto_derivados) & is.na(ing_bruto_nprio)) %>% 
  select(`_index`, s4_3_1, rend, perdida, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, ing_bruto_ap, ing_bruto_nprio, costos_fam)

db4 <- db4 %>%
  anti_join(db4_check_NAS, by = "_index")

mean(db4$ing_bruto_ap, na.rm = TRUE)
median(db4$ing_bruto_ap, na.rm = TRUE)

mean(db4$ing_bruto_nprio, na.rm = TRUE)
median(db4$ing_bruto_nprio, na.rm = TRUE)

# NO PRIORIZADO Adicional ####
rubros_priorizados <- c("Papa", "Maiz", "Tomate", "Zanahoria", "Haba", "Cebolla", "Durazno", "Manzana")

db4 <- db4 %>%
  group_by(`_parent_index`) %>%
  mutate(
    rubro_parent = if_else(
      s4_3_1 %in% rubros_priorizados,
      s4_3_1,
      if_else(
        any(s4_3_1 %in% rubros_priorizados),
        s4_3_1[which(s4_3_1 %in% rubros_priorizados)[1]],  # Toma el primer rubro priorizado encontrado en el grupo
        NA_character_  # O puedes especificar un valor por defecto si no hay rubros priorizados
      )
    )
  ) %>%
  ungroup()

check_nonprio <- db4 %>% 
  select(`_parent_index`, s4_3_1, rubro_parent)

# db4_noprio <- db4 %>% 
#   filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99)) %>% 
#   filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01))
# 
# summary(db4_noprio$ing_bruto_nprio)
  
# mean(db4_noprio$ing_bruto_nprio, na.rm = TRUE)
# median(db4_noprio$ing_bruto_nprio, na.rm = TRUE)

# INGRESOS SIN TOMAR EN CUENTA LOS QUE PERDIERON
# Actividad agrícola priorizada
# db4 <- db4 %>%
#   mutate(ing_bruto_ap_n0 = if_else(ing_bruto_ap == 0, NA_real_, ing_bruto_ap))
# 
# db4_check_n0 <- db4 %>% 
#   select(`_index`, rend, ing_bruto_venta, ing_bruto_consumo, ing_bruto_ap_n0)
# 
# mean(db4$ing_bruto_ap_n0, na.rm = TRUE)
# median(db4$ing_bruto_ap_n0, na.rm = TRUE)

# Actividad agrícola no priorizada
# db4 <- db4 %>%
#   mutate(ing_bruto_nprio_n0 = if_else(ing_bruto_nprio == 0, NA_real_, ing_bruto_nprio))
# 
# db4_check_n0_NP <- db4 %>% 
#   select(`_index`, rend, ing_bruto_venta, ing_bruto_consumo, ing_bruto_ap_n0, ing_bruto_nprio_n0)
# 
# mean(db4$ing_bruto_nprio_n0, na.rm = TRUE)
# median(db4$ing_bruto_nprio_n0, na.rm = TRUE)


# Cálculos ingreso neto AP
# Actividad agrícola priorizada
db4 <- db4 %>% 
  mutate(ing_neto_ap = ing_bruto_ap - costos_fam) #<------------------ COSTOS CON MO FAMILIAR

mean(db4$ing_neto_ap, na.rm = TRUE)
median(db4$ing_neto_ap, na.rm = TRUE)

db4_check_neto_ap <- db4 %>% 
  select(`_index`, rend, ing_bruto_venta, ing_bruto_consumo, ing_bruto_ap, costos_fam, ing_neto_ap)

# Actividad agrícola NO priorizada
db4 <- db4 %>%
  mutate(ing_neto_nprio = if_else(!is.na(ing_bruto_nprio), 
                                  ing_bruto_nprio - s_4_18, 
                                  NA_real_))

mean(db4$ing_neto_nprio, na.rm = TRUE)
median(db4$ing_neto_nprio, na.rm = TRUE)

# Remesas + bonos #
db <- db %>% 
  mutate(ing_otros = s4_5 + s4_6)

check_ingotros <-  db %>% 
  select(ing_otros, s4_5,s4_6)

mean(db$ing_otros, na.rm = TRUE)
median(db$ing_otros, na.rm = TRUE)

db_ingotros_check <- db %>% 
  select(s4_5, s4_6, ing_otros)

# Revisión INGRESOS por RUBRO ####

# Revisión papa
db4_papa <- db4 %>% 
  filter(s4_3_1 == "Papa") 

mean(db4_papa$ing_neto_ap, na.rm = TRUE)
median(db4_papa$ing_neto_ap, na.rm = TRUE)
hist(db4_papa$ing_neto_ap)

db4_papa_checkIN <- db4_papa %>% 
  filter(ing_neto_ap > 10000) %>% 
  select(`_index`, ing_neto_ap)


db4_papa_checkperd <- db4_papa %>% 
  select(`_index`, ing_neto_ap, perdida)


# REVISIÓN MAIZ #
db4_maiz <- db4 %>% 
  filter(s4_3_1 == "Maiz")

mean(db4_maiz$ing_neto_ap, na.rm = TRUE)
median(db4_maiz$ing_neto_ap, na.rm = TRUE)
hist(db4_maiz$ing_neto_ap)

# REVISIÓN Tomate #
db4_tomate <- db4 %>% 
  filter(s4_3_1 == "Tomate")

mean(db4_tomate$ing_neto_ap, na.rm = TRUE)
median(db4_tomate$ing_neto_ap, na.rm = TRUE)
hist(db4_tomate$ing_neto_ap)

# REVISIÓN Zanahoria # 
db4_zanahoria <- db4 %>% 
  filter(s4_3_1 == "Zanahoria")

mean(db4_zanahoria$ing_neto_ap, na.rm = TRUE)
median(db4_zanahoria$ing_neto_ap, na.rm = TRUE)
hist(db4_zanahoria$ing_neto_ap)

# REVISIÓN Haba #
db4_haba <- db4 %>% 
  filter(s4_3_1 == "Haba")

mean(db4_haba$ing_neto_ap, na.rm = TRUE)
median(db4_haba$ing_neto_ap, na.rm = TRUE)
hist(db4_haba$ing_neto_ap)

# REVISIÓN Cebolla #
db4_cebolla <- db4 %>% 
  filter(s4_3_1 == "Cebolla")

mean(db4_cebolla$ing_neto_ap, na.rm = TRUE)
median(db4_cebolla$ing_neto_ap, na.rm = TRUE)
hist(db4_cebolla$ing_neto_ap)

# REVISIÓN Durazno #
db4_durazno <- db4 %>% 
  filter(s4_3_1 == "Durazno")

mean(db4_durazno$ing_neto_ap, na.rm = TRUE)
median(db4_durazno$ing_neto_ap, na.rm = TRUE)
hist(db4_durazno$ing_neto_ap)

# REVISIÓN Manzana #
db4_manzana <- db4 %>% 
  filter(s4_3_1 == "Manzana")

mean(db4_manzana$ing_neto_ap, na.rm = TRUE)
median(db4_manzana$ing_neto_ap, na.rm = TRUE)
hist(db4_manzana$ing_neto_ap)




# REVISIÓN PAPA #### 
# Cantidad #

mean(db4_papa$s4_q_std, na.rm = TRUE)
median(db4_papa$s4_q_std, na.rm = TRUE)

q_papa_ext <- extremos(df = db4, rubro = "Papa", variable_analisis = "s4_q_std")

q_papa_extremos_arriba <- q_papa_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_papa_extremos_abajo <- q_papa_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

# Rendimiento #

mean(db4_papa$rend, na.rm = TRUE)
median(db4_papa$rend, na.rm = TRUE)

ren_papa <- extremos(df = db4, rubro = "Papa", variable_analisis = "rend")
 
papa_extremos_arriba <- ren_papa$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
papa_extremos_abajo <- ren_papa$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

# Consumo

mean(db4_papa$s4_q_cons_std, na.rm = TRUE)
median(db4_papa$s4_q_cons_std, na.rm = TRUE)

cons_papa <- extremos(df = db4, rubro = "Papa", variable_analisis = "s4_q_cons_std")

papa_extremos_arriba_cons <- cons_papa$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
papa_extremos_abajo_cons <- cons_papa$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# Venta

mean(db4_papa$s4_q_venta_std, na.rm = TRUE)
median(db4_papa$s4_q_venta_std, na.rm = TRUE)

venta_papa <- extremos(df = db4, rubro = "Papa", variable_analisis = "s4_q_venta_std")

papa_extremos_arriba_venta <- venta_papa$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_venta_std, rend, `_index`, `_submission__submitted_by`)
papa_extremos_abajo_venta <- venta_papa$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_venta_std, rend, `_index`, `_submission__submitted_by`)

calcular_estadisticos(db4_papa, "s4_q_venta_std")
summary(db4_papa$s4_q_std)

# Análisis perdidas PAPA
db4_papa_perdida <- db4 %>% 
  filter(perdida == 100)

db4_papa_perdida_0 <- db4 %>% 
  filter(perdida == 0) %>% 
  select(`_index`,s4_3_2, s4n7, perdida)

table(db4_papa$perdida_10, useNA = "always")

ggplot(db4_papa, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(title = "Histograma de s4_ren_std (Percentiles 1-99)", x = "s4_rend_std", y = "Frecuencia")

# INGRESOS PAPA
# Cálculo precio promedio
# p_papa_prom <- db4_papa %>% 
#    filter(s4n9_u == 4) %>% 
#    select(`_index`, s4n9, s4n9_u, s4n9_p) 
#  mean(p_papa_prom$s4n9_p, na.rm = TRUE)
#  
# p_papa_prom_fix <- p_papa_prom %>% 
#  mutate(s4n9_p = ifelse((s4n9_p == 0), NA_real_, s4n9_p))
# 
# pp_papa <- mean(p_papa_prom$s4n9_p, na.rm = TRUE)
# pp_papa = (pp_papa/46)


# REVISIÓN MAÍZ #### 
db4_maiz <- db4 %>% 
  filter(s4_3_1 == "Maiz")

table(db4_maiz$s4n9_u)
  

# Cantidad #
mean(db4_maiz$s4_q_std, na.rm = TRUE)
median(db4_maiz$s4_q_std, na.rm = TRUE)

q_maiz_ext <- extremos(df = db4, rubro = "Maiz", variable_analisis = "s4_q_std")

q_maiz_extremos_arriba <- q_maiz_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_maiz_extremos_abajo <- q_maiz_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_maiz %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_maiz %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_maiz$rend, na.rm = TRUE)
median(db4_maiz$rend, na.rm = TRUE)

ren_maiz <- extremos(df = db4, rubro = "Maiz", variable_analisis = "rend")

maiz_extremos_arriba <- ren_maiz$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
maiz_extremos_abajo <- ren_maiz$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)


# Consumo

mean(db4_maiz$s4_q_cons_std, na.rm = TRUE)
median(db4_maiz$s4_q_cons_std, na.rm = TRUE)

cons_maiz <- extremos(df = db4, rubro = "Maiz", variable_analisis = "s4_q_cons_std")

maiz_extremos_arriba_cons <- cons_maiz$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
maiz_extremos_abajo_cons <- cons_maiz$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# INGRESOS MAÍZ
# Cálculo precio promedio
# p_papa_prom <- db4_papa %>% 
#   filter(s4n9_u == 4) %>% 
#   select(`_index`, s4n9, s4n9_u, s4n9_p) 
# mean(p_papa_prom$s4n9_p, na.rm = TRUE)
# 
# p_papa_prom_fix <- p_papa_prom %>% 
#   mutate(s4n9_p = ifelse((s4n9_p == 0), NA_real_, s4n9_p))
# 
# pp_papa <- mean(p_papa_prom$s4n9_p, na.rm = TRUE)
# pp_papa = (pp_papa/46)

check_ing_maiz <- db4 %>% 
  filter(s4_3_1 == "Maiz") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida)
  

# REVISIÓN TOMATE #### 
db4_tomate <- db4 %>% 
  filter(s4_3_1 == "Tomate")

# Cantidad #
mean(db4_tomate$s4_q_std, na.rm = TRUE)
median(db4_tomate$s4_q_std, na.rm = TRUE)

q_tomate_ext <- extremos(df = db4, rubro = "Tomate", variable_analisis = "s4_q_std")

q_tomate_extremos_arriba <- q_tomate_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_tomate_extremos_abajo <- q_tomate_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_tomate %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_tomate %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_tomate$rend, na.rm = TRUE)
median(db4_tomate$rend, na.rm = TRUE)

tomate_check01 <- db4_tomate %>% 
  filter(rend < 5000) %>% 
  select(`_index`, grupo, s1_2, s4_sup_std, s4_q_std, rend)

tomate_check02 <- db4_tomate %>% 
  filter(grupo == "control") 

mean(tomate_check02$rend, na.rm = TRUE)
median(tomate_check02$rend, na.rm = TRUE)

ren_tomate <- extremos(df = db4, rubro = "tomate", variable_analisis = "rend")

tomate_extremos_arriba <- ren_tomate$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
tomate_extremos_abajo <- ren_tomate$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_tomate$s4_q_cons_std, na.rm = TRUE)
median(db4_tomate$s4_q_cons_std, na.rm = TRUE)

cons_tomate <- extremos(df = db4, rubro = "tomate", variable_analisis = "s4_q_cons_std")

tomate_extremos_arriba_cons <- cons_tomate$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
tomate_extremos_abajo_cons <- cons_tomate$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# INGRESOS TOAMTE
check_ing_tomate <- db4 %>% 
  filter(s4_3_1 == "Tomate") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida)

# REVISIÓN ZANAHORIA #### 
db4_zanahoria <- db4 %>% 
  filter(s4_3_1 == "Zanahoria")

# Cantidad #
mean(db4_zanahoria$s4_q_std, na.rm = TRUE)
median(db4_zanahoria$s4_q_std, na.rm = TRUE)

q_zanahoria_ext <- extremos(df = db4, rubro = "Zanahoria", variable_analisis = "s4_q_std")

q_zanahoria_extremos_arriba <- q_zanahoria_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_zanahoria_extremos_abajo <- q_zanahoria_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_zanahoria %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_zanahoria %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_zanahoria$rend, na.rm = TRUE)
median(db4_zanahoria$rend, na.rm = TRUE)

zanahoria_check01 <- db4_zanahoria %>% 
  filter(rend > 9000) %>% 
  select(`_index`, grupo, s1_2, s4_sup_std, s4_q_std, rend)

zanahoria_check02 <- db4_zanahoria %>% 
  filter(grupo == "control") 

mean(zanahoria_check02$rend, na.rm = TRUE)
median(zanahoria_check02$rend, na.rm = TRUE)

ren_zanahoria <- extremos(df = db4, rubro = "zanahoria", variable_analisis = "rend")

zanahoria_extremos_arriba <- ren_zanahoria$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
zanahoria_extremos_abajo <- ren_zanahoria$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_zanahoria$s4_q_cons_std, na.rm = TRUE)
median(db4_zanahoria$s4_q_cons_std, na.rm = TRUE)

cons_zanahoria <- extremos(df = db4, rubro = "zanahoria", variable_analisis = "s4_q_cons_std")

zanahoria_extremos_arriba_cons <- cons_zanahoria$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
zanahoria_extremos_abajo_cons <- cons_zanahoria$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# INGRESOS ZANAHORIA
check_ing_zanahoria <- db4 %>% 
  filter(s4_3_1 == "Zanahoria") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida, grupo)

# REVISIÓN Haba #### 
db4_haba <- db4 %>% 
  filter(s4_3_1 == "Haba")

# Cantidad #
mean(db4_haba$s4_q_std, na.rm = TRUE)
median(db4_haba$s4_q_std, na.rm = TRUE)

q_haba_ext <- extremos(df = db4, rubro = "Haba", variable_analisis = "s4_q_std")

q_haba_extremos_arriba <- q_haba_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_haba_extremos_abajo <- q_haba_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_haba %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_haba %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_haba$rend, na.rm = TRUE)
median(db4_haba$rend, na.rm = TRUE)

haba_check01 <- db4_haba %>% 
  filter(rend > 10000) %>% 
  select(`_index`, grupo, s1_2, s4_sup_std, s4_q_std, rend)

haba_check02 <- db4_haba %>% 
  filter(grupo == "control") 

mean(haba_check02$rend, na.rm = TRUE)
median(haba_check02$rend, na.rm = TRUE)

ren_haba <- extremos(df = db4, rubro = "Haba", variable_analisis = "rend")

haba_extremos_arriba <- ren_haba$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
haba_extremos_abajo <- ren_haba$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_haba$s4_q_cons_std, na.rm = TRUE)
median(db4_haba$s4_q_cons_std, na.rm = TRUE)

cons_haba <- extremos(df = db4, rubro = "haba", variable_analisis = "s4_q_cons_std")

haba_extremos_arriba_cons <- cons_haba$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
haba_extremos_abajo_cons <- cons_haba$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# INGRESOS HABA
check_ing_haba <- db4 %>% 
  filter(s4_3_1 == "Haba") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida, grupo)


# REVISIÓN CEBOLLA #### 
db4_cebolla <- db4 %>% 
  filter(s4_3_1 == "Cebolla")

# Cantidad #
mean(db4_cebolla$s4_q_std, na.rm = TRUE)
median(db4_cebolla$s4_q_std, na.rm = TRUE)

q_cebolla_ext <- extremos(df = db4, rubro = "Cebolla", variable_analisis = "s4_q_std")

q_cebolla_extremos_arriba <- q_cebolla_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_cebolla_extremos_abajo <- q_cebolla_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_cebolla %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_cebolla %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_cebolla$rend, na.rm = TRUE)
median(db4_cebolla$rend, na.rm = TRUE)

cebolla_check01 <- db4_cebolla %>% 
  filter(rend > 25000) %>% 
  select(`_index`, grupo, s1_2, s4_sup_std, s4_q_std, rend)

cebolla_check02 <- db4_cebolla %>% 
  filter(grupo == "control") 

mean(cebolla_check02$rend, na.rm = TRUE)
median(cebolla_check02$rend, na.rm = TRUE)

ren_cebolla <- extremos(df = db4, rubro = "Cebolla", variable_analisis = "rend")

cebolla_extremos_arriba <- ren_cebolla$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
cebolla_extremos_abajo <- ren_cebolla$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_cebolla$s4_q_cons_std, na.rm = TRUE)
median(db4_cebolla$s4_q_cons_std, na.rm = TRUE)

cons_cebolla <- extremos(df = db4, rubro = "cebolla", variable_analisis = "s4_q_cons_std")

cebolla_extremos_arriba_cons <- cons_cebolla$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
cebolla_extremos_abajo_cons <- cons_cebolla$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

# INGRESOS CEBOLLA
check_ing_cebolla <- db4 %>% 
  filter(s4_3_1 == "Cebolla") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida, grupo)

# REVISIÓN DURAZNO #### 
db4_durazno <- db4 %>% 
  filter(s4_3_1 == "Durazno")

# Cantidad #
mean(db4_durazno$s4_q_std, na.rm = TRUE)
median(db4_durazno$s4_q_std, na.rm = TRUE)

q_durazno_ext <- extremos(df = db4, rubro = "Durazno", variable_analisis = "s4_q_std")

q_durazno_extremos_arriba <- q_durazno_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_durazno_extremos_abajo <- q_durazno_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_durazno %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_durazno %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_durazno$rend, na.rm = TRUE)
median(db4_durazno$rend, na.rm = TRUE)

durazno_check01 <- db4_durazno %>% 
  filter(rend < 500) %>% 
  select(`_index`, grupo, s1_2, s4n7, s4n7_u, s4_sup_std, s4_3_2, s4_3_2u, s4_q_std, rend, perdida)

durazno_check02 <- db4_durazno %>% 
  filter(grupo == "control") 

mean(durazno_check02$rend, na.rm = TRUE)
median(durazno_check02$rend, na.rm = TRUE)

ren_durazno <- extremos(df = db4, rubro = "Durazno", variable_analisis = "rend")

durazno_extremos_arriba <- ren_durazno$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
durazno_extremos_abajo <- ren_durazno$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_durazno$s4_q_cons_std, na.rm = TRUE)
median(db4_durazno$s4_q_cons_std, na.rm = TRUE)

cons_durazno <- extremos(df = db4, rubro = "durazno", variable_analisis = "s4_q_cons_std")

durazno_extremos_arriba_cons <- cons_durazno$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
durazno_extremos_abajo_cons <- cons_durazno$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

#Ingresos DURAZNO
check_ing_durazno <- db4 %>% 
  filter(s4_3_1 == "Durazno") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida, s4_sup_std, grupo)

# REVISIÓN MANZANA #### 
db4_manzana <- db4 %>% 
  filter(s4_3_1 == "Manzana")

# Cantidad #
mean(db4_manzana$s4_q_std, na.rm = TRUE)
median(db4_manzana$s4_q_std, na.rm = TRUE)

q_manzana_ext <- extremos(df = db4, rubro = "Manzana", variable_analisis = "s4_q_std")

q_manzana_extremos_arriba <- q_manzana_ext$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
q_manzana_extremos_abajo <- q_manzana_ext$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4_check_q1 <- db4_manzana %>% 
  filter(grupo == "tratamiento") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

db4_check_q2 <- db4_manzana %>% 
  filter(grupo == "control") %>% 
  select(`_index`, s1_2, s4_sup_std, s4_q_std, rend)

mean(db4_check_q1$s4_q_std, na.rm = TRUE)
mean(db4_check_q2$s4_q_std, na.rm = TRUE)

median(db4_check_q1$s4_q_std, na.rm = TRUE)
median(db4_check_q2$s4_q_std, na.rm = TRUE)

table(db4_check_q1$s1_2)
table(db4_check_q2$s1_2)

ks.test(db4_check_q1$s4_q_std, db4_check_q2$s4_q_std)

# Rendimiento #

mean(db4_manzana$rend, na.rm = TRUE)
median(db4_manzana$rend, na.rm = TRUE)

manzana_check01_ext <- db4_manzana %>% 
  filter(rend > 30000) %>% 
  select(grupo, s1_2, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, perdida, `_index`)

manzana_check01 <- db4_manzana %>% 
  filter(grupo == "tratamiento") 

manzana_check02 <- db4_manzana %>% 
  filter(grupo == "control") 

mean(manzana_check01$rend, na.rm = TRUE)
median(manzana_check01$rend, na.rm = TRUE)

mean(manzana_check02$rend, na.rm = TRUE)
median(manzana_check02$rend, na.rm = TRUE)

ren_manzana <- extremos(df = db4, rubro = "Manzana", variable_analisis = "rend")

manzana_extremos_arriba <- ren_manzana$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)
manzana_extremos_abajo <- ren_manzana$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, rend, `_index`, `_submission__submitted_by`)

db4$s4_3_2[db4$"_index" == 1137]
db4$s4_3_2u[db4$"_index" == 1137]

# Consumo

mean(db4_manzana$s4_q_cons_std, na.rm = TRUE)
median(db4_manzana$s4_q_cons_std, na.rm = TRUE)

cons_manzana <- extremos(df = db4, rubro = "manzana", variable_analisis = "s4_q_cons_std")

manzana_extremos_arriba_cons <- cons_manzana$valores_extremos_arriba %>%
  select(grupo, s4_3_2, s4_3_2u, s4n7, s4n7_u, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)
manzana_extremos_abajo_cons <- cons_manzana$valores_extremos_abajo %>%
  select(grupo, s4_sup_std, s4_q_std, s4_q_cons_std, rend, `_index`, `_submission__submitted_by`)

#Ingresos MANZANA
check_ing_manzana <- db4 %>% 
  filter(s4_3_1 == "Manzana") %>% 
  select(`_index`, ing_bruto_venta, ing_bruto_consumo, ing_bruto_derivados, costos_fam, ing_bruto_ap, ing_neto_ap, perdida, s4_sup_std, grupo)

# GUARDADO DE BASES ####
saveRDS(db, "data_in/3_db_clean/db.rds")
saveRDS(db2, "data_in/3_db_clean/db2.rds")
saveRDS(db4, "data_in/3_db_clean/db4.rds")



