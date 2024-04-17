# Línea de base APROCAM 
# Proyecto ACCESOS RURAL
# Ministerio de Desarrollo Rural y Tierras - Fondo Internacional para el Desarrollo Agrícola

# Revisión Base de datos Bruta
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
generar_tabla_brigada <- function(uni_df, comp_df, planificadas) {
  rubros_esperados <- c("Papa", "Maiz", "Haba", "Durazno", "Manzana", "Tomate", "Cebolla", "Zanahoria")
  
  # Preparar data frames uni_df y comp_df eliminando posibles NAs en s4_3_1_n
  uni_df <- na.omit(uni_df)
  comp_df <- na.omit(comp_df)
  
  # Función auxiliar para sumar las encuestas por rubro
  sumar_por_rubro <- function(df, rubro) {
    suma <- sum(df$conteo[df$s4_3_1_n == rubro], na.rm = TRUE)
    return(suma)
  }
  
  # Inicializar el dataframe de resultados
  resultados_BX <- data.frame(
    Rubro = rubros_esperados,
    Numero_encuestas_planificadas = planificadas,
    Numero_encuestas_iniciales = rep(0, length(rubros_esperados)),
    Numero_encuestas_efectivas = rep(0, length(rubros_esperados))
  )
  
  # Aplicar la suma por rubro
  for (rubro in rubros_esperados) {
    idx <- which(resultados_BX$Rubro == rubro)
    resultados_BX$Numero_encuestas_iniciales[idx] <- sumar_por_rubro(uni_df, rubro)
    resultados_BX$Numero_encuestas_efectivas[idx] <- sumar_por_rubro(comp_df, rubro)
  }
  
  # Calcular porcentaje de avance y excedente o faltante
  resultados_BX$Porcentaje_de_Avance <- with(resultados_BX, round((Numero_encuestas_efectivas / Numero_encuestas_planificadas) * 100, 2))
  resultados_BX$Porcentaje_de_Avance[is.infinite(resultados_BX$Porcentaje_de_Avance)] <- NA
  resultados_BX$Excedente_o_Faltante <- with(resultados_BX, Numero_encuestas_efectivas - Numero_encuestas_planificadas)
  
  # Añadir fila de total al final
  totalizador <- data.frame(
    Rubro = "Total",
    Numero_encuestas_planificadas = sum(resultados_BX$Numero_encuestas_planificadas, na.rm = TRUE),
    Numero_encuestas_iniciales = sum(resultados_BX$Numero_encuestas_iniciales, na.rm = TRUE),
    Numero_encuestas_efectivas = sum(resultados_BX$Numero_encuestas_efectivas, na.rm = TRUE),
    Porcentaje_de_Avance = round((sum(resultados_BX$Numero_encuestas_efectivas, na.rm = TRUE) / sum(resultados_BX$Numero_encuestas_planificadas, na.rm = TRUE)) * 100, 2),
    Excedente_o_Faltante = sum(resultados_BX$Numero_encuestas_efectivas, na.rm = TRUE) - sum(resultados_BX$Numero_encuestas_planificadas, na.rm = TRUE)
  )
  
  resultados_BX <- rbind(resultados_BX, totalizador)
  
  return(resultados_BX)
}

# General
generar_uni_table_gen <- function(id_brigada) {
  # Asumiendo que db_analisis está disponible en el entorno global y contiene la columna 'grupo'
  uni_table <- db_analisis_uniq %>%
    filter(s1_5 == id_brigada) %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}

generar_comp_table_gen <- function(id_brigada) {
  # Asumiendo que db_analisis_comp está disponible en el entorno global y contiene la columna 'grupo'
  comp_table <- db_analisis_comp %>%
    filter(s1_5 == id_brigada) %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}


# Tratamiento
generar_uni_table_tratamiento <- function(id_brigada) {
  # Asumiendo que db_analisis está disponible en el entorno global y contiene la columna 'grupo'
  uni_table <- db_analisis_uniq %>%
    filter(s1_5 == id_brigada, grupo.y == "tratamiento") %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}

generar_comp_table_tratamiento <- function(id_brigada) {
  # Asumiendo que db_analisis_comp está disponible en el entorno global y contiene la columna 'grupo'
  comp_table <- db_analisis_comp %>%
    filter(s1_5 == id_brigada, grupo.y == "tratamiento") %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}

#Control

generar_uni_table_control <- function(id_brigada) {
  # Asumiendo que db_analisis está disponible en el entorno global y contiene la columna 'grupo'
  uni_table <- db_analisis_uniq %>%
    filter(s1_5 == id_brigada, grupo.y == "control") %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}

generar_comp_table_control <- function(id_brigada) {
  # Asumiendo que db_analisis_comp está disponible en el entorno global y contiene la columna 'grupo'
  comp_table <- db_analisis_comp %>%
    filter(s1_5 == id_brigada, grupo.y == "control") %>%
    group_by(s4_3_1_n) %>%
    summarise(conteo = n(), .groups = 'drop') %>%
    arrange(s4_3_1_n)
}

#Función para resumen por operativo

sumarizar_datos_por_rubro <- function(lista_resultados) {
  # Unir todos los resultados de las brigadas en un solo dataframe
  df_combinado <- bind_rows(lista_resultados)
  
  # Sumarizar los datos por rubro, excluyendo las filas de "Total"
  resumen_operativo <- df_combinado %>%
    filter(Rubro != "Total") %>%
    group_by(Rubro) %>%
    summarise(
      `Número encuestas planificadas` = sum(Numero_encuestas_planificadas, na.rm = TRUE),
      `Número encuestas iniciales` = sum(Numero_encuestas_iniciales, na.rm = TRUE),
      `Número encuestas efectivas` = sum(Numero_encuestas_efectivas, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      `Porcentaje de Avance` = ifelse(`Número encuestas planificadas` > 0,
                                      paste0(round((`Número encuestas efectivas` / `Número encuestas planificadas`) * 100), "%"),
                                      NA_character_),
      `Excedente o Faltante` = `Número encuestas efectivas` - `Número encuestas planificadas`
    )
  
  # Calcular el totalizador y agregarlo al final del dataframe
  totalizador <- summarise(resumen_operativo,
                           Rubro = "Total",
                           `Número encuestas planificadas` = sum(`Número encuestas planificadas`, na.rm = TRUE),
                           `Número encuestas iniciales` = sum(`Número encuestas iniciales`, na.rm = TRUE),
                           `Número encuestas efectivas` = sum(`Número encuestas efectivas`, na.rm = TRUE),
                           `Excedente o Faltante` = sum(`Excedente o Faltante`, na.rm = TRUE),
                           .groups = 'drop'
  ) %>%
    mutate(
      `Porcentaje de Avance` = NA_character_ # No se calcula el porcentaje total
    )
  
  # Unir el totalizador al resumen operativo
  resumen_operativo <- bind_rows(resumen_operativo, totalizador)
  
  return(resumen_operativo)
}

# Número de encuestas realizadas ####
# Revisión de rubros en físico DB4 ####

db4_rev1 <- db4 %>% 
  filter(s4_3_1_n == 1)

db_rev_rubro <- db %>%
  inner_join(db4_rev1, by = c("_index" = "_parent_index"))


# Número de encuestas por comunidad #
db4_1 <- db4 %>%
  filter(s4_2_5 == 1) %>%
  add_count(`_parent_index`, name = "n_matches") %>%
  mutate(advertencia = if_else(n_matches > 1, "más de un match", NA_character_)) %>%
  group_by(`_parent_index`) %>%
  slice(1) %>%
  ungroup()

db_analisis_uniq <- db %>%
  left_join(db4_1, by = c("_index" = "_parent_index"))

# Número de encuestas totales #
db4_2 <- db4 %>%
  filter(s4_2_5 == 1) %>%
  add_count(`_parent_index`, name = "n_matches") %>%
  mutate(advertencia = if_else(n_matches > 1, "más de un match", NA_character_)) %>%
  group_by(`_parent_index`) %>%
  ungroup()

db_analisis_comp <- db %>%
  left_join(db4_2, by = c("_index" = "_parent_index"))

# Brigada 1 #

id_brigada <- 111
planificadas_b1_gen <- c(0, 0, 14, 28, 70, 28, 14, 0)
planificadas_b1_t <- c(0, 0, 14, 28, 70, 28, 14, 0)
planificadas_b1_c <- c(0, 0, 0, 0, 0, 0, 0, 0)

uni_table_b1_gen <- generar_uni_table_gen(id_brigada)
comp_table_b1_gen <- generar_comp_table_gen(id_brigada)

uni_table_b1_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b1_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b1_c <- generar_uni_table_control(id_brigada)
comp_table_b1_c <- generar_comp_table_control(id_brigada)

resultados_brigada1_gen <- generar_tabla_brigada(uni_table_b1_gen, comp_table_b1_gen, planificadas_b1_gen)
resultados_brigada1_t <- generar_tabla_brigada(uni_table_b1_t, comp_table_b1_t, planificadas_b1_t)
resultados_brigada1_c <- generar_tabla_brigada(uni_table_b1_c, comp_table_b1_c, planificadas_b1_c)

# Brigada 2 #
id_brigada <- 121
planificadas_b2_gen <- c(70, 0, 56, 28, 14, 0, 0, 0)
planificadas_b2_t <- c(56, 0, 42, 28, 14, 0, 0, 0)
planificadas_b2_c <- c(14, 0, 14, 0, 0, 0, 0, 0)

uni_table_b2_gen <- generar_uni_table_gen(id_brigada)
comp_table_b2_gen <- generar_comp_table_gen(id_brigada)

uni_table_b2_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b2_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b2_c <- generar_uni_table_control(id_brigada)
comp_table_b2_c <- generar_comp_table_control(id_brigada)

resultados_brigada2_gen <- generar_tabla_brigada(uni_table_b2_gen, comp_table_b2_gen, planificadas_b2_gen)
resultados_brigada2_t <- generar_tabla_brigada(uni_table_b2_t, comp_table_b2_t, planificadas_b2_t)
resultados_brigada2_c <- generar_tabla_brigada(uni_table_b2_c, comp_table_b2_c, planificadas_b2_c)

# Brigada 3 #
id_brigada <- 131
planificadas_b3_gen <- c(56, 28, 14, 0, 0, 14, 0, 0)
planificadas_b3_t <- c(56, 28, 14, 0, 0, 14, 0, 0)
planificadas_b3_c <- c(0, 0, 0, 0, 0, 0, 0, 0)

uni_table_b3_gen <- generar_uni_table_gen(id_brigada)
comp_table_b3_gen <- generar_comp_table_gen(id_brigada)

uni_table_b3_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b3_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b3_c <- generar_uni_table_control(id_brigada)
comp_table_b3_c <- generar_comp_table_control(id_brigada)

resultados_brigada3_gen <- generar_tabla_brigada(uni_table_b3_gen, comp_table_b3_gen, planificadas_b3_gen)
resultados_brigada3_t <- generar_tabla_brigada(uni_table_b3_t, comp_table_b3_t, planificadas_b3_t)
resultados_brigada3_c <- generar_tabla_brigada(uni_table_b3_c, comp_table_b3_c, planificadas_b3_c)

# Brigada 4 #
id_brigada <- 141
planificadas_b4_gen <- c(126, 0, 14, 0, 0, 0, 0, 0)
planificadas_b4_t <- c(42, 0, 14, 0, 0, 0, 0, 0)
planificadas_b4_c <- c(84, 0, 0, 0, 0, 0, 0, 0)

uni_table_b4_gen <- generar_uni_table_gen(id_brigada)
comp_table_b4_gen <- generar_comp_table_gen(id_brigada)

uni_table_b4_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b4_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b4_c <- generar_uni_table_control(id_brigada)
comp_table_b4_c <- generar_comp_table_control(id_brigada)

resultados_brigada4_gen <- generar_tabla_brigada(uni_table_b4_gen, comp_table_b4_gen, planificadas_b4_gen)
resultados_brigada4_t <- generar_tabla_brigada(uni_table_b4_t, comp_table_b4_t, planificadas_b4_t)
resultados_brigada4_c <- generar_tabla_brigada(uni_table_b4_c, comp_table_b4_c, planificadas_b4_c)

# Brigada 5 #
id_brigada <- 151
planificadas_b5_gen <- c(126, 0, 42, 0, 0, 0, 0, 0)
planificadas_b5_t <- c(14, 0, 0, 0, 0, 0, 0, 0)
planificadas_b5_c <- c(112, 0, 42, 0, 0, 0, 0, 0)

uni_table_b5_gen <- generar_uni_table_gen(id_brigada)
comp_table_b5_gen <- generar_comp_table_gen(id_brigada)

uni_table_b5_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b5_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b5_c <- generar_uni_table_control(id_brigada)
comp_table_b5_c <- generar_comp_table_control(id_brigada)

resultados_brigada5_gen <- generar_tabla_brigada(uni_table_b5_gen, comp_table_b5_gen, planificadas_b5_gen)
resultados_brigada5_t <- generar_tabla_brigada(uni_table_b5_t, comp_table_b5_t, planificadas_b5_t)
resultados_brigada5_c <- generar_tabla_brigada(uni_table_b5_c, comp_table_b5_c, planificadas_b5_c)


# Brigada 6 #
id_brigada <- 211
planificadas_b6_gen <- c(42, 42, 0, 0, 0, 42, 28, 0)
planificadas_b6_t <- c(42, 42, 0, 0, 0, 42, 28, 0)
planificadas_b6_c <- c(0, 0, 0, 0, 0, 0, 0, 0)

uni_table_b6_gen <- generar_uni_table_gen(id_brigada)
comp_table_b6_gen <- generar_comp_table_gen(id_brigada)

uni_table_b6_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b6_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b6_c <- generar_uni_table_control(id_brigada)
comp_table_b6_c <- generar_comp_table_control(id_brigada)

resultados_brigada6_gen <- generar_tabla_brigada(uni_table_b6_gen, comp_table_b6_gen, planificadas_b6_gen)
resultados_brigada6_t <- generar_tabla_brigada(uni_table_b6_t, comp_table_b6_t, planificadas_b6_t)
resultados_brigada6_c <- generar_tabla_brigada(uni_table_b6_c, comp_table_b6_c, planificadas_b6_c)


# Brigada 7 #
id_brigada <- 221
planificadas_b7_gen <- c(0, 0, 0, 70, 0, 28, 28, 56)
planificadas_b7_t <- c(0, 0, 0, 0, 0, 0, 0, 0)
planificadas_b7_c <- c(0, 0, 0, 70, 0, 28, 28, 56)

uni_table_b7_gen <- generar_uni_table_gen(id_brigada)
comp_table_b7_gen <- generar_comp_table_gen(id_brigada)

uni_table_b7_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b7_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b7_c <- generar_uni_table_control(id_brigada)
comp_table_b7_c <- generar_comp_table_control(id_brigada)

resultados_brigada7_gen <- generar_tabla_brigada(uni_table_b7_gen, comp_table_b7_gen, planificadas_b7_gen)
resultados_brigada7_t <- generar_tabla_brigada(uni_table_b7_t, comp_table_b7_t, planificadas_b7_t)
resultados_brigada7_c <- generar_tabla_brigada(uni_table_b7_c, comp_table_b7_c, planificadas_b7_c)


# Brigada 8 #
id_brigada <- 231
planificadas_b8_gen <- c(0, 42, 0, 14, 56, 0, 14, 42)
planificadas_b8_t <- c(0, 0, 0, 0, 0, 0, 0, 0)
planificadas_b8_c <- c(0, 42, 0, 14, 56, 0, 14, 42)

uni_table_b8_gen <- generar_uni_table_gen(id_brigada)
comp_table_b8_gen <- generar_comp_table_gen(id_brigada)

uni_table_b8_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b8_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b8_c <- generar_uni_table_control(id_brigada)
comp_table_b8_c <- generar_comp_table_control(id_brigada)

resultados_brigada8_gen <- generar_tabla_brigada(uni_table_b8_gen, comp_table_b8_gen, planificadas_b8_gen)
resultados_brigada8_t <- generar_tabla_brigada(uni_table_b8_t, comp_table_b8_t, planificadas_b8_t)
resultados_brigada8_c <- generar_tabla_brigada(uni_table_b8_c, comp_table_b8_c, planificadas_b8_c)

# Brigada 9 #
id_brigada <- 311
planificadas_b9_gen <- c(56, 56, 56, 0, 14, 0, 0, 14)
planificadas_b9_t <- c(14, 42, 14, 0, 0, 0, 0, 14)
planificadas_b9_c <- c(42, 14, 42, 0, 14, 0, 0, 0)

uni_table_b9_gen <- generar_uni_table_gen(id_brigada)
comp_table_b9_gen <- generar_comp_table_gen(id_brigada)

uni_table_b9_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b9_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b9_c <- generar_uni_table_control(id_brigada)
comp_table_b9_c <- generar_comp_table_control(id_brigada)

resultados_brigada9_gen <- generar_tabla_brigada(uni_table_b9_gen, comp_table_b9_gen, planificadas_b9_gen)
resultados_brigada9_t <- generar_tabla_brigada(uni_table_b9_t, comp_table_b9_t, planificadas_b9_t)
resultados_brigada9_c <- generar_tabla_brigada(uni_table_b9_c, comp_table_b9_c, planificadas_b9_c)


# Brigada 10 #
id_brigada <- 411
planificadas_b10_gen <- c(0, 42, 0, 28, 56, 0, 56, 0)
planificadas_b10_t <- c(0, 0, 0, 14, 28, 0, 42, 0)
planificadas_b10_c <- c(0, 42, 0, 14, 28, 0, 14, 0)

uni_table_b10_gen <- generar_uni_table_gen(id_brigada)
comp_table_b10_gen <- generar_comp_table_gen(id_brigada)

uni_table_b10_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b10_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b10_c <- generar_uni_table_control(id_brigada)
comp_table_b10_c <- generar_comp_table_control(id_brigada)

resultados_brigada10_gen <- generar_tabla_brigada(uni_table_b10_gen, comp_table_b10_gen, planificadas_b10_gen)
resultados_brigada10_t <- generar_tabla_brigada(uni_table_b10_t, comp_table_b10_t, planificadas_b10_t)
resultados_brigada10_c <- generar_tabla_brigada(uni_table_b10_c, comp_table_b10_c, planificadas_b10_c)

# Brigada 11 #
id_brigada <- 511
planificadas_b11_gen <- c(0, 0, 0, 56, 0, 14, 42, 84)
planificadas_b11_t <- c(0, 0, 0, 42, 0, 14, 42, 84)
planificadas_b11_c <- c(0, 0, 0, 14, 0, 0, 0, 0)

uni_table_b11_gen <- generar_uni_table_gen(id_brigada)
comp_table_b11_gen <- generar_comp_table_gen(id_brigada)

uni_table_b11_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b11_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b11_c <- generar_uni_table_control(id_brigada)
comp_table_b11_c <- generar_comp_table_control(id_brigada)

resultados_brigada11_gen <- generar_tabla_brigada(uni_table_b11_gen, comp_table_b11_gen, planificadas_b11_gen)
resultados_brigada11_t <- generar_tabla_brigada(uni_table_b11_t, comp_table_b11_t, planificadas_b11_t)
resultados_brigada11_c <- generar_tabla_brigada(uni_table_b11_c, comp_table_b11_c, planificadas_b11_c)

# Brigada 12 #
id_brigada <- 611
planificadas_b12_gen <- c(28, 14, 0, 0, 0, 126, 14, 0)
planificadas_b12_t <- c(28, 0, 0, 0, 0, 14, 0, 0)
planificadas_b12_c <- c(0, 14, 0, 0, 0, 112, 14, 0)

uni_table_b12_gen <- generar_uni_table_gen(id_brigada)
comp_table_b12_gen <- generar_comp_table_gen(id_brigada)

uni_table_b12_t <- generar_uni_table_tratamiento(id_brigada)
comp_table_b12_t <- generar_comp_table_tratamiento(id_brigada)

uni_table_b12_c <- generar_uni_table_control(id_brigada)
comp_table_b12_c <- generar_comp_table_control(id_brigada)

resultados_brigada12_gen <- generar_tabla_brigada(uni_table_b12_gen, comp_table_b12_gen, planificadas_b12_gen)
resultados_brigada12_t <- generar_tabla_brigada(uni_table_b12_t, comp_table_b12_t, planificadas_b12_t)
resultados_brigada12_c <- generar_tabla_brigada(uni_table_b12_c, comp_table_b12_c, planificadas_b12_c)

# Resumen brigadas ####

lista_resultados_gen <- list(resultados_brigada1_gen, resultados_brigada2_gen, resultados_brigada3_gen, 
                             resultados_brigada4_gen, resultados_brigada5_gen, resultados_brigada6_gen,
                             resultados_brigada7_gen, resultados_brigada8_gen, resultados_brigada9_gen,
                             resultados_brigada10_gen, resultados_brigada11_gen, resultados_brigada12_gen)


resumen_operativo_gen <- sumarizar_datos_por_rubro(lista_resultados_gen)

lista_resultados_t <- list(resultados_brigada1_t, resultados_brigada2_t, resultados_brigada3_t, 
                           resultados_brigada4_t, resultados_brigada5_t, resultados_brigada6_t,
                           resultados_brigada7_t, resultados_brigada8_t, resultados_brigada9_t,
                           resultados_brigada10_t, resultados_brigada11_t, resultados_brigada12_t)

resumen_operativo_t <- sumarizar_datos_por_rubro(lista_resultados_t)

lista_resultados_c <- list(resultados_brigada1_c, resultados_brigada2_c, resultados_brigada3_c, 
                           resultados_brigada4_c, resultados_brigada5_c, resultados_brigada6_c,
                           resultados_brigada7_c, resultados_brigada8_c, resultados_brigada9_c,
                           resultados_brigada10_c, resultados_brigada11_c, resultados_brigada12_c)

resumen_operativo_c <- sumarizar_datos_por_rubro(lista_resultados_c)

#write.table(resumen_operativo_gen, "clipboard-16384", sep="\t", row.names=FALSE)
#write.table(resumen_operativo_t, "clipboard-16384", sep="\t", row.names=FALSE)
#write.table(resumen_operativo_c, "clipboard-16384", sep="\t", row.names=FALSE)

#write.table(resultados_brigada12_gen, "clipboard-16384", sep="\t", row.names=FALSE)
#write.table(resultados_brigada12_t, "clipboard-16384", sep="\t", row.names=FALSE)
#write.table(resultados_brigada12_c, "clipboard-16384", sep="\t", row.names=FALSE)

b12_unicos <- db_analisis_uniq %>%
  filter(s1_5 == id_brigada, grupo.y == "control") %>%
  group_by(s4_3_1_n) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  arrange(s4_3_1_n)


db4_comp <- db4 %>%
  left_join(db %>% select("_index", grupo, s1_5), by = c("_parent_index" = "_index")) %>% 
  filter(s1_5 == 611)
write.table(db4_comp, "clipboard-16384", sep="\t", row.names=FALSE)