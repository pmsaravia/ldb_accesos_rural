# Línea de base Programa ACCESOS RURAL
# Ministerio de Desarrollo Rural y Tierras - FIDA

# Código principal de resultados ####
# Compilación 26/01/2024
# Elaborado por ARIA SRL

rm(list = ls())

# Librerías
library(tidyverse)
library(haven)
library(labelled)
library(Hmisc)
library(readxl)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(magrittr)
library(scales)
library(purrr)

# Base de datos
db <- readRDS("data_in/3_db_clean/db.rds")
db2 <- readRDS("data_in/3_db_clean/db2.rds")
db4 <- readRDS("data_in/3_db_clean/db4.rds")

# Funciones ####
# Generador Tablas tratamiento/control #
gen_table <- function(db, var_name, new_var_name = NULL) {
  var_sym <- sym(var_name)
  
  tab <- db %>%
    filter(!is.na(!!var_sym)) %>% # filter out NA values in the key variable before grouping
    group_by(!!var_sym, grupo) %>%
    summarise(count = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = grupo, values_from = c(count))
  
  total_tratamiento <- sum(tab$tratamiento, na.rm = TRUE)
  total_control <- sum(tab$control, na.rm = TRUE)
  
  tab <- tab %>%
    mutate(Perc_Trata = tratamiento / total_tratamiento,
           Perc_Control = control / total_control)
  
  tab <- tab %>%
    select(!!var_sym, Perc_Trata, Perc_Control) 
  
  # Rename variable if new name provided
  if (!is.null(new_var_name)) {
    tab <- tab %>%
      rename(!!new_var_name := !!var_sym)
  }
  
  tab <- rename(tab, 
                `Tratamiento %` = Perc_Trata,
                `Control %` = Perc_Control)
  
  tab <- tab %>%
    mutate(`Tratamiento %` = percent(`Tratamiento %`, accuracy = 0.1),
           `Control %` = percent(`Control %`, accuracy = 0.1))
  
  return(tab)
}

# FUnción de recorte 1,99 #
trim_df <- function(df, variable) {
  limite_inferior <- quantile(df[[variable]], 0.10, na.rm = TRUE)
  limite_superior <- quantile(df[[variable]], 0.99, na.rm = TRUE)
  
  df_recortado <- df %>%
    filter(df[[variable]] >= limite_inferior & df[[variable]] <= limite_superior)
  
  return(df_recortado)
}

# Función estadísticos #
estadisticos <- function(df, variable, unidad, digitos) {
  # Asegurarse de que variable es una cadena
  variable <- deparse(substitute(variable))
  
  # Calcular estadísticos sin contar NA's
  estadisticos <- c(
    `Mínimo` = min(df[[variable]], na.rm = TRUE),
    `Percentil 5%` = quantile(df[[variable]], probs = 0.05, na.rm = TRUE)[[1]],
    `Percentil 25%` = quantile(df[[variable]], 0.25, na.rm = TRUE)[[1]],
    `Mediana (P50%)` = median(df[[variable]], na.rm = TRUE)[[1]],
    `Media` = mean(df[[variable]], na.rm = TRUE),
    `Percentil 75%` = quantile(df[[variable]], 0.75, na.rm = TRUE)[[1]],
    `Percentil 95%` = quantile(df[[variable]], 0.95, na.rm = TRUE)[[1]],
    `Maximo` = max(df[[variable]], na.rm = TRUE)
  )
  
  # Convertir en dataframe con estadísticos en una columna y sus valores en otra
  resumen <- data.frame(
    Estadistico = names(estadisticos),
    Valor = as.numeric(estadisticos)
  )
  
  # Redondear los valores al número de dígitos especificado
  resumen$Valor <- round(resumen$Valor, digits = digitos)
  
  # Agregar unidad a la columna Valor
  #resumen$Valor <- paste(resumen$Valor, unidad, sep=" ")
  
  return(resumen)
}

# Colores ####
colors <- list(
  Base = "#1A428A",
  Complementary = "#0081C9", 
  DarkerNeutral = "#005B79", 
  LighterNeutral = "#00A79D", 
  Accent = "#474C55" 
)

# 1. IDENTIFICACIÓN ####

# 2. DEMOGRAFÍA ####
# Sexo
sexo_todos <- gen_table(db2, "s2_4", "Sexo de todos los encuestados")
sexo_jefe <- db2 %>% 
  filter(s2_1c == "Jefe o jefa de hogar")

sexo_jefe <- gen_table(sexo_jefe, "s2_4", "Sexo del jefe de hogar")

# Edad 

age_breaks <- c(0, 16, 28, 59, 79, Inf)  # Define the age group breaks
tab_edad <- db2 %>%
  filter(!is.na(s2_6)) %>%
  mutate(age_group = cut(s2_6, breaks = age_breaks, labels = c("0-15", "16-28", "29-59", "60-79", ">79"), right = FALSE)) %>%
  filter(!is.na(age_group))

tab_edad <- gen_table(tab_edad, "age_group", "Grupo etario")
tab_identificacion <- gen_table(db2, "s2_5", "Autoidentificación originaria")
tab_idioma <- gen_table(db2, "s2_3", "Idioma que se habla principalemente en el hogar")
tab_estado_civil <- gen_table(db2, "s2_7", "Estado civil conyugal")
tab_celu <- gen_table(db2, "s2_8", "Tenencia de teléfono celular")


# 3. VIVIENDA Y ACTIVOS ####
##Tipo de vivienda
tipo_viviendas <- gen_table(db, "s3_1", "Tipo Vivienda")
propiedad_vivienda <- gen_table(db,"s3_2", "Propiedad vivienda")

db$s3_3 <- ifelse(db$s3_3 <= 5, as.character(db$s3_3), ">5")
num_cuartos <- gen_table(db, "s3_3", "Numero de cuartos")

material_paredes <- gen_table(db, "s3_4", "Material paredes")
material_techo <- gen_table(db, "s3_5", "Material techo")
material_pisos <- gen_table(db, "s3_6", "Material pisos")
tipo_sanitario <- gen_table(db, "s3_7", "Tipo de servicio sanitario")
fuente_luz <- gen_table(db, "s3_8", "Cuenta con energía electrica")
tipo_combustible <- gen_table(db, "s3_10", "Tipo de Combustible")
agua_beber <- gen_table(db, "s3_11", "Fuente de Agua para beber")

##Activos 
## Tractor
tractor <- gen_table(db, "s3_12_1", "Tractor")
arado <- gen_table(db, "s3_12_2", "Arado")
azadon <- gen_table(db, "s3_12_3", "Azadón")
hoz_hocino <- gen_table(db, "s3_12_4", "Hoz/hocino")
hacha <- gen_table(db, "s3_12_5", "Hacha")
bomba_de_agua <- gen_table(db, "s3_12_6", "Bomba de agua motorizada/electrica")
tijeras_de_podar <- gen_table(db, "s3_12_7", "Tijeras de podar")
perdiga <- gen_table(db, "s3_12_8", "Perdiga o vara recogedora")

#Resumen

herramienta <- c("Tractor", "Arado", "Azadón", "Hoz/Hocino", "Hacha", 
                 "Bomba de agua motorizada/eléctrica", "Tijeras de podar", "Perdiga o vara recogedora")

tratamiento_porcentaje <- c("7.9%", "22.7%", "70.8%", "64.4%", "78.6%", "34.1%", "36.6%", "8.7%")

control_porcentaje <- c("8.8%", "22.7%", "71.7%", "68.9%", "68.4%", "31.4%", "34.2%", "17.3%")

# Combinar los vectores en un dataframe
resumen_herramientas <- data.frame(
  `Activo productivo` = herramienta,
  `Porcentaje Tratamiento` = tratamiento_porcentaje,
  `Porcentaje Control` = control_porcentaje
)



# 4. AGRÍCOLA Y OTROS INGRESOS ####
# ANÁLISIS PAPA ####
db4_papa <- db4 %>% 
  filter(s4_3_1 == "Papa")

db4_papa_trat <- db4 %>% 
  filter(s4_3_1 == "Papa", grupo == "tratamiento")
db4_papa_control <- db4 %>% 
  filter(s4_3_1 == "Papa", grupo == "control")

# Superficie ####
# Muestra completa (riego y secano)
sup_papa_tyc <- db4_papa %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_papa <- db4_papa %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_papa <- sup_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_papa <- sup_papa %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_papa_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_papa_tyc$sup_median_total
  ))
  
sup_papa <- sup_papa %>%
  select(variable, Total, tratamiento, control)

db4_papa_trim <- trim_df(db4_papa, "s4_sup_std") #Trimmeado para histograma

fig_sup_papa_mc <- ggplot(db4_papa_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_papa_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_papa_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_papa_tyc$sup_median_total, y = max(db4_papa_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_papa_tyc$sup_mean_total, y = max(db4_papa_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_papa_trim_trat <- db4_papa_trim %>% 
  filter(grupo == "tratamiento")

sup_papa_t <- db4_papa_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_papa_t <-ggplot(db4_papa_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_papa_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_papa_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_papa_t$sup_median_total, y = max(db4_papa_trim$s4_sup_std) * 22, hjust = -0.1, label = "Mediana (0.5 [ha])", color = colors$Accent) +
  annotate("text", x = sup_papa_t$sup_mean_total, y = max(db4_papa_trim$s4_sup_std) * 20, hjust = -0.1, label = "Media (0.78 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_papa_trim_control <- db4_papa_trim %>% 
  filter(grupo == "control")

sup_papa_c <- db4_papa_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_papa_c <-ggplot(db4_papa_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_papa_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_papa_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_papa_c$sup_median_total, y = max(db4_papa_trim$s4_sup_std) * 30, hjust = -0.3, label = "Mediana (0.25 [ha])", color = colors$Accent) +
  annotate("text", x = sup_papa_c$sup_mean_total, y = max(db4_papa_trim$s4_sup_std) * 28, hjust = -0.3, label = "Media (0.49 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_papa_sup <- estadisticos(db4_papa, s4_sup_std, "[ha]", 3)
estadisticos_papa_sup_t <- estadisticos(db4_papa_trat, s4_sup_std, "[ha]", 3)
estadisticos_papa_sup_c <- estadisticos(db4_papa_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_papa_sup$Valor
valores_t <- estadisticos_papa_sup_t$Valor
valores_c <- estadisticos_papa_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_papa_sup <- data.frame(
  Estadistico = estadisticos_papa_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Superficie con riego
# No vale la pena mostrar datos sobre la superficie con riego y secano, son similares
# De manera similar, no se mostrarán la cantidades. Lo importante es mostrar los rendimiento

db4_chk_riego <- db4_papa %>%
  filter(grupo == "control") %>% 
  filter(s4n4 == "No") 

db4_papa_r <- db4 %>% 
  filter(s4_3_1 == "Papa",s4n4 == "Si")

db4_papa_r_trat <- db4_papa_r %>% 
  filter(grupo == "tratamiento")
db4_papa_r_control <- db4_papa_r %>% 
  filter(grupo == "control")

sup_papa_riego <- db4_papa %>% 
  filter(s4n4 == 1) %>% 
  group_by(grupo) %>%
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_papa_riego <- sup_papa_riego %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_papa_secano <- db4_papa %>% 
  filter(s4n4 == 2) %>% 
  group_by(grupo) %>%
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2))

sup_papa_secano <- sup_papa_secano %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

mean_sup_by_municipio <- db4_papa %>% #USAR PARA MAPA; AGREGAR CANTIDADES Y RENDIMIENTOS
  group_by(s1_2) %>%
  summarise(mean_sup_std = mean(s4_sup_std, na.rm = TRUE), n = n()) %>%
  arrange(mean_sup_std)

# Riego ####
acceso_riego_papa <- gen_table(db4_papa, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_papa <- gen_table(db4_papa, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_papa <- gen_table(db4_papa, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####

fig_hist_perd_papa_t <-ggplot(db4_papa_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_papa_c <-ggplot(db4_papa_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_papa_perd <- estadisticos(db4_papa, perdida, "%", 0)
estadisticos_papa_perd_t <- estadisticos(db4_papa_trat, perdida, "%", 0)
estadisticos_papa_perd_c <- estadisticos(db4_papa_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_papa_perd$Valor
valores_perd_t <- estadisticos_papa_perd_t$Valor
valores_perd_c <- estadisticos_papa_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_papa_perd <- data.frame(
  Estadistico = estadisticos_papa_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

# Cantidad ####
# Muestra completa #
q_papa_tyc <- db4_papa %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_papa <- db4_papa %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_papa <- q_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_papa <- q_papa %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_papa_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_papa_tyc$q_median_total
  ))

q_papa <- q_papa %>%
  select(variable, Total, tratamiento, control)

db4_papa_trim_q <- trim_df(db4_papa, "s4_q_std") #Trimmeado para histograma

fig_hist_q_mc <- ggplot(db4_papa_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_papa_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_papa_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_papa_tyc$q_median_total, y = max(db4_papa_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (368 [t])", color = colors$Accent) +
  annotate("text", x = q_papa_tyc$q_mean_total, y = max(db4_papa_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (1249 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_hist_q_t <- ggplot(db4_papa_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_papa$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_papa$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_papa$tratamiento[2], y = max(db4_papa_trim$s4_sup_std) * 42, hjust = -0.15, label = "Mediana (0.5 [t])", color = colors$Accent) +
  annotate("text", x = q_papa$tratamiento[1], y = max(db4_papa_trim$s4_sup_std) * 37, hjust = -0.15, label = "Media (3.8 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")
  #scale_x_continuous(breaks = seq(0, 15000, length.out = 7))

fig_hist_q_c <- ggplot(db4_papa_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_papa$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_papa$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_papa$control[2], y = max(db4_papa_trim$s4_sup_std) * 55, hjust = -0.25, label = "Mediana (0.3 [t])", color = colors$Accent) +
  annotate("text", x = q_papa$control[1], y = max(db4_papa_trim$s4_sup_std) * 50, hjust = -0.25, label = "Media (0.8 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") 

chackpapapapa <- db4_papa %>% 
  select(`_index`, rend)

estadisticos_papa_q <- estadisticos(db4_papa, s4_q_std, "%", 0)
estadisticos_papa_q_t <- estadisticos(db4_papa_trat, s4_q_std, "%", 0)
estadisticos_papa_q_c <- estadisticos(db4_papa_control, s4_q_std, "%", 0)

valores_q_mc <- estadisticos_papa_q$Valor
valores_q_t <- estadisticos_papa_q_t$Valor
valores_q_c <- estadisticos_papa_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_papa_q <- data.frame(
  Estadistico = estadisticos_papa_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_papa_tyc <- db4_papa %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

checkpapa <- db4_papa %>% 
  select(`_index`, s4_3_2, s4_sup_std, s4_q_std, rend, s4n7,s4n7_u)

rend_papa <- db4_papa %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_papa <- rend_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_papa <- rend_papa %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_papa_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_papa_tyc$rend_median_total
  ))

rend_papa <- rend_papa %>%
  select(variable, Total, tratamiento, control)

db4_papa_trim_rend <- trim_df(db4_papa, "rend") #Trimmeado para histograma

fig_sup_rend_t <- ggplot(db4_papa_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_papa$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_papa$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_papa$tratamiento[2], y = max(db4_papa_trim$s4_sup_std) * 18, hjust = -0.1, label = "Mediana (2.21 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_papa$tratamiento[1], y = max(db4_papa_trim$s4_sup_std) * 13, hjust = -0.1, label = "Media (5.28 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia")

fig_sup_rend_c <- ggplot(db4_papa_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_papa$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_papa$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_papa$control[2], y = max(db4_papa_trim$s4_sup_std) * 18, hjust = -0.1, label = "Mediana (1.15 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_papa$control[1], y = max(db4_papa_trim$s4_sup_std) * 13, hjust = -0.1, label = "Media (2.21 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia")

estadisticos_papa_rend <- estadisticos(db4_papa, rend, "%", 2)
estadisticos_papa_rend_t <- estadisticos(db4_papa_trat, rend, "%", 2)
estadisticos_papa_rend_c <- estadisticos(db4_papa_control, rend, "%", 2)

valores_rend_mc <- estadisticos_papa_rend$Valor
valores_rend_t <- estadisticos_papa_rend_t$Valor
valores_rend_c <- estadisticos_papa_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_papa_rend <- data.frame(
  Estadistico = estadisticos_papa_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Pérdidas
incidencia_total_papa <- db4_papa %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_papa <- incidencia_total_papa %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_papa <- incidencia_total_papa %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_papa$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_papa$Factor)
incidencia_total_papa$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_papa$Factor)
incidencia_total_papa$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_papa$Factor)
incidencia_total_papa$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_papa$Factor)
incidencia_total_papa$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_papa$Factor)
incidencia_total_papa$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_papa$Factor)

incidencia_total_papa$Porcentaje <- as.numeric(sub("%", "", incidencia_total_papa$Porcentaje))
incidencia_total_papa$Factor <- factor(incidencia_total_papa$Factor, levels = incidencia_total_papa$Factor)

incidencia_total_papa$Factor <- factor(
  incidencia_total_papa$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_papa_incidencia_graph <- 
  ggplot(incidencia_total_papa, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_papa %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_papa %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_papa_tyc <- db4_papa %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_papa <- db4_papa %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_papa <- cons_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de papa [t]",
    variable == "cons_median" ~ "Mediana consumo de papa [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_papa %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_papa %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_papa <-
ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_papa_tyc <- db4_papa %>%
  filter(s4_q_venta_std < quantile(s4_q_venta_std, 0.95, na.rm = TRUE)) %>% 
    summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_papa <- db4_papa %>%
  group_by(grupo) %>%
  filter(s4_q_venta_std < quantile(s4_q_venta_std, 0.95, na.rm = TRUE)) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_papa <- venta_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de papa [t]",
    variable == "venta_median" ~ "Mediana venta de papa [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_papa <- gen_table(db4_papa, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO PRIORIZADO
ingreso_bruto_papa <- db4_papa %>%
  #filter(ing_bruto_ap < quantile(db4_papa$ing_bruto_ap, 0.99)) %>% 
  #filter(ing_bruto_ap > quantile(db4_papa$ing_bruto_ap, 0.01)) %>% 
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_papa <- ingreso_bruto_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO PRIORIZADO
ing_neto_papa_tyc <- db4_papa %>%
  summarise(
    ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
    venta_neto_median_total = round(median(ing_neto_ap, na.rm = TRUE), 2)
  )

ingreso_neto_papa <- db4_papa %>%
  #filter(ing_neto_ap < quantile(db4_papa$ing_neto_ap, 0.995, na.rm = TRUE)) %>% 
  #filter(ing_neto_ap > quantile(db4_papa$ing_neto_ap, 0.01, na.rm = TRUE)) %>% 
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_papa <- ingreso_neto_papa %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
rubros_priorizados <- c("Papa", "Maiz", "Tomate", "Zanahoria", "Haba", "Cebolla", "Durazno", "Manzana")

ingreso_bruto_papa_nprio <- db4 %>% 
  filter(rubro_parent == "Papa") %>% 
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

# ingreso_bruto_papa_nprio <- db4_papa %>%
#   group_by(grupo) %>%
#   filter(ing_bruto_nprio < quantile(db4_papa$ing_bruto_nprio, 0.99)) %>% 
#   filter(ing_bruto_nprio > quantile(db4_papa$ing_bruto_nprio, 0.01)) %>% 
#   summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
#             ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

ingreso_bruto_papa_nprio <- ingreso_bruto_papa_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_papa_nprio <- db4 %>% 
  #filter(!(s4_3_1 %in% c("Papa", "Maiz", "Tomate", "Zanahoria", "Cebolla", "Haba", "Durazno", "Manzana"))) %>% 
  filter(rubro_parent == "Papa") %>%
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

cehksejhhsio <- db4 %>% 
  select(`_index`, grupo, ing_neto_nprio)

ingreso_neto_papa_nprio <- ingreso_neto_papa_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Papa") %>% 
  group_by(grupo) %>% #Solo de los productores de papa
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Papa") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)


# ANÁLISIS MAIZ ####
db4_maiz <- db4 %>% 
  filter(s4_3_1 == "Maiz") 

db4_maiz_trat <- db4 %>% 
  filter(s4_3_1 == "Maiz", grupo == "tratamiento")
db4_maiz_control <- db4 %>% 
  filter(s4_3_1 == "Maiz", grupo == "control")

sup_maiz_tyc <- db4_maiz %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_maiz <- db4_maiz %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_maiz <- sup_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_maiz <- sup_maiz %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_maiz_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_maiz_tyc$sup_median_total
  ))

sup_maiz <- sup_maiz %>%
  select(variable, Total, tratamiento, control)

db4_maiz_trim <- trim_df(db4_maiz, "s4_sup_std") #Trimmeado para histograma

fig_sup_maiz_mc <- ggplot(db4_maiz_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_maiz_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_maiz_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_maiz_tyc$sup_median_total, y = max(db4_maiz_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_maiz_tyc$sup_mean_total, y = max(db4_maiz_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_maiz_trim_trat <- db4_maiz_trim %>% 
  filter(grupo == "tratamiento")

sup_maiz_t <- db4_maiz_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_maiz_t <-ggplot(db4_maiz_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_maiz_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_maiz_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_maiz_t$sup_median_total, y = max(db4_maiz_trim$s4_sup_std) * 15, hjust = -0.15, label = "Mediana (0.25 [ha])", color = colors$Accent) +
  annotate("text", x = sup_maiz_t$sup_mean_total, y = max(db4_maiz_trim$s4_sup_std) * 13, hjust = -0.1, label = "Media (0.58 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_maiz_trim_control <- db4_maiz_trim %>% 
  filter(grupo == "control")

sup_maiz_c <- db4_maiz_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_maiz_c <-ggplot(db4_maiz_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_maiz_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_maiz_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_maiz_c$sup_median_total, y = max(db4_maiz_trim$s4_sup_std) * 15, hjust = -0.1, label = "Mediana (0.33 [ha])", color = colors$Accent) +
  annotate("text", x = sup_maiz_c$sup_mean_total, y = max(db4_maiz_trim$s4_sup_std) * 13.5, hjust = -0.1, label = "Media (0.54 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_maiz_sup <- estadisticos(db4_maiz, s4_sup_std, "[ha]", 3)
estadisticos_maiz_sup_t <- estadisticos(db4_maiz_trat, s4_sup_std, "[ha]", 3)
estadisticos_maiz_sup_c <- estadisticos(db4_maiz_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_maiz_sup$Valor
valores_t <- estadisticos_maiz_sup_t$Valor
valores_c <- estadisticos_maiz_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_maiz_sup <- data.frame(
  Estadistico = estadisticos_maiz_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

#Superficie con riego
# No vale la pena mostrar datos sobre la superficie con riego y secano, son similares
# De manera similar, no se mostrarán la cantidades. Lo importante es mostrar los rendimiento

db4_chk_riego <- db4_maiz %>%
  filter(grupo == "control") %>% 
  filter(s4n4 == "No") 

db4_maiz_r <- db4 %>% 
  filter(s4_3_1 == "Maiz",s4n4 == "Si")

db4_maiz_r_trat <- db4_maiz_r %>% 
  filter(grupo == "tratamiento")
db4_maiz_r_control <- db4_maiz_r %>% 
  filter(grupo == "control")

sup_maiz_riego <- db4_maiz %>% 
  filter(s4n4 == "Si") %>% 
  group_by(grupo) %>%
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_maiz_riego <- sup_maiz_riego %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_maiz_secano <- db4_maiz %>% 
  filter(s4n4 == "No") %>% 
  group_by(grupo) %>%
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2))

sup_maiz_secano <- sup_maiz_secano %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

mean_sup_by_municipio <- db4_maiz %>% #USAR PARA MAPA; AGREGAR CANTIDADES Y RENDIMIENTOS
  group_by(s1_2) %>%
  summarise(mean_sup_std = mean(s4_sup_std, na.rm = TRUE), n = n()) %>%
  arrange(mean_sup_std)

# Riego ####
acceso_riego_maiz <- gen_table(db4_maiz, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_maiz <- gen_table(db4_maiz, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_maiz <- gen_table(db4_maiz, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####

fig_hist_perd_maiz_t <-ggplot(db4_maiz_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_maiz_c <-ggplot(db4_maiz_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_maiz_perd <- estadisticos(db4_maiz, perdida, "%", 0)
estadisticos_maiz_perd_t <- estadisticos(db4_maiz_trat, perdida, "%", 0)
estadisticos_maiz_perd_c <- estadisticos(db4_maiz_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_maiz_perd$Valor
valores_perd_t <- estadisticos_maiz_perd_t$Valor
valores_perd_c <- estadisticos_maiz_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_maiz_perd <- data.frame(
  Estadistico = estadisticos_maiz_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_maiz <- db4_maiz %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_maiz <- incidencia_total_maiz %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_maiz <- incidencia_total_maiz %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_maiz$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_maiz$Factor)
incidencia_total_maiz$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_maiz$Factor)
incidencia_total_maiz$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_maiz$Factor)
incidencia_total_maiz$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_maiz$Factor)
incidencia_total_maiz$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_maiz$Factor)
incidencia_total_maiz$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_maiz$Factor)

incidencia_total_maiz$Porcentaje <- as.numeric(sub("%", "", incidencia_total_maiz$Porcentaje))
incidencia_total_maiz$Factor <- factor(incidencia_total_maiz$Factor, levels = incidencia_total_maiz$Factor)

incidencia_total_maiz$Factor <- factor(
  incidencia_total_maiz$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_maiz_incidencia_graph <- 
  ggplot(incidencia_total_maiz, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

# Cantidad ####
# Muestra completa #
q_maiz_tyc <- db4_maiz %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_maiz <- db4_maiz %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_maiz <- q_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_maiz <- q_maiz %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_maiz_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_maiz_tyc$q_median_total
  ))

q_maiz <- q_maiz %>%
  select(variable, Total, tratamiento, control)

db4_maiz_trim_q <- trim_df(db4_maiz, "s4_q_std") #Trimmeado para histograma

fig_maiz_hist_q_mc <- ggplot(db4_maiz_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_maiz_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_maiz_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_maiz_tyc$q_median_total, y = max(db4_maiz_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (448 [t])", color = colors$Accent) +
  annotate("text", x = q_maiz_tyc$q_mean_total, y = max(db4_maiz_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (874 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_maiz_hist_q_t <- ggplot(db4_maiz_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_maiz$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_maiz$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_maiz$tratamiento[2], y = max(db4_maiz_trim$s4_sup_std) * 15, hjust = -0.1, label = "Mediana (0.4 [t])", color = colors$Accent) +
  annotate("text", x = q_maiz$tratamiento[1], y = max(db4_maiz_trim$s4_sup_std) * 14, hjust = -0.1, label = "Media (0.9 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

fig_maiz_hist_q_c <- ggplot(db4_maiz_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_maiz$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_maiz$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_maiz$control[2], y = max(db4_maiz_trim$s4_sup_std) * 15, hjust = -0.1, label = "Mediana (0.4 [t])", color = colors$Accent) +
  annotate("text", x = q_maiz$control[1], y = max(db4_maiz_trim$s4_sup_std) * 14, hjust = -0.1, label = "Media (0.7 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

estadisticos_maiz_q <- estadisticos(db4_maiz, s4_q_std, "%", 0)
estadisticos_maiz_q_t <- estadisticos(db4_maiz_trat, s4_q_std, "%", 0)
estadisticos_maiz_q_c <- estadisticos(db4_maiz_control, s4_q_std, "%", 0)

valores_q_mc <- estadisticos_maiz_q$Valor
valores_q_t <- estadisticos_maiz_q_t$Valor
valores_q_c <- estadisticos_maiz_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_maiz_q <- data.frame(
  Estadistico = estadisticos_maiz_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_maiz_tyc <- db4_maiz %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_maiz <- db4_maiz %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_maiz <- rend_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_maiz <- rend_maiz %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_maiz_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_maiz_tyc$rend_median_total
  ))

rend_maiz <- rend_maiz %>%
  select(variable, Total, tratamiento, control)

db4_maiz_trim_rend <- trim_df(db4_maiz, "rend") #Trimmeado para histograma

fig_maiz_rend_t <- ggplot(db4_maiz_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_maiz$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_maiz$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_maiz$tratamiento[2], y = max(db4_maiz_trim$s4_sup_std) * 7, hjust = -0.1, label = "Mediana (2.21 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_maiz$tratamiento[1], y = max(db4_maiz_trim$s4_sup_std) * 6, hjust = -0.1, label = "Media (5.63 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 50000, length.out = 10))

fig_maiz_rend_c <- ggplot(db4_maiz_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_maiz$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_maiz$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_maiz$control[2], y = max(db4_maiz_trim$s4_sup_std) * 5, hjust = -0.1, label = "Mediana (1.84 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_maiz$control[1], y = max(db4_maiz_trim$s4_sup_std) * 4, hjust = -0.1, label = "Media (2.58 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_maiz_rend <- estadisticos(db4_maiz, rend, "%", 0)
estadisticos_maiz_rend_t <- estadisticos(db4_maiz_trat, rend, "%", 0)
estadisticos_maiz_rend_c <- estadisticos(db4_maiz_control, rend, "%", 0)

valores_rend_mc <- estadisticos_maiz_rend$Valor
valores_rend_t <- estadisticos_maiz_rend_t$Valor
valores_rend_c <- estadisticos_maiz_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_maiz_rend <- data.frame(
  Estadistico = estadisticos_maiz_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_maiz %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_maiz %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_maiz <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_maiz_tyc <- db4_maiz %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_maiz <- db4_maiz %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_maiz <- cons_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de maiz [t]",
    variable == "cons_median" ~ "Mediana consumo de maiz [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_maiz %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_maiz %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_maiz <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_maiz_tyc <- db4_maiz %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_maiz <- db4_maiz %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_maiz <- venta_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de maiz [t]",
    variable == "venta_median" ~ "Mediana venta de maiz [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_maiz <- gen_table(db4_maiz, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO PRIORIZADO
ingreso_bruto_maiz <- db4_maiz %>%
  group_by(grupo) %>%
  filter(ing_bruto_ap < quantile(db4_maiz$ing_bruto_ap, 0.99)) %>% 
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_maiz <- ingreso_bruto_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO PRIORIZADO
ingreso_neto_maiz <- db4_maiz %>%
  #filter(ing_neto_ap < quantile(db4_maiz$ing_neto_ap, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_neto_ap > quantile(db4_maiz$ing_neto_ap, 0.05, na.rm = TRUE)) %>% 
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

check_maiz <- db4_maiz %>% 
  select(`_index`, ing_neto_ap, ing_bruto_ap, costos_fam, ing_bruto_venta, s4_sup_std, s4_q_std, rend, perdida, grupo)

ingreso_neto_maiz <- ingreso_neto_maiz %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_maiz_nprio <- db4 %>% 
filter(rubro_parent == "Maiz") %>%
  group_by(grupo) %>%
  filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.05)) %>% 
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

ingreso_bruto_maiz_nprio <- ingreso_bruto_maiz_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_maiz_nprio <- db4 %>%
  filter(rubro_parent == "Maiz") %>%
  group_by(grupo)%>%
  filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_maiz_nprio <- ingreso_neto_maiz_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Maiz") %>% 
  group_by(grupo) %>%
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Maiz") %>%
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# ANÁLISIS TOMATE ####
db4_tomate <- db4 %>% 
  filter(s4_3_1 == "Tomate") 

db4_tomate_trat <- db4 %>% 
  filter(s4_3_1 == "Tomate", grupo == "tratamiento")
db4_tomate_control <- db4 %>% 
  filter(s4_3_1 == "Tomate", grupo == "control")

sup_tomate_tyc <- db4_tomate %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_tomate <- sup_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_tomate <- sup_tomate %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_tomate_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_tomate_tyc$sup_median_total
  ))

sup_tomate <- sup_tomate %>%
  select(variable, Total, tratamiento, control)

db4_tomate_trim <- trim_df(db4_tomate, "s4_sup_std") #Trimmeado para histograma

fig_sup_tomate_mc <- ggplot(db4_tomate_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_tomate_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_tomate_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_tomate_tyc$sup_median_total, y = max(db4_tomate_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_tomate_tyc$sup_mean_total, y = max(db4_tomate_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_tomate_trim_trat <- db4_tomate_trim %>% 
  filter(grupo == "tratamiento")

sup_tomate_t <- db4_tomate_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_tomate_t <-ggplot(db4_tomate_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_tomate_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_tomate_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_tomate_t$sup_median_total, y = max(db4_tomate_trim$s4_sup_std) * 15, hjust = -0.1, label = "Mediana (0.35 [ha])", color = colors$Accent) +
  annotate("text", x = sup_tomate_t$sup_mean_total, y = max(db4_tomate_trim$s4_sup_std) * 13, hjust = -0.05, label = "Media (0.51 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_tomate_trim_control <- db4_tomate_trim %>% 
  filter(grupo == "control")

sup_tomate_c <- db4_tomate_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_tomate_c <-ggplot(db4_tomate_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_tomate_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_tomate_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_tomate_c$sup_median_total, y = max(db4_tomate_trim$s4_sup_std) * 14.5, hjust = -0.3, label = "Mediana (0.25 [ha])", color = colors$Accent) +
  annotate("text", x = sup_tomate_c$sup_mean_total, y = max(db4_tomate_trim$s4_sup_std) * 12.5, hjust = -0.1, label = "Media (0.49 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_tomate_sup <- estadisticos(db4_tomate, s4_sup_std, "[ha]", 3)
estadisticos_tomate_sup_t <- estadisticos(db4_tomate_trat, s4_sup_std, "[ha]", 3)
estadisticos_tomate_sup_c <- estadisticos(db4_tomate_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_tomate_sup$Valor
valores_t <- estadisticos_tomate_sup_t$Valor
valores_c <- estadisticos_tomate_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_tomate_sup <- data.frame(
  Estadistico = estadisticos_tomate_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_tomate <- gen_table(db4_tomate, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_tomate <- gen_table(db4_tomate, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_tomate <- gen_table(db4_tomate, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
fig_hist_perd_tomate_t <-ggplot(db4_tomate_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_tomate_c <-ggplot(db4_tomate_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_tomate_perd <- estadisticos(db4_tomate, perdida, "%", 0)
estadisticos_tomate_perd_t <- estadisticos(db4_tomate_trat, perdida, "%", 0)
estadisticos_tomate_perd_c <- estadisticos(db4_tomate_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_tomate_perd$Valor
valores_perd_t <- estadisticos_tomate_perd_t$Valor
valores_perd_c <- estadisticos_tomate_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_tomate_perd <- data.frame(
  Estadistico = estadisticos_tomate_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_tomate <- db4_tomate %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_tomate <- incidencia_total_tomate %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_tomate <- incidencia_total_tomate %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_tomate$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_tomate$Factor)
incidencia_total_tomate$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_tomate$Factor)
incidencia_total_tomate$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_tomate$Factor)
incidencia_total_tomate$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_tomate$Factor)
incidencia_total_tomate$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_tomate$Factor)
incidencia_total_tomate$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_tomate$Factor)

incidencia_total_tomate$Porcentaje <- as.numeric(sub("%", "", incidencia_total_tomate$Porcentaje))
incidencia_total_tomate$Factor <- factor(incidencia_total_tomate$Factor, levels = incidencia_total_tomate$Factor)

incidencia_total_tomate$Factor <- factor(
  incidencia_total_tomate$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_tomate_incidencia_graph <- 
  ggplot(incidencia_total_tomate, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

# Cantidad ####
# Muestra completa #
q_tomate_tyc <- db4_tomate %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_tomate <- q_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_tomate <- q_tomate %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_tomate_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_tomate_tyc$q_median_total
  ))

q_tomate <- q_tomate %>%
  select(variable, Total, tratamiento, control)

db4_tomate_trim_q <- trim_df(db4_tomate, "s4_q_std") #Trimmeado para histograma

fig_tomate_hist_q_mc <- ggplot(db4_tomate_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_tomate_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_tomate_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_tomate_tyc$q_median_total, y = max(db4_tomate_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (448 [t])", color = colors$Accent) +
  annotate("text", x = q_tomate_tyc$q_mean_total, y = max(db4_tomate_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (874 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_tomate_hist_q_t <- ggplot(db4_tomate_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_tomate$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_tomate$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_tomate$tratamiento[2], y = max(db4_tomate_trim$s4_sup_std) * 8, hjust = -0.1, label = "Mediana (5.5 [t])", color = colors$Accent) +
  annotate("text", x = q_tomate$tratamiento[1], y = max(db4_tomate_trim$s4_sup_std) * 9, hjust = -0.1, label = "Media (8 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 15000, length.out = 7))

fig_tomate_hist_q_c <- ggplot(db4_tomate_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_tomate$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_tomate$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_tomate$control[2], y = max(db4_tomate_trim$s4_sup_std) * 12, hjust = -0.3, label = "Mediana (3.3 [t])", color = colors$Accent) +
  annotate("text", x = q_tomate$control[1], y = max(db4_tomate_trim$s4_sup_std) * 13, hjust = -0.1, label = "Media (6.7 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
  #scale_x_continuous(breaks = seq(0, 15000, length.out = 7))

estadisticos_tomate_q <- estadisticos(db4_tomate, s4_q_std, "%", 2)
estadisticos_tomate_q_t <- estadisticos(db4_tomate_trat, s4_q_std, "%", 2)
estadisticos_tomate_q_c <- estadisticos(db4_tomate_control, s4_q_std, "%", 2)

valores_q_mc <- estadisticos_tomate_q$Valor
valores_q_t <- estadisticos_tomate_q_t$Valor
valores_q_c <- estadisticos_tomate_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_tomate_q <- data.frame(
  Estadistico = estadisticos_tomate_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_tomate_tyc <- db4_tomate %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_tomate <- rend_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_tomate <- rend_tomate %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_tomate_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_tomate_tyc$rend_median_total
  ))

rend_tomate <- rend_tomate %>%
  select(variable, Total, tratamiento, control)

db4_tomate_trim_rend <- trim_df(db4_tomate, "rend") #Trimmeado para histograma

fig_tomate_rend_t <- ggplot(db4_tomate_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_tomate$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_tomate$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_tomate$tratamiento[2], y = max(db4_tomate_trim$s4_sup_std) * 7, hjust = -0.1, label = "Mediana (11.37 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_tomate$tratamiento[1], y = max(db4_tomate_trim$s4_sup_std) * 6.5, hjust = -0.1, label = "Media (18.18 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 10))

fig_tomate_rend_c <- ggplot(db4_tomate_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_tomate$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_tomate$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_tomate$control[2], y = max(db4_tomate_trim$s4_sup_std) * 5, hjust = -0.1, label = "Mediana (13.20 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_tomate$control[1], y = max(db4_tomate_trim$s4_sup_std) * 4.5, hjust = -0.1, label = "Media (19.61 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_tomate_rend <- estadisticos(db4_tomate, rend, "%", 0)
estadisticos_tomate_rend_t <- estadisticos(db4_tomate_trat, rend, "%", 0)
estadisticos_tomate_rend_c <- estadisticos(db4_tomate_control, rend, "%", 0)

valores_rend_mc <- estadisticos_tomate_rend$Valor
valores_rend_t <- estadisticos_tomate_rend_t$Valor
valores_rend_c <- estadisticos_tomate_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_tomate_rend <- data.frame(
  Estadistico = estadisticos_tomate_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_tomate %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_tomate %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_tomate <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_tomate_tyc <- db4_tomate %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_tomate <- cons_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de tomate [t]",
    variable == "cons_median" ~ "Mediana consumo de tomate [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_tomate %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_tomate %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_tomate <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_tomate_tyc <- db4_tomate %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_tomate <- venta_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de tomate [t]",
    variable == "venta_median" ~ "Mediana venta de tomate [t]"
  )) %>% 
  select(variable, tratamiento, control)


# Comercialización

comercial_tomate <- gen_table(db4_tomate, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_tomate <- ingreso_bruto_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ing_neto_tomate_tyc <- db4_tomate %>%
  summarise(
    ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
    venta_neto_median_total = round(median(ing_neto_ap, na.rm = TRUE), 2)
  )

ingreso_neto_tomate <- db4_tomate %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_tomate <- ingreso_neto_tomate %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_tomate_nprio <- db4 %>% 
  filter(rubro_parent == "Tomate") %>% 
  group_by(grupo) %>%
  filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

# daedae <- ingreso_bruto_tomate_nprio %>% 
#   select(`_index`, `_parent_index`, s4_3_1, rubro_parent, grupo, ing_neto_ap, ing_bruto_nprio)

ingreso_bruto_tomate_nprio <- ingreso_bruto_tomate_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_tomate_nprio <- db4 %>% 
  filter(rubro_parent == "Tomate") %>%
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_tomate_nprio <- ingreso_neto_tomate_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Tomate") %>% 
  group_by(grupo) %>% #Solo de los productores de tomate
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Tomate") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# ANÁLISIS ZANAHORIA ####
db4_zanahoria <- db4 %>% 
  filter(s4_3_1 == "Zanahoria") 

db4_zanahoria_trat <- db4 %>% 
  filter(s4_3_1 == "Zanahoria", grupo == "tratamiento")
db4_zanahoria_control <- db4 %>% 
  filter(s4_3_1 == "Zanahoria", grupo == "control")

sup_zanahoria_tyc <- db4_zanahoria %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_zanahoria <- sup_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_zanahoria <- sup_zanahoria %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_zanahoria_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_zanahoria_tyc$sup_median_total
  ))

sup_zanahoria <- sup_zanahoria %>%
  select(variable, Total, tratamiento, control)

db4_zanahoria_trim <- trim_df(db4_zanahoria, "s4_sup_std") #Trimmeado para histograma

fig_sup_zanahoria_mc <- ggplot(db4_zanahoria_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_zanahoria_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_zanahoria_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_zanahoria_tyc$sup_median_total, y = max(db4_zanahoria_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_zanahoria_tyc$sup_mean_total, y = max(db4_zanahoria_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_zanahoria_trim_trat <- db4_zanahoria_trim %>% 
  filter(grupo == "tratamiento")

sup_zanahoria_t <- db4_zanahoria_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_zanahoria_t <-ggplot(db4_zanahoria_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_zanahoria_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_zanahoria_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_zanahoria_t$sup_median_total, y = max(db4_zanahoria_trim$s4_sup_std) * 6, hjust = -0.18, label = "Mediana (0.50 [ha])", color = colors$Accent) +
  annotate("text", x = sup_zanahoria_t$sup_mean_total, y = max(db4_zanahoria_trim$s4_sup_std) * 5.5, hjust = -0.15, label = "Media (0.62 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_zanahoria_trim_control <- db4_zanahoria_trim %>% 
  filter(grupo == "control")

sup_zanahoria_c <- db4_zanahoria_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_zanahoria_c <-ggplot(db4_zanahoria_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_zanahoria_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_zanahoria_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_zanahoria_c$sup_median_total, y = max(db4_zanahoria_trim$s4_sup_std) * 6.5, hjust = -0.2, label = "Mediana (0.36 [ha])", color = colors$Accent) +
  annotate("text", x = sup_zanahoria_c$sup_mean_total, y = max(db4_zanahoria_trim$s4_sup_std) * 5, hjust = -0.1, label = "Media (0.70 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_zanahoria_sup <- estadisticos(db4_zanahoria, s4_sup_std, "[ha]", 3)
estadisticos_zanahoria_sup_t <- estadisticos(db4_zanahoria_trat, s4_sup_std, "[ha]", 3)
estadisticos_zanahoria_sup_c <- estadisticos(db4_zanahoria_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_zanahoria_sup$Valor
valores_t <- estadisticos_zanahoria_sup_t$Valor
valores_c <- estadisticos_zanahoria_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_zanahoria_sup <- data.frame(
  Estadistico = estadisticos_zanahoria_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_zanahoria <- gen_table(db4_zanahoria, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_zanahoria <- gen_table(db4_zanahoria, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_zanahoria <- gen_table(db4_zanahoria, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
fig_hist_perd_zanahoria_t <-ggplot(db4_zanahoria_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_zanahoria_c <-ggplot(db4_zanahoria_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_zanahoria_perd <- estadisticos(db4_zanahoria, perdida, "%", 0)
estadisticos_zanahoria_perd_t <- estadisticos(db4_zanahoria_trat, perdida, "%", 0)
estadisticos_zanahoria_perd_c <- estadisticos(db4_zanahoria_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_zanahoria_perd$Valor
valores_perd_t <- estadisticos_zanahoria_perd_t$Valor
valores_perd_c <- estadisticos_zanahoria_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_zanahoria_perd <- data.frame(
  Estadistico = estadisticos_zanahoria_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_zanahoria <- db4_zanahoria %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_zanahoria <- incidencia_total_zanahoria %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_zanahoria <- incidencia_total_zanahoria %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_zanahoria$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_zanahoria$Factor)
incidencia_total_zanahoria$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_zanahoria$Factor)
incidencia_total_zanahoria$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_zanahoria$Factor)
incidencia_total_zanahoria$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_zanahoria$Factor)
incidencia_total_zanahoria$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_zanahoria$Factor)
incidencia_total_zanahoria$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_zanahoria$Factor)

incidencia_total_zanahoria$Porcentaje <- as.numeric(sub("%", "", incidencia_total_zanahoria$Porcentaje))
incidencia_total_zanahoria$Factor <- factor(incidencia_total_zanahoria$Factor, levels = incidencia_total_zanahoria$Factor)

incidencia_total_zanahoria$Factor <- factor(
  incidencia_total_zanahoria$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_zanahoria_incidencia_graph <- 
  ggplot(incidencia_total_zanahoria, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

# Cantidad ####
# Muestra completa #
q_zanahoria_tyc <- db4_zanahoria %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_zanahoria <- q_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_zanahoria <- q_zanahoria %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_zanahoria_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_zanahoria_tyc$q_median_total
  ))

q_zanahoria <- q_zanahoria %>%
  select(variable, Total, tratamiento, control)

db4_zanahoria_trim_q <- trim_df(db4_zanahoria, "s4_q_std") #Trimmeado para histograma

fig_zanahoria_hist_q_mc <- ggplot(db4_zanahoria_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_zanahoria_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_zanahoria_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_zanahoria_tyc$q_median_total, y = max(db4_zanahoria_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (4.6 [t])", color = colors$Accent) +
  annotate("text", x = q_zanahoria_tyc$q_mean_total, y = max(db4_zanahoria_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (12.2 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_zanahoria_hist_q_t <- ggplot(db4_zanahoria_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_zanahoria$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_zanahoria$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_zanahoria$tratamiento[2], y = max(db4_zanahoria_trim$s4_sup_std) * 6, hjust = -0.1, label = "Mediana (4.6 [t])", color = colors$Accent) +
  annotate("text", x = q_zanahoria$tratamiento[1], y = max(db4_zanahoria_trim$s4_sup_std) * 5, hjust = -0.1, label = "Media (12.2 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 7))

fig_zanahoria_hist_q_c <- ggplot(db4_zanahoria_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_zanahoria$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_zanahoria$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_zanahoria$control[2], y = max(db4_zanahoria_trim$s4_sup_std) * 6, hjust = -0.3, label = "Mediana (2.8 [t])", color = colors$Accent) +
  annotate("text", x = q_zanahoria$control[1], y = max(db4_zanahoria_trim$s4_sup_std) * 5, hjust = -0.1, label = "Media (5.7 [t]])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 7))

estadisticos_zanahoria_q <- estadisticos(db4_zanahoria, s4_q_std, "%", 2)
estadisticos_zanahoria_q_t <- estadisticos(db4_zanahoria_trat, s4_q_std, "%", 2)
estadisticos_zanahoria_q_c <- estadisticos(db4_zanahoria_control, s4_q_std, "%", 2)

valores_q_mc <- estadisticos_zanahoria_q$Valor
valores_q_t <- estadisticos_zanahoria_q_t$Valor
valores_q_c <- estadisticos_zanahoria_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_zanahoria_q <- data.frame(
  Estadistico = estadisticos_zanahoria_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_zanahoria_tyc <- db4_zanahoria %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_zanahoria <- rend_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_zanahoria <- rend_zanahoria %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_zanahoria_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_zanahoria_tyc$rend_median_total
  ))

rend_zanahoria <- rend_zanahoria %>%
  select(variable, Total, tratamiento, control)

db4_zanahoria_trim_rend <- trim_df(db4_zanahoria, "rend") #Trimmeado para histograma

fig_zanahoria_rend_t <- ggplot(db4_zanahoria_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_zanahoria$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_zanahoria$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_zanahoria$tratamiento[2], y = max(db4_zanahoria_trim$s4_sup_std) * 2, hjust = -0.1, label = "Mediana (11.5 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_zanahoria$tratamiento[1], y = max(db4_zanahoria_trim$s4_sup_std) * 1.5, hjust = -0.1, label = "Media (18.95 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 10))

fig_zanahoria_rend_c <- ggplot(db4_zanahoria_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_zanahoria$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_zanahoria$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_zanahoria$control[2], y = max(db4_zanahoria_trim$s4_sup_std) * 2, hjust = -0.1, label = "Mediana (10.35 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_zanahoria$control[1], y = max(db4_zanahoria_trim$s4_sup_std) * 1.5, hjust = -0.1, label = "Media (13.35 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_zanahoria_rend <- estadisticos(db4_zanahoria, rend, "%", 2)
estadisticos_zanahoria_rend_t <- estadisticos(db4_zanahoria_trat, rend, "%", 2)
estadisticos_zanahoria_rend_c <- estadisticos(db4_zanahoria_control, rend, "%", 2)

valores_rend_mc <- estadisticos_zanahoria_rend$Valor
valores_rend_t <- estadisticos_zanahoria_rend_t$Valor
valores_rend_c <- estadisticos_zanahoria_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_zanahoria_rend <- data.frame(
  Estadistico = estadisticos_zanahoria_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo
solo_consumo_t <- db4_zanahoria %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_zanahoria %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_zanahoria <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_zanahoria_tyc <- db4_zanahoria %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_zanahoria <- cons_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de zanahoria [t]",
    variable == "cons_median" ~ "Mediana consumo de zanahoria [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_zanahoria %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_zanahoria %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_zanahoria <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_zanahoria_tyc <- db4_zanahoria %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_zanahoria <- venta_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de zanahoria [t]",
    variable == "venta_median" ~ "Mediana venta de zanahoria [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_zanahoria <- gen_table(db4_zanahoria, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_zanahoria <- ingreso_bruto_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ingreso_neto_zanahoria <- db4_zanahoria %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_zanahoria <- ingreso_neto_zanahoria %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_zanahoria_nprio <- db4 %>% 
  filter(rubro_parent == "Zanahoria") %>% 
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

ingreso_bruto_zanahoria_nprio <- ingreso_bruto_zanahoria_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_zanahoria_nprio <- db4 %>% 
  filter(rubro_parent == "Zanahoria") %>%
  group_by(grupo) %>%
  filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.95, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_zanahoria_nprio <- ingreso_neto_zanahoria_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Zanahoria") %>% 
  group_by(grupo) %>% #Solo de los productores de zanahoria
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Zanahoria") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# ANÁLISIS HABA ####
db4_haba <- db4 %>% 
  filter(s4_3_1 == "Haba") 

db4_haba_trat <- db4 %>% 
  filter(s4_3_1 == "Haba", grupo == "tratamiento")
db4_haba_control <- db4 %>% 
  filter(s4_3_1 == "Haba", grupo == "control")

sup_haba_tyc <- db4_haba %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_haba <- sup_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_haba <- sup_haba %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_haba_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_haba_tyc$sup_median_total
  ))

sup_haba <- sup_haba %>%
  select(variable, Total, tratamiento, control)

db4_haba_trim <- trim_df(db4_haba, "s4_sup_std") #Trimmeado para histograma

fig_sup_haba_mc <- ggplot(db4_haba_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_haba_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_haba_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_haba_tyc$sup_median_total, y = max(db4_haba_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_haba_tyc$sup_mean_total, y = max(db4_haba_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_haba_trim_trat <- db4_haba_trim %>% 
  filter(grupo == "tratamiento")

sup_haba_t <- db4_haba_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_haba_t <-ggplot(db4_haba_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_haba_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_haba_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_haba_t$sup_median_total, y = max(db4_haba_trim$s4_sup_std) * 25, hjust = -0.5, label = "Mediana (0.25 [ha])", color = colors$Accent) +
  annotate("text", x = sup_haba_t$sup_mean_total, y = max(db4_haba_trim$s4_sup_std) * 20, hjust = -0.7, label = "Media (0.22 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_haba_trim_control <- db4_haba_trim %>% 
  filter(grupo == "control")

sup_haba_c <- db4_haba_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_haba_c <-ggplot(db4_haba_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_haba_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_haba_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_haba_c$sup_median_total, y = max(db4_haba_trim$s4_sup_std) * 11.5, hjust = -0.5, label = "Mediana (0.20 [ha])", color = colors$Accent) +
  annotate("text", x = sup_haba_c$sup_mean_total, y = max(db4_haba_trim$s4_sup_std) * 10.5, hjust = -0.5, label = "Media (0.25 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_haba_sup <- estadisticos(db4_haba, s4_sup_std, "[ha]", 3)
estadisticos_haba_sup_t <- estadisticos(db4_haba_trat, s4_sup_std, "[ha]", 3)
estadisticos_haba_sup_c <- estadisticos(db4_haba_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_haba_sup$Valor
valores_t <- estadisticos_haba_sup_t$Valor
valores_c <- estadisticos_haba_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_haba_sup <- data.frame(
  Estadistico = estadisticos_haba_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_haba <- gen_table(db4_haba, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_haba <- gen_table(db4_haba, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_haba <- gen_table(db4_haba, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
cheackperd <- db4_haba %>% 
  filter(perdida > 100) %>% 
  select(`_index`, perdida)


fig_hist_perd_haba_t <-ggplot(db4_haba_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_haba_c <-ggplot(db4_haba_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_haba_perd <- estadisticos(db4_haba, perdida, "%", 0)
estadisticos_haba_perd_t <- estadisticos(db4_haba_trat, perdida, "%", 0)
estadisticos_haba_perd_c <- estadisticos(db4_haba_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_haba_perd$Valor
valores_perd_t <- estadisticos_haba_perd_t$Valor
valores_perd_c <- estadisticos_haba_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_haba_perd <- data.frame(
  Estadistico = estadisticos_haba_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_haba <- db4_haba %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_haba <- incidencia_total_haba %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_haba <- incidencia_total_haba %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_haba$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_haba$Factor)
incidencia_total_haba$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_haba$Factor)
incidencia_total_haba$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_haba$Factor)
incidencia_total_haba$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_haba$Factor)
incidencia_total_haba$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_haba$Factor)
incidencia_total_haba$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_haba$Factor)

incidencia_total_haba$Porcentaje <- as.numeric(sub("%", "", incidencia_total_haba$Porcentaje))
incidencia_total_haba$Factor <- factor(incidencia_total_haba$Factor, levels = incidencia_total_haba$Factor)

incidencia_total_haba$Factor <- factor(
  incidencia_total_haba$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_haba_incidencia_graph <- 
  ggplot(incidencia_total_haba, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

#Cantidad ####
# Muestra completa #
q_haba_tyc <- db4_haba %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_haba <- q_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_haba <- q_haba %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_haba_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_haba_tyc$q_median_total
  ))

q_haba <- q_haba %>%
  select(variable, Total, tratamiento, control)

db4_haba_trim_q <- trim_df(db4_haba, "s4_q_std") #Trimmeado para histograma

fig_haba_hist_q_mc <- ggplot(db4_haba_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_haba_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_haba_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_haba_tyc$q_median_total, y = max(db4_haba_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (0.1 [t])", color = colors$Accent) +
  annotate("text", x = q_haba_tyc$q_mean_total, y = max(db4_haba_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (0.2 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_haba_hist_q_t <- ggplot(db4_haba_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_haba$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_haba$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_haba$tratamiento[2], y = max(db4_haba_trim$s4_sup_std) * 15, hjust = -0.1, label = "Mediana (0.2 [t])", color = colors$Accent) +
  annotate("text", x = q_haba$tratamiento[1], y = max(db4_haba_trim$s4_sup_std) * 13.5, hjust = -0.1, label = "Media (0.3 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

fig_haba_hist_q_c <- ggplot(db4_haba_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_haba$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_haba$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_haba$control[2], y = max(db4_haba_trim$s4_sup_std) * 6, hjust = -0.1, label = "Mediana (184 [t])", color = colors$Accent) +
  annotate("text", x = q_haba$control[1], y = max(db4_haba_trim$s4_sup_std) * 5, hjust = -0.1, label = "Media (299 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

estadisticos_haba_q <- estadisticos(db4_haba, s4_q_std, "%", 2)
estadisticos_haba_q_t <- estadisticos(db4_haba_trat, s4_q_std, "%", 2)
estadisticos_haba_q_c <- estadisticos(db4_haba_control, s4_q_std, "%", 2)

valores_q_mc <- estadisticos_haba_q$Valor
valores_q_t <- estadisticos_haba_q_t$Valor
valores_q_c <- estadisticos_haba_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_haba_q <- data.frame(
  Estadistico = estadisticos_haba_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_haba_tyc <- db4_haba %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_haba <- rend_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_haba <- rend_haba %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_haba_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_haba_tyc$rend_median_total
  ))

rend_haba <- rend_haba %>%
  select(variable, Total, tratamiento, control)

db4_haba_trim_rend <- trim_df(db4_haba, "rend") #Trimmeado para histograma

fig_haba_rend_t <- ggplot(db4_haba_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_haba$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_haba$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_haba$tratamiento[2], y = max(db4_haba_trim$s4_sup_std) * 17, hjust = -0.1, label = "Mediana (828 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_haba$tratamiento[1], y = max(db4_haba_trim$s4_sup_std) * 14, hjust = -0.1, label = "Media (1569 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 10))

fig_haba_rend_c <- ggplot(db4_haba_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_haba$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_haba$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_haba$control[2], y = max(db4_haba_trim$s4_sup_std) * 8, hjust = -0.1, label = "Mediana (828 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_haba$control[1], y = max(db4_haba_trim$s4_sup_std) * 7, hjust = -0.1, label = "Media (1824 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_haba_rend <- estadisticos(db4_haba, rend, "%", 0)
estadisticos_haba_rend_t <- estadisticos(db4_haba_trat, rend, "%", 0)
estadisticos_haba_rend_c <- estadisticos(db4_haba_control, rend, "%", 0)

valores_rend_mc <- estadisticos_haba_rend$Valor
valores_rend_t <- estadisticos_haba_rend_t$Valor
valores_rend_c <- estadisticos_haba_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_haba_rend <- data.frame(
  Estadistico = estadisticos_haba_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_haba %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_haba %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_haba <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_haba_tyc <- db4_haba %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_haba <- cons_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de haba [t]",
    variable == "cons_median" ~ "Mediana consumo de haba [t/ha]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_haba %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_haba %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_haba <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_haba_tyc <- db4_haba %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_haba <- venta_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de haba [t]",
    variable == "venta_median" ~ "Mediana venta de haba [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_haba <- gen_table(db4_haba, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_haba <- ingreso_bruto_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ingreso_neto_haba <- db4_haba %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_haba <- ingreso_neto_haba %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_haba_nprio <- db4 %>% 
  filter(rubro_parent == "Haba") %>% 
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

ingreso_bruto_haba_nprio <- ingreso_bruto_haba_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_haba_nprio <- db4 %>% 
  filter(rubro_parent == "Haba") %>%
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_haba_nprio <- ingreso_neto_haba_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Haba") %>% 
  group_by(grupo) %>% #Solo de los productores de haba
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Haba") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)


# ANÁLISIS Cebolla ####
db4_cebolla <- db4 %>% 
  filter(s4_3_1 == "Cebolla") 

db4_cebolla_trat <- db4 %>% 
  filter(s4_3_1 == "Cebolla", grupo == "tratamiento")
db4_cebolla_control <- db4 %>% 
  filter(s4_3_1 == "Cebolla", grupo == "control")

sup_cebolla_tyc <- db4_cebolla %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_cebolla <- sup_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_cebolla <- sup_cebolla %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_cebolla_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_cebolla_tyc$sup_median_total
  ))

sup_cebolla <- sup_cebolla %>%
  select(variable, Total, tratamiento, control)

db4_cebolla_trim <- trim_df(db4_cebolla, "s4_sup_std") #Trimmeado para histograma

fig_sup_cebolla_mc <- ggplot(db4_cebolla_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_cebolla_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_cebolla_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_cebolla_tyc$sup_median_total, y = max(db4_cebolla_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_cebolla_tyc$sup_mean_total, y = max(db4_cebolla_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_cebolla_trim_trat <- db4_cebolla_trim %>% 
  filter(grupo == "tratamiento")

sup_cebolla_t <- db4_cebolla_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_cebolla_t <-ggplot(db4_cebolla_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_cebolla_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_cebolla_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_cebolla_t$sup_median_total, y = max(db4_cebolla_trim$s4_sup_std) * 12, hjust = -0.1, label = "Mediana (0.5 [ha])", color = colors$Accent) +
  annotate("text", x = sup_cebolla_t$sup_mean_total, y = max(db4_cebolla_trim$s4_sup_std) * 11, hjust = -0, label = "Media (0.88 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_cebolla_trim_control <- db4_cebolla_trim %>% 
  filter(grupo == "control")

sup_cebolla_c <- db4_cebolla_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_cebolla_c <-ggplot(db4_cebolla_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_cebolla_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_cebolla_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_cebolla_c$sup_median_total, y = max(db4_cebolla_trim$s4_sup_std) * 9.5, hjust = -0.3, label = "Mediana (0.13 [ha])", color = colors$Accent) +
  annotate("text", x = sup_cebolla_c$sup_mean_total, y = max(db4_cebolla_trim$s4_sup_std) * 8.5, hjust = -0.3, label = "Media (0.30 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_cebolla_sup <- estadisticos(db4_cebolla, s4_sup_std, "[ha]", 3)
estadisticos_cebolla_sup_t <- estadisticos(db4_cebolla_trat, s4_sup_std, "[ha]", 3)
estadisticos_cebolla_sup_c <- estadisticos(db4_cebolla_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_cebolla_sup$Valor
valores_t <- estadisticos_cebolla_sup_t$Valor
valores_c <- estadisticos_cebolla_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_cebolla_sup <- data.frame(
  Estadistico = estadisticos_cebolla_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_cebolla <- gen_table(db4_cebolla, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_cebolla <- gen_table(db4_cebolla, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_cebolla <- gen_table(db4_cebolla, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
cheackperd <- db4_cebolla %>% 
  filter(perdida > 100) %>% 
  select(`_index`, perdida)


fig_hist_perd_cebolla_t <-ggplot(db4_cebolla_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_cebolla_c <-ggplot(db4_cebolla_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_cebolla_perd <- estadisticos(db4_cebolla, perdida, "%", 0)
estadisticos_cebolla_perd_t <- estadisticos(db4_cebolla_trat, perdida, "%", 0)
estadisticos_cebolla_perd_c <- estadisticos(db4_cebolla_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_cebolla_perd$Valor
valores_perd_t <- estadisticos_cebolla_perd_t$Valor
valores_perd_c <- estadisticos_cebolla_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_cebolla_perd <- data.frame(
  Estadistico = estadisticos_cebolla_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_cebolla <- db4_cebolla %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_cebolla <- incidencia_total_cebolla %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_cebolla <- incidencia_total_cebolla %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_cebolla$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_cebolla$Factor)
incidencia_total_cebolla$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_cebolla$Factor)
incidencia_total_cebolla$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_cebolla$Factor)
incidencia_total_cebolla$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_cebolla$Factor)
incidencia_total_cebolla$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_cebolla$Factor)
incidencia_total_cebolla$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_cebolla$Factor)

incidencia_total_cebolla$Porcentaje <- as.numeric(sub("%", "", incidencia_total_cebolla$Porcentaje))
incidencia_total_cebolla$Factor <- factor(incidencia_total_cebolla$Factor, levels = incidencia_total_cebolla$Factor)

incidencia_total_cebolla$Factor <- factor(
  incidencia_total_cebolla$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_cebolla_incidencia_graph <- 
  ggplot(incidencia_total_cebolla, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

#Cantidad ####
# Muestra completa #
q_cebolla_tyc <- db4_cebolla %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_cebolla <- q_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_cebolla <- q_cebolla %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_cebolla_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_cebolla_tyc$q_median_total
  ))

q_cebolla <- q_cebolla %>%
  select(variable, Total, tratamiento, control)

db4_cebolla_trim_q <- trim_df(db4_cebolla, "s4_q_std") #Trimmeado para histograma

fig_cebolla_hist_q_mc <- ggplot(db4_cebolla_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_cebolla_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_cebolla_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_cebolla_tyc$q_median_total, y = max(db4_cebolla_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (448 [t])", color = colors$Accent) +
  annotate("text", x = q_cebolla_tyc$q_mean_total, y = max(db4_cebolla_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (874 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_cebolla_hist_q_t <- ggplot(db4_cebolla_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_cebolla$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_cebolla$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_cebolla$tratamiento[2], y = max(db4_cebolla_trim$s4_sup_std) * 8, hjust = -0.25, label = "Mediana (4.1 [t])", color = colors$Accent) +
  annotate("text", x = q_cebolla$tratamiento[1], y = max(db4_cebolla_trim$s4_sup_std) * 7, hjust = -0.1, label = "Media (9.5 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

fig_cebolla_hist_q_c <- ggplot(db4_cebolla_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_cebolla$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_cebolla$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_cebolla$control[2], y = max(db4_cebolla_trim$s4_sup_std) * 8, hjust = -0.1, label = "Mediana (0.9 [t])", color = colors$Accent) +
  annotate("text", x = q_cebolla$control[1], y = max(db4_cebolla_trim$s4_sup_std) * 7, hjust = -0.1, label = "Media (3.1 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

estadisticos_cebolla_q <- estadisticos(db4_cebolla, s4_q_std, "%", 0)
estadisticos_cebolla_q_t <- estadisticos(db4_cebolla_trat, s4_q_std, "%", 0)
estadisticos_cebolla_q_c <- estadisticos(db4_cebolla_control, s4_q_std, "%", 0)

valores_q_mc <- estadisticos_cebolla_q$Valor
valores_q_t <- estadisticos_cebolla_q_t$Valor
valores_q_c <- estadisticos_cebolla_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_cebolla_q <- data.frame(
  Estadistico = estadisticos_cebolla_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_cebolla_tyc <- db4_cebolla %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_cebolla <- rend_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_cebolla <- rend_cebolla %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_cebolla_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_cebolla_tyc$rend_median_total
  ))

rend_cebolla <- rend_cebolla %>%
  select(variable, Total, tratamiento, control)

db4_cebolla_trim_rend <- trim_df(db4_cebolla, "rend") #Trimmeado para histograma

fig_cebolla_rend_t <- ggplot(db4_cebolla_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_cebolla$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_cebolla$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_cebolla$tratamiento[2], y = max(db4_cebolla_trim$s4_sup_std) * 17, hjust = -0.1, label = "Mediana (7.95 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_cebolla$tratamiento[1], y = max(db4_cebolla_trim$s4_sup_std) * 14, hjust = -0.1, label = "Media (14.97 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 10))

fig_cebolla_rend_c <- ggplot(db4_cebolla_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_cebolla$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_cebolla$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_cebolla$control[2], y = max(db4_cebolla_trim$s4_sup_std) * 8, hjust = -0.1, label = "Mediana (4.6 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_cebolla$control[1], y = max(db4_cebolla_trim$s4_sup_std) * 7, hjust = -0.1, label = "Media (13.81 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_cebolla_rend <- estadisticos(db4_cebolla, rend, "%", 2)
estadisticos_cebolla_rend_t <- estadisticos(db4_cebolla_trat, rend, "%", 2)
estadisticos_cebolla_rend_c <- estadisticos(db4_cebolla_control, rend, "%", 2)

valores_rend_mc <- estadisticos_cebolla_rend$Valor
valores_rend_t <- estadisticos_cebolla_rend_t$Valor
valores_rend_c <- estadisticos_cebolla_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_cebolla_rend <- data.frame(
  Estadistico = estadisticos_cebolla_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_cebolla %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_cebolla %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_cebolla <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_cebolla_tyc <- db4_cebolla %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_cebolla <- cons_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de cebolla [t]",
    variable == "cons_median" ~ "Mediana consumo de cebolla [t/ha]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_cebolla %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_cebolla %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_cebolla <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_cebolla_tyc <- db4_cebolla %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_cebolla <- venta_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de cebolla [t]",
    variable == "venta_median" ~ "Mediana venta de cebolla [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_cebolla <- gen_table(db4_cebolla, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_cebolla <- ingreso_bruto_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ingreso_neto_cebolla <- db4_cebolla %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_cebolla <- ingreso_neto_cebolla %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# ANÁLISIS DURAZNO ####
db4_durazno <- db4 %>% 
  filter(s4_3_1 == "Durazno") 

db4_durazno_trat <- db4 %>% 
  filter(s4_3_1 == "Durazno", grupo == "tratamiento")
db4_durazno_control <- db4 %>% 
  filter(s4_3_1 == "Durazno", grupo == "control")

sup_durazno_tyc <- db4_durazno %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_durazno <- sup_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_durazno <- sup_durazno %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_durazno_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_durazno_tyc$sup_median_total
  ))

sup_durazno <- sup_durazno %>%
  select(variable, Total, tratamiento, control)

db4_durazno_trim <- trim_df(db4_durazno, "s4_sup_std") #Trimmeado para histograma

fig_sup_durazno_mc <- ggplot(db4_durazno_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_durazno_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_durazno_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_durazno_tyc$sup_median_total, y = max(db4_durazno_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_durazno_tyc$sup_mean_total, y = max(db4_durazno_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_durazno_trim_trat <- db4_durazno_trim %>% 
  filter(grupo == "tratamiento")

sup_durazno_t <- db4_durazno_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_durazno_t <-ggplot(db4_durazno_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_durazno_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_durazno_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_durazno_t$sup_median_total, y = max(db4_durazno_trim$s4_sup_std) * 15, hjust = -0.3, label = "Mediana (0.25 [ha])", color = colors$Accent) +
  annotate("text", x = sup_durazno_t$sup_mean_total, y = max(db4_durazno_trim$s4_sup_std) * 13, hjust = -0.1, label = "Media (0.38 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_durazno_trim_control <- db4_durazno_trim %>% 
  filter(grupo == "control")

sup_durazno_c <- db4_durazno_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_durazno_c <-ggplot(db4_durazno_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_durazno_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_durazno_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_durazno_c$sup_median_total, y = max(db4_durazno_trim$s4_sup_std) * 13.5, hjust = -0.1, label = "Mediana (0.36 [ha])", color = colors$Accent) +
  annotate("text", x = sup_durazno_c$sup_mean_total, y = max(db4_durazno_trim$s4_sup_std) * 12.5, hjust = -0.1, label = "Media (0.50 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_durazno_sup <- estadisticos(db4_durazno, s4_sup_std, "[ha]", 3)
estadisticos_durazno_sup_t <- estadisticos(db4_durazno_trat, s4_sup_std, "[ha]", 3)
estadisticos_durazno_sup_c <- estadisticos(db4_durazno_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_durazno_sup$Valor
valores_t <- estadisticos_durazno_sup_t$Valor
valores_c <- estadisticos_durazno_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_durazno_sup <- data.frame(
  Estadistico = estadisticos_durazno_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_durazno <- gen_table(db4_durazno, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_durazno <- gen_table(db4_durazno, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_durazno <- gen_table(db4_durazno, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
cheackperd <- db4_durazno %>% 
  filter(perdida > 100) %>% 
  select(`_index`, perdida, rend)


fig_hist_perd_durazno_t <-ggplot(db4_durazno_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_durazno_c <-ggplot(db4_durazno_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_durazno_perd <- estadisticos(db4_durazno, perdida, "%", 0)
estadisticos_durazno_perd_t <- estadisticos(db4_durazno_trat, perdida, "%", 0)
estadisticos_durazno_perd_c <- estadisticos(db4_durazno_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_durazno_perd$Valor
valores_perd_t <- estadisticos_durazno_perd_t$Valor
valores_perd_c <- estadisticos_durazno_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_durazno_perd <- data.frame(
  Estadistico = estadisticos_durazno_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_durazno <- db4_durazno %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_durazno <- incidencia_total_durazno %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_durazno <- incidencia_total_durazno %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_durazno$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_durazno$Factor)
incidencia_total_durazno$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_durazno$Factor)
incidencia_total_durazno$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_durazno$Factor)
incidencia_total_durazno$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_durazno$Factor)
incidencia_total_durazno$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_durazno$Factor)
incidencia_total_durazno$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_durazno$Factor)

incidencia_total_durazno$Porcentaje <- as.numeric(sub("%", "", incidencia_total_durazno$Porcentaje))
incidencia_total_durazno$Factor <- factor(incidencia_total_durazno$Factor, levels = incidencia_total_durazno$Factor)

incidencia_total_durazno$Factor <- factor(
  incidencia_total_durazno$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_durazno_incidencia_graph <- 
  ggplot(incidencia_total_durazno, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

#Cantidad ####
# Muestra completa #
q_durazno_tyc <- db4_durazno %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_durazno <- q_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_durazno <- q_durazno %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_durazno_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_durazno_tyc$q_median_total
  ))

q_durazno <- q_durazno %>%
  select(variable, Total, tratamiento, control)

db4_durazno_trim_q <- trim_df(db4_durazno, "s4_q_std") #Trimmeado para histograma

fig_durazno_hist_q_mc <- ggplot(db4_durazno_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_durazno_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_durazno_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_durazno_tyc$q_median_total, y = max(db4_durazno_trim$s4_sup_std) * 20, hjust = -0.15, label = "Mediana (0.4 [t])", color = colors$Accent) +
  annotate("text", x = q_durazno_tyc$q_mean_total, y = max(db4_durazno_trim$s4_sup_std) * 15, hjust = -0.15, label = "Media (0.9 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_durazno_hist_q_t <- ggplot(db4_durazno_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_durazno$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_durazno$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_durazno$tratamiento[2], y = max(db4_durazno_trim$s4_sup_std) * 20, hjust = -0.35, label = "Mediana (0.4 [t])", color = colors$Accent) +
  annotate("text", x = q_durazno$tratamiento[1], y = max(db4_durazno_trim$s4_sup_std) * 15, hjust = -0.25, label = "Media (0.9 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

fig_durazno_hist_q_c <- ggplot(db4_durazno_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_durazno$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_durazno$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_durazno$control[2], y = max(db4_durazno_trim$s4_sup_std) * 20, hjust = -0.1, label = "Mediana (0.4 [t])", color = colors$Accent) +
  annotate("text", x = q_durazno$control[1], y = max(db4_durazno_trim$s4_sup_std) * 18, hjust = -0.1, label = "Media (1.3 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

estadisticos_durazno_q <- estadisticos(db4_durazno, s4_q_std, "%", 2)
estadisticos_durazno_q_t <- estadisticos(db4_durazno_trat, s4_q_std, "%", 2)
estadisticos_durazno_q_c <- estadisticos(db4_durazno_control, s4_q_std, "%", 2)

valores_q_mc <- estadisticos_durazno_q$Valor
valores_q_t <- estadisticos_durazno_q_t$Valor
valores_q_c <- estadisticos_durazno_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_durazno_q <- data.frame(
  Estadistico = estadisticos_durazno_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_durazno_tyc <- db4_durazno %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_durazno <- rend_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_durazno <- rend_durazno %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_durazno_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_durazno_tyc$rend_median_total
  ))

rend_durazno <- rend_durazno %>%
  select(variable, Total, tratamiento, control)

db4_durazno_trim_rend <- trim_df(db4_durazno, "rend") #Trimmeado para histograma

fig_durazno_rend_t <- ggplot(db4_durazno_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_durazno$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_durazno$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_durazno$tratamiento[2], y = max(db4_durazno_trim$s4_sup_std) * 17, hjust = -0.1, label = "Mediana (1.80 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_durazno$tratamiento[1], y = max(db4_durazno_trim$s4_sup_std) * 14, hjust = -0.1, label = "Media (4.31 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 17))

fig_durazno_rend_c <- ggplot(db4_durazno_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_durazno$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_durazno$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_durazno$control[2], y = max(db4_durazno_trim$s4_sup_std) * 8, hjust = -0.1, label = "Mediana (0.92 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_durazno$control[1], y = max(db4_durazno_trim$s4_sup_std) * 7, hjust = -0.1, label = "Media (2.82 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_durazno_rend <- estadisticos(db4_durazno, rend, "%", 2)
estadisticos_durazno_rend_t <- estadisticos(db4_durazno_trat, rend, "%", 2)
estadisticos_durazno_rend_c <- estadisticos(db4_durazno_control, rend, "%", 2)

valores_rend_mc <- estadisticos_durazno_rend$Valor
valores_rend_t <- estadisticos_durazno_rend_t$Valor
valores_rend_c <- estadisticos_durazno_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_durazno_rend <- data.frame(
  Estadistico = estadisticos_durazno_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)

# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_durazno %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_durazno %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_durazno <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_durazno_tyc <- db4_durazno %>%
  filter(s4_q_cons_std < quantile(db4_durazno$s4_q_cons_std, 0.95, na.rm = TRUE)) %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_durazno <- db4_durazno %>%
  filter(s4_q_cons_std < quantile(db4_durazno$s4_q_cons_std, 0.95, na.rm = TRUE)) %>%
  group_by(grupo) %>%  
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_durazno <- cons_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de durazno [t]",
    variable == "cons_median" ~ "Mediana consumo de durazno [t/ha]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_durazno %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_durazno %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_durazno <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_durazno_tyc <- db4_durazno %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_durazno <- venta_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de durazno [t]",
    variable == "venta_median" ~ "Mediana venta de durazno [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_durazno <- gen_table(db4_durazno, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_durazno <- ingreso_bruto_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ingreso_neto_durazno <- db4_durazno %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_durazno <- ingreso_neto_durazno %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_durazno_nprio <- db4 %>% 
  filter(rubro_parent == "Durazno") %>% 
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

ingreso_bruto_durazno_nprio <- ingreso_bruto_durazno_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_durazno_nprio <- db4 %>% 
  filter(rubro_parent == "Durazno") %>%
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_durazno_nprio <- ingreso_neto_durazno_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Durazno") %>% 
  group_by(grupo) %>% #Solo de los productores de durazno
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Durazno") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# ANÁLISIS MANZANA ####
db4_manzana <- db4 %>% 
  filter(s4_3_1 == "Manzana") 

db4_manzana_trat <- db4 %>% 
  filter(s4_3_1 == "Manzana", grupo == "tratamiento")
db4_manzana_control <- db4 %>% 
  filter(s4_3_1 == "Manzana", grupo == "control")

sup_manzana_tyc <- db4_manzana %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

sup_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(sup_mean = round(mean(s4_sup_std, na.rm = TRUE),2),
            sup_median = round(median(s4_sup_std, na.rm = TRUE),2))

sup_manzana <- sup_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "sup_mean" ~ "Media Superficie [ha]",
    variable == "sup_median" ~ "Mediana Superficie [ha]"
  ))

sup_manzana <- sup_manzana %>%
  mutate(Total = case_when(
    variable == "Media Superficie [ha]" ~ sup_manzana_tyc$sup_mean_total,
    variable == "Mediana Superficie [ha]" ~ sup_manzana_tyc$sup_median_total
  ))

sup_manzana <- sup_manzana %>%
  select(variable, Total, tratamiento, control)

db4_manzana_trim <- trim_df(db4_manzana, "s4_sup_std") #Trimmeado para histograma

fig_sup_manzana_mc <- ggplot(db4_manzana_trim, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_manzana_tyc$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_manzana_tyc$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_manzana_tyc$sup_median_total, y = max(db4_manzana_trim$s4_sup_std) * 75, hjust = -0.1, label = "Mediana (XXXX [ha])", color = colors$Accent) +
  annotate("text", x = sup_manzana_tyc$sup_mean_total, y = max(db4_manzana_trim$s4_sup_std) * 70, hjust = -0.1, label = "Media (XXXX [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Tratamiento

db4_manzana_trim_trat <- db4_manzana_trim %>% 
  filter(grupo == "tratamiento")

sup_manzana_t <- db4_manzana_trat %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_manzana_t <-ggplot(db4_manzana_trim_trat, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$Complementary) +
  geom_vline(xintercept = sup_manzana_t$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_manzana_t$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_manzana_t$sup_median_total, y = max(db4_manzana_trim$s4_sup_std) * 28, hjust = -0.1, label = "Mediana (0.18 [ha])", color = colors$Accent) +
  annotate("text", x = sup_manzana_t$sup_mean_total, y = max(db4_manzana_trim$s4_sup_std) * 25, hjust = -0.1, label = "Media (0.06 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Superficie muestra completa (riego y secano) Control

db4_manzana_trim_control <- db4_manzana_trim %>% 
  filter(grupo == "control")

sup_manzana_c <- db4_manzana_control %>%
  summarise(
    sup_mean_total = round(mean(s4_sup_std, na.rm = TRUE), 2),
    sup_median_total = round(median(s4_sup_std, na.rm = TRUE), 2)
  )

fig_sup_manzana_c <-ggplot(db4_manzana_trim_control, aes(x = s4_sup_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = sup_manzana_c$sup_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = sup_manzana_c$sup_mean_total , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = sup_manzana_c$sup_median_total, y = max(db4_manzana_trim$s4_sup_std) * 7.5, hjust = -0.1, label = "Mediana (0.18 [ha])", color = colors$Accent) +
  annotate("text", x = sup_manzana_c$sup_mean_total, y = max(db4_manzana_trim$s4_sup_std) * 6.8, hjust = -0.1, label = "Media (0.27 [ha]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Superfice [ha]", y = "Frecuencia")

# Estadísticos muestra completa
estadisticos_manzana_sup <- estadisticos(db4_manzana, s4_sup_std, "[ha]", 3)
estadisticos_manzana_sup_t <- estadisticos(db4_manzana_trat, s4_sup_std, "[ha]", 3)
estadisticos_manzana_sup_c <- estadisticos(db4_manzana_control, s4_sup_std, "[ha]", 3)

valores_mc <- estadisticos_manzana_sup$Valor
valores_t <- estadisticos_manzana_sup_t$Valor
valores_c <- estadisticos_manzana_sup_c$Valor

# Combinar las columnas en un nuevo data frame
stats_manzana_sup <- data.frame(
  Estadistico = estadisticos_manzana_sup$Estadistico, # Asumiendo todos tienen el mismo orden de estadísticos
  `Muestra Completa` = valores_mc,
  Tratamiento = valores_t,
  Control = valores_c
)

# Riego ####
acceso_riego_manzana <- gen_table(db4_manzana, "s4n4", "Tenencia de sistemas de riego")
tipo_riego_manzana <- gen_table(db4_manzana, "s4n5", "Tenencia de sistemas de riego")
tipo_otro_riego_manzana <- gen_table(db4_manzana, "s4n5_e", "Tenencia de sistemas de riego")

# Pérdidas ####
cheackperd <- db4_manzana %>% 
  filter(perdida > 100) %>% 
  select(`_index`, perdida, rend)


fig_hist_perd_manzana_t <-ggplot(db4_manzana_trat, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$Base, color = colors$Complementary) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

fig_hist_perd_manzana_c <-ggplot(db4_manzana_control, aes(x = perdida_10)) +
  geom_histogram(bins = 10, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Pérdidas reportadas (%)", y = "Frecuencia")

estadisticos_manzana_perd <- estadisticos(db4_manzana, perdida, "%", 0)
estadisticos_manzana_perd_t <- estadisticos(db4_manzana_trat, perdida, "%", 0)
estadisticos_manzana_perd_c <- estadisticos(db4_manzana_control, perdida, "%", 0)

valores_perd_mc <- estadisticos_manzana_perd$Valor
valores_perd_t <- estadisticos_manzana_perd_t$Valor
valores_perd_c <- estadisticos_manzana_perd_c$Valor

# Combinar las columnas en un nuevo data frame
stats_manzana_perd <- data.frame(
  Estadistico = estadisticos_manzana_perd$Estadistico, 
  `Muestra Completa` = valores_perd_mc,
  Tratamiento = valores_perd_t,
  Control = valores_perd_c
)

incidencia_total_manzana <- db4_manzana %>%
  summarise(across(.cols = names(.)[grepl("^perdida_", names(.)) & !grepl("perdida_10", names(.))], 
                   ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Total_Incidencia") %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100)

incidencia_total_manzana <- incidencia_total_manzana %>%
  mutate(Porcentaje = (Total_Incidencia / sum(Total_Incidencia, na.rm = TRUE)) * 100) 

incidencia_total_manzana <- incidencia_total_manzana %>%
  mutate(Porcentaje = paste0(round(Porcentaje, 1), "%")) %>% 
  select(Factor, Porcentaje)

# Cambiar los nombres de los factores a un formato más descriptivo
incidencia_total_manzana$Factor <- sub("perdida_seq", "Perdida por sequía", incidencia_total_manzana$Factor)
incidencia_total_manzana$Factor <- sub("perdida_hel", "Perdida por helada", incidencia_total_manzana$Factor)
incidencia_total_manzana$Factor <- sub("perdida_gran", "Perdida por granizo", incidencia_total_manzana$Factor)
incidencia_total_manzana$Factor <- sub("perdida_ria", "Perdida por riada", incidencia_total_manzana$Factor)
incidencia_total_manzana$Factor <- sub("perdida_des", "Perdida por deslizamiento", incidencia_total_manzana$Factor)
incidencia_total_manzana$Factor <- sub("perdida_plaga", "Perdida por plaga", incidencia_total_manzana$Factor)

incidencia_total_manzana$Porcentaje <- as.numeric(sub("%", "", incidencia_total_manzana$Porcentaje))
incidencia_total_manzana$Factor <- factor(incidencia_total_manzana$Factor, levels = incidencia_total_manzana$Factor)

incidencia_total_manzana$Factor <- factor(
  incidencia_total_manzana$Factor, 
  levels = c(
    "Perdida por deslizamiento",
    "Perdida por riada",
    "Perdida por granizo",
    "Perdida por plaga",
    "Perdida por helada",
    "Perdida por sequía"
  )
)

perdidas_manzana_incidencia_graph <- 
  ggplot(incidencia_total_manzana, aes(x = Factor, y = Porcentaje, fill = Factor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 1)) +
  scale_fill_brewer(palette = "OrRd") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)),
            position = position_stack(vjust = 0.5),
            size = 3, hjust = 0) # hjust ajustado para mejor posicionamiento

#Cantidad ####
# Muestra completa #
q_manzana_tyc <- db4_manzana %>%
  summarise(
    q_mean_total = round(mean(s4_q_std, na.rm = TRUE), 1),
    q_median_total = round(median(s4_q_std, na.rm = TRUE), 1)
  )

q_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(q_mean = round(mean(s4_q_std, na.rm = TRUE),1),
            q_median = round(median(s4_q_std, na.rm = TRUE),1))

q_manzana <- q_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "q_mean" ~ "Media cantidad [t]",
    variable == "q_median" ~ "Mediana cantidad [t]"
  ))

q_manzana <- q_manzana %>%
  mutate(Total = case_when(
    variable == "Media cantidad [t]" ~ q_manzana_tyc$q_mean_total,
    variable == "Mediana cantidad [t]" ~ q_manzana_tyc$q_median_total
  ))

q_manzana <- q_manzana %>%
  select(variable, Total, tratamiento, control)

db4_manzana_trim_q <- trim_df(db4_manzana, "s4_q_std") #Trimmeado para histograma

fig_manzana_hist_q_mc <- ggplot(db4_manzana_trim_q, aes(x = s4_q_std)) +
  geom_histogram(bins = 30, fill = colors$Complementary, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_manzana_tyc$q_median_total, linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_manzana_tyc$q_mean_total, linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_manzana_tyc$q_median_total, y = max(db4_manzana_trim$s4_sup_std) * 55, hjust = -0.1, label = "Mediana (440 [t])", color = colors$Accent) +
  annotate("text", x = q_manzana_tyc$q_mean_total, y = max(db4_manzana_trim$s4_sup_std) * 50, hjust = -0.1, label = "Media (887 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia")

fig_manzana_hist_q_t <- ggplot(db4_manzana_trim_q %>% filter(grupo == "tratamiento"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_manzana$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_manzana$tratamiento[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_manzana$tratamiento[2], y = max(db4_manzana_trim$s4_sup_std) * 33, hjust = -0.1, label = "Mediana (0.2 [t])", color = colors$Accent) +
  annotate("text", x = q_manzana$tratamiento[1], y = max(db4_manzana_trim$s4_sup_std) * 31, hjust = -0.1, label = "Media (0.5 [t]", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

fig_manzana_hist_q_c <- ggplot(db4_manzana_trim_q %>% filter(grupo == "control"), aes(x = s4_q_std)) +
  geom_histogram(bins = 15, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = q_manzana$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = q_manzana$control[1], linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = q_manzana$control[2], y = max(db4_manzana_trim$s4_sup_std) * 20, hjust = -0.1, label = "Mediana (0.3 [t])", color = colors$Accent) +
  annotate("text", x = q_manzana$control[1], y = max(db4_manzana_trim$s4_sup_std) * 18.5, hjust = -0.1, label = "Media (1 [t])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Cantidad [t]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 15000, length.out = 4))

estadisticos_manzana_q <- estadisticos(db4_manzana, s4_q_std, "%", 2)
estadisticos_manzana_q_t <- estadisticos(db4_manzana_trat, s4_q_std, "%", 2)
estadisticos_manzana_q_c <- estadisticos(db4_manzana_control, s4_q_std, "%", 2)

valores_q_mc <- estadisticos_manzana_q$Valor
valores_q_t <- estadisticos_manzana_q_t$Valor
valores_q_c <- estadisticos_manzana_q_c$Valor

# Combinar las columnas en un nuevo data frame
stats_manzana_q <- data.frame(
  Estadistico = estadisticos_manzana_q$Estadistico, 
  `Muestra Completa` = valores_q_mc,
  Tratamiento = valores_q_t,
  Control = valores_q_c
)

# Rendimiento ####
rend_manzana_tyc <- db4_manzana %>%
  summarise(
    rend_mean_total = round(mean(rend, na.rm = TRUE), 2),
    rend_median_total = round(median(rend, na.rm = TRUE), 2)
  )

rend_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(rend_mean = round(mean(rend, na.rm = TRUE),2),
            rend_median = round(median(rend, na.rm = TRUE),2))

rend_manzana <- rend_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "rend_mean" ~ "Media Rendimiento [t/ha]",
    variable == "rend_median" ~ "Mediana Rendimiento [t/ha]"
  ))

rend_manzana <- rend_manzana %>%
  mutate(Total = case_when(
    variable == "Media Rendimiento [t/ha]" ~ rend_manzana_tyc$rend_mean_total,
    variable == "Mediana Rendimiento [t/ha]" ~ rend_manzana_tyc$rend_median_total
  ))

rend_manzana <- rend_manzana %>%
  select(variable, Total, tratamiento, control)

db4_manzana_trim_rend <- trim_df(db4_manzana, "rend") #Trimmeado para histograma

fig_manzana_rend_t <- ggplot(db4_manzana_trim_rend %>% filter(grupo == "tratamiento"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$Base, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_manzana$tratamiento[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_manzana$tratamiento[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_manzana$tratamiento[2], y = max(db4_manzana_trim$s4_sup_std) * 14, hjust = -0.1, label = "Mediana (3.30 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_manzana$tratamiento[1], y = max(db4_manzana_trim$s4_sup_std) * 12.5, hjust = -0.1, label = "Media (8.59 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 50000, length.out = 17))

fig_manzana_rend_c <- ggplot(db4_manzana_trim_rend %>% filter(grupo == "control"), aes(x = rend)) +
  geom_histogram(bins = 30, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = rend_manzana$control[2], linetype = "dashed", color = colors$Accent) +
  geom_vline(xintercept = rend_manzana$control[1] , linetype = "dashed", color = colors$LighterNeutral) +
  annotate("text", x = rend_manzana$control[2], y = max(db4_manzana_trim$s4_sup_std) * 10, hjust = -0.1, label = "Mediana (2.30 [t/ha])", color = colors$Accent) +
  annotate("text", x = rend_manzana$control[1], y = max(db4_manzana_trim$s4_sup_std) * 11.3, hjust = -0.1, label = "Media (4.85 [t/ha])", color = colors$DarkerNeutral) +
  theme_minimal() +
  labs(x = "Rendimiento [t/ha]", y = "Frecuencia") #+
#scale_x_continuous(breaks = seq(0, 25000, length.out = 9))

estadisticos_manzana_rend <- estadisticos(db4_manzana, rend, "%", 2)
estadisticos_manzana_rend_t <- estadisticos(db4_manzana_trat, rend, "%", 2)
estadisticos_manzana_rend_c <- estadisticos(db4_manzana_control, rend, "%", 2)

valores_rend_mc <- estadisticos_manzana_rend$Valor
valores_rend_t <- estadisticos_manzana_rend_t$Valor
valores_rend_c <- estadisticos_manzana_rend_c$Valor

# Combinar las columnas en un nuevo data frame
stats_manzana_rend <- data.frame(
  Estadistico = estadisticos_manzana_rend$Estadistico, 
  `Muestra Completa` = valores_rend_mc,
  Tratamiento = valores_rend_t,
  Control = valores_rend_c
)


# Destino de la producción ####
# Consumo

solo_consumo_t <- db4_manzana %>%
  filter(grupo == "tratamiento") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_c <- db4_manzana %>%
  filter(grupo == "control") %>% 
  mutate(consumo_igual_produccion = s4_q_std <= s4_q_cons_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Consumo_Igual_Produccion = sum(consumo_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Consumo_Igual_Produccion / Total) * 100)

solo_consumo_t$Grupo <- "Tratamiento"
solo_consumo_c$Grupo <- "Control"

solo_consumo_total_manzana <- bind_rows(solo_consumo_t, solo_consumo_c) %>% 
  select(Grupo, Porcentaje) %>%
  mutate(Porcentaje = sprintf("%.1f%%", Porcentaje))

# cálculo estadisticos consumo
consumo_manzana_tyc <- db4_manzana %>%
  summarise(
    cons_mean_total = round(mean(s4_q_cons_std, na.rm = TRUE), 2),
    cons_median_total = round(median(s4_q_cons_std, na.rm = TRUE), 2)
  )

cons_manzana <- db4_manzana %>%
  group_by(grupo) %>% 
  filter(s4_q_cons_std < quantile(db4_manzana$s4_q_cons_std, 0.95, na.rm = TRUE)) %>%
  summarise(cons_mean = round(mean(s4_q_cons_std, na.rm = TRUE),2),
            cons_median = round(median(s4_q_cons_std, na.rm = TRUE),2))

cons_manzana <- cons_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "cons_mean" ~ "Media consumo de manzana [t]",
    variable == "cons_median" ~ "Mediana consumo de manzana [t/ha]"
  )) %>% 
  select(variable, tratamiento, control)

# Ventas
solo_venta_t <- db4_manzana %>%
  filter(grupo == "tratamiento") %>% 
  mutate(venta_igual_produccion = s4_q_std <= s4_q_venta_std) %>%
  summarise(
    Total = n(), # Número total de observaciones
    Venta_Igual_Produccion = sum(venta_igual_produccion, na.rm = TRUE)
  ) %>%
  mutate(Porcentaje = (Venta_Igual_Produccion / Total) * 100)


solo_venta_t <- db4_manzana %>%
  filter(grupo == "tratamiento", !is.na(s4_q_std), s4_q_std > 0, !is.na(s4_q_venta_std)) %>%
  mutate(
    Proporcion_Venta = s4_q_venta_std / s4_q_std,
    Categoria_Venta = cut(
      Proporcion_Venta,
      breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.90, 1),
      labels = c("0% a 10%", "11% a 25%", "26% a 50%", "51% a 75%", "76% a 90%", "91% a 100%"),
      right = FALSE
    )
  ) %>%
  group_by(Categoria_Venta) %>%
  summarise(
    Cantidad = n(),
  ) %>%
  ungroup() %>%
  # Calcular el porcentaje sobre el total de observaciones válidas
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE) * 100)

# Grafico
solo_venta_t_filtrado <- solo_venta_t %>%
  filter(!is.na(Categoria_Venta))

solo_venta_t_filtrado$Categoria_Venta <- factor(solo_venta_t_filtrado$Categoria_Venta, 
                                                levels = c("90% a 100%",  "76% a 90%", "51% a 75%", "26% a 50%", "11% a 25%", "0% a 10%"))

solo_venta_t_filtrado <- solo_venta_t_filtrado %>% 
  filter(!is.na(Categoria_Venta))

comport_venta_manzana <-
  ggplot(solo_venta_t_filtrado, aes(x = Categoria_Venta, y = Porcentaje, fill = Categoria_Venta)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "Categoría de Venta", y = "Porcentaje") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = sprintf("%0.1f%%", Porcentaje)), position = position_stack(vjust = 0.5), size = 4, hjust = -1.5)

# cálculo estadisticos venta
venta_manzana_tyc <- db4_manzana %>%
  summarise(
    venta_mean_total = round(mean(s4_q_venta_std, na.rm = TRUE), 2),
    venta_median_total = round(median(s4_q_venta_std, na.rm = TRUE), 2)
  )

venta_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(venta_mean = round(mean(s4_q_venta_std, na.rm = TRUE),2),
            venta_median = round(median(s4_q_venta_std, na.rm = TRUE),2))

venta_manzana <- venta_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "venta_mean" ~ "Media venta de manzana [t]",
    variable == "venta_median" ~ "Mediana venta de manzana [t]"
  )) %>% 
  select(variable, tratamiento, control)

# Comercialización

comercial_manzana <- gen_table(db4_manzana, "s4n9_l", "Comercialización")

# Ingresos ####
# INGRESO BRUTO
ingreso_bruto_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(ingreso_bruto_mean = round(mean(ing_bruto_ap, na.rm = TRUE),2),
            ingreso_bruto_median = round(median(ing_bruto_ap, na.rm = TRUE),2))

ingreso_bruto_manzana <- ingreso_bruto_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO
ingreso_neto_manzana <- db4_manzana %>%
  group_by(grupo) %>%  
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2),
            ingreso_neto_median = round(median(ing_neto_ap, na.rm = TRUE),2))

ingreso_neto_manzana <- ingreso_neto_manzana %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median" ~ "Mediana del ingreso [BS]"
  )) %>% 
  select(variable, tratamiento, control)

#INGRESO BRUTO NO PRIORIZADO
ingreso_bruto_manzana_nprio <- db4 %>% 
  filter(rubro_parent == "Manzana") %>% 
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_bruto_mean_nprio = round(mean(ing_bruto_nprio, na.rm = TRUE),2),
            ingreso_bruto_median_nprio = round(median(ing_bruto_nprio, na.rm = TRUE),2))

# adwawd <-ingreso_bruto_manzana_nprio %>% 
#   select(`_index`, ing_bruto_nprio)

ingreso_bruto_manzana_nprio <- ingreso_bruto_manzana_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_bruto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_bruto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO RUBROS NO PRIORIZADOS
ingreso_neto_manzana_nprio <- db4 %>% 
  filter(rubro_parent == "Manzana") %>%
  group_by(grupo) %>%
  #filter(ing_bruto_nprio < quantile(db4$ing_bruto_nprio, 0.99, na.rm = TRUE)) %>% 
  #filter(ing_bruto_nprio > quantile(db4$ing_bruto_nprio, 0.01, na.rm = TRUE)) %>%  
  summarise(ingreso_neto_mean_nprio = round(mean(ing_neto_nprio, na.rm = TRUE),2),
            ingreso_neto_median_nprio = round(median(ing_neto_nprio, na.rm = TRUE),2))

ingreso_neto_manzana_nprio <- ingreso_neto_manzana_nprio %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_nprio" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_nprio" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# INGRESO NETO DE ACTIVIDADES NO AGROPECUARIAS
ing_neto_no_agropecuario <- db %>%
  filter(rubro == "Manzana") %>% 
  group_by(grupo) %>% #Solo de los productores de manzana
  #filter(s4_4 < quantile(db$s4_4, 0.99, na.rm = TRUE)) %>% 
  filter(s4_4 > quantile(db$s4_4, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_noagropecuario = round(mean(s4_4, na.rm = TRUE),2),
            ingreso_neto_median_noagropecuario = round(median(s4_4, na.rm = TRUE),2))

ing_neto_no_agropecuario <- ing_neto_no_agropecuario %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_noagropecuario" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_noagropecuario" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# OTROS INGRESOS (NETO) #
ing_neto_otros <- db %>%
  filter(rubro == "Manzana") %>% 
  group_by(grupo) %>%
  #filter(ing_otros < quantile(db$ing_otros, 0.99, na.rm = TRUE)) %>% 
  filter(ing_otros > quantile(db$ing_otros, 0.01, na.rm = TRUE)) %>%
  summarise(ingreso_neto_mean_otros = round(mean(ing_otros, na.rm = TRUE),2),
            ingreso_neto_median_otros = round(median(ing_otros, na.rm = TRUE),2))

ing_neto_otros <- ing_neto_otros %>%
  pivot_longer(-grupo, names_to = "variable", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  mutate(variable = case_when(
    variable == "ingreso_neto_mean_otros" ~ "Ingreso medio [Bs]",
    variable == "ingreso_neto_median_otros" ~ "Mediana del ingreso [Bs]"
  )) %>% 
  select(variable, tratamiento, control)

# RESUMEN FACTORES PRODUCTIVOS ####

resumen_sup_tratamiento <- data.frame(
  "Variable/Rubro" = c("Media Superficie [ha]", "Mediana Superficie [ha]"),
  Papa = c(sup_papa$tratamiento[1], sup_papa$tratamiento[2]),
  Maiz = c(sup_maiz$tratamiento[1], sup_maiz$tratamiento[2]),
  Tomate = c(sup_tomate$tratamiento[1], sup_tomate$tratamiento[2]),
  Zanahoria = c(sup_zanahoria$tratamiento[1], sup_zanahoria$tratamiento[2]),
  Haba = c(sup_haba$tratamiento[1], sup_haba$tratamiento[2]),
  Cebolla = c(sup_cebolla$tratamiento[1], sup_cebolla$tratamiento[2]),
  Durazno = c(sup_durazno$tratamiento[1], sup_durazno$tratamiento[2]),
  Manzana = c(sup_manzana$tratamiento[1], sup_manzana$tratamiento[2])
)

resumen_sup_control <- data.frame(
  "Variable/Rubro" = c("Media Superficie [ha]", "Mediana Superficie [ha]"),
  Papa = c(sup_papa$control[1], sup_papa$control[2]),
  Maiz = c(sup_maiz$control[1], sup_maiz$control[2]),
  Tomate = c(sup_tomate$control[1], sup_tomate$control[2]),
  Zanahoria = c(sup_zanahoria$control[1], sup_zanahoria$control[2]),
  Haba = c(sup_haba$control[1], sup_haba$control[2]),
  Cebolla = c(sup_cebolla$control[1], sup_cebolla$control[2]),
  Durazno = c(sup_durazno$control[1], sup_durazno$control[2]),
  Manzana = c(sup_manzana$control[1], sup_manzana$control[2])
)

resumen_riego_tratamiento <- data.frame(
  "Variable/Rubro" = c("Tiene acceso a riego", "No tiene acceso a riego"),
  Papa = c(acceso_riego_papa$`Tratamiento %`[1], acceso_riego_papa$`Tratamiento %`[2]),
  Maiz = c(acceso_riego_maiz$`Tratamiento %`[1], acceso_riego_maiz$`Tratamiento %`[2]),
  Tomate = c(acceso_riego_tomate$`Tratamiento %`[1], acceso_riego_tomate$`Tratamiento %`[2]),
  Zanahoria = c(acceso_riego_zanahoria$`Tratamiento %`[1], acceso_riego_zanahoria$`Tratamiento %`[2]),
  Haba = c(acceso_riego_haba$`Tratamiento %`[1], acceso_riego_haba$`Tratamiento %`[2]),
  Cebolla = c(acceso_riego_cebolla$`Tratamiento %`[1], acceso_riego_cebolla$`Tratamiento %`[2]),
  Durazno = c(acceso_riego_durazno$`Tratamiento %`[1], acceso_riego_durazno$`Tratamiento %`[2]),
  Manzana = c(acceso_riego_manzana$`Tratamiento %`[1], acceso_riego_manzana$`Tratamiento %`[2])
)

resumen_riego_control <- data.frame(
  "Variable/Rubro" = c("Tiene acceso a riego", "No tiene acceso a riego"),
  Papa = c(acceso_riego_papa$`Control %`[1], acceso_riego_papa$`Control %`[2]),
  Maiz = c(acceso_riego_maiz$`Control %`[1], acceso_riego_maiz$`Control %`[2]),
  Tomate = c(acceso_riego_tomate$`Control %`[1], acceso_riego_tomate$`Control %`[2]),
  Zanahoria = c(acceso_riego_zanahoria$`Control %`[1], acceso_riego_zanahoria$`Control %`[2]),
  Haba = c(acceso_riego_haba$`Control %`[1], acceso_riego_haba$`Control %`[2]),
  Cebolla = c(acceso_riego_cebolla$`Control %`[1], acceso_riego_cebolla$`Control %`[2]),
  Durazno = c(acceso_riego_durazno$`Control %`[1], acceso_riego_durazno$`Control %`[2]),
  Manzana = c(acceso_riego_manzana$`Control %`[1], acceso_riego_manzana$`Control %`[2])
)

resumen_rend_tratamiento <- data.frame(
  "Variable/Rubro" = c("Media Rendimiento [t/ha]", "Mediana Rendimiento [t/ha]"),
  Papa = c(rend_papa$tratamiento[1], rend_papa$tratamiento[2]),
  Maiz = c(rend_maiz$tratamiento[1], rend_maiz$tratamiento[2]),
  Tomate = c(rend_tomate$tratamiento[1], rend_tomate$tratamiento[2]),
  Zanahoria = c(rend_zanahoria$tratamiento[1], rend_zanahoria$tratamiento[2]),
  Haba = c(rend_haba$tratamiento[1], rend_haba$tratamiento[2]),
  Cebolla = c(rend_cebolla$tratamiento[1], rend_cebolla$tratamiento[2]),
  Durazno = c(rend_durazno$tratamiento[1], rend_durazno$tratamiento[2]),
  Manzana = c(rend_manzana$tratamiento[1], rend_manzana$tratamiento[2])
)

resumen_rend_control <- data.frame(
  "Variable/Rubro" = c("Media Rendimiento [t/ha]", "Mediana Rendimiento [t/ha]"),
  Papa = c(rend_papa$control[1], rend_papa$control[2]),
  Maiz = c(rend_maiz$control[1], rend_maiz$control[2]),
  Tomate = c(rend_tomate$control[1], rend_tomate$control[2]),
  Zanahoria = c(rend_zanahoria$control[1], rend_zanahoria$control[2]),
  Haba = c(rend_haba$control[1], rend_haba$control[2]),
  Cebolla = c(rend_cebolla$control[1], rend_cebolla$control[2]),
  Durazno = c(rend_durazno$control[1], rend_durazno$control[2]),
  Manzana = c(rend_manzana$control[1], rend_manzana$control[2])
)

resumen_rend_control <- data.frame(
  "Variable/Rubro" = c("Papa", "Maiz", "Tomate", "Zanahoria", "Haba", "Cebolla", "Durazno", "Manzana"),
  'Media Rendimiento' = c(rend_papa$control[1], rend_maiz$control[1], rend_tomate$control[1],
                          rend_zanahoria$control[1], rend_haba$control[1], rend_cebolla$control[1],
                          rend_durazno$control[1], rend_manzana$control[1]),
  `Mediana Rendimiento` = c(rend_papa$control[2], rend_maiz$control[2], rend_tomate$control[2],
                            rend_zanahoria$control[2], rend_haba$control[2], rend_cebolla$control[2],
                            rend_durazno$control[2],rend_manzana$control[2])
)


# 5. PECUARIO  ####

# 6. PRÁCTICAS Y TECNOLOGÍAS SOSTENIBLES  ####
# Sequía ##### 
evento_sequía <- gen_table(db, "s6_1_1", "Eventos por sequía")

afectacion_sequia <- gen_table(db, "s6_1_2", "Eventos por sequía")
afectacion_sequia <- afectacion_sequia %>%
  pivot_longer(cols = c("Tratamiento %", "Control %"), names_to = "Grupo", values_to = "Porcentaje") %>%
  mutate(Porcentaje = as.numeric(gsub("%", "", Porcentaje))) %>%
  mutate(Rango = cut(`Eventos por sequía`, breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")))

afectacion_sequia_agrupada <- afectacion_sequia %>%
  group_by(Grupo, Rango) %>%
  summarise(Porcentaje = sum(Porcentaje, na.rm = TRUE))

afectacion_sequia_wide <- afectacion_sequia_agrupada %>%
  pivot_wider(names_from = Grupo, values_from = Porcentaje)

seq_uso_cosecha_agua <- gen_table(db, "s6_1_3a", "Uso de tecnología para cosecha de agua o sistemas de distribución")
seq_utilidad_cosecha_agua <- gen_table(db, "s6_1_3b", "Utilidad de tecnología para cosecha de agua o sistemas de distribución")
seq_continuidad_cosecha_agua <- gen_table(db, "s6_1_3c", "Continuidad en el uso de tecnología para cosecha de agua o sistemas de distribución")

seq_uso_microriego <- gen_table(db, "s6_1_4a", "Uso de tecnología para microriego")
seq_utilidad_microriego <- gen_table(db, "s6_1_4b", "Utilidad de tecnología para microriego")
seq_continuidad_microriego <- gen_table(db, "s6_1_4c", "Continuidad en el uso de tecnología para microriego")

seq_uso_operacion <- gen_table(db, "s6_1_5a", "Recibío capacitaciones en operación y mantenimiento de la infraestructura")
seq_utilidad_operacion <- gen_table(db, "s6_1_5b", "Utilidad de capacitaciones en operación y mantenimiento de la infraestructura")
seq_continuidad_operacion <- gen_table(db, "s6_1_5c", "Adopción de capacitaciones en operación y mantenimiento de la infraestructura")

# Para Índice de resiliencia
db <- db %>%
  mutate(
    puntos_sequia_1 = case_when(
      s6_1_3a == "No" ~ 0,
      s6_1_3a == "Si" & s6_1_3b == "No" & s6_1_3c == "No" ~ 1,
      s6_1_3a == "Si" & (s6_1_3b == "Si" | s6_1_3c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_sequia_2 = case_when(
      s6_1_4a == "No" ~ 0,
      s6_1_4a == "Si" & s6_1_4b == "No" & s6_1_4c == "No" ~ 1,
      s6_1_4a == "Si" & (s6_1_4b == "Si" | s6_1_4c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>%
  mutate(
    puntos_sequia_3 = case_when(
      s6_1_5a == "No" ~ 0,
      s6_1_5a == "Si" & s6_1_5b == "No" & s6_1_5c == "No" ~ 1,
      s6_1_5a == "Si" & (s6_1_5b == "Si" | s6_1_5c == "Si") ~ 2,
      TRUE ~ NA_real_))

# Sequía por municipio
sequia_mun <- db %>% 
  group_by(grupo, s1_2) %>% 
  summarise(mean(s6_1_2, na.rm = TRUE))

# Helada #####
evento_helada <- gen_table(db, "s6_2_1", "Eventos por helada")

afectacion_helada <- gen_table(db, "s6_2_2", "Eventos por helada")
afectacion_helada <- afectacion_helada %>%
  pivot_longer(cols = c("Tratamiento %", "Control %"), names_to = "Grupo", values_to = "Porcentaje") %>%
  mutate(Porcentaje = as.numeric(gsub("%", "", Porcentaje))) %>%
  mutate(Rango = cut(`Eventos por helada`, breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")))

afectacion_helada_agrupada <- afectacion_helada %>%
  group_by(Grupo, Rango) %>%
  summarise(Porcentaje = sum(Porcentaje, na.rm = TRUE))

afectacion_helada_wide <- afectacion_helada_agrupada %>%
  pivot_wider(names_from = Grupo, values_from = Porcentaje)

hel_uso_microriego <- gen_table(db, "s6_2_3a", "Uso de tecnología para microriego")
hel_utilidad_microriego <- gen_table(db, "s6_2_3b", "Utilidad de tecnología para microriego")
hel_continuidad_microriego <- gen_table(db, "s6_2_3c", "Continuidad en el uso de tecnología para microriego")

hel_uso_operacion <- gen_table(db, "s6_2_4a", "Recibío capacitaciones en operación y mantenimiento de la infraestructura")
hel_utilidad_operacion <- gen_table(db, "s6_2_4b", "Utilidad de capacitaciones en operación y mantenimiento de la infraestructura")
hel_continuidad_operacion <- gen_table(db, "s6_2_4c", "Adopción de capacitaciones en operación y mantenimiento de la infraestructura")

# Para Índice de resiliencia
db <- db %>%
  mutate(
    puntos_helada_1 = case_when(
      s6_2_3a == "No" ~ 0,
      s6_2_3a == "Si" & s6_2_3b == "No" & s6_2_3c == "No" ~ 1,
      s6_2_3a == "Si" & (s6_2_3b == "Si" | s6_2_3c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_helada_2 = case_when(
      s6_2_4a == "No" ~ 0,
      s6_2_4a == "Si" & s6_2_4b == "No" & s6_2_4c == "No" ~ 1,
      s6_2_4a == "Si" & (s6_2_4b == "Si" | s6_2_4c == "Si") ~ 2,
      TRUE ~ NA_real_)) 

# Helada por municipio
helada_mun <- db %>% 
  group_by(grupo, s1_2) %>% 
  summarise(mean(s6_2_2, na.rm = TRUE))

# Granizo ####
evento_granizo <- gen_table(db, "s6_3_1", "Eventos por granizo")

afectacion_granizo <- gen_table(db, "s6_3_2", "Eventos por granizo")
afectacion_granizo <- afectacion_granizo %>%
  pivot_longer(cols = c("Tratamiento %", "Control %"), names_to = "Grupo", values_to = "Porcentaje") %>%
  mutate(Porcentaje = as.numeric(gsub("%", "", Porcentaje))) %>%
  mutate(Rango = cut(`Eventos por granizo`, breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")))

afectacion_granizo_agrupada <- afectacion_granizo %>%
  group_by(Grupo, Rango) %>%
  summarise(Porcentaje = sum(Porcentaje, na.rm = TRUE))

afectacion_granizo_wide <- afectacion_granizo_agrupada %>%
  pivot_wider(names_from = Grupo, values_from = Porcentaje)

gran_uso_antigran <- gen_table(db, "s6_3_3a", "Uso de mallas antigranizo u otras formas de protección")
gran_utilidad_antigran <- gen_table(db, "s6_3_3b", "Utilidad de mallas antigranizo u otras formas de protección")
gran_continuidad_antigran <- gen_table(db, "s6_3_3c", "Continuidad en el uso de mallas antigranizo u otras formas de protección")

# Para Índice de resiliencia
db <- db %>%
  mutate(
    puntos_granizo_1 = case_when(
      s6_3_3a == "No" ~ 0,
      s6_3_3a == "Si" & s6_3_3b == "No" & s6_3_3c == "No" ~ 1,
      s6_3_3a == "Si" & (s6_3_3b == "Si" | s6_3_3c == "Si") ~ 2,
      TRUE ~ NA_real_))

# Granizo por municipio
granizo_mun <- db %>% 
  group_by(grupo, s1_2) %>% 
  summarise(mean(s6_3_2, na.rm = TRUE))

# Riada/Inundación ####
evento_inundacion <- gen_table(db, "s6_4_1", "Eventos por inundacion")

afectacion_inundacion <- gen_table(db, "s6_4_2", "Eventos por inundacion")
afectacion_inundacion <- afectacion_inundacion %>%
  pivot_longer(cols = c("Tratamiento %", "Control %"), names_to = "Grupo", values_to = "Porcentaje") %>%
  mutate(Porcentaje = as.numeric(gsub("%", "", Porcentaje))) %>%
  mutate(Rango = cut(`Eventos por inundacion`, breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")))

afectacion_inundacion_agrupada <- afectacion_inundacion %>%
  group_by(Grupo, Rango) %>%
  summarise(Porcentaje = sum(Porcentaje, na.rm = TRUE))

afectacion_inundacion_wide <- afectacion_inundacion_agrupada %>%
  pivot_wider(names_from = Grupo, values_from = Porcentaje)

# Riada/Inundación por municipio
inundacion_mun <- db %>% 
  group_by(grupo, s1_2) %>% 
  summarise(mean(s6_4_2, na.rm = TRUE))

# Deslizamiento ####
evento_desliz <- gen_table(db, "s6_5_1", "Eventos por desliz")

afectacion_desliz <- gen_table(db, "s6_5_2", "Eventos por desliz")
afectacion_desliz <- afectacion_desliz %>%
  pivot_longer(cols = c("Tratamiento %", "Control %"), names_to = "Grupo", values_to = "Porcentaje") %>%
  mutate(Porcentaje = as.numeric(gsub("%", "", Porcentaje))) %>%
  mutate(Rango = cut(`Eventos por desliz`, breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0-20%", "21-40%", "41-60%", "61-80%", "81-100%")))

afectacion_desliz_agrupada <- afectacion_desliz %>%
  group_by(Grupo, Rango) %>%
  summarise(Porcentaje = sum(Porcentaje, na.rm = TRUE))

afectacion_desliz_wide <- afectacion_desliz_agrupada %>%
  pivot_wider(names_from = Grupo, values_from = Porcentaje)

desliz_uso_gabiones <- gen_table(db, "s6_5_3a", "Uso de gaviones o terrazas de formación lenta")
desliz_utilidad_gabiones <- gen_table(db, "s6_5_3b", "Utilidad de gaviones o terrazas de formación lenta")
desliz_continuidad_gabiones <- gen_table(db, "s6_5_3c", "Continuidad en el uso degaviones o terrazas de formación lenta")

# Para Índice de resiliencia
db <- db %>%
  mutate(
    puntos_desliz_1 = case_when(
      s6_5_3a == "No" ~ 0,
      s6_5_3a == "Si" & s6_5_3b == "No" & s6_5_3c == "No" ~ 1,
      s6_5_3a == "Si" & (s6_5_3b == "Si" | s6_5_3c == "Si") ~ 2,
      TRUE ~ NA_real_))

# Deslizamiento por municipio
deslizamiento_mun <- db %>% 
  group_by(grupo, s1_2) %>% 
  summarise(mean(s6_5_2, na.rm = TRUE))

# Generales
gen_uso_variedades <- gen_table(db, "s6_5_5a", "Recibíó capacitaciones en introducción de variedades precoces y rotación de cultivos")
gen_utilidad_variedades <- gen_table(db, "s6_5_5b", "Utilidad de capacitaciones en introducción de variedades precoces y rotación de cultivos")
gen_continuidad_variedades <- gen_table(db, "s6_5_5c", "Adopción de capacitaciones en introducción de variedades precoces y rotación de cultivos")

gen_uso_acopio <- gen_table(db, "s6_5_4a", "Uso de silos, ambientes de acopio ventilados y capacitación")
gen_utilidad_acopio <- gen_table(db, "s6_5_4b", "Utilidad de silos, ambientes de acopio ventilados y capacitación")
gen_continuidad_acopio <- gen_table(db, "s6_5_4c", "Continuidad en el uso de silos, ambientes de acopio ventilados y capacitación")

gen_uso_satagro <- gen_table(db, "s6_5_6a", "Participación en talleres sobre manejo de sistemas de pronóstico del tiempo")
gen_utilidad_satagro <- gen_table(db, "s6_5_6b", "Utilidad de talleres sobre manejo de sistemas de pronóstico del tiempo")
gen_continuidad_satagro <- gen_table(db, "s6_5_6c", "Adopción de técnicas expuestas en talleres sobre manejo de sistemas de pronóstico del tiempo")

gen_uso_seguro <- gen_table(db, "s6_5_7a", "Actualmente cuenta con un seguro agrícola")
gen_utilidad_seguro <- gen_table(db, "s6_5_7b", "Utilidad de seguros agrícolas sean utiles o eficaces para la gestión de riesgos climáticos")
gen_continuidad_seguro <- gen_table(db, "s6_5_7c", "Adopción continua de un seguro agrícola")

gen_recibe_cuencas <- gen_table(db, "s6_6_1a", "Ha recibido capacitación en manejo de cuencas o integral del agua o gestión del agua")
gen_usa_cuencas <- gen_table(db, "s6_6_1b", "Ha aplicado prácticas en manejo de cuencas o integral del agua o gestión del agua")
gen_utilidad_cuencas <- gen_table(db, "s6_6_1c", "Utilidad de prácticas en manejo de cuencas o integral del agua o gestión del agua")
gen_continuidad_cuencas <- gen_table(db, "s6_6_1d", "Uso continuo de prácticas en manejo de cuencas o integral del agua o gestión del agua")

gen_recibe_fuentesagua <- gen_table(db, "s6_6_2a", "Ha recibido capacitación en medidas para proteger y conservar las fuentes de agua")
gen_usa_fuentesagua <- gen_table(db, "s6_6_2b", "Ha aplicado prácticas en medidas para proteger y conservar las fuentes de agua")
gen_utilidad_fuentesagua <- gen_table(db, "s6_6_2c", "Utilidad de prácticas en medidas para proteger y conservar las fuentes de agua")
gen_continuidad_fuentesagua <- gen_table(db, "s6_6_2d", "Uso continuo de prácticas en medidas para proteger y conservar las fuentes de agua")

gen_recibe_empaques <- gen_table(db, "s4_17_1", "Cuenta con capacitación en Mejoramiento de empaques para comercialización")
gen_recibe_precios <- gen_table(db, "s4_17_2", "Cuenta con capacitación en Monitoreo y difusión de precios de productos agrícolas")

# Puntuación resiliencia General
db <- db %>%
  mutate(s6_5_4a = replace_na(s6_5_4a, "No"))

db <- db %>%
  mutate(
    puntos_gen_1 = case_when(
      s6_5_5a == "No" ~ 0,
      s6_5_5a == "Si" & s6_5_5b == "No" & s6_5_5c == "No" ~ 1,
      s6_5_5a == "Si" & (s6_5_5b == "Si" | s6_5_5c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_gen_2 = case_when(
      s6_5_4a == "No" ~ 0,
      s6_5_4a == "Si" & s6_5_4b == "No" & s6_5_4c == "No" ~ 1,
      s6_5_4a == "Si" & (s6_5_4b == "Si" | s6_5_4c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>%
  mutate(
    puntos_gen_3 = case_when(
      s6_5_6a == "No" ~ 0,
      s6_5_6a == "Si" & s6_5_6b == "No" & s6_5_6c == "No" ~ 1,
      s6_5_6a == "Si" & (s6_5_6b == "Si" | s6_5_6c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>%
  mutate(
    puntos_gen_4 = case_when(
      s6_5_7a == "No" ~ 0,
      s6_5_7a == "Si" & s6_5_7b == "No" & s6_5_7c == "No" ~ 1,
      s6_5_7a == "Si" & (s6_5_7b == "Si" | s6_5_7c == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_gen_5 = case_when(
      s6_6_1a == "No" ~ 0,
      s6_6_1a == "Si" & s6_6_1b == "No" & s6_6_1c == "No" & s6_6_1d == "No" ~ 1,
      s6_6_1a == "Si" & (s6_6_1b == "Si" | s6_6_1c == "Si" | s6_6_1d == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_gen_6 = case_when(
      s6_6_2a == "No" ~ 0,
      s6_6_2a == "Si" & s6_6_2b == "No" & s6_6_2c == "No" & s6_6_2d == "No" ~ 1,
      s6_6_2a == "Si" & (s6_6_2b == "Si" | s6_6_2c == "Si" | s6_6_2d == "Si") ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_gen_7 = case_when(
      s4_17_1 == 0 ~ 0,
      s4_17_1 == 1 ~ 2,
      TRUE ~ NA_real_)) %>% 
  mutate(
    puntos_gen_8 = case_when(
      s4_17_2 == 0 ~ 0,
      s4_17_2 == 1 ~ 2,
      TRUE ~ NA_real_))

# USO DE PLAGUICIDAS

gen_usa_plagas <- gen_table(db, "s4_18", "as")
plag1<-sum(db$s4_18_1, na.rm = TRUE)
plag2<-sum(db$s4_18_2, na.rm = TRUE)
plag3<-sum(db$s4_18_3, na.rm = TRUE)
plag4<-sum(db$s4_18_4, na.rm = TRUE)
plag5<-sum(db$s4_18_5, na.rm = TRUE)
plag6<-sum(db$s4_18_6, na.rm = TRUE)
plag7<-sum(db$s4_18_7, na.rm = TRUE)


datos_plagas <- tibble(
  Manejo = c("Aplicación de bioinsumos", "Trampas de insectos", "Rotación de cultivos", "Utilización de cultivos tolerantes a plagas", "Utilización de bio-indicadores", "Uso de agroquímicos", "Prácticas ancestrales"),
  Importancia = c(plag1, plag2, plag3, plag4, plag5, plag6, plag7)
)

total_plagas <- sum(datos_plagas$Importancia)
datos_plagas$Porcentaje <- (datos_plagas$Importancia / total_plagas) * 100

# Gráfico de barras con ggplot2
manejo_plagas <- ggplot(datos_plagas, aes(x = Manejo, y = Importancia, fill = Manejo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.8), size = 3) +
  theme_minimal() +
  labs(x = "Tipo de Manejo",
       y = "Cuenta") +
  coord_flip() +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.position = "none")

afectacion_plagas <- gen_table(db, "s4_19", "Percepción de afectación de plagas")

# 7. PARTICIPACIÓN Y EMPODERAMIENTO ####
# Participación
Organizaciones <- gen_table(db, "s7_4", "A que organizacion pertenece")
Otras_organizaciones<- gen_table(db, "s7_4e", "Otras organizaciones")

Miembro_Organización<- gen_table(db, "s7_5", "Miembro de la Organización")
Incidencia_miembro<- gen_table(db, "s7_6", "Incidencia del miembro en la organización")
Incidencia_Asociación<- gen_table(db, "s7_7", "Incidencia de la Asociación")

# Empoderamiento
empoderamiento <- gen_table(db, "s7_1", "Capacitaciones sobre empoderamiento")
se_aplico_lo_aprendido <- gen_table(db, "s7_1a", "Se aplico lo aprendido")
satisfechos_con_estos_talleres <- gen_table(db, "s7_1b", "Sastisfecho con los talleres")
educacion_financiera <- gen_table(db, "s7_2", "Capacitaciones sobre educación financiera")
aprendido_educ_fin <- gen_table(db, "s7_2a", "Se aplico lo aprendido de educación fianciera")
satisfechos_con_educ_fin <- gen_table(db, "s7_2b", "Sastisfecho con los talleres de educacion financiera")

# Para índice de resiliencia
db <- db %>%
  mutate(
    puntos_gen_9 = case_when(
      s7_2 == 0 ~ 0,
      s7_2 == 1 & s7_2a == 0 & s7_2b == 0 ~ 1,
      s7_2 == 1 & (s7_2a == 1 | s7_2b == 1) ~ 2,
      TRUE ~ NA_real_))

#Organizaciones
organizaciones <- gen_table(db, "s7_4", "A que organizacion pertenece")
miembro_organización<- gen_table(db, "s7_5", "Miembro de la Organización")
incidencia_miembro<- gen_table(db, "s7_6", "Incidencia del miembro en la organización")
incidencia_asociación<- gen_table(db, "s7_7", "Incidencia de la Asociación")

# Para índice de resiliencia
db <- db %>%
  mutate(
    puntos_gen_10 = case_when(
      s7_4 == "Ninguna" ~ 0,
      s7_4 != "Ninguna" ~ 2,
      TRUE ~ NA_real_))

# 9. EMPRESAS RURALES###
empresa_rural <- gen_table(db, "s9_1", "Dueño de empresa rural")

# 11. ÍNDICE DE RESILIENCIA ####
db <- db %>%
  mutate(
    suma_sequia = rowSums(select(., starts_with("puntos_sequia_")), na.rm = TRUE),
    suma_helada = rowSums(select(., starts_with("puntos_helada_")), na.rm = TRUE),
    suma_granizo = rowSums(select(., starts_with("puntos_granizo_")), na.rm = TRUE),
    suma_desliz = rowSums(select(., starts_with("puntos_desliz_")), na.rm = TRUE),
    suma_gen = rowSums(select(., starts_with("puntos_gen_")), na.rm = TRUE),
    total_suma = suma_sequia + suma_helada + suma_gen,
    conteo_no_NA = apply(select(., starts_with("puntos_sequia_"), 
                                starts_with("puntos_helada_"), starts_with("puntos_granizo_"),
                                starts_with("puntos_desliz_"), starts_with("puntos_gen_")), 1, function(x) sum(!is.na(x))) * 2
  )

db <- db %>%
  mutate(resilience_index = ifelse(conteo_no_NA == 0, NA, total_suma / conteo_no_NA))

db_resilience_muestra <- db %>% 
  summarise(mean_indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2))

media_resiliencia <- mean(db$resilience_index, na.rm = TRUE)

db_resilience <- db %>%
  group_by(grupo) %>%  
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2), .groups = 'drop')


fig_resilience <- ggplot(db, aes(x = resilience_index)) +
  geom_histogram(binwidth = 0.05, fill = colors$LighterNeutral, color = colors$DarkerNeutral) +
  geom_vline(xintercept = media_resiliencia, linetype = "dashed", color = colors$Accent) +
  annotate("text", x = media_resiliencia, y = Inf, label = "Media = 0.09", hjust = -0.1, vjust = 2, color = colors$Accent) +
  theme_minimal() +
  labs(x = "Índice de resiliencia", y = "Frecuencia")

db_resilience <- db %>%
  group_by(rubro, grupo) %>%  
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2), .groups = 'drop')

db_resilience_wide <- db_resilience %>%
  pivot_wider(names_from = grupo, values_from = indice_resiliencia) %>% 
  select(rubro, tratamiento, control)

db_resilience_wide <- db_resilience_wide %>%
  add_row(rubro = "ACCESOS RURAL", tratamiento = 0.13, control = 0.08)




#Resiliencia por municipio (indicativo!)
db_resilience_mun_t <- db %>%
  filter(grupo == "tratamiento") %>% 
  group_by(s1_2) %>%  
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2), .groups = 'drop')

db_resilience_mun_c <- db %>%
  filter(grupo == "control") %>% 
  group_by(s1_2) %>%  
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2), .groups = 'drop')

# 12. INGRESOS TOTALES E ÍNDICE DE POBREZA MONETARIA ####
resultados_suma <- aggregate(cbind(ing_neto_ap, ing_neto_nprio) ~ `_parent_index`, data = db4, FUN = function(x) {
  if(all(is.na(x))) {
    return(NA)
  } else {
    return(sum(x, na.rm = TRUE))
  }
}, na.action = NULL)
resultados_max <- aggregate(s4_4 ~ `_parent_index`, data = db4, FUN = function(x) {
  # Excluimos -Inf y luego verificamos si quedan valores para evaluar
  valores_validos <- x[x != -Inf]
  
  if (length(valores_validos) == 0 || all(is.na(valores_validos))) {
    # Si no quedan valores válidos (todos eran NA o -Inf), devolvemos NA
    return(NA)
  } else {
    # De lo contrario, devolvemos el máximo de los valores restantes
    return(max(valores_validos, na.rm = TRUE))
  }
}, na.action = NULL)

ingreso_total <- merge(resultados_suma, resultados_max, by = "_parent_index")

ingreso_total <- ingreso_total %>%
  left_join(db %>% select("_index", ing_otros), by = c("_parent_index" = "_index"))

ingreso_total$ing_total = rowSums(ingreso_total[, c("ing_neto_ap", "ing_neto_nprio", "s4_4", "ing_otros")], na.rm = TRUE)

# Ingreso TOTAL EN DB ####
db <- db %>% 
  left_join(ingreso_total %>% select("_parent_index", ing_total), by = c("_index" = "_parent_index"))
  
db_ingreso_total <- db %>%
  group_by(grupo) %>%  
  summarise(ingreso_total = round(mean(ing_total, na.rm = TRUE),2), .groups = 'drop')

db_ingreso_total_rubro <- db %>%
  group_by(rubro, grupo) %>%  
  summarise(ingreso_total = round(mean(ing_total, na.rm = TRUE),2), .groups = 'drop')

db_ingreso_total_rubro_wide <- db_ingreso_total_rubro %>%
  pivot_wider(names_from = grupo, values_from = ingreso_total) %>% 
  select(rubro, tratamiento, control)

db_ingreso_total_rubro_wide <- db_ingreso_total_rubro_wide %>%
  add_row(rubro = "ACCESOS RURAL", tratamiento = 10316, control = 8271)

# Índice de pobreza simple ####
personas_por_hogar <- db2 %>%
  filter(s2_6 >= 14) %>%
  group_by(`_parent_index`) %>%
  summarise(num_personas = n())

personas_por_hogar <- personas_por_hogar %>%
  left_join(db %>% select(`_index`, s0_num_ninos), by = c("_parent_index" = "_index"))

personas_por_hogar <- personas_por_hogar %>%
  mutate(total_personas = rowSums(select(., num_personas, s0_num_ninos), na.rm = TRUE))

#mean(personas_por_hogar$total_personas)

db <- db %>%
  left_join(personas_por_hogar %>% select(`_parent_index`, total_personas), by = c("_index" = "_parent_index"))

db <- db %>%
  mutate(ing_per_capita = ifelse(is.na(ing_total) | is.na(total_personas), NA, ing_total / total_personas))

#mean(db$ing_per_capita, na.rm = TRUE)
#median(db$ing_per_capita, na.rm = TRUE)

linea_pobreza_moderada_anual <- 8017.2
linea_pobreza_extrema_anual <- 4572

db <- db %>%
  mutate(
    por_debajo_pobreza_moderada = ifelse(ing_per_capita < linea_pobreza_moderada_anual, 1, 0),
    por_debajo_pobreza_extrema = ifelse(ing_per_capita < linea_pobreza_extrema_anual, 1, 0)
  )

#mean(db$por_debajo_pobreza_moderada, na.rm = TRUE)
#mean(db$por_debajo_pobreza_extrema, na.rm = TRUE)

#table(db$s4_4, useNA = "always")
checlk_otros_na <- db %>% 
  filter(is.na(s4_4)) %>% 
  select(s4_4, `_index`, ing_per_capita, por_debajo_pobreza_moderada,  por_debajo_pobreza_extrema)

#table(checlk_otros_na$por_debajo_pobreza_moderada)
#table(checlk_otros_na$por_debajo_pobreza_extrema)

chekccostos <- db4_maiz %>% 
  select(s4_sup_std, rend, ing_bruto_ap, costos_fam, ing_neto_ap, perdida)


# 13. INGRESOS ACTIVIDAD PRODUCTIVA
ingreso_neto_muestracompleta_a <- db4 %>%
  group_by(grupo) %>% 
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE),2))

valor_tratamiento <- filter(ingreso_neto_muestracompleta_a, grupo == "tratamiento")$ingreso_neto_mean
valor_control <- filter(ingreso_neto_muestracompleta_a, grupo == "control")$ingreso_neto_mean

ingreso_neto_muestracompleta <- db4 %>%
  group_by(grupo, s4_3_1) %>% 
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE), 2), .groups = "drop") %>%
  filter(!is.na(ingreso_neto_mean))

ingreso_neto_muestracompleta <- ingreso_neto_muestracompleta %>%
  pivot_wider(names_from = grupo, values_from = ingreso_neto_mean)

ingreso_neto_muestracompleta <- ingreso_neto_muestracompleta %>%
  add_row(s4_3_1 = "ACCESOS RURAL", tratamiento = valor_tratamiento, 
          control = valor_control) %>% 
  select(s4_3_1, tratamiento, control)

# 14.1 CANTIDAD PrOMEDIO PRODUCIDA
cantidad_promedio <- db4 %>%
  filter(s4_q_std < quantile(db4$s4_q_std, 0.99, na.rm = TRUE)) %>% 
  summarise(q_prom_general = mean(s4_q_std, na.rm = TRUE))

# 14.2 Hectáreas con tecnología

cols_uso_tecnologia <- c("s6_1_3a", "s6_1_4a", "s6_2_3a", "s6_3_3a", "s6_5_3a", "s6_5_4a", "s6_5_7a")

db <- db %>%
  mutate(across(all_of(cols_uso_tecnologia), ~ ifelse(. == "Si", 1, ifelse(. == "No", 0, NA))))

db <- db %>%
  rowwise() %>%
  mutate(total_tecnologias_usadas = sum(c_across(all_of(cols_uso_tecnologia)), na.rm = TRUE))

productores_con_tecnologia <- db %>%
  ungroup() %>%
  group_by(grupo) %>% 
  summarise(
    al_menos_una = sum(total_tecnologias_usadas >= 1, na.rm = TRUE) / n() * 100,
    al_menos_dos = sum(total_tecnologias_usadas >= 2, na.rm = TRUE) / n() * 100,
    al_menos_tres = sum(total_tecnologias_usadas >= 3, na.rm = TRUE) / n() * 100
  )

db4 <- db4 %>%
  left_join(db %>% select(`_index`, total_tecnologias_usadas), by = c("_parent_index" = "_index"))

total_hectareas <- sum(db4$s4_sup_std, na.rm = TRUE)

total_hectareas_con_tecnologia <- db4 %>%
  filter(total_tecnologias_usadas >= 3) %>%
  summarise(total = sum(s4_sup_std, na.rm = TRUE)) %>%
  pull(total)

porcentaje_hectareas_con_tecnologia <- (total_hectareas_con_tecnologia / total_hectareas) * 100

personas_con_tecnologia_por_municipio <- db %>%
  filter(grupo == "tratamiento") %>% 
  filter(total_tecnologias_usadas >= 3) %>%
  group_by(s1_2) %>%
  summarise(num_personas = n())

personas_con_tecnologia_por_comunidad <- db %>%
  filter(grupo == "tratamiento") %>% 
  filter(total_tecnologias_usadas >= 3) %>%
  group_by(s1_3) %>%
  summarise(num_personas = n())

# Personas con capacitación
cols_uso_capacitacion <- c("s6_2_4a", "s6_1_5a", "s6_5_5a", "s6_5_4a", "s6_5_6a", "s6_6_1a", "s6_6_2a")

db <- db %>%
  mutate(across(all_of(cols_uso_capacitacion), ~ ifelse(. == "Si", 1, ifelse(. == "No", 0, NA))))

db <- db %>%
  rowwise() %>%
  mutate(total_capacitacion_usadas = sum(c_across(all_of(cols_uso_capacitacion)), na.rm = TRUE))

productores_con_capacitacion <- db %>%
  ungroup() %>%
  group_by(grupo) %>% 
  summarise(
    al_menos_una = sum(total_capacitacion_usadas >= 1, na.rm = TRUE) / n() * 100,
    al_menos_dos = sum(total_capacitacion_usadas >= 2, na.rm = TRUE) / n() * 100,
    al_menos_tres = sum(total_capacitacion_usadas >= 3, na.rm = TRUE) / n() * 100
  )

db2_unique <- db2 %>%
  group_by(`_parent_index`) %>%
  slice(1) %>%
  ungroup()

db <- db %>%
  left_join(db2_unique %>% select(`_parent_index`, s2_4, s2_6), by = c("_index" = "_parent_index"))

productores_con_capacitacion <- db %>%
  filter(grupo == "tratamiento") %>% 
  ungroup() %>%
  group_by(s2_4.y) %>% 
  summarise(
    al_menos_una = sum(total_capacitacion_usadas >= 1, na.rm = TRUE) / n() * 100,
    al_menos_dos = sum(total_capacitacion_usadas >= 2, na.rm = TRUE) / n() * 100,
    al_menos_tres = sum(total_capacitacion_usadas >= 3, na.rm = TRUE) / n() * 100
  )

# Jóvenes con capacitación
# productores_jovenes_con_capacitacion <- db %>%
#   filter(s2_6 >= 16, s2_6 <= 28) %>% # Filtrar por rango de edad
#   ungroup() %>%
#   summarise(
#     total_jovenes = n(), # Contar el total de jóvenes en el rango de edad
#     con_al_menos_una_capacitacion = sum(total_capacitacion_usadas >= 3, na.rm = TRUE) # Contar los que tienen al menos una capacitación
#   ) %>%
#   mutate(
#     porcentaje_con_capacitacion = con_al_menos_una_capacitacion / total_jovenes * 100 # Calcular el porcentaje
#   )
# 

# 15. Principales resultados por Zona agroproductiva ####
# Definición de zonas agroproductivas
# valles_cerrados <- 
# altiplano_norte <-
# altiplano_centro <-
# valles_norte <-
# valles_centro <-
# valles_sur <-
# chaco_serrano <-


# Superficies cultivadas #

  
# Riesgos

saveRDS(db, "data_out/db.rds")
saveRDS(db2, "data_out/db2.rds")
saveRDS(db4, "data_out/db4.rds")

write.csv(db, "data_out/db.csv", row.names = FALSE)
write.csv(db2, "data_out/db2.csv", row.names = FALSE)
write.csv(db4, "data_out/db4.csv", row.names = FALSE)

