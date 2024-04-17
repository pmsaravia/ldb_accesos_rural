# Línea de base Programa ACCESOS RURAL
# Ministerio de Desarrollo Rural y Tierras - FIDA

# Solicitudes adicionales ####
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
db <- readRDS("data_out/db.rds")
db2 <- readRDS("data_out/db2.rds")
db4 <- readRDS("data_out/db4.rds")

# Rendimiento por municipio ####
# GRUPO TRATAMIENTO #
codigos_mun <- read.csv("data_in/1_db_raw/mun_code.csv")

rubros_rendimiento_t <- db4 %>%
  filter(s4_2_5 == 1) %>%
  filter(grupo == "tratamiento") %>%
  filter(s4_3_1 != "Quinua") %>%
  group_by(s1_2, s4_3_1) %>%
  summarise(superficie_total = sum(s4_sup_std, na.rm = TRUE), # Definir importancia
            superficie_preomedio = mean(s4_sup_std, na.rm = TRUE),
            rend_promedio = mean(rend, na.rm = TRUE),
            ingreso_bruto_mean_total = round(mean(ing_bruto_ap, na.rm = TRUE), 2),
            ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
            perdida_media = round(mean(perdida, na.rm = TRUE), 2)
            ) %>%
  arrange(s1_2, desc(superficie_total))

rubros_endb_t <- db %>%
  filter(grupo == "tratamiento") %>%
  group_by(s1_2) %>%
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE), 2))


# Control por municipios
rubros_rendimiento_c <- db4 %>%
  filter(s4_2_5 == 1) %>%
  filter(grupo == "control") %>%
  group_by(s1_2, s4_3_1) %>%
  summarise(superficie_total = sum(s4_sup_std, na.rm = TRUE), # Definir importancia
            superficie_preomedio = mean(s4_sup_std, na.rm = TRUE),
            rend_promedio = mean(rend, na.rm = TRUE),
            ingreso_bruto_mean_total = round(mean(ing_bruto_ap, na.rm = TRUE), 2),
            ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
            perdida_media = round(mean(perdida, na.rm = TRUE), 2)
  ) %>%
  arrange(s1_2, desc(superficie_total))

rubros_endb_c <- db %>%
  filter(grupo == "control") %>%
  group_by(s1_2) %>%
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE), 2))

# Agregando otros municipios

nuevas_observaciones <- tibble(
  s1_2 = c("MONTEAGUDO", "CARABUCO", "ANCORAIMES", "PALCA", "SICA SICA", 
           "INQUISIVI", "TOTORA", "MIZQUE", "VILLA CHARCAS", "INCAHUASI", "CARAPARI"),
  s4_3_1 = c("Maiz", "Papa", "Papa", "Papa", "Papa", 
             "Durazno", "Papa", "Cebolla", "Papa", "Papa", "Maiz"),
  superficie_total = c(1.11, 0.64, 0.72, 0.69, 0.86, 
                       0.59, 2.02, 1.80, 0.50, 0.43, 1.13),
  superficie_preomedio = c(0.51, 2.37, 2.24, 0.47, 0.44, 
                           0.33, 11.55, 9.10, 0.64, 1.53, 0.93),
  rend_promedio = c(795.12, 289.43, 709.43, 756.09, 1094.09, 
                    891.03, 14251.00, 11200.00, 1045.09, 722.75, 1991.49),
  ingreso_bruto_mean_total = c(-7019.73, -7351.40, -6910.40, -6426.52, -6507.52, 
                               -3217.28, 7521.00, 4522.00, -5750.52, -4373.25, -5957.63),
  ingreso_neto_mean_total = c(35.2, 22.5, 34.8, 45.3, 33.4, 
                              52, 45.30, 33.3, 35.7, 55.4, 23.5),
  perdida_media = c(0.08, 0.07, 0.06, 0.1, 0.15, 
                    0.09, 0.12, 0.19, 0.11, 0.12, 0.13)
)

rubros_rendimiento_t <- bind_rows(rubros_rendimiento_t, nuevas_observaciones)

# Indicadores por zonas agroproductivas
db4 <- db4 %>%
  mutate(
    zona_agroproductiva = case_when(
      s1_2 %in% c("MECAPACA", "PALCA", "SAPAHAQUI", "CAIROMA", "LURIBAY", "INQUISIVI", "CHARAZANI", "MOCOMOCO") ~ "valles_cerrados",
      s1_2 %in% c("ACHOCALLA", "CARABUCO", "ANCORAIMES", "MECAPACA", "MOCOMOCO", "HUARINA") ~ "altiplano_norte",
      s1_2 %in% c("PATACAMAYA", "SICA SICA") ~ "altiplano_centro",
      s1_2 %in% c("TOTORA", "POJO", "OMEREQUE", "MIZQUE", "AIQUILE", "PASORAPA") ~ "valles_norte",
      s1_2 %in% c("BETANZOS", "SAN LUCAS", "CULPINA") ~ "valles_centro",
      s1_2 %in% c("COTAGAITA", "TUPIZA", "VILLAZÓN", "VILLA CHARCAS", "INCAHUASI", "CULPINA", "VILLA SAN LORENZO", 
                  "EL PUENTE", "LAS CARRERAS") ~ "valles_sur",
      s1_2 %in% c("CARAPARI", "MONTEAGUDO", "PADCAYA") ~ "chaco_serrano",
      TRUE ~ NA_character_ # Para cualquier municipio que no coincida con los anteriores
    )
  )



agroprod_t <- db4 %>%
  filter(s4_2_5 == 1) %>%
  filter(grupo == "tratamiento") %>%
  filter(s4_3_1 != "Lechuga", s4_3_1 != "Quinua") %>%
  group_by(zona_agroproductiva, s4_3_1) %>%
  summarise(superficie_total = sum(s4_sup_std, na.rm = TRUE), # Definir importancia
            superficie_preomedio = mean(s4_sup_std, na.rm = TRUE),
            rend_promedio = mean(rend, na.rm = TRUE),
            ingreso_bruto_mean_total = round(mean(ing_bruto_ap, na.rm = TRUE), 2),
            ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
            perdida_media = round(mean(perdida, na.rm = TRUE), 2)
  ) 


# Para resiliencia:

db <- db %>%
  mutate(
    zona_agroproductiva = case_when(
      s1_2 %in% c("MECAPACA", "PALCA", "SAPAHAQUI", "CAIROMA", "LURIBAY", "INQUISIVI", "CHARAZANI", "MOCOMOCO") ~ "valles_cerrados",
      s1_2 %in% c("ACHOCALLA", "CARABUCO", "ANCORAIMES", "MECAPACA", "MOCOMOCO", "HUARINA") ~ "altiplano_norte",
      s1_2 %in% c("PATACAMAYA", "SICA SICA") ~ "altiplano_centro",
      s1_2 %in% c("TOTORA", "POJO", "OMEREQUE", "MIZQUE", "AIQUILE", "PASORAPA") ~ "valles_norte",
      s1_2 %in% c("BETANZOS", "SAN LUCAS", "CULPINA") ~ "valles_centro",
      s1_2 %in% c("COTAGAITA", "TUPIZA", "VILLAZÓN", "VILLA CHARCAS", "INCAHUASI", "CULPINA", "VILLA SAN LORENZO", 
                  "EL PUENTE", "LAS CARRERAS") ~ "valles_sur",
      s1_2 %in% c("CARAPARI", "MONTEAGUDO", "PADCAYA") ~ "chaco_serrano",
      TRUE ~ NA_character_ # Para cualquier municipio que no coincida con los anteriores
    )
  )


agroprod_endb_t <- db %>%
  filter(grupo == "tratamiento") %>%
  group_by(zona_agroproductiva) %>%
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE), 2))

agroprod_t <- agroprod_t %>%
  left_join(agroprod_endb_t, by = "zona_agroproductiva")

# Resiliencia por zona agroprod

db_resilience_agroprod <- db %>%
  filter(!is.na(zona_agroproductiva)) %>%
  group_by(zona_agroproductiva) %>%  
  summarise(indice_resiliencia = round(mean(resilience_index, na.rm = TRUE),2), .groups = 'drop')


ingreso_neto_muestracompleta_zonaagro <- db4 %>%
  filter(!is.na(zona_agroproductiva)) %>%
  group_by(zona_agroproductiva) %>% 
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE), 2), .groups = "drop") %>%
  filter(!is.na(ingreso_neto_mean))

ingreso_neto_muestracompleta_zonaagro_rubro <- db4 %>%
  filter(!is.na(zona_agroproductiva)) %>%
  group_by(zona_agroproductiva, s4_3_1) %>% 
  summarise(ingreso_neto_mean = round(mean(ing_neto_ap, na.rm = TRUE), 2), .groups = "drop") %>%
  filter(!is.na(ingreso_neto_mean))



# Otros cálculos  
rubro_mas_importante_rend_t <- rubros_rendimiento_t %>%
  group_by(s1_2) %>%
  slice_max(superficie_total, n = 1) %>%
  ungroup()

rubro_mas_importante_rend_t$s1_2 <- as.character(rubro_mas_importante_rend_t$s1_2)
codigos_mun$municipio <- as.character(codigos_mun$municipio)

rubros_con_codigos <- left_join(rubro_mas_importante_rend_t, codigos_mun, by = c("s1_2" = "municipio"))
write.table(rubros_con_codigos, "clipboard-16384", sep="\t", row.names=FALSE)

# ADICIONALES TRATAMIENTO 
municipios_solicitados <- c(
  "MONTEAGUDO",
  "CARABUCO",
  "ANCORAIMES",
  "PALCA",
  "SICA SICA",
  "INQUISIVI",
  "TOTORA",
  "MIZQUE",
  "VILLA CHARCAS",
  "INCAHUASI",
  "CARAPARI"
)

codigos_solicitados <- codigos_mun %>%
  filter(municipio %in% municipios_solicitados)

codigos_solicitados <- codigos_solicitados %>%
  mutate(orden = match(municipio, municipios_solicitados)) %>%
  arrange(orden) %>%
  select(-orden) 

#write.table(codigos_solicitados, "clipboard-16384", sep="\t", row.names=FALSE)

# GRUPO CONTROL #
rubros_rendimiento_c <- db4 %>%
  filter(grupo == "control") %>%
  filter(s4_3_1 != "Cebada") %>%
  group_by(s1_2, s4_3_1) %>%
  summarise(superficie_total = sum(s4_sup_std, na.rm = TRUE),
            rend_promedio = mean(rend, na.rm = TRUE)) %>%
  arrange(s1_2, desc(superficie_total))

rubro_mas_importante_rend_c <- rubros_rendimiento_c %>%
  group_by(s1_2) %>%
  slice_max(superficie_total, n = 1) %>%
  ungroup()

rubro_mas_importante_rend_c$s1_2 <- as.character(rubro_mas_importante_rend_c$s1_2)
codigos_mun$municipio <- as.character(codigos_mun$municipio)

rubros_con_codigos <- left_join(rubro_mas_importante_rend_c, codigos_mun, by = c("s1_2" = "municipio"))
write.table(rubros_con_codigos, "clipboard-16384", sep="\t", row.names=FALSE)

#write.csv(rubro_rend_importante_t, "rendimientos_t.csv",fileEncoding = "ISO-8859-1")
#write.csv(rubro_rend_importante_c, "rendimientos_c.csv",fileEncoding = "ISO-8859-1")


# Ingresos por municipio ####
# GRUPO TRATAMIENTO #
ingreso_neto_t <- db4 %>%
  filter(grupo == "tratamiento") %>%
  group_by(s1_2) %>%
  summarise(
    ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
    venta_neto_median_total = round(median(ing_neto_ap, na.rm = TRUE), 2)
  )

# GRUPO CONTROL #
ingreso_neto_c <- db4 %>%
  filter(grupo == "control") %>%
  group_by(s1_2) %>%
  summarise(
    ingreso_neto_mean_total = round(mean(ing_neto_ap, na.rm = TRUE), 2),
    venta_neto_median_total = round(median(ing_neto_ap, na.rm = TRUE), 2)
  )

# Lista de entrevistados ####
lista <- db2 %>% 
  select(`_index`,`_parent_index`, s2_1a, s2_1b)

lista_unica <- db2 %>%
  select(`_index`, `_parent_index`, s2_1a, s2_1b) %>%
  distinct(`_parent_index`, .keep_all = TRUE)

num_encuestas_tratamiento <- db4 %>%
  filter(grupo == "tratamiento", s4_2_5 == 1) %>%
  distinct(s4_3_1, `_parent_index`) %>%
  group_by(s4_3_1) %>%
  summarise(Num_Encuestas = n())

num_encuestas_control <- db4 %>%
  filter(grupo == "control", s4_2_5 == 1) %>%
  distinct(s4_3_1, `_parent_index`) %>%
  group_by(s4_3_1) %>%
  summarise(Num_Encuestas = n())

#write.table(lista_unica, "clipboard-16384", sep="\t", row.names=FALSE)