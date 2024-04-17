# Línea de base APROCAM 
# Proyecto ACCESOS RURAL
# Ministerio de Desarrollo Rural y Tierras - Fondo Internacional para el Desarrollo Agrícola

# Identificación y renombrado de variables
# Compilación 31/01/2024
# Elaborado por ARIA SRL

rm(list = ls())

# Librerías
library(readxl)
library(tidyverse)
library(haven)
library(labelled)
library(tidyr)

db <- read_excel("data_in/1_db_raw/ldb.xlsx", sheet = "Encuesta Linea Base")
db2 <- read_excel("data_in/1_db_raw/ldb.xlsx", sheet = "SECCION_2")
db4 <- read_excel("data_in/1_db_raw/ldb.xlsx", sheet = "S4_Parcelas")
codigos_mun <- read.csv("data_in/1_db_raw/mun_code.csv")

db_label <- read_excel("data_in/1_db_raw/ldb_labels.xlsx", sheet = "Encuesta Linea Base")
db2_label <- read_excel("data_in/1_db_raw/ldb_labels.xlsx", sheet = "SECCION_2")
db4_label <- read_excel("data_in/1_db_raw/ldb_labels.xlsx", sheet = "S4_Parcelas")

# Funciones ####

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

# Etiquetado db ####
etiquetas_db <- names(db_label)
etiquetas_db <- sapply(names(db_label), as.character)

# Etiquetado db2 ####
etiquetas_db2 <- names(db2_label)
etiquetas_db2 <- sapply(names(db2_label), as.character)

# Etiquetado db4 ####
etiquetas_db4 <- names(db4_label)
etiquetas_db4 <- sapply(names(db4_label), as.character)


# Asignar las etiquetas a las variables en db
for (i in 1:length(etiquetas_db)) {
  # Comprobar si la columna es numérica o de caracteres
  if (is.numeric(db[[i]]) || is.character(db[[i]])) {
    db[[i]] <- haven::labelled(db[[i]], label = etiquetas_db[i])
  }
}

# Asignar las etiquetas a las variables en db2
for (i in 1:length(etiquetas_db2)) {
  # Comprobar si la columna es numérica o de caracteres
  if (is.numeric(db2[[i]]) || is.character(db2[[i]])) {
    db2[[i]] <- haven::labelled(db2[[i]], label = etiquetas_db2[i])
  }
}

# Asignar las etiquetas a las variables en db4
for (i in 1:length(etiquetas_db4)) {
  # Comprobar si la columna es numérica o de caracteres
  if (is.numeric(db4[[i]]) || is.character(db4[[i]])) {
    db4[[i]] <- haven::labelled(db4[[i]], label = etiquetas_db4[i])
  }
}

# Asignación tratamiento/control ####
# Municipios tratamiento
mun_tratamiento = c(10501, 10702, 10703, 10704, 10902, 10903, #Chuquisaca
                    20102, 20103, 20104, 20202, 20204, 20402, 20403, 20405, 20901, 
                    20902, 20905, 21001, 21301, 21305, 21601, 21602, #La Paz
                    30201, 30202, 30203, 31201, 31202, 31301, #Cochabamba
                    50301, 50601, 50801, 51501, #Potosí
                    60201, 60302, 60502, 60501) #Tarija

# Asignación #
db <- db %>%
  mutate(grupo = ifelse(s1_2 %in% mun_tratamiento, "tratamiento", "control"))

# Filtrado valores de prueba piloto####
db <- db %>% 
  filter(!(`_index` %in% 1:16))

db2 <- db2 %>% 
  filter(!(`_parent_index` %in% 1:16)) 

db4 <- db4 %>% 
  filter(!(`_parent_index` %in% 1:16)) 


# Rename DB####
db <- db %>% 
  rename(
    s0_prod = "Usted_produce_rubro_priorizad",
    s0_num_ninos = "_Cu_ntos_ni_os_entre_ualmente_en_su_hogar",
    s3_9 = s3_9_001,
    s3_10 = s3_10_001,
    s3_10e = s3_e,
    s5_3_7_11_x = s5_3_7_11,
    s5_3_7_11 = s5_3_7_11_001,
    s6_6_1a = "Manejo_de_cuencas_o_integral_d",
    s6_6_1b = "En_el_ltimo_a_o_ha_oramiento_que_recibi",
    s6_6_1c = "_Cree_que_la_tecnolo_le_sea_til_o_eficaz",
    s6_6_1d = "_Ha_adoptado_y_seguir_usando_",
    s6_6_2a = "Medidas_para_protege_cercos_perimetrales",
    s6_6_2b = "En_el_ltimo_a_o_ha_oramiento_que_recibi_001",
    s6_6_2c = "_Cree_que_la_tecnolo_le_sea_til_o_eficaz_001",
    s6_6_2d = "_Ha_adoptado_y_segui_ntalmente_sostenible"
  )

db <- db %>% 
  rename(
    s4_17_lista = s4_3,
    s4_17_1 = "s4_3/1",
    s4_17_2 = "s4_3/2",
    s4_17_3 = "s4_3/3",
    s4_17_mejora = s4_31,
    s4_18 = s44,
    s4_18_lista = s44_1,
    s4_18_1 = `s44_1/1`,
    s4_18_2 = `s44_1/2`,
    s4_18_3 = `s44_1/3`,
    s4_18_4 = `s44_1/4`,
    s4_18_5 = `s44_1/5`,
    s4_18_6 = `s44_1/6`,
    s4_18_7 = `s44_1/7`,
    s4_18_8 = `s44_1/8`,
    s4_19 = s46,
    s4_20 = s4_21_2,
    s7_1_x = s7_1,
    s7_1 = "Capacitaciones_en_g_nero_y_o_m",
    s7_1a = s7_2,
    s7_1b = s7_3,
    s7_2 = s7_4_001,
    s7_2a = s7_5_001,
    s7_2b = s7_6_001,
    s7_6 = s7_6_header,
    s7_7 = s7_7_header
  )

# Valores DB ####
deptos = c("1" = "CHUQUISACA", 
           "2" = "LA PAZ", 
           "3" = "COCHABAMBA", 
           "5" = "POTOSI", 
           "6" = "TARIJA")

db$s1_1 <- factor(db$s1_1, levels = c("1", "2", "3", "5", "6"), labels = deptos)
labelled::var_label(db$s1_1) <- "Departamento"

municipios <- setNames(as.character(codigos_mun$mun), codigos_mun$codigo)

db$s1_2 <- factor(db$s1_2, levels = names(municipios), labels = municipios)
labelled::var_label(db$s1_2) <- "Municipio"

unique(db4$s4_3_1)

# Rename DB2####

# Rename DB4####

# Incorporar nombres para revisión en db1 y db4
primer_nombre_por_familia <- db2 %>%
  group_by(`_parent_index`) %>%
  summarise(nombre = first(s2_1a),
            apellido = first(s2_1b),
            .groups = 'drop')

db <- db %>%
  left_join(primer_nombre_por_familia, by = c("_index" = "_parent_index"))


# Incorporar grupos/mun/com/sexo ####

db4 <- db4 %>%
  left_join(db %>% select("_index", grupo, s1_2, s1_3, `_s1_6_altitude`, nombre, apellido, s4_4, s4_5), by = c("_parent_index" = "_index")) 

db2 <- db2 %>%
  left_join(db %>% select("_index", grupo, s1_2, s1_3, nombre, apellido, s4_4, s4_5), by = c("_parent_index" = "_index")) 

# Corrección caracteres ####
# Corrección de usuarios - brigada_id#
db <- db %>%
  mutate(s1_5 = ifelse(username %in% c("franklinllanos", "carlosllanosc", 
                                       "maevelibelula", "agustinlp018"), 111, s1_5)) %>% 
  mutate(s1_5 = ifelse(username %in% c("lissetlp010", "corinalp009", 
                                       "brendalp008", "nilsonlp012"), 121, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("rosmerylp005", "nohelialp007", 
                                       "miriamlp006", "patriciacg020"), 131, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("rolandolp011", "josuelp001", 
                                       "osbaldolp004", "silvialp002"), 141, s1_5)) %>% 
  mutate(s1_5 = ifelse(username %in% c("nidialp014", "paulcalle_23", 
                                       "gracielalp003", "edilchoquelp020"), 151, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("yolandacbba001", "eudorocbba002", 
                                       "fabiolacbba017", "isabelcbba012"), 211, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("carloscbba014", "fabriciocbba007", 
                                       "moreliacbba008", "cristinacbba010"), 221, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("mariacbba009", "andreacbba006", 
                                       "clotildecbba016", "jheremycbba015"), 231, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("rodrigoserrano002", "weymarsantos001", 
                                       "carlosmollo004", "sergioescalier014"), 311, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("javiergonzales013", "abimaelgonzales001", 
                                       "mirthagorena001", "rolandogutierrez001"), 411, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("johanngutierrez014", "oscarcruz013", 
                                       "alizonchavez016", "aliciahuanca017"), 511, s1_5)) %>%
  mutate(s1_5 = ifelse(username %in% c("gabrielalima011", "fernandomogro012", 
                                       "waltermendoza010", "nicolcusi009"), 611, s1_5))

table(db$s1_5)

# Corrección de rubros #
db4 <- db4 %>%
  filter(s4_3_1 != "00") %>% 
  filter(s4_3_1 != "0") %>% 
  filter(s4_3_1 != "o") %>% 
  filter(s4_3_1 != "O") %>% 
  filter(s4_3_1 != "Xx")

db4$s4_3_1 <- tolower(db4$s4_3_1)
db4$s4_3_1 <- tools::toTitleCase(db4$s4_3_1)

rubros_correctos <- c("Papa", "Arveja", "Haba", "Cebolla", "Zanahoria", "Manzana", "Maiz", 
                      "Durazno", "Tomate", "Damasco", "Pacay", "Pera", "Ciruelo",
                      "Cebada", "Uva", "Tuna", "Oca", "Sandia")

# Usar función "encontrar cercano"
  
umbral <- 2 
# db4$s4_3_1_n <- sapply(db4$s4_3_1, encontrar_cercano, lista_correcta = rubros_correctos, umbral = umbral)
db4$s4_3_1 <- sapply(db4$s4_3_1, encontrar_cercano, lista_correcta = rubros_correctos, umbral = umbral)

# s4_3_cambios <- db4 %>% 
#   filter(s4_3_1 != s4_3_1_n) %>% 
#   select(s4_3_1, s4_3_1_n)

table(db4$s4_3_1)

# Corrección manual de caracteres DB4 #
# Corrección de Rubros #
db4$s4_3_1[db4$s4_3_1 == "80"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "\r\n\r\nDurazno"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "1000"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Alfa"] <- "Alfalfa"
db4$s4_3_1[db4$s4_3_1 == "Alfa Alfa"] <- "Alfalfa"
db4$s4_3_1[db4$s4_3_1 == "Alfaalfa"] <- "Alfalfa"
db4$s4_3_1[db4$s4_3_1 == "Brócoli"] <- "Brocoli"
db4$s4_3_1[db4$s4_3_1 == "Almacigo De Cebollin"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Cebolla 1000m"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Recilio"] <- "Ciruelo"
db4$s4_3_1[db4$s4_3_1 == "Alverja"] <- "Arveja"
db4$s4_3_1[db4$s4_3_1 == "Alberja"] <- "Arveja"
db4$s4_3_1[db4$s4_3_1 == "Arbeja"] <- "Arveja"
db4$s4_3_1[db4$s4_3_1 == "Alverba"] <- "Arveja"
db4$s4_3_1[db4$s4_3_1 == "Arverja"] <- "Arveja"
db4$s4_3_1[db4$s4_3_1 == "Cebada (Forraje Para El Ganado)"] <- "Cebada"
db4$s4_3_1[db4$s4_3_1 == "Cebada (Forraje Para Ganado)"] <- "Cebada"
db4$s4_3_1[db4$s4_3_1 == "Cebollac c"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Cebolla y Zanahoria"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Cebolla (Almacigo)"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Cebolla (Almacigo"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Cebolla ( Almacigo)"] <- "Cebolla"
db4$s4_3_1[db4$s4_3_1 == "Chechuga"] <- "Lechuga"
db4$s4_3_1[db4$s4_3_1 == "Lechuga Repollada"] <- "Lechuga"
db4$s4_3_1[db4$s4_3_1 == "Choclo Maiz"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Maiz Rumi Pampa"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Pimenton"] <- "Morron"
db4$s4_3_1[db4$s4_3_1 == "Ambos"] <- "Tomate"
db4$s4_3_1[db4$s4_3_1 == "Tomate Hibrido"] <- "Tomate"
db4$s4_3_1[db4$s4_3_1 == "Choclo"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Choclo (Maiz)"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Morrón"] <- "Morron"
db4$s4_3_1[db4$s4_3_1 == "Crbada (Forraje Para Ganado Bovino)"] <- "Cebada"
db4$s4_3_1[db4$s4_3_1 == "Durazno , Maiz"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno Perchico"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno Ulincate"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno y Manzana"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno, Maíz, Papa"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno, Manzana"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Durazno,maíz Papa"] <- "Durazno"
db4$s4_3_1[db4$s4_3_1 == "Espinaca y Apio"] <- "Espinaca"
db4$s4_3_1[db4$s4_3_1 == "Lechuga (Almacigo)"] <- "Lechuga"
db4$s4_3_1[db4$s4_3_1 == "Maiz Blanco"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Maíz Choclero"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Maiz Choclo"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Maíz Choqlero"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Maiz Morocho"] <- "Maiz"
db4$s4_3_1[db4$s4_3_1 == "Manzana Eva"] <- "Manzana"
db4$s4_3_1[db4$s4_3_1 == "Manzana Princesa"] <- "Manzana"
db4$s4_3_1[db4$s4_3_1 == "Moron"] <- "Morron"
db4$s4_3_1[db4$s4_3_1 == "Papa Haycha"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Huaycha"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Huaycho"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Hueycha"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Luque"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Negra"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Polonia"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Sacambaya"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Sani"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Papa Sani"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Pera De Agua"] <- "Pera"
db4$s4_3_1[db4$s4_3_1 == "Pera Luyma"] <- "Pera"
db4$s4_3_1[db4$s4_3_1 == "Pera Menuda"] <- "Pera"
db4$s4_3_1[db4$s4_3_1 == "Peramote"] <- "Peramota"
db4$s4_3_1[db4$s4_3_1 == "Ssndia"] <- "Sandia"
db4$s4_3_1[db4$s4_3_1 == "Uva Blanca"] <- "Uva"
db4$s4_3_1[db4$s4_3_1 == "Wiinua"] <- "Quinua"
db4$s4_3_1[db4$s4_3_1 == "Papá\r\n"] <- "Papa"
db4$s4_3_1[db4$s4_3_1 == "Duraznoo\r\n"] <- "Durazno"
db4$s4_3_1[db4$`_index` == 595] <- "Papa"
db4$s4_3_1[db4$`_index` == 1264] <- "Papa"
db4$s4_3_1[db4$`_index` == 97] <- "Haba"
db4$s4_3_1[db4$`_index` == 98] <- "Haba"
db4$s4_3_1[db4$`_index` == 872] <- "Alfa Alfa"
db4$s4_3_1[db4$`_index` == 1494] <- "Alfa Alfa"
db4$s4_3_1[db4$`_index` == 1164] <- "Durazno"
db4$s4_3_1[db4$`_index` == 3070] <- "Tomate"


db4 <-db4 %>% 
  filter(!(s4_3_1 == "No Tomar Encuta"))

db4 <- db4 %>% 
  rename(
    factor_perdida = "Factor_de_perdida",
    perdida_seq = "Factor_de_perdida/1",
    perdida_hel = "Factor_de_perdida/2",
    perdida_gran = "Factor_de_perdida/3",
    perdida_ria = "Factor_de_perdida/4",
    perdida_des = "Factor_de_perdida/5",
    perdida_plaga = "Factor_de_perdida/6",
    s4n8_u_dep = s4n8_u,
    s4n8_u = s4n8_u_001,
    s4n10_der = "_Cu_l_fue_el_princip_s4_3_1_que_produjo",
    s_4_17_l_1 = "s_4_17_l/1",
    s_4_17_l_2 = "s_4_17_l/2",
    s_4_17_l_3 = "s_4_17_l/3",
    s_4_17_l_99= "s_4_17_l/99",
    s_4_17_l_99e= "s_4_17_e_001"
)

table(db4$s4_3_1)

# Incorporar rubros en db
rubros_priorizados <- c("Papa", "Haba", "Cebolla", "Zanahoria", "Manzana", "Maiz", 
                      "Durazno", "Tomate")

rubro_principal <- db4 %>%
  group_by(`_parent_index`) %>%
  mutate(es_priorizado = s4_3_1 %in% rubros_priorizados) %>%
  # Asegura que el primer elemento sea prioritario si existe, de lo contrario, toma el primero de la lista
  arrange(desc(es_priorizado)) %>%
  summarise(rubro = first(s4_3_1), .groups = 'drop')

table(rubro_principal$rubro)

db <- db %>%
  left_join(rubro_principal, by = c("_index" = "_parent_index"))

table(db$rubro, useNA = "always")

db$rubro[db$`_index` == 27] <- "Haba"
db$rubro[db$`_index` == 28] <- "Haba"
db$rubro[db$`_index` == 29] <- "Haba"
db$rubro[db$`_index` == 30] <- "Haba"

#  GENERACIÓN NUEVOS .rds ####

saveRDS(db, "data_in/2_db_renamed/db.rds")
saveRDS(db2, "data_in/2_db_renamed/db2.rds")
saveRDS(db4, "data_in/2_db_renamed/db4.rds")





