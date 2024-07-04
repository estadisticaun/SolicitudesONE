
##%######################################################%##
#                                                          #
####                     Librerías                      ####
#                                                          #
##%######################################################%##

library(UnalData)
library(UnalR)
library(dplyr)
library(tidyr)
library(readxl)
library(xlsx)
library(writexl)
library(openxlsx)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(skimr)
library(forcats)
library(magrittr)
library(htmlwidgets)
library(RColorBrewer)
library(RSocrata)
library(viridis)
library(gt)

######################################-
# 1 Solicitud 14-05-2021 -----
######################################-

# Demanda : Facultad de Minas de la Universidad
          # FLOR ÁNGELA MARULANDA VALENCIA
          # Secretaria Facultad de Minas
# Descripción: Total de matriculados por programa académico
               # de la Facultad de Minas por sexo biológico
# Medio de la solicitud: estadistica_nal@unal.edu.co
# Fecha : 14/05/2021
# Tiempo de respuesta: 2 días
# Observaciones: La información se entregó sin problemas

# Por programas Nivel de formación
Sexo_Minas_nivel <- UnalData::Matriculados %>% filter(FACULTAD %in% c("Minas")) %>% 
  mutate(PERIODO = paste0(YEAR, SEMESTRE)) %>%   
  group_by(PERIODO, FACULTAD, TIPO_NIVEL, SEXO) %>% 
  count() %>% 
  pivot_wider(names_from = PERIODO, values_from = n)

# Exportar resultados a Excel
write_xlsx(Sexo_Minas_nivel, "Datos/Sexo_Minas_nivel.xlsx")

# Por programas Académicos
Sexo_Minas <- UnalData::Matriculados %>% filter(TIPO_NIVEL == "Pregrado", FACULTAD %in% c("Minas")) %>% 
  mutate(PERIODO = paste0(YEAR, SEMESTRE)) %>%   
  group_by(PERIODO, FACULTAD, SNIES_PROGRA, SEXO) %>% 
  count() %>% 
  pivot_wider(names_from = PERIODO, values_from = n)

# Exportar resultados a Excel
write_xlsx(Sexo_Minas, "Datos/Sexo_Minas.xlsx")

######################################-
# 2 Solicitud 23-06-2021 -----
######################################-

# Demanda : Mesa de género de la sede Medellín
# FKevin Manuel Ortiz Vargas 

# Descripción: Desde la mesa de género de la sede Medellín 
# estamos haciendo un reporte de datos estadísticos através de los años. 
# Me gustaría pedirle el favor que me facilitara el consolidado de los trabajadores de la universidad 
# (profesores, vigilantes, administrativos, aseadores, entre otros) con la categoría de sexo (Hombre-mujer) para hacer un análisis estadístico de estas cifras a lo largo de los años, agradezco infinitamente su colaboración.

# Se me olvidó comentarlo en en anterior correo. 
# También estamos iniciando un reporte sobre los factores de riesgo de suicidio 
# en la universidad nacional, para eso necesitamos el consolidado de los estudiantes 
# detallando la edad, estrato socio económico, 
# sexo (y de ser posible el departamento y municipio de residencia). 
# Agradezco de nuevo su colaboración, mil gracias.

# Medio de la solicitud: estadistica_nal@unal.edu.co
# Fecha Entrega : 23/06/2021
# Tiempo de respuesta: 3 días
# Observaciones: La información se entregó sin problemas

# Sexo FUNCIONARIOS por modalidad de vinculación

Sexo_mod_fun <- UnalData::Administrativos %>% 
                mutate(PERIODO = paste0(YEAR, SEMESTRE)) %>%
                group_by(PERIODO, NIVEL, SEXO) %>% 
                summarise(Total = n()) %>% 
                pivot_wider(names_from = PERIODO, values_from = Total)

write_xlsx(Sexo_mod_fun, "Datos/Entrega2/Sexo_funcionarios.xlsx")

# Sexo DOCENTES por categoría

Sexo_cat_doc <- UnalData::Docentes %>% 
  mutate(PERIODO = paste0(YEAR, SEMESTRE)) %>%
  group_by(PERIODO, CATEGORIA, SEXO) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = PERIODO, values_from = Total)

write_xlsx(Sexo_cat_doc, "Datos/Entrega2/Sexo_doc_categoria.xlsx")

# Sexo DOCENTES por dedicación

Sexo_dedi_doc <- UnalData::Docentes %>% 
  mutate(PERIODO = paste0(YEAR, SEMESTRE)) %>%
  group_by(PERIODO, DEDICACION, SEXO) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = PERIODO, values_from = Total)

write_xlsx(Sexo_dedi_doc, "Datos/Entrega2/Sexo_doc_dedicacion.xlsx")


######################################-
# 3 Solicitud 28-06-2021 -----
######################################-

# Demanda : Maria Claudia Galindo Gonzales 

# Descripción: Entrega de base de datos histórica de graduados 
# 2009-2020 para contrastarla con lo disponible en el OLE

write_xlsx(UnalData::Graduados, "Datos/Entrega3/Consolidado_graduados.xlsx")

######################################-
# 4 Solicitud 07-07-2021 -----
######################################-

# Demanda : Yeidri Sulay Taborda Rojas

# Descripción: Entrega de bases agregadas de aspirantes, admitidos y
# matriculados al programa de zootecnia en las sedes de Bogotá, Palmira y Medellín
# Se requieren para el proceso de acreditación del programa de Zootecnia de la
# Universidad Universidad Francisco de Paula Santander seccional Ocaña

# Medio de la solicitud: correo de la oficina a través de diregis_med@unal.edu.co
# Fecha Entrega : 8/07/2021
# Tiempo de respuesta: 1 días
# Observaciones: No fue posible, por la política de admisiones, entregar aspirantes
                # para los programas de Zootecnia


# Base de matriculados

Matricula_UFPS <- UnalData::Matriculados %>% filter(SNIES_PROGRA %in% c(3, 143, 16926))
Matricula_UFPS$SNIES_PROGRA <- as.factor(Matricula_UFPS$SNIES_PROGRA)

# Crear consolidado de matriculados
Zootecnia_UFPS_Mat <- Agregar(SNIES_PROGRA ~ YEAR + SEMESTRE, 
        frecuencia = list(c(2015:2020), c(1:2)),
        intervalo = list(c(2015, 2), c(2020, 2)),
        datos = Matricula_UFPS,
        textNA = "Sin información") %>% 
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total)

View(Zootecnia_UFPS_Mat)

write_xlsx(Zootecnia_UFPS_Mat, "Datos/Entrega4/Zootecnia_UFPS_Mat.xlsx")


# Base de admitidos

Admitidos_UFPS <- UnalData::Aspirantes %>% filter(ADMITIDO == "Sí", 
SNIES_PROGRA %in% c(3, 143, 16926))
Admitidos_UFPS$SNIES_PROGRA <- as.factor(Admitidos_UFPS$SNIES_PROGRA)

# Crear consolidado de admitidos
Zootecnia_UFPS_Adm <- Agregar(SNIES_PROGRA ~ YEAR + SEMESTRE, 
                          frecuencia = list(c(2015:2021), c(1:2)),
                          intervalo = list(c(2015, 2), c(2021, 1)),
                          datos = Admitidos_UFPS,
                          textNA = "Sin información") %>% 
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total)

View(Zootecnia_UFPS_Adm)

write_xlsx(Zootecnia_UFPS_Adm, "Datos/Entrega4/Zootecnia_UFPS_Adm.xlsx")

######################################-
# 5 Solicitud 03-08-2021 -----
######################################-

# Demanda : Maria Claudia Galindo Gonzales

# Descripción: Tabla con graduados por estrato y año
# Para firma de convenio con el ICETEX

Graduados_Estrato <- UnalData::Graduados %>% 
                     filter(TIPO_NIVEL == "Pregrado") %>% 
                      group_by(YEAR, ESTRATO_ORIG) %>% 
                      summarise(Total = n()) %>% 
                      ungroup() %>%  
pivot_wider(names_from = c(ESTRATO_ORIG), values_from = Total)

write_xlsx(Graduados_Estrato, "Datos/Entrega5/Graduados_Estrato.xlsx")

# Alternativa 2

DGraduados_Estrato <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado") 

Graduados_Estrato_1 <- Agregar(formula = ESTRATO_ORIG ~ YEAR, 
          frecuencia = c(2009:2020),
          intervalo = c(2009, 2020),
          datos = DGraduados_Estrato,
          textNA = "Sin información") %>% 
  pivot_wider(names_from = c(Clase), values_from = Total) %>% 
  select(-Variable)

View(Graduados_Estrato_1)

# Comparar las dos bases de datos

all_equal(Graduados_Estrato, Graduados_Estrato_1, convert = TRUE)
# OJO: Se utiliza el argumento convert en razón a que los tipos de las
# variables (class) de los dos conjuntos de datos son diferentes

######################################-
# 6 Solicitud 06-08-2021 -----
######################################-

# Demanda : Maria Claudia Galindo Gonzales

# Descripción: Tablas con distribución de aspirantes(postgrado), admitidos, matriculados y
# graduados por programas académicos

# Aspirantes a postgrado

Pro_postgrado <- UnalData:: Aspirantes %>%
  filter(TIPO_NIVEL == "Postgrado")

# Convertir código SNIES a character

Pro_postgrado$SNIES_PROGRA <- as.character(Pro_postgrado$SNIES_PROGRA)

Pro_pos <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2009:2020), c(1:2)),
                   intervalo = list(c(2009, 1), c(2020, 2)),
                   datos = Pro_postgrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)

Pro_pos_Nivel <- Pro_postgrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, INS_SEDE_NOMBRE) %>%
  count() %>% select(-n)


Pro_pos_Nivel <- Pro_postgrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, INS_SEDE_NOMBRE) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, INS_SEDE_NOMBRE) %>% count() %>%
  ungroup() %>% group_by(SNIES_PROGRA) %>% count()


Pro_post_aspirantes <- left_join(Pro_pos, Pro_pos_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, INS_SEDE_NOMBRE)

write_xlsx(Pro_post_aspirantes, "Datos/Entrega6/Prog_Asp_Post.xlsx")


View(Pro_postgrado %>% filter(SNIES_PROGRA == "16892" ))

# Aspirantes a pregrado


Pro_pregrado <- UnalData:: Aspirantes %>%
  filter(TIPO_NIVEL == "Pregrado",
         ADMITIDO == "Sí",
         INS_SEDE_NOMBRE %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))


# Convertir código SNIES a character

Pro_pregrado$SNIES_PROGRA <- as.character(Pro_pregrado$SNIES_PROGRA)

Pro_pre <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2008:2020), c(1:2)),
                   intervalo = list(c(2008, 1), c(2020, 2)),
                   datos = Pro_pregrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)

# Crear base de datos con nombre de programas

Pro_pre_Nivel <- Pro_pregrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, INS_SEDE_NOMBRE) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, NIVEL, INS_SEDE_NOMBRE) %>%
  summarise(PROGRAMA = max(PROGRAMA))

# Cruzar base de datos


Pro_pre_aspirantes <- left_join(Pro_pre, Pro_pre_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, INS_SEDE_NOMBRE)

write_xlsx(Pro_pre_aspirantes, "Datos/Entrega6/Prog_Asp_Preg.xlsx")

###################-
# Matriculados en postgrado
###################-


Mat_postgrado <- UnalData::Matriculados %>%
  filter(TIPO_NIVEL == "Postgrado")

# Convertir código SNIES a character

Mat_postgrado$SNIES_PROGRA <- as.character(Mat_postgrado$SNIES_PROGRA)

Mat_pos <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2009:2020), c(1:2)),
                   intervalo = list(c(2009, 1), c(2020, 2)),
                   datos = Mat_postgrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)


# Nombres de programas académicos

Mat_pos_Nivel <- Mat_postgrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, SEDE_NOMBRE_MAT) %>%
  summarise(PROGRAMA = max(PROGRAMA), NIVEL = max(NIVEL))

# Cruce de bases de datos

Post_matriculados <- left_join(Mat_pos, Mat_pos_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT)

write_xlsx(Post_matriculados, "Datos/Entrega6/Prog_Mat_Post.xlsx")


View(Mat_postgrado %>% filter(SNIES_PROGRA == "52737" ))


###################-
# Matriculados en pregrado
###################-


Mat_pregrado <- UnalData:: Matriculados %>%
  filter(TIPO_NIVEL == "Pregrado",
         SEDE_NOMBRE_MAT %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))


# Convertir código SNIES a character

Mat_pregrado$SNIES_PROGRA <- as.character(Mat_pregrado$SNIES_PROGRA)

Mat_pre <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2009:2020), c(1:2)),
                   intervalo = list(c(2009, 1), c(2020, 2)),
                   datos = Mat_pregrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)


# Nombres de programas académicos

Mat_pre_Nivel <- Mat_pregrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, SEDE_NOMBRE_MAT) %>%
  summarise(PROGRAMA = max(PROGRAMA), NIVEL = max(NIVEL))


View(Mat_pre_Nivel %>% group_by(SNIES_PROGRA) %>% count())

# Cruce de bases de datos

Pre_matriculados <- left_join(Mat_pre, Mat_pre_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT)

write_xlsx(Pre_matriculados, "Datos/Entrega6/Prog_Mat_Pre.xlsx")


View(Mat_postgrado %>% filter(SNIES_PROGRA == "52737" ))


###################-
# Graduados en Postgrado
###################-


Gra_postgrado <- UnalData::Graduados %>%
  filter(TIPO_NIVEL == "Postgrado")


# Convertir código SNIES a character

Gra_postgrado$SNIES_PROGRA <- as.character(Gra_postgrado$SNIES_PROGRA)

Gra_pos <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2009:2020), c(1:2)),
                   intervalo = list(c(2009, 1), c(2020, 2)),
                   datos = Gra_postgrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)


# Nombres de programas académicos

Gra_pos_Nivel <- Gra_postgrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, SEDE_NOMBRE_MAT) %>%
  summarise(PROGRAMA = max(PROGRAMA), NIVEL = max(NIVEL))


View(Gra_pos_Nivel %>% group_by(SNIES_PROGRA) %>% count())


# Cruce de bases de datos

Post_graduados <- left_join(Gra_pos, Gra_pos_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT)

write_xlsx(Post_graduados, "Datos/Entrega6/Prog_Gra_Post.xlsx")


View(Mat_postgrado %>% filter(SNIES_PROGRA == "52737" ))

###################-
# Graduados en pregrado
###################-


Gra_pregrado <- UnalData:: Graduados %>%
  filter(TIPO_NIVEL == "Pregrado",
         SEDE_NOMBRE_MAT %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))


# Convertir código SNIES a character

Gra_pregrado$SNIES_PROGRA <- as.character(Gra_pregrado$SNIES_PROGRA)

Gra_pre <- Agregar(formula = SNIES_PROGRA ~ YEAR + SEMESTRE,
                   frecuencia = list(c(2009:2020), c(1:2)),
                   intervalo = list(c(2009, 1), c(2020, 2)),
                   datos = Gra_pregrado,
                   textNA = "Sin información") %>%
  pivot_wider(names_from = c(YEAR, SEMESTRE), values_from = Total) %>%
  select(-Variable) %>% rename(SNIES_PROGRA = Clase)


# Nombres de programas académicos

Gra_pre_Nivel <- Gra_pregrado %>% group_by(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT) %>%
  count() %>% select(-n) %>% ungroup() %>% group_by(SNIES_PROGRA, SEDE_NOMBRE_MAT) %>%
  summarise(PROGRAMA = max(PROGRAMA), NIVEL = max(NIVEL))


View(Gra_pre_Nivel %>% group_by(SNIES_PROGRA) %>% count())

# Cruce de bases de datos

Pre_graduados <- left_join(Gra_pre, Gra_pre_Nivel) %>%
  relocate(SNIES_PROGRA, NIVEL, PROGRAMA, SEDE_NOMBRE_MAT)

write_xlsx(Pre_graduados, "Datos/Entrega6/Prog_Gra_Pre.xlsx")

#################################
# Cruzar las bases de datos Admitidos - Matriculados y Graduados
#################################

###
# Postgrado
###

# Apirantes
postgrado_Asp <- UnalData:: Aspirantes %>%
  filter(TIPO_NIVEL == "Postgrado") %>% group_by(SNIES_PROGRA) %>%
  count(name = "Aspirantes")

# Matriculados
postgrado_mat <- UnalData::Matriculados %>%
  filter(TIPO_NIVEL == "Postgrado") %>% group_by(SNIES_PROGRA) %>%
  count(name = "Matriculados")

# Graduados
postgrado_gra <- UnalData::Graduados %>%
  filter(TIPO_NIVEL == "Postgrado") %>% group_by(SNIES_PROGRA) %>%
  count(name = "Graduados")

# Base de datos consolidada

Pos1 <- full_join(postgrado_Asp, postgrado_mat)  
SNIES_Postgrado <- full_join(Pos1,  postgrado_gra) %>% ungroup() %>%
  mutate(across(Aspirantes:Graduados, ~if_else(is.na(.), 0, 1)))


write_xlsx(SNIES_Postgrado, "Datos/Entrega6/SNIES_Postgrado.xlsx")

###
# Pregrado
###

# Aspirantes

Pro_pregrado <- UnalData:: Aspirantes %>%
  filter(TIPO_NIVEL == "Pregrado",
         ADMITIDO == "Sí",
         INS_SEDE_NOMBRE %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))%>% group_by(SNIES_PROGRA) %>%
  count(name = "Aspirantes")

# Matriculados

Mat_pregrado <- UnalData:: Matriculados %>%
  filter(TIPO_NIVEL == "Pregrado",
         SEDE_NOMBRE_MAT %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))%>% group_by(SNIES_PROGRA) %>%
  count(name = "Matriculados")

# Graduados

Gra_pregrado <- UnalData:: Graduados %>%
  filter(TIPO_NIVEL == "Pregrado",
         SEDE_NOMBRE_MAT %in% c("Bogotá", "Medellín", "Manizales", "Palmira", "De La Paz"))%>% group_by(SNIES_PROGRA) %>%
  count(name = "Graduados")

# Base de datos consolidada

Pre1 <- full_join(Pro_pregrado, Mat_pregrado)  
SNIES_Pregrado <- full_join(Pre1, Gra_pregrado) %>% ungroup() %>%
  mutate(across(Aspirantes:Graduados, ~if_else(is.na(.), 0, 1)))

write_xlsx(SNIES_Pregrado, "Datos/Entrega6/SNIES_Pregrado.xlsx")

######################################-
# 7 Solicitud 02-09-2021 -----
######################################-

# Demanda : amixsegura_nal@unal.edu.co
# Estudio Multidimensional De Las Violencias eje cual 

# Descripción: Nos comunicamos con ustedes para consultar la 
# disponibilidad de información sociodemográfica (edad, estrato 
# socioeconómico, sexo/género, orientación e identidad sexual, 
# adscripción étnica, lugar de nacimiento y residencia) 
# del estudiantado matriculado en los últimos cinco años
# académicos en los programas de pregrado de Ingeniería Geológica,
# Ingeniería de Petróleos e Ingeniería Ambiental de la Facultad 
# de Minas de la sede de Medellín y del pregrado de Diseño 
# Gráfico de la Facultad de Artes en la sede de Bogotá. 
# Esta información aportará sustancialmente a la selección de 
# la muestra conceptual para las entrevistas a profundidad que
# realizaremos con estudiantes de pregrado que han sido víctimas 
# de violencia sexual. 

# Seleccionar programas de interés
# Ingeniería Geológica - Medellín - 126
# Ingeniería de Petróleos - Medellín - 119
# Ingeniería Ambiental - Medellín - 55189
# Diseño Gráfico _ Bogotá - 4

Mat_Programas <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA %in% c(126, 119, 55189, 4))


# Variable Edad
Mat_Programas <- mutate(Mat_Programas, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
Mat_Programas <- mutate(Mat_Programas, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 

Mat_Programas_Edad <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020)) %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, CAT_EDAD) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = c(CAT_EDAD), values_from = Total) %>% 
  arrange(SNIES_PROGRA)

# Variable Estrato

Mat_Programas_Estrato <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020)) %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, ESTRATO_ORIG) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = c(ESTRATO_ORIG), values_from = Total) %>% 
  arrange(SNIES_PROGRA) %>% 
  mutate(across(.cols = c(`Estrato 1`: `Estrato 6`),
                .fns = ~replace_na(.x, 0)))
  

# Variable Sexo

Mat_Programas_Sexo <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020)) %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, SEXO) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = c(SEXO), values_from = Total) %>% 
  arrange(SNIES_PROGRA)
  
# Variable Tipo de Admisión - TIPO_ADM

Mat_Programas_Admi <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020)) %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, TIPO_ADM) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = c(TIPO_ADM), values_from = Total) %>% 
  arrange(SNIES_PROGRA) %>% 
  mutate(across(.cols = c(PAES:PEAMA),
                .fns = ~replace_na(.x, 0)))


# Variable Tipo de Admisión - TIPO_ADM - PAES

Mat_Programas_PAES <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020), TIPO_ADM == "PAES") %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, PAES) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = c(PAES), values_from = Total) %>% 
  arrange(SNIES_PROGRA) %>% 
  mutate(across(.cols = c(`Comunidades indígenas`:`Victimas del conflicto armado interno en Colombia`),
                .fns = ~replace_na(.x, 0)))


# Cruzar Bases de Datos Edad, Estrato, Sexo, TIPO_ADM, PAES

Mat_Programas_Edad
Mat_Programas_Estrato
Mat_Programas_Sexo
Mat_Programas_Admi 
Mat_Programas_PAES

c1 <- left_join(Mat_Programas_Edad, Mat_Programas_Estrato, by = c("YEAR", "SEMESTRE", "SNIES_PROGRA", "PROGRAMA"))
c2 <- left_join(c1,  Mat_Programas_Sexo, by = c("YEAR", "SEMESTRE", "SNIES_PROGRA", "PROGRAMA"))
c3 <- left_join(c2,  Mat_Programas_Admi, by = c("YEAR", "SEMESTRE", "SNIES_PROGRA", "PROGRAMA"))
Base_Final <- left_join(c3,  Mat_Programas_PAES, by = c("YEAR", "SEMESTRE", "SNIES_PROGRA", "PROGRAMA"))

write_xlsx(Base_Final, "Datos/Entrega7/Programas.xlsx")


# Variable Lugar/Municipio de Nacimiento

Mat_Programas_Naci <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020))%>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, DEP_NAC, CIU_NAC) %>% 
  summarise(Total = n()) %>% ungroup() %>% 
  mutate(across(.cols = c(DEP_NAC, CIU_NAC),
                .fns = ~replace_na(.x, "Sin información")))

write_xlsx(Mat_Programas_Naci, "C:/Users/Alberto/Documents/Sistema Estadistico/Rta_ONE/Nacimiento.xlsx")


# Variable Lugar/Municipio de Procedencia

Mat_Programas_Proc <- Mat_Programas %>% 
  filter(YEAR %in% c(2016:2020))%>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, DEP_PROC, CIU_PROC) %>% 
  summarise(Total = n()) %>% ungroup() %>% 
  mutate(across(.cols = c(DEP_PROC, CIU_PROC),
                .fns = ~replace_na(.x, "Sin información")))


write_xlsx(Mat_Programas_Proc, "C:/Users/Alberto/Documents/Sistema Estadistico/Rta_ONE/Procedencia.xlsx")

# NUEVO REQUERIMIENTO - ENTREGA BASE CRUZADA TOTAL VARIABLES DE INTERÉS

Mat_Programas_agregado <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA %in% c(126, 119, 55189, 4), 
         YEAR %in% c(2016:2020)) %>% 
  group_by(YEAR, SEMESTRE, SNIES_PROGRA, PROGRAMA, SEXO, 
           CAT_EDAD, ESTRATO_ORIG, TIPO_ADM, PAES,
           DEP_NAC, CIU_NAC, DEP_PROC, CIU_PROC) %>% 
  summarise(`Total matriculados` = n())         

write_xlsx(Mat_Programas_agregado, "Datos/Entrega7/Agregado.xlsx")



######################################-
# 8 Solicitud 23-09-2021 -----
######################################-

# Demanda : Paula Rios - Sede Manizales
# Solicitud datos de 2 programas curriculares Sede 

# Dado que la sede se está preparando para recibir visita de
# evaluación externa para la renovación de la acreditación de
#l pregrado en Ingeniería Industrial y  doctorado en ingeniería - industria y organizaciones, 
# muy cordialmente, solicitamos colaboración en el suministro de la siguiente información:
#   
# Cifras de estudiantes matriculados, 
# procedencia de los estudiantes matriculados y graduados 
# de los programas en mención.
# 
# Lo anterior, en una línea de tiempo de 5 años.

# Pregrado Ingeniería Industrial SNIES = 4124
# Doctorado en ingeniería - industria y organizaciones = 55184

# Base de datos de matriculados por programa

Mat_Programas <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA %in% c(4124, 55184), YEAR %in% c(2016:2021)) %>% 
  group_by(YEAR, SEMESTRE, NIVEL, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(Total = n()) %>% 
  arrange(desc(NIVEL))

# Base de datos de graduados por programa

Gra_Programas <- UnalData::Graduados %>% 
  filter(SNIES_PROGRA %in% c(4124, 55184), YEAR %in% c(2016:2021)) %>% 
  group_by(YEAR, SEMESTRE, NIVEL, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(Total = n()) %>% 
  arrange(desc(NIVEL))


# Variable Lugar/Municipio de Nacimiento - Matriculados

Mat_Programas_Naci <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA %in% c(4124, 55184), YEAR %in% c(2016:2021)) %>%  
  group_by(YEAR, SEMESTRE, NIVEL, SNIES_PROGRA, PROGRAMA, DEP_NAC, CIU_NAC) %>% 
  summarise(Total = n(), .groups = "drop" ) %>% 
  mutate(across(.cols = c(DEP_NAC, CIU_NAC),
                .fns = ~replace_na(.x, "Sin información")))%>% 
  arrange(desc(NIVEL))

# Variable Lugar/Municipio de Nacimiento - Graduados

Gra_Programas_Naci <- UnalData::Graduados %>% 
  filter(SNIES_PROGRA %in% c(4124, 55184), YEAR %in% c(2016:2021)) %>%  
  group_by(YEAR, SEMESTRE, NIVEL, SNIES_PROGRA, PROGRAMA, DEP_NAC, CIU_NAC) %>% 
  summarise(Total = n(), .groups = "drop" ) %>% 
  mutate(across(.cols = c(DEP_NAC, CIU_NAC),
                .fns = ~replace_na(.x, "Sin información")))%>% 
  arrange(desc(NIVEL))

# Exportar Resultados

write_xlsx(Mat_Programas, "Datos/Entrega8/Mat_Programas.xlsx")
write_xlsx(Gra_Programas, "Datos/Entrega8/Gra_Programas.xlsx")
write_xlsx(Mat_Programas_Naci, "Datos/Entrega8/Mat_Programas_Naci.xlsx")
write_xlsx(Gra_Programas_Naci, "Datos/Entrega8/Gra_Programas_Naci.xlsx")


######################################-
# 9 Solicitud 28-09-2021 -----
######################################-

# Demanda : Paola Andrea Ribero Ortega - ENEL - Codensa
# Solicitud datos para PROYECTO ENEL - MUJERES EN CARRERAS STEM

# Buenas tardes, estimados. Cordial saludo.
# 
# El Grupo Enel creemos en la diversidad y actualmente 
# estamos trabajando fuertemente en abrir excelentes 
# oportunidades para que cada vez más mujeres talentosas 
# se puedan unir a nuestro equipo y puedan aportar en un 
# sector que históricamente ha sido liderado en su gran 
# mayoría por hombres. Es por esta razón, que hemos 
# implementado diferentes acciones para lograr este 
# objetivo, y en esta oportunidad ustedes son un actor 
# fundamental para lograrlo.
# 
# A través de su pagina web pudimos encontrar información 
# referente al número de personas matriculadas y graduadas 
# en los diferentes periodos académicos desde el 2009
# hasta el 2021, pero necesitamos conocer la cantidad de 
# hombres y mujeres matriculados y egresados por facultad, 
# para el estudio previo que estamos llevando a cabo. Es por
# esto que quisiéramos pedirles apoyo con el envío de una 
# base de datos que contenga esta información con respecto 
# a los periodos académicos de los últimos 5 años, para 
# las facultades de Enfermería, Medicina, Ciencias, 
# Ingeniería, y Ciencias Económicas únicamente.

# Base de datos de matriculados por Facultad y Sexo

Mat_Programas <- UnalData::Matriculados %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  filter(SEDE_NOMBRE_MAT == "Bogotá",
         FACULTAD %in% c("Ingeniería", "Enfermería", "Medicina", "Ciencias", "Ciencias económicas"),
         TIPO_NIVEL == "Pregrado",
         YEAR %in% c(2016:2021),
         MAT_PVEZ == "Sí") %>% 
  group_by(PERIODO, SEXO, FACULTAD) %>% 
  summarise(Total = n()) %>%
  pivot_wider(names_from = c(FACULTAD, SEXO), values_from = Total) %>% 
  relocate(c(4, 9, 6, 11, 2, 7, 5, 10, 3, 8), .after = 1)


# Base de datos de graduados por Facultad y Sexo

Gra_Programas <- UnalData::Graduados %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  filter(SEDE_NOMBRE_MAT == "Bogotá",
         FACULTAD %in% c("Ingeniería", "Enfermería", "Medicina", "Ciencias", "Ciencias económicas"),
         TIPO_NIVEL == "Pregrado",
         YEAR %in% c(2016:2021)) %>% 
  group_by(PERIODO, SEXO, FACULTAD) %>% 
  summarise(Total = n()) %>%
  pivot_wider(names_from = c(FACULTAD, SEXO), values_from = Total) %>% 
  relocate(c(4, 9, 6, 11, 2, 7, 5, 10, 3, 8), .after = 1) %>% 
  arrange(desc(PERIODO))


# Exportar Resultados

write_xlsx(Mat_Programas, "Datos/Entrega9/Mat_Facultades.xlsx")
write_xlsx(Gra_Programas, "Datos/Entrega9/Gra_Facultades.xlsx")


######################################-
# 10 Solicitud 27-10-2021 -----
######################################-

# Total de municipios con estudiantes matriculados

names(UnalData::Matriculados)

View(UnalData::Matriculados %>% mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-" )) %>% 
  filter(Periodo == "2021-1") %>% 
  group_by(COD_CIU_NAC) %>% count())

######################################-
# 11 Solicitud 09-11-2021 -----
######################################-

# Total de graduados de la Sede Orinoquía

Grad_Orinoquia <- UnalData::Graduados %>% filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == "Orinoquía") %>% 
                  select(c("ID", "TID", "YEAR", "SEMESTRE", "EDAD_MOD", "CAT_EDAD", 
                           "DEP_NAC", "COD_DEP_NAC", "CIU_NAC", "COD_CIU_NAC",
                           "SEDE_NOMBRE_MAT", "FACULTAD", "SNIES_PROGRA", "PROGRAMA"))


# Exportar Resultados

write_xlsx(Grad_Orinoquia, "C:/Users/Alberto/Documents/Sistema Estadistico/Rta_ONE/Gra_Orinoquia.xlsx")

######################################-
# 12 Solicitud 16-11-2021 -----
######################################-

# Demanda : Luis Alfredo Montes - Director Área Curricular
# 

# Buenos días:
  
# Cordial saludo, para fines de nuestro proceso de autoevaluación y construcción del informe de gestión 2021, 
# amablemente solicito la siguiente información:
  
# Número de admitidos 2021 de los programas PAES y PEAMA, 
# indicando sede de origen.

##########################-
# Programa de Geología
##########################-

# Admitidos PEAMA 2021 

Adm_PEAMA <- UnalData::Aspirantes %>% 
             filter(SNIES_PROGRA == 34, YEAR == 2021, TIPO_INS == "PEAMA") %>% 
             mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
             group_by(SNIES_PROGRA,	PROGRAMA, PERIODO, PEAMA) %>% 
             summarise(Total = n(), .groups = "drop") %>% 
             pivot_wider(names_from = c(PEAMA), values_from = Total, values_fill = 0) 

# Admitidos PAES 2021 

Adm_PAES <- UnalData::Aspirantes %>% 
  filter(SNIES_PROGRA == 34, YEAR == 2021, TIPO_INS == "PAES") %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  group_by(SNIES_PROGRA,	PROGRAMA, PERIODO, PAES) %>% 
  summarise(Total = n(), .groups = "drop") %>% 
  pivot_wider(names_from = c(PAES), values_from = Total, values_fill = 0)

Adm_Geologia <- left_join(Adm_PEAMA, Adm_PAES, by = c("PERIODO","SNIES_PROGRA", "PROGRAMA"))

write_xlsx(Adm_Geologia, "Datos/Entrega12/Adm_Geologia.xlsx")

#####################-
# Sede Bogotá
#####################-

# Admitidos PEAMA 2021 

Adm_PEAMA_Bog <- UnalData::Aspirantes %>% 
  filter(TIPO_INS == "PEAMA", ADM_ANDINA_PEAMA == "Bogotá", YEAR == 2021) %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  group_by(ADM_ANDINA_PEAMA, PERIODO, PEAMA) %>% 
  summarise(Total = n(), .groups = "drop") %>% 
  pivot_wider(names_from = c(PEAMA), values_from = Total, values_fill = 0) 

# Admitidos PAES 2021 

Adm_PAES_Bog <- UnalData::Aspirantes %>% 
  filter(TIPO_INS == "PAES", ADM_SEDE_NOMBRE == "Bogotá" , YEAR == 2021) %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  group_by(ADM_SEDE_NOMBRE, PERIODO, PAES) %>% 
  summarise(Total = n(), .groups = "drop") %>% 
  pivot_wider(names_from = c(PAES), values_from = Total, values_fill = 0)


######################################-
# 13 Solicitud 22-11-2021 -----
######################################-

# Asunto: Derecho de Petición
# NOMBRE DEL PETICIONARIO: Carlos Alberto
# APELLIDO DEL PETICIONARIO: Pulgarin Galvis
# Estudiante del programa de Biología
# Universidad del Quindío
# TIPO DE IDENTIFICACIÓN:
# CÉDULA DE CIUDADANÍA NÚMERO: 1094929876
# DIRECCIÓN:
# BARRIO: Portal de Pinares Manzana 16 casa 3
# MUNICIPIO DÓNDE RESIDE: Armenia
# CORREO ELECTRÓNICO: carlosa.pulgaring@uqvirtual.edu.co
# TELÉFONOS DE CONTACTO: 3122207799

# Solicito amablemente
# La siguiente información referente al Programa de Biología que oferta su universidad
# Costo en pesos de la matricula académica.
# Número de egresados acumulado hasta la fecha desde la creación del programa.
# Número de egresados del año más reciente.
# Número de estudiantes actuales en el programa.
# Año en que se inició el programa.

# Selecionar base de datos de graduados en Biología

Grad_Biologia <- UnalData::Graduados %>% 
  filter(SNIES_PROGRA == 31) %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  group_by(SNIES_PROGRA,	PROGRAMA, PERIODO) %>% 
  summarise(Total = n(), .groups = "drop") %>% 
  arrange(PERIODO)

######################################-
# 14 Solicitud 11-01-2022 -----
######################################-

# Respuesta Derecho de Petición
# Guillem Frederick Solo
# C.C. 1.095.929.687
# Memorando de Rectoría 15 de 2022

# 1. ¿Cuál es la edad promedio de los jóvenes que obtienen su título de pregrado en la
# institución? En caso de no poseer la información dejarlo claro en su respuesta.
# 
# 2. Clasificación de la plata docente discriminado por máximo nivel académico logrado, es decir,
# cuantos cuentan con doctorados, cuantos con maestría, cuantos con especialización o solo
# con título de pregrado.
# 
# 3. Se precisa conocer datos de la infraestructura física tales como: número de salones de clase
# de clases, número de laboratorios, área total construida en metros cuadrados. 

# EDAD PROMEDIO GRADUADOS PREGRADO

Edad_graduados <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado") %>% 
  group_by(YEAR, SEMESTRE) %>% 
  summarise('Total Graduado' = n(),
            'Edad Promedio' = round(mean(EDAD_MOD, na.rm = TRUE), 2)) %>% 
  arrange(desc(YEAR), desc(SEMESTRE))

# MÁXIMO NIVEL DE FORMACIÓN DOCENTES

Formacion_Docentes <- UnalData::Docentes %>% 
  group_by(YEAR, SEMESTRE, FORMACION) %>% 
  summarise('Total Docentes' = n())

# Exportar resultados

write_xlsx(Edad_graduados, "Datos/Entrega14/Edad_Graduados.xlsx")
write_xlsx(Formacion_Docentes, "Datos/Entrega14/Formacion_Docentes.xlsx")

######################################-
# 15 Solicitud 12-01-2022 -----
######################################-


# Demandante: Angie Geraldin Aldana Fandino

# Buenas tardes señores Estadistica UNAL 
# 
# Mi nombre es Angie Aldana, estudiante de medicina veterinaria de la Universidad Nacional sede Bogotá, me encuentro realizando una encuesta para la asignatura de salud pública, para lo cual necesito saber cuántos estudiantes están matriculados SOLAMENTE en la carrera de Medicina Veterinaria, hasta el año 2021, 
# ya que consulté la plataforma estadística sin embargo me aparecen estudiantes de "veterinaria y zootecnia" juntos.  
# 
# Agradezco su atención y quedo atenta a sus comentarios  

# CONSULTA MATRICULADOS MEDICINA VETERINARIA Y ZOOTECNIA

Matricula_MVZ <- UnalData::Matriculados %>% filter(SNIES_PROGRA == 2) %>% group_by(YEAR, SEMESTRE) %>% 
                 count(name = "Total Matriculados") %>% 
                 arrange(desc(YEAR), desc(SEMESTRE))

# Exportar resultados

write_xlsx(Matricula_MVZ, "Datos/Entrega15/Matricula_MVZ.xlsx")

######################################-
# 16 Solicitud 18-01-2022 -----
######################################-

# Demandante: SOLÁNGEL MARÍN PEÑARANDA - Secretaria de Sede - Sede Amazonia

# Buenos días Alberto, mi nombre es Solangel Marin Pañaranda, 
# Secretaría de Sede. 
# Mi solicitud respetuosa es que desde la sede Amazonía se 
# requiere el reporte de graduados de los periodos 2020-1, 2020-2, 2021-1 y 2021-2,
# por lo que teniendo en cuenta que la Dirección de Planeación es la dependencia que
# emite las cifras oficiales de la Universidad, es muy importante contar con dicha 
# información oficial para los respectivos fines y actividades.

# Microdatos

  Grad_Amazonia <- UnalData::Graduados %>% 
  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>%
  filter(SNIES_SEDE_ADM == 1125, YEAR %in% c(2020, 2021))

# Consolidado

Con_Grad_Amazonia <- UnalData::Graduados %>% 
                 mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>%
                 filter(SNIES_SEDE_ADM == 1125, YEAR %in% c(2020, 2021)) %>% 
                 group_by(PERIODO, TIPO_NIVEL) %>%  
                 summarise(Total = n()) %>%
                 pivot_wider(names_from = c(TIPO_NIVEL), values_from = Total)

# Exportar resultados

write_xlsx(Grad_Amazonia, "Datos/Entrega16/Grad_Amazonia.xlsx")
write_xlsx(Con_Grad_Amazonia, "Datos/Entrega16/Con_Grad_Amazonia.xlsx")


######################################-
# 17 Solicitud 19-01-2022 -----
######################################-

# Demanda: Maria Claudia Galindo Gonzales
# Análisis Sisben IV - Impacto en la UNAL

# Importar y transformar datos de pregrado

SisbenIV <- read_excel("Datos/Fuentes/SisbenIV.xlsx") %>% 
            filter(NIVEL == "PREGRADO") %>% 
            mutate(SEXO = case_when(SEXO == 'F' ~ "Femenino",
                                    SEXO == 'M' ~ "Masculino"),
                   CAT_PBM = case_when(PBM <= 11 ~ "11 o menos",
                                       between(PBM, 12, 17) ~ "12 a 17",
                                       between(PBM, 18, 50) ~ "18 a 50",
                                       between(PBM, 51, 100) ~ "51 a 100",
                                       is.na(PBM) ~ "Sin información"),
                   CAT_EDAD = case_when(EDAD <= 17 ~ "17 años o menos",
                                       between(EDAD, 18, 20) ~ "18 a 20 años",
                                       between(EDAD, 21, 25) ~ "21 a 25 años",
                                       EDAD >= 26 ~ "26 o más años")

                   )

# Construcción de Gráficos


# ANÁLISIS SISBEN

# Estudiantes Sisbenizados

# Obtener las posiciones

Sisben <- SisbenIV %>% 
  group_by(SISBEN) %>%
  summarize(Total = n()) %>% 
  mutate(pct = Total/sum(Total),
         Porcentaje = scales::percent(pct),
         Porcentaje = paste(Porcentaje, "\n", paste0("(", "N=", Total, ")"))) 


ggplot(Sisben, aes(x = "" , y = Total, fill = fct_inorder(SISBEN))) +
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(vjust = -1.6, y = Total, label= Porcentaje), col = "white", size = 5, fontface = "bold")+
  scale_fill_manual(values = c("#377eb8", "#e41a1c"))+
  labs(fill = "¿Con Sisben IV?")+
  theme_void() +
  ggtitle("Proporción de estudiantes matriculados en pregrado en la UNAL con Sisben IV",
          subtitle = "Periodo 2021-1")+
  theme(legend.position = c(1,0.1),
        legend.title = element_text(size=13),
        legend.text = element_text(size=13))+
  
# Grupos Sisben

Sisben_grupo <- SisbenIV %>% 
  filter(SISBEN == "Sí") %>% 
  group_by(GRUPO_S) %>%
  summarize(Total = n()) %>% 
  mutate(pct = Total/sum(Total),
         Porcentaje = scales::percent(pct),
         Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 

ggplot(Sisben_grupo, aes(x = GRUPO_S, y=pct)) + 
  geom_bar(stat = 'identity', fill = "#74c476")+
  scale_y_continuous(limits=c(0, 1), 
                     breaks = seq(0, 1, .2), 
                     label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  geom_text(aes(label = Porcentaje), 
            size = 4, 
            position = position_stack(vjust = 0.5)) +
  ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n por grupos Sisben") +
  xlab("Grupo Sisben") + ylab("Porcentaje \n ")+
  theme_stata() +
  labs(fill = "¿Con Sisben IV?")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
    axis.title.x = element_text(color="black", size=13, face="bold"),
    axis.title.y = element_text(color="black", size=13, face="bold" ),
    legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))+
    annotate("text", x = 1:4, y = c(0.20, 0.37, 0.53, 0.35), angle = 90, color = "red", 
           label = c("Pobreza\nextrema", "Pobreza\nmoderada", "Vulnerable", "Población no pobre,\nno vulnerable"), 
           size=4, fontface = 'bold') 
  

# ANÁLISIS DISTRIBUCIÓN SISBEN POR VARIABLES

# Distribución Sisben IV - Por sexo

  Sisben_sex <- SisbenIV %>%
  group_by(SEXO, SISBEN) %>%
  summarize(Total = n()) %>% 
  mutate(pct = Total/sum(Total),
         Porcentaje = scales::percent(pct),
         Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 

  ggplot(Sisben_sex, aes(x = SEXO, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n por sexo") +
    xlab("Sexo") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))

# Distribución Sisben IV - Por Estrato
  
  Sisben_estrato <- SisbenIV %>%
    group_by(ESTRATO, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")")))  
  
  ggplot(Sisben_estrato, aes(x = ESTRATO, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n por estrato") +
    xlab(" \n Estrato") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))
  
# Distribución Sisben IV - Tipología del colegio
  
  Sisben_colegio <- SisbenIV %>%
    group_by(TIPCOLEGIO, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 
  
  ggplot(Sisben_colegio, aes(x = TIPCOLEGIO, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n por tipología de colegio") +
    xlab(" \n Tipo Colegio") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))
  
# Distribución Sisben IV - Programa PAES
  
  Sisben_paes <- SisbenIV %>%
    group_by(PAES, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")")))  
  
  ggplot(Sisben_paes, aes(x = PAES, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Programa PAES") +
    xlab(" \n Integrantes Programa PAES") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))

# Distribución Sisben IV - Modalidades PAES
  
  Sisben_paes_mod <- SisbenIV %>%
    filter(PAES == "Sí") %>% 
    group_by(MOD_PAES, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")")))  
  
  ggplot(Sisben_paes_mod, aes(x = MOD_PAES, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Modalidades PAES") +
    xlab(" \n Integrantes Modalidades Programa PAES") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))
  

# Distribución Sisben IV - Programa PEAMA
  
  Sisben_peama <- SisbenIV %>%
    group_by(PEAMA, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 
  
  ggplot(Sisben_peama, aes(x = PEAMA, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Programa PEAMA") +
    xlab(" \n Integrantes Programa PEAMA") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))

# Distribución Sisben IV - PBM
  
  Sisben_pbm <- SisbenIV %>%
    filter(CAT_PBM != "Sin información") %>% 
    group_by(CAT_PBM, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 
  
  ggplot(Sisben_pbm, aes(x = CAT_PBM, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Grupos Puntaje Básico de Matrícula (PBM)") +
    xlab(" \n Puntaje Básico de Matrícula (PBM)") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))

# Distribución Sisben IV - PBM11
  
  Sisben_PBM11 <- SisbenIV %>% 
    filter(SISBEN == "Sí", CAT_PBM == "11 o menos") %>% 
    group_by(GRUPO_S) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 
  
  ggplot(Sisben_PBM11, aes(x = GRUPO_S, y=pct)) + 
    geom_bar(stat = 'identity', fill = "#74c476")+
    scale_y_continuous(limits=c(0, 1), 
                       breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Grupos Sisben Estudiantes con PBM <= 11 (11 o menos)") +
    xlab("Grupo Sisben") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))+
      annotate("text", x = 1:4, y = c(0.27, 0.45, 0.45, 0.25), angle = 90, color = "red", 
             label = c("Pobreza\nextrema", "Pobreza\nmoderada", "Vulnerable", "Población no pobre,\nno vulnerable"), 
             size=4, fontface = 'bold') 
  
# Distribución Sisben IV - EDAD
  
Sisben_edad <- SisbenIV %>%
    filter(CAT_PBM != "Sin información") %>% 
    group_by(CAT_EDAD, SISBEN) %>%
    summarize(Total = n()) %>% 
    mutate(pct = Total/sum(Total),
           Porcentaje = scales::percent(pct),
           Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 
  
ggplot(Sisben_edad, aes(x = CAT_EDAD, y=pct, fill=SISBEN)) + 
    geom_bar(position = "fill", stat = 'identity')+
    geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, .2), 
                       label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
    geom_text(aes(label = Porcentaje), 
              size = 4, 
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set2")+
    ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n Grupos de edad") +
    xlab(" \n Grupos de edad") + ylab("Porcentaje \n ")+
    theme_stata() +
    labs(fill = "¿Con Sisben IV?")+
    theme(
      plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
      axis.title.x = element_text(color="black", size=13, face="bold"),
      axis.title.y = element_text(color="black", size=13, face="bold" ),
      legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))   

# Distribución Sisben IV - SEDES

Sisben_sedes <- SisbenIV %>%
  group_by(SEDE, SISBEN) %>%
  summarize(Total = n()) %>% 
  mutate(pct = Total/sum(Total),
         Porcentaje = scales::percent(pct),
         Porcentaje = paste(Porcentaje, "\n", paste0("(", Total, ")"))) 

ggplot(Sisben_sedes, aes(x = SEDE, y=pct, fill=SISBEN)) + 
  geom_bar(position = "fill", stat = 'identity')+
  geom_hline(yintercept = 0.34, linetype="dashed", color = "red", size = 0.5) +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = c('0%', '20%', '40%', '60%', '80%', '100%')) +
  geom_text(aes(label = Porcentaje), 
            size = 4, 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Estudiantes Pregrado UNAL en la base Sisben IV \n  Sedes de la Universidad") +
  xlab(" \n Sedes UNAL") + ylab("Porcentaje \n ")+
  theme_stata() +
  labs(fill = "¿Con Sisben IV?")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5 ),
    axis.title.x = element_text(color="black", size=13, face="bold"),
    axis.title.y = element_text(color="black", size=13, face="bold" ),
    legend.title = element_text(colour="blue", size=10, face="bold", vjust = 0.5))   

######################################-
# 18 Solicitud 11-02-2022 -----
######################################-

# Demanda: Sistema de Quejas Sede Bogotá
# A1.	Conocer el número de admitidos por posgrado en cada programa y en cada semestre de la sede Bogotá desde el 2010 al 2021
# Peticionario: Mg. Psic. Olmo Jesús Sierra Moreno
# CC: 1020771265
# Tel.: 300563530
# CORREO: ojsierram@unal.edu.co

Base_Pos_Bog <- UnalData::Aspirantes %>% 
                filter(YEAR >= 2010, 
                       MOD_INS == "Regular",
                       TIPO_NIVEL == "Postgrado",
                       SNIES_SEDE == 1101) %>% 
                group_by(YEAR, SEMESTRE, NIVEL, ADMITIDO, SNIES_PROGRA, PROGRAMA) %>% 
                group_by(YEAR, SEMESTRE, NIVEL, ADMITIDO, SNIES_PROGRA, PROGRAMA) %>% 
                count(name = "Total") %>% 
                pivot_wider(names_from = c(YEAR, SEMESTRE, ADMITIDO), values_from = Total, values_fill = 0) %>% 
                arrange(SNIES_PROGRA)
    

# Exportar resultados

write_xlsx(Base_Pos_Bog, "Datos/Entrega18/Base_Pos_Bog.xlsx")

######################################-
# 19 Solicitud 30-03-2022 -----
######################################-

# Demanda: DNPE Jose Ignacio Maya
# Demanda: Cifras de las facultades de Ciencias Medellín y
# Ciencias Exactas y Naturales de Manizales
# Para propuesta modificación CA estructura académico administrativas


# Evolución de aspirantes a postgrado Sede Manizales

View(UnalData::Aspirantes %>% filter(TIPO_INS == "Regular",
                                ADM_SEDE_NOMBRE == "Manizales", 
                                TIPO_NIVEL == "Postgrado", 
                                FACULTAD == "Ciencias exactas y naturales") %>% 
                         group_by(YEAR, SEMESTRE) %>% count())

names(UnalData::Aspirantes)
unique(UnalData::Aspirantes$TIPO_INS)

######################################-
# 20 Solicitud 21-04-2022 -----
######################################-

# Demanda: Mónica Mantilla
# Descripción: ajustar/corregir, en el documento del PGD y el PLEI, el gráfico
# de tendencias de la evolución de programas académcios en la Universidad

Programas <- read_excel("Datos/Fuentes/Serie_Programas.xlsx") %>% 
             pivot_longer(cols = Pregrado:Total, names_to = "Clase", values_to = "Total") %>% 
             arrange(Clase) %>% 
             mutate(Variable = ifelse(Clase == "Total", "TOTAL", Variable)) 

# Serie General 

Plot.Series(
  datos     = Programas,
  categoria = "TOTAL",
  colores   = c("#0071bc"),
  titulo    = "Evolución de programas académicos",
  labelY    = "N\u00famero de Programas"
)

# Serie por nivel de formación

Plot.Series(
    datos     = Programas,
    categoria = "TIPO_NIVEL",
    colores   = c("#f15a24", "#8cc63f"),
    titulo    = "Evolución de programas académicos por nivel de formación",
    labelY    = "N\u00famero de Programas"
  )

######################################-
# 21 Solicitud 26-04-2022 -----
######################################-

# Demanda: RICHARD MORENO RODRIGUEZ -
# CONSEJO NACIONAL DE PAZ AFROCOLOMBIANO - CONPA
# DERECHO DE PETICIÓN CON EL FIN DE OBTENER 
# INFORMACIÓN, SOBRE EL ACCESO, PERMANENCIA Y GRADUACIÓN 
# DE LAS COMUNIDADES NEGRAS, AFROCOLOMBIANAS, RAIZALES Y 
# PALENQUERAS QUE SE ENCUENTRAN EN LA EDUCACIÓN SUPERIOR.

######################################-
# 22 Solicitud 06-05-2022 -----
######################################-

# Demanda: DNPE - Director
# Propósito: Análisis Saber Pro país e institución
# Análisis aspirantes y admitidos de pregrado

# Importar datos

SaberPais2016 <- read_excel("Datos/SaberPais/SaberPais2016.xlsx", 
                            sheet = "Saber2016",
                            guess_max = 300000)

SaberPais2016 <- SaberPais2016 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                           ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE, 
                                           ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATO_VIVIENDA, INST_COD_INSTITUCION,
                                           INST_NOMBRE_INSTITUCION, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                           ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, 
                                           MOD_RAZONA_CUANTITAT_PUNT, MOD_LECTURA_CRITICA_PUNT, 
                                           MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT, MOD_COMUNI_ESCRITA_PUNT)) |> 
                                           mutate(MOD_COMUNI_ESCRITA_PUNT = replace_na(MOD_COMUNI_ESCRITA_PUNT, 0),
                                                  PUNT_GLOBAL = round((MOD_RAZONA_CUANTITAT_PUNT +
                                                                 MOD_LECTURA_CRITICA_PUNT +
                                                                 MOD_COMPETEN_CIUDADA_PUNT +
                                                                 MOD_INGLES_PUNT +
                                                                 MOD_COMUNI_ESCRITA_PUNT)/5, 0),
                                                  YEAR = 2016) |> 
                                          rename(FAMI_ESTRATOVIVIENDA = FAMI_ESTRATO_VIVIENDA)

SaberPais2016$ESTU_FECHANACIMIENTO <- as.character(SaberPais2016$ESTU_FECHANACIMIENTO)


SaberPais2017 <- read_excel("Datos/SaberPais/SaberPais2017.xlsx", 
                            sheet = "Saber2017",
                            guess_max = 300000)

SaberPais2017 <- SaberPais2017 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                          ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE,
                                          ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATOVIVIENDA, INST_COD_INSTITUCION, 
                                          INST_NOMBRE_INSTITUCION, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                          ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, MOD_RAZONA_CUANTITAT_PUNT,
                                          MOD_LECTURA_CRITICA_PUNT, MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT,
                                          MOD_COMUNI_ESCRITA_PUNT, PUNT_GLOBAL)) |> 
                                  mutate(YEAR = 2017)

SaberPais2018 <- read_excel("Datos/SaberPais/SaberPais2018.xlsx", 
                            sheet = "Saber2018",
                            guess_max = 300000)

SaberPais2018 <- SaberPais2018 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                           ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE,
                                           ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATOVIVIENDA, INST_COD_INSTITUCION, 
                                           INST_NOMBRE_INSTITUCION, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                           ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, MOD_RAZONA_CUANTITAT_PUNT,
                                           MOD_LECTURA_CRITICA_PUNT, MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT,
                                           MOD_COMUNI_ESCRITA_PUNT, PUNT_GLOBAL)) |> 
                                           mutate(YEAR = 2018)

SaberPais2019 <- read_excel("Datos/SaberPais/SaberPais2019.xlsx", 
                            sheet = "Saber2019",
                            guess_max = 300000)

SaberPais2019 <- SaberPais2019 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                           ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE,
                                           ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATOVIVIENDA, INST_COD_INSTITUCION, 
                                           INST_NOMBRE_INSTITUCION, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                           ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, MOD_RAZONA_CUANTITAT_PUNT,
                                           MOD_LECTURA_CRITICA_PUNT, MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT,
                                           MOD_COMUNI_ESCRITA_PUNT, PUNT_GLOBAL)) |> 
                                           mutate(YEAR = 2019)


SaberPais2020 <- read_excel("Datos/SaberPais/SaberPais2020.xlsx", 
                            sheet = "Saber2020",
                            guess_max = 300000)

SaberPais2020 <- SaberPais2020 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                           ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE,
                                           ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATOVIVIENDA, INST_COD_INSTITUCION,
                                           NOMBRE_IES, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                           ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, MOD_RAZONA_CUANTITAT_PUNT,
                                           MOD_LECTURA_CRITICA_PUNT, MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT,
                                           MOD_COMUNI_ESCRITA_PUNT, PUNT_GLOBAL)) |> 
                                   rename(INST_NOMBRE_INSTITUCION = NOMBRE_IES) |> 
                                   mutate(YEAR = 2020)

SaberPais2021 <- read_excel("Datos/SaberPais/SaberPais2021.xlsx", 
                            sheet = "Saber2021",
                            guess_max = 300000)

SaberPais2021 <- SaberPais2021 |> select(c(ESTU_TIPODOCUMENTO, PERIODO, ESTU_FECHANACIMIENTO, ESTU_GENERO, ESTU_CONSECUTIVO,
                                           ESTU_DEPTO_RESIDE, ESTU_COD_RESIDE_DEPTO, ESTU_MCPIO_RESIDE,
                                           ESTU_COD_RESIDE_MCPIO, FAMI_ESTRATOVIVIENDA, INST_COD_INSTITUCION,
                                           INST_NOMBRE_INSTITUCION, ESTU_PRGM_ACADEMICO, ESTU_SNIES_PRGMACADEMICO,
                                           ESTU_METODO_PRGM, INST_CARACTER_ACADEMICO, INST_ORIGEN, MOD_RAZONA_CUANTITAT_PUNT,
                                           MOD_LECTURA_CRITICA_PUNT, MOD_COMPETEN_CIUDADA_PUNT, MOD_INGLES_PUNT,
                                           MOD_COMUNI_ESCRITA_PUNT, PUNT_GLOBAL)) |> 
                                           mutate(YEAR = 2021)


# Unir bases de datos históricas Saber Pro

SaberPro <- bind_rows(SaberPais2016, SaberPais2017,
                       SaberPais2018, SaberPais2019,
                       SaberPais2020, SaberPais2021)

# Crear variable UNAL

SaberPro <- SaberPro %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UNAL",
                           TRUE ~ "Resto IES"))

# Crear variable Sedes UNAL

SaberPro <- SaberPro %>% 
  mutate(Sunal =  case_when(.$INST_COD_INSTITUCION == 1101  ~ "UNAL-Bogota",
                            .$INST_COD_INSTITUCION == 1102  ~ "UNAL-Medellin",
                            .$INST_COD_INSTITUCION == 1103  ~ "UNAL-Manizales",
                            .$INST_COD_INSTITUCION == 1104  ~ "UNAL-Palmira",
                            TRUE ~ "Resto IES"))
# Crear G12

SaberPro <- SaberPro %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1711  ~ "U. Sabana",
                          .$INST_COD_INSTITUCION == 2813  ~ "EIA - Medellín",
                          .$INST_COD_INSTITUCION == 2704  ~ "CESA - Bogotá",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))


# Crear G15 (sedes Unal)

SaberPro <- SaberPro %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1711  ~ "U. Sabana",
                          .$INST_COD_INSTITUCION == 2813  ~ "EIA - Medellín",
                          .$INST_COD_INSTITUCION == 2704  ~ "CESA - Bogotá",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UNAL-Bogota",
                          .$INST_COD_INSTITUCION == 1102  ~ "UNAL-Medellin",
                          .$INST_COD_INSTITUCION == 1103  ~ "UNAL-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UNAL-Palmira",
                          TRUE ~ "Resto IES"))

# Crear SUE General

SaberPro <- SaberPro %>% 
   mutate(SUE =  case_when(.$INST_COD_INSTITUCION %in%  c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920, 9933, 1105,
                                                          1106, 1107, 1108, 1109, 1110, 1111, 1112, 1113, 1114, 1115,
                                                          1117, 1118, 1119, 1120, 1123, 1121, 1122, 1201, 1219, 1220,
                                                          1221, 1222, 1223, 9125, 1202, 1203, 1204, 1205, 1206, 1207,
                                                          1208, 1209, 1210, 1212, 1213, 1214, 1215, 1216, 1121, 1217,
                                                          1218, 1301, 2102, 9929, 2743) ~ "SUE",
                                                          TRUE ~ "Resto IES"))


# Crear SUE + UNAL + General


SaberPro <- SaberPro %>% 
  mutate(SUE_UNAL =  case_when(.$INST_COD_INSTITUCION %in%  c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UNAL",
                               .$INST_COD_INSTITUCION %in%  c(9933, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112, 
                                                              1113, 1114, 1115, 1117, 1118, 1119, 1120, 1123, 1121, 
                                                              1122, 1201, 1219, 1220, 1221, 1222, 1223, 9125, 1202, 
                                                              1203, 1204, 1205, 1206, 1207, 1208, 1209, 1210, 1212, 
                                                              1213, 1214, 1215, 1216, 1121, 1217, 1218, 1301, 2102, 
                                                              9929, 2743) ~ "Resto SUE",
                                                              TRUE ~ "Resto IES"))

# Crear SUE Puro (Cada una de las IES del SUE)

SaberPro <- SaberPro %>% 
  mutate(SUE_UNO =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UNAL",
                               .$INST_COD_INSTITUCION %in% c(1105) ~ "Pedagógica",
                               .$INST_COD_INSTITUCION %in% c(1106, 1107, 1108, 1109) ~ "UPTC",
                               .$INST_COD_INSTITUCION %in% c(1110) ~ "Cauca",
                               .$INST_COD_INSTITUCION %in% c(1111) ~ "UTP",
                               .$INST_COD_INSTITUCION %in% c(1112) ~ "Caldas",
                               .$INST_COD_INSTITUCION %in% c(1113) ~ "Córdoba",
                               .$INST_COD_INSTITUCION %in% c(1114) ~ "Surcolombiana",
                               .$INST_COD_INSTITUCION %in% c(1115) ~ "Amazonía",
                               .$INST_COD_INSTITUCION %in% c(1117) ~ "Militar",
                               .$INST_COD_INSTITUCION %in% c(1118) ~ "Tecn. Choco",
                               .$INST_COD_INSTITUCION %in% c(1119) ~ "Llanos",
                               .$INST_COD_INSTITUCION %in% c(1120, 1123) ~ "Pop. Cesar",
                               .$INST_COD_INSTITUCION %in% c(1121) ~ "ColMay. C/marca",
                               .$INST_COD_INSTITUCION %in% c(1122) ~ "Pacífico",
                               .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1222, 1223, 9125)  ~ "U. Antioquia",
                               .$INST_COD_INSTITUCION %in% c(1202) ~ "Atlantico",
                               .$INST_COD_INSTITUCION %in% c(1203) ~ "Valle",
                               .$INST_COD_INSTITUCION %in% c(1204) ~ "UIS",
                               .$INST_COD_INSTITUCION %in% c(1205) ~ "Cartagena",
                               .$INST_COD_INSTITUCION %in% c(1206) ~ "Nariño",
                               .$INST_COD_INSTITUCION %in% c(1207) ~ "Tolima",
                               .$INST_COD_INSTITUCION %in% c(1208) ~ "Quindio",
                               .$INST_COD_INSTITUCION %in% c(1209) ~ "UFPS-Cúcuta",
                               .$INST_COD_INSTITUCION %in% c(1210) ~ "UFPS-Ocaña",
                               .$INST_COD_INSTITUCION %in% c(1212) ~ "Pamplona",
                               .$INST_COD_INSTITUCION %in% c(1213) ~ "Magdalena",
                               .$INST_COD_INSTITUCION %in% c(1214, 1215, 1216) ~ "Cundinamarca",
                               .$INST_COD_INSTITUCION %in% c(1217) ~ "Sucre",
                               .$INST_COD_INSTITUCION %in% c(1218) ~ "Guajira",
                               .$INST_COD_INSTITUCION %in% c(1301) ~ "Distrital",
                               .$INST_COD_INSTITUCION %in% c(2102) ~ "UNAD",
                               .$INST_COD_INSTITUCION %in% c(9929) ~ "Indigena",
                               .$INST_COD_INSTITUCION %in% c(2743) ~ "Unitropico",
                               TRUE ~ "Resto IES"))


# Crear SUE + UNAL_SEDES

SaberPro <- SaberPro %>% 
  mutate(SUE_UNO_SUNAL =  case_when(.$INST_COD_INSTITUCION %in% c(1101) ~ "UNAL-Bogotá",
                              .$INST_COD_INSTITUCION %in% c(1102) ~ "UNAL-Medellín",
                              .$INST_COD_INSTITUCION %in% c(1103) ~ "UNAL-Manizales",
                              .$INST_COD_INSTITUCION %in% c(1104) ~ "UNAL-Palmira",
                              .$INST_COD_INSTITUCION %in% c(1124) ~ "UNAL-Orinoquía",
                              .$INST_COD_INSTITUCION %in% c(1125) ~ "UNAL-Amazonía",
                              .$INST_COD_INSTITUCION %in% c(1126) ~ "UNAL-Caribe",
                              .$INST_COD_INSTITUCION %in% c(9920) ~ "UNAL-Tumaco",
                              .$INST_COD_INSTITUCION %in% c(9933) ~ "UNAL-La Paz",
                              .$INST_COD_INSTITUCION %in% c(1105) ~ "Pedagógica",
                              .$INST_COD_INSTITUCION %in% c(1106, 1107, 1108, 1109) ~ "UPTC",
                              .$INST_COD_INSTITUCION %in% c(1110) ~ "Cauca",
                              .$INST_COD_INSTITUCION %in% c(1111) ~ "UTP",
                              .$INST_COD_INSTITUCION %in% c(1112) ~ "Caldas",
                              .$INST_COD_INSTITUCION %in% c(1113) ~ "Córdoba",
                              .$INST_COD_INSTITUCION %in% c(1114) ~ "Surcolombiana",
                              .$INST_COD_INSTITUCION %in% c(1115) ~ "Amazonía",
                              .$INST_COD_INSTITUCION %in% c(1117) ~ "Militar",
                              .$INST_COD_INSTITUCION %in% c(1118) ~ "Tecn. Choco",
                              .$INST_COD_INSTITUCION %in% c(1119) ~ "Llanos",
                              .$INST_COD_INSTITUCION %in% c(1120, 1123) ~ "Pop. Cesar",
                              .$INST_COD_INSTITUCION %in% c(1121) ~ "ColMay. C/marca",
                              .$INST_COD_INSTITUCION %in% c(1122) ~ "Pacífico",
                              .$INST_COD_INSTITUCION %in% c(1201, 1219, 1220, 1221, 1222, 1223, 9125)  ~ "U. Antioquia",
                              .$INST_COD_INSTITUCION %in% c(1202) ~ "Atlantico",
                              .$INST_COD_INSTITUCION %in% c(1203) ~ "Valle",
                              .$INST_COD_INSTITUCION %in% c(1204) ~ "UIS",
                              .$INST_COD_INSTITUCION %in% c(1205) ~ "Cartagena",
                              .$INST_COD_INSTITUCION %in% c(1206) ~ "Nariño",
                              .$INST_COD_INSTITUCION %in% c(1207) ~ "Tolima",
                              .$INST_COD_INSTITUCION %in% c(1208) ~ "Quindio",
                              .$INST_COD_INSTITUCION %in% c(1209) ~ "UFPS-Cúcuta",
                              .$INST_COD_INSTITUCION %in% c(1210) ~ "UFPS-Ocaña",
                              .$INST_COD_INSTITUCION %in% c(1212) ~ "Pamplona",
                              .$INST_COD_INSTITUCION %in% c(1213) ~ "Magdalena",
                              .$INST_COD_INSTITUCION %in% c(1214, 1215, 1216) ~ "Cundinamarca",
                              .$INST_COD_INSTITUCION %in% c(1217) ~ "Sucre",
                              .$INST_COD_INSTITUCION %in% c(1218) ~ "Guajira",
                              .$INST_COD_INSTITUCION %in% c(1301) ~ "Distrital",
                              .$INST_COD_INSTITUCION %in% c(2102) ~ "UNAD",
                              .$INST_COD_INSTITUCION %in% c(9929) ~ "Indigena",
                              .$INST_COD_INSTITUCION %in% c(2743) ~ "Unitropico",
                              TRUE ~ "Resto IES"))

# 1 UNIVERSIDAD NACIONAL DE COLOMBIA: c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920, 9933)
# 2 UNIVERSIDAD PEDAGOGICA NACIONAL: c(1105)
# 3 UNIVERSIDAD TEC.DE COL. TUNJA: c(1106, 1107, 1108, 1109)
# 4 UNIVERSIDAD DEL CAUCA: c(1110)
# 5 UNIVERSIDAD TECNOLOGICA DE PEREIRA: c(1111)
# 6 UNIVERSIDAD DE CALDAS: c(1112)
# 7 UNIVERSIDAD CORDOBA: c(1113)
# 8 UNIVERSIDAD SURCOLOMBIANA DE NEIVA: c(1114)
# 9 UNIVERSIDAD DE LA AMAZONIA: c(1115)
# 10 UNIVERSIDAD MILITAR NUEVA GRANADA: c(1117)
# 11 UNIVERSIDAD TECNOLOGICA DEL CHOCO: c(1118)
# 12 UNIVERSIDAD DE LLANOS ORIENTALES: c(1119)
# 13 UNIVERSIDAD POPULAR DEL CESAR: c(1120, 1123)
# 14 UNIVERSIDAD COLEGIO MAYOR DE C/MARCA: c(1121)
# 15 UNIVERSIDAD DEL PACIFICO: c(1122)
# 16 UNIVERSIDAD DE ANTIOQUIA: c(1201, 1219, 1220, 1221, 1222, 1223, 9125) 
# 17 UNIVERSIDAD DEL ATLANTICO: c(1202)
# 18 UNIVERSIDAD DEL VALLE: c(1203)
# 19 UNIVERSIDAD INDUSTRIAL DE SANTANDER: c(1204)
# 20 UNIVERSIDAD DE CARTAGENA: c(1205)
# 21 UNIVERSIDAD DE NARINO: c(1206)
# 22 UNIVERSIDAD DEL TOLIMA: c(1207)
# 23 UNIVERSIDAD DEL QUINDIO: c(1208)
# 24 UNIVERSIDAD FRANCISCO DE PAULA SANTANDER, CUCUTA: c(1209)
# 25 UNIVERSIDAD FRANCISCO DE PAULA SANTANDER- OCANA: c(1210)
# 26 UNIVERSIDAD DE PAMPLONA: c(1212)
# 27 UNIVERSIDAD TEC. DEL MAGDALENA: c(1213)
# 28 UNIVERSIDAD DE CUNDINAMARCA: c(1214, 1215, 1216)
# 29 UNIVERSIDAD-COLEGIO MAYOR DE CUNDINAMARCA: c(1121)
# 30 UNIVERSIDAD DE SUCRE: c(1217)
# 31 UNIVERSIDAD DE LA GUAJIRA: c(1218)
# 32 UNIVERSIDAD DISTRITAL "FRANCISCO JOSE DE CALDAS": c(1301)
# 33 UNIVERSIDAD ABIERTA Y A DISTANCIA: c(2102)
# 34 UNIVERSIDADA AUTONOMA INTERCULTURAL INDIGENA: c(9929)
# 35 UNIVERSIDAD INTERNACIONAL DEL TRÓPICO AMERICANO: c(2743)

# Inicio Fase de Análisis Saber Pro


# Promedio Saber UNAL


Media_Pais <- mean(SaberPro$PUNT_GLOBAL, na.rm = TRUE)
Media_Unal <- SaberPro |> group_by(Unal) |> summarise(Media = mean(PUNT_GLOBAL, na.rm = TRUE))
Media_Unal <- round(as.numeric(Media_Unal[2,2]), 2)



# UNAL vs IES País - Variable Unal

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(Unal, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 UNAL vs Otras IES", subtitle = paste("Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))


# Sedes UNAL vs IES país - Variable Sunal

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(Sunal, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 Sedes de la UNAL vs Otras IES", subtitle = paste("Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))


# Sedes UNAL vs IES país - Variable Sunal

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(SUE_UNAL, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 UNAL, Resto del SUE y Otras IES", subtitle = paste("Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))

# Resultados Saber Pro G12

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(G12, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 ", subtitle = paste("Principales Universidades del País - ", "Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))

# Resultados Saber Pro G15

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(G15, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 ", subtitle = "Principales universidades del país\n ")+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))


# Resultados Saber Pro Universidades del SUE

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(SUE_UNO, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 Universidades SUE", subtitle = paste("Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))


# Resultados Saber Pro Universidades del SUE + Sedes UNAL

ggplot(data = SaberPro, aes(y = PUNT_GLOBAL, x = fct_reorder(SUE_UNO_SUNAL, PUNT_GLOBAL, .fun = median))) + 
  geom_boxplot(outlier.color = "#addd8e", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = Media_Pais, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ggtitle("Distribución puntajes globales Saber Pro 2016-2021 Universidades SUE vs Sedes UNAL", subtitle = paste("Total Evaluados = ", nrow(SaberPro)))+
  ylab(" \n Puntaje Promedio Global Saber PRO")+
  xlab("Universidades")+coord_flip() +
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(size = 13, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=16))

# Análisis de resultados pruebas Saber Pro competencias genéricas

# Crear variables competencias genéricas con nuevos nombres


SaberPro1 <-  SaberPro |> rename(
`Lectura Crítica` = MOD_LECTURA_CRITICA_PUNT, 
`Razonamiento Cuantitativo` = MOD_RAZONA_CUANTITAT_PUNT, 
`Competencias Ciudadanas` = MOD_COMPETEN_CIUDADA_PUNT,
`Comunicación Escrita` = MOD_COMUNI_ESCRITA_PUNT,
`Inglés` = MOD_INGLES_PUNT,
`Global` = PUNT_GLOBAL)

# Resultados Saber Pro Competencias UNAL

SaberPro1 |> filter(Unal == "Unal") |> 
  mutate(UNAL = "UNAL") |> 
  Plot.Radar(categoria = Unal,
  variables = vars(Global,  
                   `Razonamiento Cuantitativo`, 
                   `Competencias Ciudadanas`,
                   `Comunicación Escrita`,
                   Inglés,
                   `Lectura Crítica`
                   ),
  # libreria  = "echarts",
  # colores   = c("#af8dc3"),
  rango     = c(0, 300)
  # estilo      = list(
  #   ply.LegendTitle = "SEDE:", ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h"),
  #   ply.Relleno = "tonext", ply.Opacidad = 0.8)
)


# Resultados Saber Pro Competencias UNAL - Por años


SaberPro1 |> filter(Unal == "Unal") |> 
  Plot.Radar(categoria = YEAR,
             variables = vars(Global,  
                              `Razonamiento Cuantitativo`, 
                              `Competencias Ciudadanas`,
                              `Comunicación Escrita`,
                              Inglés,
                              `Lectura Crítica`),
             titulo      = "Evolución Resultados Promedio Competencias Genéricas UNAL Examen Saber Pro, por años.",
             rango     = c(0, 300),
             estilo      = list(
               ply.LegendTitle = "Año:",
               ply.Relleno = "tonext", ply.Opacidad = 0.8)
  )


# Resultados Saber Pro Competencias UNAL - Por Sedes

color = c("#8CC63F", # VERDE       | Bogota
          "#0071BC", # AZUL VIVO   | Manizales
          "#F15A24", # NARANJA     | Medellin
          "#93278F") # MORADO      | Palmira


SaberPro1 |> filter(Unal == "Unal") |> 
  Plot.Radar(categoria = Sunal,
             variables = vars(Global,  
                              `Razonamiento Cuantitativo`, 
                              `Competencias Ciudadanas`,
                              `Comunicación Escrita`,
                              Inglés,
                              `Lectura Crítica`),
             # titulo = "<br>Evolución Resultados Promedio Competencias Genéricas UNAL Examen Saber Pro, <br> por años.",
             rango = c(0, 200),
             colores = color,
             # libreria  = "echarts",
             estilo      = list(
               ply.LegendTitle = "Sedes:",
               ply.Relleno = "none")
  )

#### -------------------------------------
## Análisis posiciones UNAL vs G12 y SUE

# Posiciones en el G12

PosG12 <- SaberPro |> 
          group_by(G12, YEAR) |> 
          summarise(Promedio = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
                    Evaluados = n()) |> 
          pivot_wider(names_from = YEAR, 
                      values_from = c(Promedio, Evaluados)) |> 
          ungroup() |> 
          mutate(across(.cols = starts_with("Promedio"),
                        .fns = ~row_number(desc(.x)), .names = "Posicion_{2016:2021}")) |> 
          relocate(starts_with("Posicion"), .after = "Promedio_2021") |> 
          arrange(desc(Promedio_2021))

# Posiciones en el G12 + Sedes UNAL

PosG12UNAL <- SaberPro |> 
  group_by(G15, YEAR) |> 
  summarise(Promedio = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
            Evaluados = n()) |> 
  pivot_wider(names_from = YEAR, 
              values_from = c(Promedio, Evaluados)) |> 
  ungroup() |> 
  mutate(across(.cols = starts_with("Promedio"),
                .fns = ~row_number(desc(.x)), .names = "Posicion_{2016:2021}")) |> 
  relocate(starts_with("Posicion"), .after = "Promedio_2021") |> 
  arrange(desc(Promedio_2021))


# Evolución Posiciones en el SUE
  
PosSUE <- SaberPro |> 
  group_by(SUE_UNO, YEAR) |> 
  summarise(Promedio = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
            Evaluados = n()) |> 
  pivot_wider(names_from = YEAR, 
              values_from = c(Promedio, Evaluados)) |> 
  ungroup() |> 
  mutate(across(.cols = starts_with("Promedio"),
                .fns = ~row_number(desc(.x)), .names = "Posicion_{2016:2021}")) |> 
  relocate(starts_with("Posicion"), .after = "Promedio_2021") |> 
  arrange(desc(Promedio_2021))

# Evolución Posiciones en el SUE + Sedes UNAL

PosSUEUNAL <- SaberPro |> 
  group_by(SUE_UNO_SUNAL, YEAR) |> 
  summarise(Promedio = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
            Evaluados = n()) |> 
  pivot_wider(names_from = YEAR, 
              values_from = c(Promedio, Evaluados)) |> 
  ungroup() |> 
  mutate(across(.cols = starts_with("Promedio"),
                .fns = ~row_number(desc(.x)), .names = "Posicion_{2016:2021}")) |> 
  relocate(starts_with("Posicion"), .after = "Promedio_2021") |> 
  arrange(desc(Promedio_2021))


# Comparativo Nacional Rápido

PosGeneral <- SaberPro |> 
  group_by(INST_COD_INSTITUCION, YEAR) |> 
  summarise(Promedio = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
            Evaluados = n()) |> 
  pivot_wider(names_from = YEAR, 
              values_from = c(Promedio, Evaluados)) |> 
  ungroup() |> 
  mutate(across(.cols = starts_with("Promedio"),
                .fns = ~row_number(desc(.x)), .names = "Posicion_{2016:2021}")) |> 
  relocate(starts_with("Posicion"), .after = "Promedio_2021") |> 
  arrange(desc(Promedio_2021))


# Exportar Resultados

write_xlsx(PosG12, "Datos/Entrega22/PosG12.xlsx")
write_xlsx(PosG12UNAL, "Datos/Entrega22/PosG12UNAL.xlsx")
write_xlsx(PosSUE, "Datos/Entrega22/PosSUE.xlsx")
write_xlsx(PosSUEUNAL, "Datos/Entrega22/PosSUEUNAL.xlsx")
write_xlsx(PosGeneral, "Datos/Entrega22/PosGeneral.xlsx")


####------------------------------------------
# Análisis Resultados Examen Admisión vs Saber Pro

## Importar base de cupos UNAL

CuposUNAL <- read_excel("Datos/Fuentes/2016_2021_PRECuposUNAL.xlsx")

# Tabla evolución total cupos

Cupos_UNAL <- CuposUNAL |> 
  group_by(YEAR, Sede) |> 
  summarise(Cupos = sum(Total)) |> 
  mutate(ID = paste0(YEAR, Sede),
         YEAR_CUPOS = YEAR)

# Promedio examen admisión por sedes y periodos

Exa_UNAL <- UnalData::Aspirantes |> 
            filter(TIPO_NIVEL == "Pregrado", !is.na(MOD_INS)) |> 
            # mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) |> 
            group_by(YEAR, INS_SEDE_NOMBRE) |> 
            mutate(Total = n()) |> ungroup() |> 
            filter(ADMITIDO == "Sí", YEAR %in% c(2016:2020)) |> 
            group_by(YEAR, INS_SEDE_NOMBRE) |> 
            summarise(Admitidos = n(),
                      Aspirantes = max(Total),
                      Mean_propio = mean(PTOTAL, na.rm = TRUE)) |>                      
            filter(INS_SEDE_NOMBRE != "De La Paz") |> 
            mutate(ID = paste0(YEAR, INS_SEDE_NOMBRE))

# Promedio Saber Pro por Sedes

Saber_UNAL <- UnalData::SaberPro |> 
  group_by(YEAR, SEDE_NOMBRE_ADM) |> 
  summarise(Evaluados = n(),
            Mean_Saber = mean(PUNTAJE_GLOBAL, na.rm = TRUE)) |> 
  mutate(ID = paste0(YEAR, SEDE_NOMBRE_ADM))

# Cruzar bases de datos

Saber_ExaUnal <- left_join(Exa_UNAL, Saber_UNAL, by = "ID") |> 
                 left_join(Cupos_UNAL, by = "ID") |> 
                 select(YEAR, Sede, Admitidos:Mean_propio, Evaluados:Mean_Saber, Cupos) |> 
                 rename(Year= YEAR) |> 
                 mutate(R_Asp_Cup = Aspirantes/Cupos) 

# Visualización de resultados 


Tema <- theme(legend.text = element_text(size = 10),
      legend.title = element_text(color = "blue", size = 14, face = "bold"))

# Media de resultados pruebas Saber Pro vs Total de Aspirantes - Con Bogotá

Saber_ExaUnal |> ggplot(aes(x = Mean_Saber, y = Aspirantes))+
                 geom_point(aes(color = Sede, shape = Sede), size = 6)+
                 geom_smooth()+
                 scale_shape_manual(values=c(8, 16,  8, 16, 16, 8, 16,  8))+
                 scale_color_manual(values=c('#e41a1c','green','blue','#ff7f00','blue','green','red','#ff7f00'))+
  labs(title = "Puntajes Promedio Global Anual Prueba Saber Pro vs Total de Aspirantes UNAL, por Sedes.", 
       subtitle = "Años 2016 a 2020")+
  xlab(" \n Puntaje Promedio Prueba Saber Pro")+
  ylab("Total de aspirantes en la Universidad\n ")+
  Tema
 
# Media de resultados pruebas Saber Pro vs Total de Aspirantes - Sin Bogotá Y Medellín

Saber_ExaUnal |> filter(!Sede %in% c("Bogotá", "Medellín")) |> 
  ggplot(aes(x = Mean_Saber, y = Aspirantes))+
  geom_point(aes(color = Sede, shape = Sede), size = 6)+
  geom_smooth()+
  scale_shape_manual(values=c(8, 16, 16, 8, 16,  8))+
  scale_color_manual(values=c('#e41a1c','#ff7f00','blue','green','red','#ff7f00'))+
  labs(title = "Puntajes Promedio Global Anual Prueba Saber Pro vs Total de Aspirantes UNAL, por Sedes sin Bogotá y Medellín.", 
       subtitle = "Años 2016 a 2020")+
  xlab(" \n Puntaje Promedio Prueba Saber Pro")+
  ylab("Total de aspirantes en la Universidad\n ")+
  Tema


# Media de resultados pruebas Saber Pro vs Razón ASpirantes Admitidos

Saber_ExaUnal |> ggplot(aes(x = Mean_Saber, y = R_Asp_Cup))+
  geom_point(aes(color = Sede, shape = Sede), size = 6)+
  geom_smooth()+
  scale_shape_manual(values=c(8, 16,  8, 16, 16, 8, 16,  8))+
  scale_color_manual(values=c('#e41a1c','green','blue','#ff7f00','blue','green','red','#ff7f00'))+
  labs(title = "Puntajes Promedio Global Anual Prueba Saber Pro vs Razón Aspirantes por Cupos, por Sedes.", 
       subtitle = "Años 2016 a 2020")+
  xlab(" \n Puntaje Promedio Prueba Saber Pro")+
  ylab("Razón Aspirantes por Cupos\n ")+
  Tema


# Media de resultados pruebas Saber Pro vs Razón ASpirantes Admitidos
# Sin Bogotá

Saber_ExaUnal |> filter(!Sede %in% c("Bogotá")) |> 
  ggplot(aes(x = Mean_Saber, y = R_Asp_Cup))+
  geom_point(aes(color = Sede, shape = Sede), size = 6)+
  geom_smooth()+
  scale_shape_manual(values=c(8, 16,  8, 16, 16, 8, 16,  8))+
  scale_color_manual(values=c('#e41a1c','green','blue','#ff7f00','blue','green','red','#ff7f00'))+
  labs(title = "Puntajes Promedio Global Anual Prueba Saber Pro vs Razón Aspirantes por Cupos, por Sedes.", 
       subtitle = "Años 2016 a 2020")+
  xlab(" \n Puntaje Promedio Prueba Saber Pro")+
  ylab("Razón Aspirantes por Cupos\n ")+
  ylim(0, 12)+
  Tema


# Resultados puntaje de admisión UNAl

UnalData::Aspirantes |> 
              filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2008:2022), !is.na(MOD_INS)) |> 
              mutate(Unal = "UNAL") |> 
              Plot.Boxplot(
              variable = PTOTAL,
              grupo1 = YEAR,
              grupo2 = Unal,
              outliers = TRUE,
              colOutlier = "#969696")


# Resultados puntaje de admisión UNAl (Admitidos vs No Admitidos)

UnalData::Aspirantes |> 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2016:2022), !is.na(MOD_INS)) |> 
  mutate(Unal = "UNAL") |> 
  Plot.Boxplot(
    variable = PTOTAL,
    grupo1 = YEAR,
    grupo2 = ADMITIDO,
    outliers = FALSE,
    colores = c("red", "blue"))


# Resultados puntaje de admisión UNAl por sedes

UnalData::Aspirantes |> 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2021:2022), !is.na(MOD_INS)) |> 
  mutate(Unal = "UNAL") |> 
  Plot.Boxplot(
    variable = PTOTAL,
    grupo1 = YEAR,
    grupo2 = INS_SEDE_NOMBRE,
    outliers = FALSE)


# Mapa de resultados examen de admisión 
# por departamentos y municipios


# Mapa por departamentos

     UnalData::Aspirantes |> 
     filter(TIPO_NIVEL == "Pregrado", 
     YEAR %in% c(2009:2022)) |> 
     select(Code_Dept = COD_DEP_RES,
            Code_Mun  = COD_CIU_RES,
            ScoreGlobal   = PTOTAL) %$%
  Plot.Mapa(
  depto   = Code_Dept,
  mpio    = Code_Mun,
  tipo    = "Deptos",
  estadistico   = "Promedio",
  variable = ScoreGlobal,
  naTo0 = FALSE, 
  textSize = 0,
  titulo  = "Puntaje Examen <br> Admisión UNAL",
  cortes  = c(0, 400, 450, 500, 540, Inf), 
  colores = c("#fd8d3c", "#fecc5c", "#99d8c9", "#6DE35B", "#69f728")
  )

# Mapa por municipios

UnalData::Aspirantes |> 
  filter(TIPO_NIVEL == "Pregrado", 
         YEAR %in% c(2009:2022)) |> 
  select(Code_Dept = COD_DEP_RES,
         Code_Mun  = COD_CIU_RES,
         ScoreGlobal   = PTOTAL) %$%
  Plot.Mapa(
    depto   = Code_Dept,
    mpio    = Code_Mun,
    tipo    = "Mpios",
    estadistico   = "Promedio",
    variable = ScoreGlobal,
    naTo0 = FALSE, 
    textSize = 0,
    titulo  = "Puntaje Examen <br> Admisión UNAL",
    cortes  = c(0, 400, 450, 500, 540, Inf), 
    colores = c("#fd8d3c", "#fecc5c", "#99d8c9", "#6DE35B", "#69f728")
    )
  

StaticPlot(Mapa1)


class(Aspirantes_Map$PTOTAL)

names(Aspirantes_Map)

# Resultados puntaje de admisión ADMITIDOS UNAl por sedes

UnalData::Aspirantes |> 
  filter(TIPO_NIVEL == "Pregrado", YEAR %in% c(2021:2022), 
         !is.na(MOD_INS), ADMITIDO == "Sí") |> 
  mutate(Unal = "UNAL") |> 
  Plot.Boxplot(
    variable = PTOTAL,
    grupo1 = YEAR,
    grupo2 = INS_SEDE_NOMBRE,
    outliers = FALSE)


# Dispersión Puntajes Promedio Examen Propio UNAL vs Prueba Saber Pro, por Sedes.

cor2 <- cor(x = Saber_ExaUnal$Mean_propio, y = Saber_ExaUnal$Mean_Saber, use = "complete.obs")

Saber_ExaUnal |> ggplot(aes(x = Mean_propio, y = Mean_Saber))+
  geom_point(aes(color = Sede, shape = Sede), size = 6)+
  geom_smooth(method = "lm")+
  scale_shape_manual(values=c(8, 16,  8, 16, 16, 8, 16,  8))+
  scale_color_manual(values=c('#e41a1c','green','blue','#ff7f00','blue','green','red','#ff7f00'))+
  annotate(geom="text", x=650, y=150, label=paste("Correlación = ", round(cor2,3)),
           color="red",  fontface = 'italic', size =5) +
  labs(title = "Dispersión Puntajes Promedio Anuales Examen Propio UNAL vs Prueba Saber Pro, por Sedes.", 
       subtitle = "Años 2016 a 2020")+
  xlab(" \n Puntaje Promedio Examen Propio")+
  ylab("Promedio Puntaje Global Prueba Saber Pro\n ")+
  Tema
  

# Evolución puntaje global UNAL Saber Pro

Plot.Boxplot(
  datos = SaberPro,
  variable = PUNT_GLOBAL,
  grupo1 = YEAR,
  grupo2 = SUE_UNAL,
  outliers = FALSE)

Plot.Boxplot(
  datos = SaberPro,
  variable = PUNT_GLOBAL,
  grupo1 = SUE_UNO,
  outliers = TRUE,
  colOutlier = "gray")

Plot.Boxplot(
  datos = SaberPro,
  variable = PUNT_GLOBAL,
  grupo1 = YEAR,
  grupo2 = G12,
  outliers = FALSE,
  
  violin = TRUE,
  libreria = c("plotly"))







######################################-
# 23 Solicitud 16-05-2022 -----
######################################-

# Demanda: Julian Camilo Alfonso Suarez <jcalfonsosu@unal.edu.co>
# Propósito: 
# Somos los estudiantes de economía que llamamos por teléfono al medio día. Como solicitamos nos gustaría tener los datos de los matriculados para la sede Bogotá del periodo 2021-1  y las variables que necesitamos son puntaje, edad, estrato, PMB y naturaleza de colegio porfavor.
# Muchas gracias y quedamos atentos a su respuesta.

# Que pena, no son los estudiantes matriculados sino los admitidos. Muchas gracias

# LA SOLICITUD SE RESPONDIO A TRAVÉS DE GESTIÓN DIRECTA DE 
# LOS DATOS EN EXCEL

######################################-
# 24 Solicitud 16-05-2022 -----
######################################-

# Demanda: Claudia Patricia Joya Joya
# Descripción :
# Teniendo en cuenta una reunión que  va a desarrollar la
# Dirección de la Sede con la Gobernadora de Arauca (e), 
# Dra. Indira Luz Barrios Guarnizo, de manera  atenta solicito 
# su colaboración en el sentido de facilitarme o indicarme donde 
# puedo consultar  la información de los bachilleres graduados de 
# los departamentos que componen el área de influencia de la Sede Orinoquia.

# Departamentos: Arauca, Casanare, Vichada, Guainía y Guaviare

# Importar Datos

Saber11 <- read.csv("Datos/Fuentes/MEN_MATRICULA_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_2018_2020.csv")

# Agregar resultados

Saber11_Orinoquia <- Saber11 %>% rename(PERIODO = ï..ANNO_INF) %>%
                                 filter(PERIODO == "2,020",
                                        GRADO == "Once",
                                        COD_DANE_DEPARTAMENTO %in% c(81, 85, 99, 94, 95)) %>%
                     group_by(PERIODO, COD_DANE_DEPARTAMENTO, DEPARTAMENTO) %>% 
                     count(name = "Total")

View(Saber11_Orinoquia)


# Con Base en las Pruebas Saber 11 PERIODO 2019-2

# Importar Datos

Saber11_20192 <- read.csv("Datos/Fuentes/Saber_11__2019-2.csv", encoding = "UTF-8")


######################################-
# 25 Solicitud 02-06-2022 -----
######################################-

# Demanda: Coordinacion Pregrado de Diseno Grafico
           
# Descripción :

# Señores
# Dirección de Estadística
# Reciban un cordial saludo:
# De manera atenta, solicitamos por favor nos compartan el histórico de la cantidad de graduandos del programa de Diseño Gráfico 2509 (UBA 2535), así como el número de promociones que tiene la carrera hasta el momento.
# Esta información nos la están solicitando como parte del proceso de acreditación del programa y la pronta visita de pares que tendremos.
# Agradecemos su atención y ayuda en este tema.

DGrafico_Bog <- UnalData::Graduados |> 
              filter(SNIES_PROGRA == 4) |> 
              group_by(YEAR, SEMESTRE, PROGRAMA) |> 
              count(name = "Total")


# Exportar resultados

write_xlsx(DGrafico_Bog, "Datos/Entrega25/DGrafico_Bog.xlsx")

View(UnalData::Graduados)


######################################-
# 26 Solicitud 15-06-2022 -----
######################################-

# Demanda: Yuli Stefany Novoa Rodriguez

# Descripción :

# Buenos días,
# Mediante el presente correo, me permito solicitar a ustedes las estadísticas de alumnos de pregrado para la facultad de enfermería en el periodo 2021-2, discriminado por sexo, edad y demas datos que se visualizan a nivel de sede en la pagina, ya que lo requiero para un proyecto realizado para una de las asignaturas que brinda la facultad.
# Agradecería profundamente el envío de esta información.
# Muchas gracias.
# Feliz Tarde

Enfer_212 <- UnalData::Matriculados |> 
            filter(YEAR == 2021, SEMESTRE == 2, SNIES_PROGRA == 7) |> 
            group_by(NACIONALIDAD, CAT_EDAD, SEXO, ESTRATO, TIPO_COL, PBM, MAT_PVEZ, TIPO_ADM, PAES, PEAMA) |> 
            summarise(Total = n())

# Exportar resultados

write_xlsx(Enfer_212, "Datos/Entrega26/Enfer_212.xlsx")


######################################-
# 27 Solicitud 15-06-2022 -----
######################################-

# Demanda: Julio David Alvarez Ortiz 
# Julio David Alvarez Ortiz 

# Descripción :

# Le escribo porque estoy haciendo una investigación acerca de las mujeres en minería y me gustaría conocer la información acerca de las mujeres en los pregrados de ingeniería de minas y metalurgia, 
# graduadas en dicho pregrado. Además, información de los docentes por sexo del departamento de minerales y materiales de la Sede Medellín.
# Esta información no fue posible obtenerla en la página de estadísticas de la U.

Gradu_Metalurgia <- UnalData::Graduados |> 
  filter(SNIES_PROGRA %in% c(118, 126)) |> 
  group_by(PROGRAMA, YEAR, SEMESTRE, SEXO) |> 
  summarise(Total = n()) |> 
  pivot_wider(names_from = SEXO, values_from = Total) |> 
  arrange(desc(YEAR), desc(SEMESTRE)) |> 
  mutate(PERIODO = paste0(YEAR, "-", SEMESTRE),
         `Total Graduados` = Hombres + Mujeres) |> 
  ungroup() |> 
  select(-c(YEAR, SEMESTRE)) |> 
  relocate(PERIODO) 


Docen_Metalurgia <- UnalData::Docentes |> 
  filter(FACULTAD_O == "Minas",
         UNIDAD == "Departamento de Materiales y Minerales",
         YEAR >= 2018) |> 
  group_by(YEAR, SEMESTRE, SEXO) |> 
  summarise(Total = n()) |> 
  pivot_wider(names_from = SEXO, values_from = Total) |> 
  mutate(`Total Docentes` = Hombres + Mujeres,
         Periodo = paste0(YEAR, "-", SEMESTRE)) |> 
  relocate(Periodo) |> 
  ungroup() |> 
  select(-c(YEAR, SEMESTRE))

write_xlsx(Gradu_Metalurgia, "Datos/Entrega27/Gradu_Metalurgia.xlsx")
write_xlsx(Docen_Metalurgia, "Datos/Entrega27/Docen_Metalurgia.xlsx")  
    
######################################-
# 28 Solicitud 22-06-2022 -----
######################################-

# Demanda: Maria Claudia Galindo
# Nueva forma de consolidar la información histórica de programas académicos

# Programa Matriculados

Matricula_Fin <- UnalData::Matriculados %>% 
                 filter(TIPO_ADM %in% c("Regular", "PAES"))


Matricula <- Matricula_Fin %>% 
             group_by(YEAR, SEMESTRE, TIPO_NIVEL, SEDE_NOMBRE_MAT, 
                      SNIES_PROGRA, PROGRAMA) %>% 
             count(name = "Matriculados") %>% 
             mutate(ID = paste0(YEAR, SEMESTRE, TIPO_NIVEL, SEDE_NOMBRE_MAT, 
                                SNIES_PROGRA, PROGRAMA)) %>% 
             relocate(ID) %>% 
             rename(YEAR_M = YEAR,
                    SEMESTRE_M = SEMESTRE,
                    TIPO_NIVEL_M = TIPO_NIVEL,
                    SEDE_NOMBRE_MAT_M = SEDE_NOMBRE_MAT,
                    SNIES_PROGRA_M = SNIES_PROGRA,
                    PROGRAMA_M = PROGRAMA)
           
# Programas Graduados

Graduados_Fin <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("Regular", "PAES"))


Graduados <- Graduados_Fin %>% 
  group_by(YEAR, SEMESTRE, TIPO_NIVEL, SEDE_NOMBRE_MAT, 
           SNIES_PROGRA, PROGRAMA) %>% 
  count(name = "Graduados") %>% 
  mutate(ID = paste0(YEAR, SEMESTRE, TIPO_NIVEL, SEDE_NOMBRE_MAT, 
                     SNIES_PROGRA, PROGRAMA)) %>% 
  relocate(ID) %>% 
  rename(YEAR_G = YEAR,
         SEMESTRE_G = SEMESTRE,
         TIPO_NIVEL_G = TIPO_NIVEL,
         SEDE_NOMBRE_MAT_G = SEDE_NOMBRE_MAT,
         SNIES_PROGRA_G = SNIES_PROGRA,
         PROGRAMA_G = PROGRAMA)


# Aspirantes Postgrado

Asp_Post <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Postgrado")


Aspirantes_Post <- Asp_Post %>% 
  group_by(YEAR, SEMESTRE, TIPO_NIVEL, INS_SEDE_NOMBRE, 
           SNIES_PROGRA, PROGRAMA) %>% 
  count(name = "Aspirantes") %>% 
  mutate(ID = paste0(YEAR, SEMESTRE, TIPO_NIVEL, INS_SEDE_NOMBRE, 
                     SNIES_PROGRA, PROGRAMA)) %>% 
  relocate(ID) 

# Admitidos Pregrado 

Asp_Pre <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado",
         ADMITIDO == "Sí") %>% 
  filter(TIPO_INS %in% c("Regular", "PAES"))
  
Aspirantes_Pre <- Asp_Pre %>% 
  group_by(YEAR, SEMESTRE, TIPO_NIVEL, INS_SEDE_NOMBRE, 
           SNIES_PROGRA, PROGRAMA) %>% 
  count(name = "Aspirantes") %>% 
  mutate(ID = paste0(YEAR, SEMESTRE, TIPO_NIVEL, INS_SEDE_NOMBRE, 
                     SNIES_PROGRA, PROGRAMA)) %>% 
  relocate(ID) 


# Unir y renombrar base de aspirantes

Aspirantes <-  bind_rows(Aspirantes_Pre, Aspirantes_Post)%>% 
  rename(YEAR_A = YEAR, 
         SEMESTRE_A = SEMESTRE, 
         TIPO_NIVEL_A = TIPO_NIVEL, 
         INS_SEDE_NOMBRE_A = INS_SEDE_NOMBRE, 
         SNIES_PROGRAS_A = SNIES_PROGRA, 
         PROGRAMA_A = PROGRAMA)


# Cruzar bases de datos

Programas <- Aspirantes %>% 
             full_join(Matricula, by = "ID") %>% 
             full_join(Graduados, by = "ID") %>% 
             ungroup() %>% 
             rowwise() %>% 
             mutate(SNIES = max(SNIES_PROGRAS_A,
                                SNIES_PROGRA_M,
                                SNIES_PROGRA_G,
                                na.rm = TRUE)) %>% 
             relocate(SNIES, .after = ID) %>% 
  tibble()

# Exportar resultados a Excel

write_xlsx(Programas, "Datos/Entrega28/Programas.xlsx")

######################################-
# 29 Solicitud 29-06-2022 -----
######################################-

# Demanda: Alejandra Montoya

# Buenos días,
# # 
# # Mi nombre es Alejandra Montoya y soy candidata a doctorado en la Universidad de Maryland, en Estados Unidos. 
# Con el propósito de completar información que estoy usando en mi tesis doctoral, 
# quisiera saber si es posible acceder a los datos del valor de matrícula para Puntaje Básico Matrícula igual a 100 (precio sin subsidio) 
# en la Universidad Nacional entre los años 2010 y 2020. En el archivo adjunto podrán encontrar una descripción más 
# detallada de mi solicitud, 
# la cual espero pueda ser cobijada bajo la Ley 1712 del 6 de marzo de 2014 sobre transparencia y acceso a información pública.
# 
# Cordialmente,
# 
# Alejandra Montoya

# LA RESPUESTA FUE CONSOLIDAD POR MARIA CLAUDIA GALINDO. LOS ARCHIVOS ENVIADOS
# SE ENCUENTRAN DISPONIBLES EN LA CARPETA Entrega29

######################################-
# 30 Solicitud 30-06-2022 -----
######################################-

# Demanda: PABLO FELIPE MARÍN CARDONA
# Director Académico sede Manizales

# Amablemente me permito solicitarles su ayuda en la obtención 
# de la información del indicador número de estudiantes 
# por docente en la Sede Manizales para los años 2016 a 2021 a nivel de Programa,
# Unidad Académica Básica, Facultad y Sede.

# Matriculados por programas académicos de la sede

Est_Man_Prog <- UnalData::Matriculados %>% 
  filter(SNIES_SEDE_MAT == 1103) %>% 
  group_by(YEAR, SEMESTRE, NIVEL, FACULTAD, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(`Total Estudiantes` = n()) %>% 
  arrange(desc(YEAR), desc(SEMESTRE))

write_xlsx(Est_Man_Prog, "Datos/Entrega 30/Est_Man_Prog.xlsx")

# Docentes por unidades académicas

Doc_Man_Unidad <- UnalData::Docentes %>% 
                          filter(SNIES_SEDE == 1103) %>% 
                 group_by(YEAR, SEMESTRE, UNIDAD) %>% 
                 summarise(`Total Docentes` = n()) %>% 
                 arrange(desc(YEAR), desc(SEMESTRE))

write_xlsx(Doc_Man_Unidad, "Datos/Entrega 30/Doc_Man_Unidad.xlsx")

######################################-
# 31 Solicitud 19-07-2022 -----
######################################-

# Demanda: RICARDO PEREZ ALMONACID
# Profesor Psicología U de Antioquia

# Se requiere el total de docentes de carrera, ocasionales y estudiantes de
# pregrado matriculados en la carrera de Psicología

# Total Matriculados

Mat_Pre_Psi <- UnalData::Matriculados %>% 
               filter(NIVEL == "Pregrado", SNIES_PROGRA == 14) %>% 
               group_by(YEAR, SEMESTRE) %>% 
               summarise(Total = n())

write_xlsx(Mat_Pre_Psi, "Datos/Entrega31/Mat_Pre_Psi.xlsx")

# Total Docentes

Doc_Psico <- UnalData::Docentes %>%
             filter(SEDE == "Bogotá", 
                    UNIDAD %in% c("Departamento de Psicología", "Escuela de Psicoanálisis")) %>% 
             group_by(YEAR, SEMESTRE, UNIDAD) %>%
             summarise(Total = n()) %>% 
             pivot_wider(names_from = UNIDAD,
                        values_from = Total) %>% 
             mutate(`Total Docentes Carrera` = `Departamento de Psicología` + `Escuela de Psicoanálisis`)

write_xlsx(Doc_Psico, "Datos/Entrega31/Doc_Psico.xlsx")

######################################-
# 32 Solicitud 04-08-2022 -----
######################################-

# Demanda: UNIMEDIOS - LINA ROCÍO MARTÍN GUZMÁN

# Generar, en html, la totalidad de gráficos del PGD 2022-2024.
# Publicación del PGD

# Función Agregar

Agregar <- function(poblacion, var){
  poblacion %>% group_by(.dots = c("YEAR", "SEMESTRE", var), .drop = FALSE) %>% 
    summarise(Total = n()) %>% 
    rename("Clase"=var) %>% 
    mutate(Variable = var) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total) %>%
    ungroup()
}

# Función Totales

Totales <- function(poblacion){
  poblacion %>% group_by(YEAR, SEMESTRE, .drop = FALSE) %>%  summarise(Total = n()) %>% ungroup() %>%
    mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total)
}

# Función Salvar

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-0),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Figura 1. Evolución histórica de los programas académicos

Programas <- read_excel("Datos/Fuentes/Serie_Programas.xlsx") %>% 
  pivot_longer(cols = Pregrado:Total, names_to = "Clase", values_to = "Total") %>% 
  arrange(Clase) %>% 
  mutate(Variable = ifelse(Clase == "Total", "TOTAL", Variable)) %>% 
  filter(YEAR != 2021)

# Serie General 

Fig1 <- Plot.Series(
  datos     = Programas,
  categoria = "TOTAL",
  colores   = c("#0071bc"),
  titulo    = "Evolución de programas académicos",
  labelY    = "N\u00famero de Programas"
)

Salvar(Fig1, "Export/Unimedios", "Fig1.html")

# Figura 2. Evolución de programas académicos por nivel de formación 

Fig2 <- Plot.Series(
  datos     = Programas,
  categoria = "TIPO_NIVEL",
  colores   = c("#f15a24", "#8cc63f"),
  titulo    = "Evolución de programas académicos por nivel de formación",
  labelY    = "N\u00famero de Programas"
)

Salvar(Fig2, "Export/Unimedios", "Fig2.html")

# Figura 3. Evolución histórica del total de aspirantes a la Universidad

LimpiaAsp <- UnalData::Aspirantes %>% 
  filter(!is.na(TIPO_INS)) %>% 
  filter(TIPO_NIVEL == "Pregrado" |	(TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular"))

Total_Asp <- Totales(LimpiaAsp) %>% 
             filter(YEAR <= 2021)

col <-   c("#0071bc") # Azul vivo, Total

Fig3 <- Plot.Series(datos = Total_Asp,
  categoria    = "TOTAL",
  freqRelativa = FALSE,
  colores = col,
  ylim = c(0, NaN),
  titulo = "Evolución histórica del total de aspirantes a la Universidad",
  labelY = "Número de aspirantes (k: miles)")

Salvar(Fig3, "Export/Unimedios", "Fig3.html")

# Figura 4. Distribución de aspirantes por sede de inscripción, periodo 2021-2


Sede_Asp <- Agregar(LimpiaAsp, "INS_SEDE_NOMBRE") %>% 
  filter(YEAR <= 2021) %>% 
  mutate(Clase = ifelse(is.na(Clase), "Universidad", Clase))

col <-   c( "#29abe2", # azul claro, Amazonia
            "#8cc63f", # verde, Bogotá
            "#c1272d", # Rojo, Caribe
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellin
            "#fbb03b", # amarillo, Orinoquia
            "#93278f", # morado, Palmira
            "#6d6666", # gris, Tumaco
            "#2ca25f" # verde esmeralda, Universidad
) 

Fig4 <- Plot.Barras(datos = Sede_Asp,
  categoria    = "INS_SEDE_NOMBRE",
  ano          = 2021,
  periodo      = 2,
  freqRelativa = FALSE,
  vertical     = FALSE,
  colores = col,
  addPeriodo = FALSE,
  titulo = "Distribución de aspirantes por sede de inscripción, periodo 2021-2",
  labelEje = "Número de aspirantes (k: miles)")

Salvar(Fig4, "Export/Unimedios", "Fig4.html")

# Figura 5. Evolución Histórica del Total de Admitidos a la Universidad

LimpiaAdm <- UnalData::Aspirantes %>% 
  filter(!is.na(TIPO_INS), ADMITIDO == "Sí") %>% 
  filter(TIPO_NIVEL == "Pregrado" |	(TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) 

Total_Adm <- Totales(LimpiaAdm) %>% 
  filter(YEAR <= 2021)


col <-   c("#0071bc") # Azul vivo, Total

Fig5 <- Plot.Series(datos = Total_Adm,
                    categoria    = "TOTAL",
                    freqRelativa = FALSE,
                    colores = col,
                    ylim = c(0, NaN),
                    titulo = "Evolución histórica del total de admitidos a la Universidad",
                    labelY = "Número de admitidos (k: miles)")

Salvar(Fig5, "Export/Unimedios", "Fig5.html")

# Figura 6. Evolución del Número de Admitidos a Pregrado por Modalidad de Admisión

LimpiaAdmPre <- LimpiaAdm %>% filter(TIPO_NIVEL == "Pregrado")
  
TipoAdm_Adm <- Agregar(LimpiaAdmPre, "MOD_INS") %>% 
  filter(YEAR <= 2021)

col <-   c( "#f15a24", # naranja, Especial
            "#8cc63f" # verde, Regular
) 

Fig6 <- Plot.Series(datos = TipoAdm_Adm,
                    categoria    = "MOD_INS",
                    freqRelativa = FALSE,
                    ylim = c(0, NaN),
                    colores = col,
                    titulo = "Evolución del número de admitidos a pregrado por modalidad de admisión",
                    labelY = "Número de admitidos (k: miles)")

Salvar(Fig6, "Export/Unimedios", "Fig6.html")

# Figura 7. Evolución histórica del total de estudiantes matriculados

Total_Mat <- Totales(UnalData::Matriculados) %>% 
  filter(YEAR <= 2021) %>% 
  filter(!(YEAR == 2021 & SEMESTRE == 2))

col <-   c("#0071bc") # Azul vivo, Total

Fig7 <- Plot.Series(datos = Total_Mat,
                    categoria    = "TOTAL",
                    freqRelativa = FALSE,
                    colores = col,
                    ylim = c(0, NaN),
                    titulo = "Evolución histórica del total de estudiantes matriculados",
                    labelY = "Número de estudiantes (k: miles)")

Salvar(Fig7, "Export/Unimedios", "Fig7.html")

#Figura 8. Distribución De estudiantes matriculados por sede, periodo 2021 -1

Sede_Mat <- Agregar(UnalData::Matriculados, "SEDE_NOMBRE_MAT") %>% 
  filter(YEAR <= 2021)

Fig8 <- Plot.Barras(datos = Sede_Mat,
                    categoria    = "SEDE_NOMBRE_MAT",
                    ano          = 2021,
                    periodo      = 1,
                    freqRelativa = FALSE,
                    vertical     = FALSE,
                    colores = RColorBrewer::brewer.pal(9, "Spectral"),
                    addPeriodo = FALSE,
                    titulo = "Distribución de estudiantes matriculados por sede, periodo 2021-1",
                    labelEje = "Número de estuddiantes (k: miles)")

Salvar(Fig8, "Export/Unimedios", "Fig8.html")

# Figura 9. Evolución histórica del total de estudiantes graduados 

Total_Gra <- Totales(UnalData::Graduados) %>% 
  filter(YEAR <= 2021) %>% 
  filter(!(YEAR == 2021 & SEMESTRE == 2))

col <-   c("#0071bc") # Azul vivo, Total

Fig9 <- Plot.Series(datos = Total_Gra,
                    categoria    = "TOTAL",
                    freqRelativa = FALSE,
                    colores = col,
                    ylim = c(0, NaN),
                    titulo = "Evolución histórica del total de estudiantes graduados",
                    labelY = "Número de graduados (k: miles)")

Salvar(Fig9, "Export/Unimedios", "Fig9.html")

# Figura 10. Distribución de graduados por sedes de la universidad, periodo 2021-1 

Sede_Gra <- Agregar(UnalData::Graduados, "SEDE_NOMBRE_ADM") %>% 
  filter(YEAR <= 2021) %>% 
  add_row(Variable = "SEDE_NOMBRE_ADM",  
          YEAR= 2021, 
          SEMESTRE = 1, 
          Clase = "La Paz", 
          Total = 0)


Fig10 <- Plot.Barras(datos = Sede_Gra,
                    categoria    = "SEDE_NOMBRE_ADM",
                    ano          = 2021,
                    periodo      = 1,
                    freqRelativa = FALSE,
                    vertical     = FALSE,
                    colores = RColorBrewer::brewer.pal(9, "Spectral"),
                    addPeriodo = FALSE,
                    titulo = "Distribución de graduados por sedes de la Universidad, periodo 2021-1",
                    labelEje = "Número de graduados")

Salvar(Fig10, "Export/Unimedios", "Fig10.html")

# Figura 11. Evolución histórica del total de docentes de carrera

Total_Doc <- Totales(UnalData::Docentes) %>% 
  filter(YEAR <= 2021) %>% 
  filter(!(YEAR == 2021 & SEMESTRE == 2)) %>% 
  mutate(SEMESTRE = ifelse(is.na(SEMESTRE), 2, SEMESTRE))

col <-   c("#8cc63f") # Verde, Total

Fig11 <- Plot.Series(datos = Total_Doc,
                    categoria    = "TOTAL",
                    freqRelativa = FALSE,
                    colores = col,
                    ylim = c(0, NaN),
                    titulo = "Evolución histórica del total de docentes de carrera",
                    labelY = "Número de docentes (k: miles)")

Salvar(Fig11, "Export/Unimedios", "Fig11.html")

# Figura 12. Distribución de docentes de carrera según máximo nivel de formación, 2021-1 

Forma_Doc <- Agregar(UnalData::Docentes, "FORMACION") %>% 
  filter(YEAR <= 2021)

Fig12 <- Plot.Barras(datos = Forma_Doc,
                     categoria    = "FORMACION",
                     ano          = 2021,
                     periodo      = 1,
                     freqRelativa = FALSE,
                     colores = RColorBrewer::brewer.pal(5, "Spectral"),
                     addPeriodo = FALSE,
                     titulo = "Distribución de docentes de carrera según máximo nivel de formación, periodo 2021-1",
                     labelEje = "Número de docentes")

Salvar(Fig12, "Export/Unimedios", "Fig12.html")

# Figura 13. Figura 13. Evolución histórica del total de funcionarios administrativos de carrera 

Total_Adm <- Totales(UnalData::Administrativos) %>% 
  filter(YEAR <= 2021) %>% 
  filter(!(YEAR == 2021 & SEMESTRE == 2)) %>% 
  mutate(SEMESTRE = ifelse(is.na(SEMESTRE), 2, SEMESTRE))

col <-   c("#8cc63f") # Verde, Total

Fig13 <- Plot.Series(datos = Total_Adm,
                     categoria    = "TOTAL",
                     freqRelativa = FALSE,
                     colores = col,
                     ylim = c(0, NaN),
                     titulo = "Evolución histórica del total de funcionarios administrativos de carrera",
                     labelY = "Número de funcionarios (k: miles)")

Salvar(Fig13, "Export/Unimedios", "Fig13.html")

# Figura 14. Evolución del número de funcionarios administrativos por sexo 


Sexo_Adm <- Agregar(UnalData::Administrativos, "SEXO") %>% 
  filter(YEAR <= 2021) %>% 
  filter(!(YEAR == 2021 & SEMESTRE == 2))

col <-   c( "#f15a24", "#8cc63f")

Fig14 <- Plot.Series(datos = Sexo_Adm,
                     categoria    = "SEXO",
                     freqRelativa = FALSE,
                     colores = col,
                     ylim = c(0, NaN),
                     titulo = "Evolución histórica del total de funcionarios administrativos por sexo",
                     labelY = "Número de funcionarios (k: miles)")

Salvar(Fig14, "Export/Unimedios", "Fig14.html")

# Figura 15. Pruebas con mejores resultados de la Universidad en el examen Saber Pro 2020

Saber2020 <- Saber2020 <- UnalData::SaberPro %>%
               filter(YEAR == 2020) %>% 
               add_column(TOTAL = "Universidad")

Fig15 <- Plot.Radar(datos = Saber2020,
  categoria = TOTAL,
  variables = vars(`Global` = PUNTAJE_GLOBAL,
                   `Razonamiento cuantitativo` = PUNT_RAZO_CUANT,
                   `Inglés` = PUNT_INGLES, 
                   `Lectura crítica` = PUNT_LECT_CRIT,
                   `Competencias ciudadanas` = PUNT_COMP_CIUD, 
                   `Comunicación escrita` = PUNT_COMU_ESCR),
  colores   = c("#2ACE82"),
  rango     = c(0, 200),
  estilo    = list(ply.Relleno = "tonext",
                   ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h"))
  
)

Salvar(Fig15, "Export/Unimedios", "Fig15.html")

# Figura 16. Resultados de la UNAL en la Prueba Saber Pro, por sedes.

Fig16 <- Plot.Radar(datos = Saber2020,
                    categoria = SEDE_NOMBRE_ADM,
                    variables = vars(`Global` = PUNTAJE_GLOBAL,
                                     `Razonamiento cuantitativo` = PUNT_RAZO_CUANT,
                                     `Inglés` = PUNT_INGLES, 
                                     `Lectura crítica` = PUNT_LECT_CRIT,
                                     `Competencias ciudadanas` = PUNT_COMP_CIUD, 
                                     `Comunicación escrita` = PUNT_COMU_ESCR),
                    colores = RColorBrewer::brewer.pal(8, "Spectral"),
                    rango     = c(0, 200),
                    estilo    = list(ply.Relleno = "tonext",
                                     ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h")))
Salvar(Fig16, "Export/Unimedios", "Fig16.html")

# Figura 17. Resultados Prueba Saber Pro por modalidad de ingreso

Fig17 <- Plot.Radar(datos = Saber2020,
                    categoria = TIPO_ADM,
                    variables = vars(`Global` = PUNTAJE_GLOBAL,
                                     `Razonamiento cuantitativo` = PUNT_RAZO_CUANT,
                                     `Inglés` = PUNT_INGLES, 
                                     `Lectura crítica` = PUNT_LECT_CRIT,
                                     `Competencias ciudadanas` = PUNT_COMP_CIUD, 
                                     `Comunicación escrita` = PUNT_COMU_ESCR),
                    #colores = RColorBrewer::brewer.pal(3, "Spectral"),
                    rango     = c(0, 200),
                    estilo    = list(ply.Relleno = "tonext",
                                     ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h")))

Salvar(Fig17, "Export/Unimedios", "Fig17.html")

######################################-
# 33 Solicitud 08-08-2022 -----
######################################-

# Demanda: Jisleny - Asesora Planeación Amazonía
# Nos solicitaron una información que servirá de insumo para una reunión que adelantara la Universidad con parlamentarios y entre la información solicitada nos piden 
# a las SPN que reportemos el Número de: Bachilleres anuales 2021 (por departamentos área de influencia). Que para nuestro caso son:

# •	Departamentos de Influencia Directa: Amazonas, Caquetá, Guania, Guaviare, Putumayo y Vaupés.
# •	Departamentos Influencia Indirecta: Cauca, Meta y Vichada 

# Descargar base de datos Saber 11 -20202 del portal de datos abiertos

Saber11_201 <- read.socrata(
  "https://www.datos.gov.co/resource/a8xr-en99.json",
  app_token = "WwgZOPb05lIypfR62SfgbeZbK",
  email     = "albrodriguezr@unal.edu.co",
  password  = "Socrata2021"
)

Saber11_202 <- read.socrata(
  "https://www.datos.gov.co/resource/rnvb-vnyh.json",
  app_token = "WwgZOPb05lIypfR62SfgbeZbK",
  email     = "albrodriguezr@unal.edu.co",
  password  = "Socrata2021"
)

# Consolidado departamentos de interés 20-2
Consolidado_202 <- Saber11_202 %>% 
              filter(estu_cod_reside_depto %in% c(91, 18, 94, 95, 86, 97, 19, 50, 99)) %>% 
  group_by(estu_cod_reside_depto, estu_depto_reside) %>% 
  count(name = "Bachilleres 2020-2", sort = TRUE)

# Consolidado departamentos de interés 20-1

Consolidado_201 <- Saber11_201 %>% 
  filter(estu_cod_reside_depto %in% c(91, 18, 94, 95, 86, 97, 19, 50, 99)) %>% 
  group_by(estu_cod_reside_depto, estu_depto_reside) %>% 
  count(name = "Bachilleres 2020-1", sort = TRUE)

# Cruzar bases de datos

Consolidado_2020 <- left_join(Consolidado_202, 
                              Consolidado_201, 
                              by = "estu_cod_reside_depto")

write_xlsx(Consolidado_2020, "Datos/Entrega33/Consolidado_2020.xlsx")

######################################-
# 34 Solicitud 12-08-2022 -----
######################################-

# Demanda: Daniel Carvalho Mejia 
# Honorable Representante a la Cámara

# 6.	De existir alguna prueba de admisión, presente la evolución de los 
# puntajes de admisión durante los últimos 15 años detallados semestre a 
# semestre por tipo de colegio del que se gradúa (oficial o privado), 
# por tipo de programa (técnicos, tecnológicos y universitarios), por sexo, 
# por estrato socioeconómico y por lugar de procedencia (rural/urbano).

# Función para exportar resultados en html

# Función Salvar

Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-0),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}

# Evolución resultados por sexo

PreSex <- UnalData::Aspirantes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR != 2008) %>% 
  Plot.Boxplot(variable = PTOTAL,
               grupo1   = Periodo,
               grupo2   = SEXO,
               outliers = FALSE,
               ylim     = c(0, 1000),
               colores  = c("#00ABFF", "#F3224B"),
               titulo   = "Evolución Puntaje Examen de Admisión Aspirantes Pregrado UNAL por Sexo",
               labelY   = "Puntaje Examen de Admisión",
               libreria = "highcharter",
               estilo   = list(LegendTitle = "Sexo:", hc.Tema = 6)
  )

PreSex 

Salvar(PreSex, "Export/Entrega34", "Sexo.html")

# Evolución resultados por Estrato

PreEst <- UnalData::Aspirantes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  filter(TIPO_NIVEL == "Pregrado", YEAR != 2008) %>% 
  Plot.Boxplot(variable = PTOTAL,
               grupo1   = Periodo,
               grupo2   = ESTRATO,
               outliers = FALSE,
               ylim     = c(0, 1000),
               colores  = c("#00ABFF", "#F3224B", "#29DF2C", "#FCD116"),
               titulo   = "Evolución Puntaje Examen de Admisión Aspirantes Pregrado UNAL por Estrato",
               labelY   = "Puntaje Examen de Admisión",
               libreria = "highcharter",
               estilo   = list(LegendTitle = "Estrato:", hc.Tema = 6)
  )

PreEst

Salvar(PreEst, "Export/Entrega34", "Estrato.html")

# Mapa resultados promedio municipios y departamentos

df <- UnalData::Aspirantes %>%
  filter(TIPO_NIVEL == "Pregrado", YEAR == 2022, SEMESTRE == 1) %>%
  select(Code_Dept = COD_DEP_NAC,
    Code_Mun = COD_CIU_NAC,
    Puntaje = PTOTAL)

Mapa <- Plot.Mapa(depto = df$Code_Dept,
  mpio          = df$Code_Mun,
  estadistico   = "Promedio",
  variable      = df$Puntaje,
  tipo          = "DeptoMpio",
  titulo        ="Puntaje 2022-1",
  naTo0         = FALSE,
  colNA         = "#FFFFFF",
  #centroideMapa = "BOGOTÁ, D. C.",
  zoomMapa      = 6,
  cortes        = list(
    Deptos = c(0, 300, 450, 550, Inf), Mpios = c(0, 300, 450, 550, Inf)
  ),
  colores       = list(
    Deptos = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
    Mpios  = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641")),
  showSedes     = TRUE
)

Salvar(Mapa, "Export/Entrega34", "Nacimiento.html")

######################################-
# 35 Solicitud 17-08-2022 -----
######################################-

# Demanda: Maria Claudia Galindo Gonzales 

# Cruzar bases de datos de matriculados con graduados 2022-1
# con el fin de saber quiénes estaban matriculados a través de 
# convenios

# Importar base de datos de graduados 2022-1

Graduados20221 <- read_excel("Datos/Fuentes/Graduados20221.xlsx")

# Crear llave

Graduados20221 <- Graduados20221 %>% 
                   mutate(Llave = paste0(ID, SNIES_PROGRA))


MatriculaMov <- UnalData::Matriculados %>% 
           filter(CONVENIO == "Sí") %>% 
  select(ID, TID, CONVENIO, TIP_CONVENIO, SNIES_PROGRA, NIVEL, 
         SNIESU_CONVENIO) %>% 
  mutate(Llave = paste0(ID, SNIES_PROGRA), .before = ID) %>% 
  rename(IDMat = ID,
         TIDMat = TID,
         SNIES_PROGRAMat = SNIES_PROGRA) %>% 
  group_by(Llave, IDMat, TIDMat, CONVENIO, TIP_CONVENIO, SNIES_PROGRAMat, NIVEL, 
           SNIESU_CONVENIO) %>% 
  count() %>% ungroup() %>% select(-n) 
  
# Cruzar base de datos

Grad_2022_Mov <- left_join(Graduados20221, MatriculaMov, by = "Llave") 

write_xlsx(Grad_2022_Mov, "Datos/Entrega35/Grad_2022_Mov.xlsx")

######################################-
# 36 Solicitud 18-08-2022 -----
######################################-

# Demanda: Carlos Garzón - Exjefe DNPE
# Promedio SaberPro Gestión Cultural Manizales

# Resultados SaberPro Programa Gestión Cultural

Saber_GC <- UnalData::SaberPro %>% 
  filter(SNIES_PROGRA == 16914) %>% 
  group_by(YEAR) %>% 
  summarise(across(.cols = dplyr::starts_with("PUNT"),
                .fns = list(Media = ~mean(., na.rm = TRUE),
                            Evaluados = ~n())))

write_xlsx(Saber_GC, "Datos/Entrega36/Saber_GC.xlsx")

# Resultados SaberPro Facultad de Administración

Saber_Facultad <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Manizales",
         FACULTAD == "Administración") %>% 
  group_by(YEAR) %>% 
  summarise(across(.cols = dplyr::starts_with("PUNT"),
                   .fns = list(Media = ~mean(., na.rm = TRUE),
                               Evaluados = ~n())))

write_xlsx(Saber_Facultad, "Datos/Entrega36/Saber_Facultad.xlsx")

# Resultados SaberPro Facultad de Administración

Saber_Sede <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Manizales") %>% 
  group_by(YEAR) %>% 
  summarise(across(.cols = dplyr::starts_with("PUNT"),
                   .fns = list(Media = ~mean(., na.rm = TRUE),
                               Evaluados = ~n())))
write_xlsx(Saber_Sede, "Datos/Entrega36/Saber_Sede.xlsx")

# Resultados SaberPro Universidad

Saber_Unal <- UnalData::SaberPro %>% 
  group_by(YEAR) %>% 
  summarise(across(.cols = dplyr::starts_with("PUNT"),
                   .fns = list(Media = ~mean(., na.rm = TRUE),
                               Evaluados = ~n())))

write_xlsx(Saber_Unal, "Datos/Entrega36/Saber_Unal.xlsx")

######################################-
# 37 Solicitud 30-08-2022 -----
######################################-

# Demanda: División de Registro Sede Bogotá
# Respuesta Derecho de Petición Concejal de Bogotá
# JULIÁN DAVID RODRÍGUEZ SASTOQUE

# 1. Sírvase enviar la relación respecto a los cupos de la Universidad Nacional sede Bogotá en cada semestre, desde el año 2020 a la fecha. Enviar formato en excel con:
#   
# a. No. Cupos ofertados por carrera
# b. No. de personas aspirantes por carrera
# c. No. Admitidos por carrera
# d. Porcentaje de demanda sobre oferta de cupos
# e. Porcentaje de admitidos sobre el total de inscritos

# Importar base de datos de cupos

Cupos <- read_excel("Datos/Fuentes/Histórico_Cupos.xlsx") %>% 
         mutate(IDP = as.numeric(paste0(PERIODO, CRA_SNIES)), .before = IES) %>% 
         filter(PERIODO != 20222)

# Crear base de aspirantes por carrera - Solo Post

Aspirantes <- UnalData::Aspirantes %>% 
              filter(YEAR >= 2020, TIPO_NIVEL == "Postgrado") %>% 
              mutate(SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), "", SNIES_PROGRA),
                     IDP = as.numeric(paste0(YEAR, SEMESTRE, SNIES_PROGRA))) %>% 
              relocate(IDP, .after = ID) %>% 
              group_by(IDP) %>% 
              summarise(Aspirantes = n())

# Crear base de admitidos por carrera

# names(UnalData::Aspirantes)
# unique(UnalData::Aspirantes$ADMITIDO)

Admitidos <- UnalData::Aspirantes %>% 
  filter(YEAR >= 2020, ADMITIDO == "Sí") %>% 
  mutate(SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), "", SNIES_PROGRA),
         IDP = as.numeric(paste0(YEAR, SEMESTRE, SNIES_PROGRA))) %>% 
  relocate(IDP, .after = ID) %>% 
  group_by(IDP) %>% 
  summarise(Admitidos = n())

# Base de datos consolidada

Cupos_UNAL <- left_join(Cupos, Aspirantes, by = "IDP") %>% 
              left_join(Admitidos, by = "IDP") %>%
              mutate(Aspirantes = ifelse(MODALIDAD == "Postgrado", ifelse(is.na(Aspirantes), 0,  Aspirantes), Aspirantes),
                     Admitidos = ifelse(is.na(Admitidos), 0, Admitidos),
                     Demanda = round(Aspirantes / CUPOS, 3),
                     Cobertura = round(Admitidos / Aspirantes, 3))


# Exportar Resultados

write_xlsx(Cupos_UNAL, "Datos/Entrega37/Cupos_UNAL.xlsx")


######################################-
# 38 Solicitud 12-10-2022 -----
######################################-

# Demanda: Base de datos histórica de graduados de pregrado.
# Demandante: Dirección Nacional de Programas de Pregrado.

# Base de datos inicial graduados

GraduadosPreI <- UnalData::Graduados %>% 
                filter(TIPO_NIVEL == "Pregrado") %>% 
                select(-contains("_PROC"), -c(CAT_EDAD, ESTRATO, TIPO_COL:TIPO_DISC, 
                                              MOV_PEAMA:U_CONVENIO, FACULTAD_S, 
                                              PROGRAMA_S)) %>% 
                rename(ESTRATO = ESTRATO_ORIG, EDAD = EDAD_MOD) %>% 
                relocate(YEAR, SEMESTRE, TID) %>% 
                mutate(SNIES_PROGRA = ifelse(YEAR == 2009 & SNIES_PROGRA == 128 & SEDE_NOMBRE_MAT == "Manizales", 16913, SNIES_PROGRA ))

# Base de datos de programas

Programas <- UnalData::Hprogramas %>% 
             select(SNIES_PROGRA, COD_PADRE, PROGRAMAF = PROGRAMA)

# Consolidación base de datos histórica de graduados

GraduadosPre <- left_join(GraduadosPreI, Programas, by = "SNIES_PROGRA") %>% 
                select(-c(PROGRAMA)) %>% 
                relocate(COD_PADRE, .before = SNIES_PROGRA) %>% 
                relocate(PROGRAMA = PROGRAMAF, .after = SNIES_PROGRA) %>% 
                mutate(PROGRAMA = ifelse(SNIES_PROGRA == 26, "Ingeniería de Sistemas y Computación", PROGRAMA),
                       PROGRAMA = ifelse(SNIES_PROGRA == 22, "Estudios Literarios", PROGRAMA))

write_xlsx(GraduadosPre, "Datos/Entrega38/GraduadosPre.xlsx")

######################################-
# 39 Solicitud 14-10-2022 -----
######################################-

# Demanda: PABLO FELIPE MARÍN CARDONA
# Director Académico sede Manizales

# Amablemente me permito solicitarles su ayuda en la obtención 
# de la información del indicador número de estudiantes 
# por docente en la Sede Manizales para los años 2016 a 2021 a nivel de Programa,
# Unidad Académica Básica, Facultad y Sede.


# NUEVA

# Por lo anterior, muy amablemente le solicitamos nuevamente generar la misma 
# información pero con los datos de todas las sedes, con el fin de hacer un diagnóstico 
# comparativo requerido por el programa de Ingeniería Civil sobre el indicador: estudiantes por cada docente. 
# Esta solicitud se hace en el marco del plan de mejoramiento del programa, el cual 
# tiene un objetivo para cumplimiento con esta información.

# Ajustar Nombres Facultades matriculados

#Crear base de datos de programas
Programas <- UnalData::Hprogramas %>% 
  select(SNIES_PROGRA, COD_PADRE, PROGRAMAF = PROGRAMA)

# Ajustar base de datos de matriculados 
Matriculados <- UnalData::Matriculados %>% 
                mutate(FACULTAD = if_else(FACULTAD %in% c("Agronomía", "Ciencias grarias"), "Ciencias agrarias", FACULTAD),
                       FACULTAD = if_else(FACULTAD %in% c("Ingenieria"), "Ingeniería", FACULTAD),
                       FACULTAD = if_else(FACULTAD %in% c("Ciencias humanas  y económicas"), "Ciencias humanas y económicas", FACULTAD),
                       FACULTAD = if_else(FACULTAD %in% c("Ciencias agropecuarias") & SEDE_NOMBRE_MAT == "Medellín" , "Ciencias agrarias", FACULTAD),
                       FACULTAD = if_else(FACULTAD %in% c("Ingenieria y administración"), "Ingeniería y administración", FACULTAD)) %>% 
                left_join(Programas, by = c("SNIES_PROGRA")) 

# Matriculados por Facultad

Est_Facu <- Matriculados %>% 
  filter(SNIES_SEDE_MAT %in% c(1101:1104)) %>% 
  group_by(YEAR, SEMESTRE, SEDE_NOMBRE_MAT, FACULTAD) %>% 
  summarise(`Total Estudiantes` = n()) %>% 
  arrange(desc(YEAR), desc(SEMESTRE)) %>% 
  rename(SEDE = SEDE_NOMBRE_MAT)


# Ajustar Nombres Facultades Docentes

Docentes <- UnalData::Docentes %>% 
            mutate(FACULTAD_O = if_else(FACULTAD_O == "Medicina veterinaria y zootecnia",
                                                      "Medicina veterinaria y de zootecnia", FACULTAD_O),
                   SEMESTRE = if_else(is.na(SEMESTRE), 1, SEMESTRE)) %>% 
            select(-c(FACULTAD)) %>% 
  rename(FACULTAD = FACULTAD_O)

# Docentes por facultad

Doc_Facu <- Docentes %>% 
  filter(SNIES_SEDE %in% c(1101:1104)) %>% 
  group_by(YEAR, SEMESTRE, SEDE, FACULTAD) %>% 
  summarise(`Total Docentes` = n()) %>% 
  arrange(desc(YEAR), desc(SEMESTRE)) 

# Crear base de datos total matriculados y docentes por facultad

Raz_Facu <- left_join(x = Est_Facu, 
                      y = Doc_Facu, 
                      by = c("YEAR", "SEMESTRE", "SEDE", "FACULTAD")) %>% 
            pivot_wider(names_from = c(SEDE, FACULTAD),
                        values_from = c(`Total Estudiantes`, `Total Docentes`)) 

write_xlsx(Raz_Facu, "Datos/Entrega39/Raz_Facu.xlsx")


# Docentes por unidades académicas

Docentes_Unidad <- Docentes %>% 
  filter(SNIES_SEDE %in% c(1101:1104)) %>% 
  group_by(YEAR, SEMESTRE, SEDE, FACULTAD, UNIDAD) %>% 
  summarise(`Total Docentes` = n()) %>% 
  arrange(desc(YEAR), desc(SEMESTRE))

write_xlsx(Docentes_Unidad, "Datos/Entrega39/Docentes_Unidad.xlsx")

# Matriculados por programas académicos de la sede

Estudiantes_Prog <- Matriculados %>% 
  filter(SNIES_SEDE_MAT %in% c(1101:1104)) %>% 
  group_by(YEAR, SEMESTRE, SEDE = SEDE_NOMBRE_MAT, NIVEL, FACULTAD, COD_PADRE, PROGRAMA = PROGRAMAF) %>% 
  summarise(`Total Estudiantes` = n()) %>% 
  arrange(desc(YEAR), desc(SEMESTRE)) %>% 
  rename(COD_SNIES = COD_PADRE)

write_xlsx(Estudiantes_Prog, "Datos/Entrega39/Estudiantes_Prog.xlsx")


##%######################################################%##
#                                                          #
####              40 Solicitud 21-10-2022               ####
#                                                          #
##%######################################################%##

# Demanda: Sergio Alejandro Sanchez Martinez
# Rol: estudiante UNAL

# A través del presente correo me comento con ustedes que me encuentro realizando 
# un estudio de evaluación de impacto de la política de admisión por ICFES 
# implementada por la universidad durante de la pandemia.
# 
# Para ello consulto a ustedes la viabilidad de acceso a los datos anonimizados 
# de los estudiantes de pregrado de la universidad con respecto a su PAPA, 
# modalidad (ICFES o examen), puntaje de admisión, variables socioeconómicas y 
# demás variables descriptivas o de interés de los y las estudiantes. 
# Cuales serían los requerimientos legales o procedimentales para acceder a dichos datos. 

# Crear base de datos 2019-2022 desde scrip datos abiertos

MatriculadosPre <- UnalData::Matriculados %>% 
  filter(YEAR >= 2019) %>% 
  select(-c(ID, TID, CAT_EDAD, ESTRATO, PBM, DISCAPACIDAD, 
            TIPO_DISC, SNIESU_CONVENIO, U_CONVENIO, FACULTAD_S, 
            PROGRAMA_S)) %>%                 
  rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG, 
         PBM = PBM_ORIG) %>% 
  mutate(across(.cols = c(COD_DEP_NAC, COD_CIU_NAC:LAT_CIU_NAC, COD_DEP_PROC, COD_CIU_PROC:LAT_CIU_PROC),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = c(DEP_NAC, CIU_NAC, DEP_PROC, CIU_PROC),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)),
         CODS_NAC = ifelse(is.na(CODS_NAC), "Sin información", CODS_NAC),
         CODN_NAC = ifelse(is.na(CODN_NAC), -89, CODN_NAC),
         NACIONALIDAD = ifelse(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
         EDAD = ifelse(is.na(EDAD), -89, EDAD),
         ESTRATO = ifelse(is.na(ESTRATO), "ND/NE", ESTRATO),
         TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL),
         PBM = ifelse(TIPO_NIVEL == "Postgrado", -88, PBM),
         PBM = ifelse(is.na(PBM), -89, PBM),
         PAES = ifelse(TIPO_ADM == "PAES" & is.na(PAES), "Sin información", PAES),
         PAES = ifelse(TIPO_ADM != "PAES", "No aplica", PAES),
         PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(PEAMA), "Sin información", PEAMA),
         PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", PEAMA),
         MOV_PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(MOV_PEAMA), "Sin información", MOV_PEAMA),
         MOV_PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", MOV_PEAMA),
         ADM_PEAMA_ANDINA = ifelse(TIPO_ADM == "PEAMA" & is.na(ADM_PEAMA_ANDINA), "Sin información", ADM_PEAMA_ANDINA),
         ADM_PEAMA_ANDINA = ifelse(TIPO_ADM != "PEAMA", "No aplica", ADM_PEAMA_ANDINA),
         CONVENIO = ifelse(TIPO_NIVEL == "Postgrado" & is.na(CONVENIO), "Sin información", CONVENIO),
         CONVENIO = ifelse(TIPO_NIVEL != "Postgrado", "No aplica", CONVENIO),
         TIP_CONVENIO = ifelse(CONVENIO == "Sí" & is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
         TIP_CONVENIO = ifelse(CONVENIO != "Sí", "No aplica", TIP_CONVENIO),
         FACULTAD = ifelse(is.na(FACULTAD), "Sin información", FACULTAD),
         SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), -89, SNIES_PROGRA),
         PROGRAMA = ifelse(is.na(PROGRAMA), "Sin información", PROGRAMA),
         AREAC_SNIES = ifelse(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
         CA_CINE = ifelse(is.na(CA_CINE), -89, CA_CINE),
         CD_CINE = ifelse(is.na(CD_CINE), -89, CD_CINE),
         AREA_CINE = ifelse(AREA_CINE == "Ingeniería, Industria y Construcción", "Ingeniería, industria y construcción", AREA_CINE),
         AREA_CINE = ifelse(is.na(AREA_CINE), "Sin información", AREA_CINE)) %>% 
         filter(TIPO_NIVEL == "Pregrado")

write_xlsx(MatriculadosPre, "Datos/Entrega40/MatriculadosPre.xlsx")


##%######################################################%##
#                                                          #
####              41 Solicitud 03-11-2022               ####
#                                                          #
##%######################################################%##


# Demanda: Maria Claudia Galindo Gonzales
# Rol: Funcionaria DNPE
# Solicitud: Construir Box Plot 20192 vs 20202 Puntaje Examen Admisión admitidos
# a pregrado.


# Resúmen estadístico

Asp192_202 <- UnalData::Aspirantes %>% 
              filter(YEAR == 2019 & SEMESTRE == 2 | YEAR == 2020 & SEMESTRE == 2,
                     TIPO_NIVEL == "Pregrado",
                     !is.na(TIPO_INS),
                     ADMITIDO == "Sí") %>%
              group_by(YEAR, SEMESTRE) %>% 
              summarise(across(.cols = PTOTAL, 
                               .fns = list(Mínimo = ~min(., na.rm = TRUE),
                                           Media = ~mean(., na.rm = TRUE),
                                           Máximo = ~max(., na.rm = TRUE),
                                           Sd = ~sd(., na.rm = TRUE),
                                           Total = ~n())))

# Exportar estadísticas descriptivas

write_xlsx(Asp192_202, "Datos/Entrega41/EstadisticasAdm.xlsx")

# Gráfico ggplot2

UnalData::Aspirantes %>% 
  filter(YEAR == 2019 & SEMESTRE == 2 | YEAR == 2020 & SEMESTRE == 2,
         TIPO_NIVEL == "Pregrado",
         !is.na(TIPO_INS),
         ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%    
  ggplot(aes(x = Periodo, y = PTOTAL)) +
  geom_boxplot()+
  annotate('text', x = 1, y = 580, label = 'Promedio = 604.9')+
  annotate('text', x = 2, y = 560, label = 'Promedio = 583.1')+
  labs(title = 'Distribución  Puntaje Examen de Admisión Aspirantes \nAdmitidos en la UNAL por Periodo', 
       x = ' \n Periodo de Admisión', y = 'Puntaje Prueba de Admisión')
              
##%######################################################%##
#                                                          #
####              42 Solicitud 15-11-2022               ####
#                                                          #
##%######################################################%##

# Demanda: David Ortega Patiño
# Rol: Gestor Sistema de Áreas Curriculares - Fac Minas Medellín
# Solicitud: 2.    Total, de matriculados y graduados de la maestría.
# Maestría en Ingeniería - Automatización Industrial

# Total de Matriculados

Mat_IngInd <- UnalData::Matriculados %>% 
              filter(SNIES_PROGRA == 103553) %>% 
              mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
              group_by(SNIES_PROGRA, Periodo) %>% 
              count(name = "Total Matriculados")

# Total de Graduados

Gra_IngInd <- UnalData::Graduados %>% 
  filter(SNIES_PROGRA == 103553) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  group_by(SNIES_PROGRA, Periodo) %>% 
  count(name = "Total Graduados")

# Unir bases 

IngInd <- left_join(Mat_IngInd, Gra_IngInd, by = "Periodo") %>% 
  select(Periodo, `Total Matriculados`, `Total Graduados`)

# Exportar Información

write_xlsx(IngInd, "Datos/Entrega42/IngInd.xlsx")


##%######################################################%##
#                                                          #
####              43 Solicitud 15-11-2022               ####
#                                                          #
##%######################################################%##

# Demanda: Carlos Eduardo Mosquera Noguera
# Ingeniero Químico - UNAL
# Solicitud:  información referente a puntajes de admisión para el programa 
# de admisión especial PAES  


PAES <- UnalData::Aspirantes %>% filter(TIPO_INS == "PAES", ADMITIDO == "Sí") %>% 
        filter(YEAR >= 2009) %>% 
        mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
        group_by(Periodo, PAES) %>% 
        summarise(`Total Admitidos` = n(),
                  Mímino = round(min(PTOTAL, na.rm = TRUE), 2),
                  Media = round(mean(PTOTAL, na.rm = TRUE), 2),
                  Mediana = round(median(PTOTAL, na.rm = TRUE), 2),
                  Varianza = round(var(PTOTAL, na.rm = TRUE), 2),
                  Máximo = round(max(PTOTAL, na.rm = TRUE), 2)) %>% 
        arrange(PAES)

# Exportar Información

write_xlsx(PAES, "Datos/Entrega43/PAES.xlsx")


# Puntajes en el Examen de Admisión por Programas Académicos

Adm_Pregrado <- UnalData::Aspirantes %>% filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí") %>% 
  filter(YEAR >= 2020) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-"),
         ADM_SEDE = case_when(ADM_ANDINA_PEAMA == "Bogotá" ~ "Bogotá",
                                     ADM_ANDINA_PEAMA == "Medellín" ~ "Medellín",
                                     ADM_ANDINA_PEAMA == "Manizales" ~ "Manizales",
                                     ADM_ANDINA_PEAMA == "Palmira" ~ "Palmira",
                                     TRUE  ~ ADM_SEDE_NOMBRE), 
         .before = ADM_SEDE_NOMBRE) %>% 
  select(Periodo, ADM_SEDE, ADM_SEDE_NOMBRE, SNIES_PROGRA, PROGRAMA_O = PROGRAMA, 
         MOD_INS, TIPO_INS, PAES, PEAMA, PTOTAL)

# Extraer base de datos de programas de pregrado

Hist_Programas <- UnalData::Hprogramas %>% 
                  filter(TIPO_NIVEL == "Pregrado", ESTADO == "Activo") %>% 
                  select(SNIES_PROGRA, COD_PADRE, PROGRAMA, SEDE_PROG)

# Cruzar  bases de datos con histórico programas 

Puntajes <- left_join(Adm_Pregrado, Hist_Programas, by = "SNIES_PROGRA") %>% 
  group_by(Periodo, ADM_SEDE, PROGRAMA, TIPO_INS) %>% 
  summarise(`Total Admitidos` = n(),
            Mímino = round(min(PTOTAL, na.rm = TRUE), 2),
            Media = round(mean(PTOTAL, na.rm = TRUE), 2),
            Mediana = round(median(PTOTAL, na.rm = TRUE), 2),
            Varianza = round(var(PTOTAL, na.rm = TRUE), 2),
            Máximo = round(max(PTOTAL, na.rm = TRUE), 2)) %>% 
  arrange(desc(TIPO_INS)) %>% 
  pivot_wider(names_from = TIPO_INS, values_from = `Total Admitidos`:Máximo) %>% 
  arrange(ADM_SEDE, PROGRAMA) %>% 
  relocate(contains("Regular"), contains("PAES"), contains("PEAMA"), 
           .after = PROGRAMA)

# Exportar Información General Estadísticas Admitidos

write_xlsx(Puntajes, "Datos/Entrega43/Puntajes.xlsx")


##%######################################################%##
#                                                          #
####              VIGENCIA 2023                         ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####              44 Solicitud 09-01-2023               ####
#                                                          #
##%######################################################%##

# Demanda: Alberto Rodríguez R
#  Bases de datos para canal de YouTube

Base1 <-  UnalData::SaberPro %>% sample_n(1000) %>% 
          mutate(ID = 1:1000) %>% 
          select(ID, YEAR, SEXO, ESTRATO, PUNT_RAZO_CUANT) %>% 
          rename(Id = ID, Year = YEAR, Sexo = SEXO, 
                 Csocial = ESTRATO, 
                 RMatematicas = PUNT_RAZO_CUANT) %>% 
          mutate(Sexo = case_when(Sexo == 'Mujeres' ~ 1,
                                  Sexo == 'Hombres' ~ 2),
                 Csocial = case_when(Csocial == 'Estrato 2 o menos' ~ "Baja",
                                     Csocial == 'Estrato 3' ~ "Media",
                                     Csocial == 'Estrato 4 o más' ~ "Alta",
                                     TRUE ~  as.character(NA)))

# Exportar datos

write_xlsx(Base1, "Datos/Entrega44/Base Importar Datos.xlsx")

                 
##%######################################################%##
#                                                          #
####              45 Solicitud 30-01-2023               ####
#                                                          #
##%######################################################%##

# Demanda: Maria Claudia Galindo
#  Cruzar base de graduados con graduados histórico y
# matriculados.

options(scipen = 999)

# Importar base de graduados

Grad_2023 <- read_xlsx("Datos/Fuentes/GraduadosRevision2023.xlsx", 
                        col_types = c("text")) %>% 
  select(3)

# Base de datos registros duplicados en graduados

Grad_Duplicados <- UnalData::Graduados %>% 
  mutate(IDSNIES = paste0(ID, SNIES_PROGRA),
         GRADO_ANTERIOR = 1) %>% 
  select(ID, IDSNIES, GRADO_ANTERIOR) %>% 
  group_by(ID, IDSNIES, GRADO_ANTERIOR) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  select(-c(ID, GRADO_ANTERIOR)) %>% 
  distinct()
  

Grad_IDSNIES <- UnalData::Graduados %>% 
  mutate(IDSNIES = paste0(ID, SNIES_PROGRA))

# Obtener base de graduados duplicados en programa

Base_Duplicados <- left_join(Grad_Duplicados, Grad_IDSNIES, by = "IDSNIES")

# Exportar base de datos duplicados en Pregrado

write_xlsx(Base_Duplicados, "Datos/Entrega45/Base_Duplicados.xlsx")

# Crear base de graduados antiguos sin duplicados

Grad_Antiguos <- UnalData::Graduados %>% 
                 mutate(IDSNIES = paste0(ID, SNIES_PROGRA),
                        GRADO_ANTERIOR = 1) %>% 
                 select(IDSNIES, YEAR, SEMESTRE, GRADO_ANTERIOR) %>% 
  distinct()


# Cruzar base de graduados Actual con Graduados Antiguos

Grad_2023 <- left_join(Grad_2023, Grad_Antiguos, by = "IDSNIES")

# Revisar graduado anterior
Graduados <- UnalData::Graduados %>% 
  filter(ID == "80241248")

##%######################################################%##
#                                                          #
####              46 Solicitud 03-02-2023               ####
#                                                          #
##%######################################################%##

# Demanda: Maria Claudia Galindo
#  Base de datos con documentos duplicados en graduados del 
# programa de filología e idiomas.
# SNIES_PROGRA : 23


Duplicados_Filo <- UnalData::Graduados %>% filter(SNIES_PROGRA == 23) %>% 
       group_by(ID) %>% 
       filter(n()>1)

write_xlsx(Duplicados_Filo, "Datos/Entrega46/Duplicados_Filología.xlsx")

##%######################################################%##
#                                                          #
####              48 Solicitud 27-02-2023               ####
#                                                          #
##%######################################################%##

# Demanda: Maria Claudia Galindo
# Tabla acumulada con graduados del programa PEAMA por sedes andinas
# de la Universidad

# Consolidado Graduados PAES y PEAMA

Gra_Especial <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("PEAMA")) %>% 
  summarise(Total = n(), .by = c(SEDE_NOMBRE_ADM)) %>% 
  pivot_wider(names_from = c(SEDE_NOMBRE_ADM),
              values_from = c(Total)) %>% 
  mutate(Total = Caribe + Amazonía + Tumaco + Orinoquía)

# Postgrado

UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Postgrado", SEDE_NOMBRE_ADM %in% c("Caribe", "Amazonía", "Orinoquía", "Tumaco")) %>% 
  summarise(Total = n(), .by = c(SEDE_NOMBRE_ADM))


# Total graduados 2009:2022

sum(Gra_Especial$Total)


# Consolidado Graduados PEAMA

Gra_PEAMA <- UnalData::Graduados %>% 
             filter(TIPO_ADM == "PEAMA") %>% 
             summarise(Total = n(), .by = c(SEDE_NOMBRE_ADM, ADM_PEAMA_ANDINA)) %>% 
             pivot_wider(names_from = c(ADM_PEAMA_ANDINA),
                         values_from = c(Total))

# Consolidado Graduados PAES

Gra_PAES <- UnalData::Graduados %>% 
  filter(TIPO_ADM == "PAES") %>% 
  summarise(Total = n(), .by = c(SEDE_NOMBRE_MAT, PAES)) %>% 
  pivot_wider(names_from = c(SEDE_NOMBRE_MAT),
              values_from = c(Total))

# Base Graduados PAES y PEAMA

Trem_PAESyPEAMA <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("PEAMA", "PAES")) %>% 
  mutate(TOTAL = "Total")


# Serie de Tiempo Total de Graduados

Agregar(TOTAL ~ YEAR, 
        frecuencia = c(2009:2022),
        intervalo = c(2009, 2022),
        datos = Trem_PAESyPEAMA,
        textNA = "Sin información") %>% 
  Plot.Series(categoria = "TOTAL", 
              colores = c("#8cc63f"), # verde, Total 
              titulo = "Evolución histórica anual total de graduados PAES y PEAMA, Periodo 2009-2022",  
              labelY = "Número de graduados (PAES y Peama)",
              ylim = c(0,NaN),
              labelX    = "Año",
              libreria = c("highcharter"), 
              estilo = list(hc.Tema = 1, hc.Slider = FALSE)) 

# Serie de Tiempo Total de Graduados PAES y PEAMA

        Agregar(TIPO_ADM ~ YEAR, 
        frecuencia = c(2009:2022),
        intervalo = c(2009, 2022),
        datos = Trem_PAESyPEAMA,
        textNA = "Sin información") %>% 
       mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
      Plot.Series(categoria = "TIPO_ADM", 
                      colores = c("#d7191c", "#1a9641"), # verde, Total 
                      titulo = "Evolución histórica anual total de graduados PAES y PEAMA, Periodo 2009-2022",  
                      labelY = "Número de graduados",
                      ylim = c(0,NaN),
                      labelX    = "Año",
                      libreria = c("highcharter"), 
                      estilo = list(hc.Tema = 1, hc.Slider = FALSE)) 

View(UnalData::Graduados %>% 
          filter(TIPO_ADM %in% c("PEAMA", "PAES")) %>% 
          summarise(Total = n(), .by = TIPO_ADM))

# Total de Graduados Programas PAES - Modalidades

PAES <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("PAES")) 

  Agregar(PAES ~ YEAR, 
        frecuencia = c(2009:2022),
        intervalo = c(2009, 2022),
        datos = PAES,
        textNA = "Sin información") %>% 
    Plot.Barras(
     # datos        = ejConsolidadoGrad,
      categoria    = "PAES",
      # ano          = 2021,
      # periodo      = 1,
      freqRelativa = TRUE,
      vertical     = FALSE,
      ordinal      = FALSE,
      colores      = RColorBrewer::brewer.pal(5, "Spectral"),
    #  titulo       = "Total de graduados por modalidades del programa PAES, periodo 2009-2022",
      labelEje     = "Frecuencia Relativa<br>(% de graduados)",
      # addPeriodo   = TRUE,
      # textInfo     = "Porcentaje de Graduados",
      libreria     = "highcharter",
      estilo       = list(hc.Tema = 2)
    )

# Total de Graduados Programas PEAMA - Sedes

  PEAMA <- UnalData::Graduados %>% 
    filter(TIPO_ADM %in% c("PEAMA")) 
  
  Agregar(PEAMA ~ YEAR, 
          frecuencia = c(2013:2022),
          intervalo = c(2013, 2022),
          datos = PEAMA,
          textNA = "Sin información") %>% 
    Plot.Barras(
      # datos        = ejConsolidadoGrad,
      categoria    = "PEAMA",
      # ano          = 2021,
      # periodo      = 1,
      freqRelativa = TRUE,
      vertical     = FALSE,
      ordinal      = FALSE,
      colores      = RColorBrewer::brewer.pal(4, "Spectral"),
      titulo       = "Total de graduados por sedes de admisión del programa PEAMA, periodo 2009-2022",
      labelEje     = "Frecuencia Relativa<br>(% de graduados)",
      # addPeriodo   = TRUE,
      # textInfo     = "Porcentaje de Graduados",
      libreria     = "highcharter",
      estilo       = list(hc.Tema = 2)
    )
  
# Torta Graduados por Estrato

Agregar(ESTRATO ~ YEAR + SEMESTRE, 
        frecuencia = list(c(2008:2022), c(1:2)),
        intervalo = list(c(2009, 1), c(2022, 2)),
        datos = Trem_PAESyPEAMA,
        textNA = "Sin información") %>% 
  Plot.Torta(categoria = "ESTRATO",
    colores   = c("#1a9641", "#abd9e9", "#2c7bb6", "#d7191c"),
   # titulo    = Txt,
    # libreria  = "plotly",
    estilo    = list(
      ply.Legend = "inside", ply.Credits = list(
        x = 0.66, y = 1.1)))
  
View(UnalData::Graduados %>% 
       filter(YEAR >= 2009, TIPO_NIVEL == "Pregrado") %>% 
       summarise(Total = n(), .by = ESTRATO))

# Treemap PAES y PEAMA Programas

Hprogramas <- UnalData::Hprogramas %>% 
              filter(TIPO_NIVEL == "Pregrado") %>% 
              select(SNIES_PROGRA, COD_PADRE, PrograH = PROGRAMA) %>% 
              mutate(PrograH = ifelse(COD_PADRE == 22, "Estudios Literarios", PrograH),
                     PrograH = ifelse(COD_PADRE == 26, "Ingeniería de Sistemas y Computación", PrograH))
              
Trem_PAESyPEAMA <- left_join(Trem_PAESyPEAMA, Hprogramas, by = "SNIES_PROGRA")


Plot.Treemap(datos = Trem_PAESyPEAMA,
  variables   = PrograH,
  textFreq    = "Tamaño de la Muestra",
  colores     = turbo(50, direction = -1),
  titulo      = "Histórico Graduados PAES y PEAMA, Periodo 2009-2022",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 1, hc.borderRadius = 20)
)

Trem_PAESyPEAMA

View(Trem_PAESyPEAMA %>% 
  filter(TIPO_ADM %in% c("PEAMA", "PAES")) %>% 
  summarise(Total = n(), .by = c(PrograH)) %>% 
  arrange(desc(Total)))

# Treemap PAES y PEAMA Áreas del Conocimiento

Plot.Treemap(datos = Trem_PAESyPEAMA,
             variables   = AREAC_SNIES,
             #textFreq    = "Tamaño de la Muestra",
             colores     = turbo(7, direction = -1),
             titulo      = "Histórico Graduados PAES y PEAMA POR ÁREAS DEL CONOCIMIENTO, Periodo 2009-2022",
             libreria    = "highcharter",
             estilo      = list(hc.Tema = 1, hc.borderRadius = 20)
)

# Mapa de graduados PAES y PEAMA Histórico 2009-2022 Por Departamentos

Map_PAESyPEAMA <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("PEAMA", "PAES")) %>% 
  select(
    Code_Dept    = COD_DEP_NAC,
    Code_Mun     = COD_CIU_NAC,
    Departamento = DEP_NAC,
    Municipio    = CIU_NAC,
    Longitud     = LON_CIU_NAC,
    Latitud      = LAT_CIU_NAC) %$%
  Plot.Mapa(depto = Code_Dept,
            mpio  = Code_Mun,
            tipo  = "SiNoMpios",
            # cortes = list(Deptos = c(0, 155, 170, 180, 185, Inf)),
            colores  = c("#FFFFFF", "#10F235"))

Map_PAESyPEAMA


Gra_PAESyPEAMA_2 <- UnalData::Graduados %>% 
  filter(TIPO_ADM %in% c("PEAMA", "PAES")) %>% 
  select(
    Code_Dept    = COD_DEP_NAC,
    Code_Mun     = COD_CIU_NAC,
    Departamento = DEP_NAC,
    Municipio    = CIU_NAC,
    Longitud     = LON_CIU_NAC,
    Latitud      = LAT_CIU_NAC) %$%
  Plot.Mapa(depto = Code_Dept,
            mpio  = Code_Mun,
            tipo  = "DeptoMpio",
            titulo  = "Graduados histórico",
            cortes  = list(
              Deptos = c(0, 10, 20, 50, 500, Inf),
              Mpios = c(0, 0.1, 10, 50, Inf)),
            colores = list(
              Deptos = c("#6812F2", "#5769F6", "#F6ED0D", "#EE6115", "#EC2525"),
              Mpios  = c("#FFFFFF", "#99d8c9", "#41ae76", "#005824")
            ))
            

Gra_PAESyPEAMA_2



           
# Exportar resultados
write_xlsx(Gra_PEAMA, "Datos/Entrega48/Graduados_PEAMA.xlsx")

##%######################################################%##
#                                                          #

####              49 Solicitud 27-02-2023               ####
#                                                          #
##%######################################################%##

# Demanda: Khyara Juliana Vasquez Penon <kvasquezp@unal.edu.co>
# Solicitud: Estudiante de sexto semestre de Nutrición y dietética que requiere 
# los microdatos de estudiantes, docentes y administrativos para ciertas variables

Estudiantes <- UnalData::Matriculados %>% 
               filter(YEAR == 2022, SEMESTRE == 1, between(EDAD_MOD, 18, 59)) %>% 
               select(YEAR, SEMESTRE, SEDE_NOMBRE_MAT, NIVEL, FACULTAD, 
                      DEP_PROC, CIU_PROC, SEXO, EDAD_MOD, ESTRATO_ORIG)

Docentes <- UnalData::Docentes %>% 
  filter(YEAR == 2022, SEMESTRE == 2, between(EDAD, 18, 59)) %>% 
  select(YEAR, SEMESTRE, SEDE, FACULTAD, UNIDAD, SEXO, EDAD, FORMACION)

Funcionarios <- UnalData::Administrativos %>% 
  filter(YEAR == 2022, SEMESTRE == 2, between(EDAD, 18, 59)) %>% 
  select(YEAR, SEMESTRE, SEDE, NOMBRE_ZONA, NOMBRE_UNIDAD, SEXO, EDAD, 
         NIVEL, FORMACION)

# Exportar bases de datos

write_xlsx(Estudiantes, "Datos/Entrega49/Estudiantes.xlsx")
write_xlsx(Docentes, "Datos/Entrega49/Docentes.xlsx")
write_xlsx(Funcionarios, "Datos/Entrega49/Funcionarios.xlsx")

##%######################################################%##
#                                                          #
####              50 Solicitud 16-03-2023               ####
#                                                          #
##%######################################################%##

# Solicitamos el favor de la siguiente información,
# Número de aspirantes y admitidos al programa curricular  Ingeniería Mecatrónica del 2008 a 2023-1S, solo necesitamos cifras no interesan las variables 
# 
# Agradecemos su valiosa colaboración
# 
# Atentamente,
# 
# Hugo Leonel Sierra Maldonado 
# División de Registro y Matrícula
# Universidad Nacional de Colombia


Mecatronica <- UnalData::Aspirantes %>% 
               filter(SNIES_PROGRA == e) %>% 
               mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
               summarise(Total = n(), .by = c(Periodo))

# Exportar resultados
write_xlsx(Mecatronica, "Datos/Entrega50/Mecatronica.xlsx")

##%######################################################%##
#                                                          #
####              51 Solicitud 17-03-2023               ####
#                                                          #
##%######################################################%##

# Solicitante: Gabriel Andres Avendano Casadiego

# Cordialmente me gustaría informarme si ustedes cuentan 
# con gráficas/estadísticas o en el mejor de los casos tablas de la 
# distribución del P.A.P.A. de los estudiantes de la sede Bogotá. 

# Gabriel Andres Avendaño Casadiego
# Estudiante de Ingeniería de Sistemas y Computación
# Universidad Nacional de Colombia Sede Bogotá

# SELECCIONAR BASE DE DATOS CON RESULTADOS PBM SEDE BOGOTÁ


PBM_Bog <- UnalData::Matriculados %>% 
          filter(TIPO_NIVEL == "Pregrado", !is.na(PBM_ORIG), SEDE_NOMBRE_MAT == "Bogotá") %>% 
          mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-"))
   
# Gráf1. Histórico Box PLot

PBM_Bog %>% ggplot(aes(x = Periodo, y = PBM_ORIG)) + geom_boxplot(outlier.size = 0.5 , outlier.color = "green")+
  labs(title = "Distribución de Puntajes Básicos de Matricula (PBM)",
       subtitle = "Sede Bogotá, Periodo 2010-2022")+
  xlab("\nPeriodos Académicos")+
  ylab("PBM")+
  theme(axis.text.x = element_text(angle = 90))

# Gráf1.1 Histórico Violin

PBM_Bog %>% ggplot(aes(x = Periodo, y = PBM_ORIG)) + geom_violin(outlier.size = 0.5 , outlier.color = "green", fill = "#a6d96a")+
  labs(title = "Distribución de Puntajes Básicos de Matricula (PBM)",
       subtitle = "Sede Bogotá, Periodo 2010-2022")+
  xlab("\nPeriodos Académicos")+
  ylab("PBM")+
  theme(axis.text.x = element_text(angle = 90))

  
# Gráf2. Actual - 20221- Histograma

PBM_Bog %>% filter(YEAR == 2022, SEMESTRE == 1) %>% 
  ggplot(aes(PBM_ORIG)) + geom_histogram(fill = "#1a9641", col = "#92c5de")+
labs(title = "Distribución de Puntajes Básicos de Matricula (PBM)",
     subtitle = "Sede Bogotá, Periodo 2022-1")+
  xlab("\nPuntaje Básico de Matrícula (PBM)")+
  ylab("Número de estudiantes\n")

# Gráf3. Actual - 20221- Densidad

PBM_Bog %>% filter(YEAR == 2022, SEMESTRE == 1) %>% 
  ggplot(aes(PBM_ORIG)) + geom_density(col = "red", size = 0.8)+
  labs(title = "Distribución de Densidad Puntajes Básicos de Matricula (PBM)",
       subtitle = "Sede Bogotá, Periodo 2022-1")+
  labs(x = "\nPuntaje Básico de Matrícula (PBM)",
       y = "Densidad\n")

# Tabla estadísticas descriptivas

Estad_PBM_Bog <- PBM_Bog %>% 
                 summarise(Mínimo = min(PBM_ORIG),
                           Promedio = mean(PBM_ORIG),
                           Mediana = median(PBM_ORIG),
                           Máximo = max(PBM_ORIG),
                           sd = sd(PBM_ORIG),
                           Varianza = var(PBM_ORIG),
                           Percentil_10 = quantile(PBM_ORIG, prob=c(0.1)),
                           Percentil_20 = quantile(PBM_ORIG, prob=c(0.2)),
                           Percentil_25 = quantile(PBM_ORIG, prob=c(0.25)),
                           Percentil_30 = quantile(PBM_ORIG, prob=c(0.3)),
                           Percentil_40 = quantile(PBM_ORIG, prob=c(0.4)),
                           Percentil_50 = quantile(PBM_ORIG, prob=c(0.5)),
                           Percentil_60 = quantile(PBM_ORIG, prob=c(0.6)),
                           Percentil_70 = quantile(PBM_ORIG, prob=c(0.7)),
                           Percentil_75 = quantile(PBM_ORIG, prob=c(0.75)),
                           Percentil_80 = quantile(PBM_ORIG, prob=c(0.8)),
                           Percentil_90 = quantile(PBM_ORIG, prob=c(0.9)),
                           Percentil_100 = quantile(PBM_ORIG, prob=c(1)),
                           `Total Estudiantes` = n(),
                           .by = Periodo)

# Exportar resultados
write_xlsx(Estad_PBM_Bog, "Datos/Entrega51/Estad_PBM_Bog.xlsx")

##%######################################################%##
#                                                          #
####              52 Solicitud 21-03-2023               ####
#                                                          #
##%######################################################%##

# Resultados Saber Pro por competencias y años

Plot.Radar(
  datos     = UnalData::SaberPro,
  categoria = YEAR,
  variables = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  rango     = c(0, NaN)
)

# Resultados por Sedes 2020-2021

Saber20_21 <- UnalData::SaberPro %>% 
             filter(YEAR %in% c(2020, 2021))

Plot.Boxplot(
  datos    = Saber20_21,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = SEDE_NOMBRE_ADM,
  outliers = FALSE,
  ylim     = c(0, 300),
  colores  = RColorBrewer::brewer.pal(8, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro, por Sedes",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Sede:",
                  hc.Tema = 4)
)

# Resultados por Sedes 2020 y 2021

Plot.Radar(
  datos     = Saber20_21,
  categoria = SEDE_NOMBRE_ADM,
  variables = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  rango     = c(0, NaN)
)



# Resultados por PBM

Plot.Boxplot(
  datos    = Saber20_21,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = TIPO_ADM,
  outliers = FALSE,
  ylim     = c(0, 300),
  colores  = RColorBrewer::brewer.pal(3, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro, por Modalidad de Admisión",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Modalidad de Admisión:",
                  hc.Tema = 4)
)

# Resultados por Modalidad de Admisión

Plot.Boxplot(
  datos    = Saber20_21,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = PBM,
  outliers = FALSE,
  ylim     = c(0, 300),
  colores  = RColorBrewer::brewer.pal(5, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro, por Grupos PBM",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Puntaje Básico de Matrícula (PBM):",
                  hc.Tema = 4)
)

# Resultados por Modalidades PAES

Saber20_211 <- Saber20_21 %>% filter(!is.na(PAES))

Plot.Boxplot(
  datos    = Saber20_211,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = PAES,
  outliers = FALSE,
  ylim     = c(0, 300),
  colores  = RColorBrewer::brewer.pal(5, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro, por Modalidades del Programa PAES",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Mopdalidad PAES:",
                  hc.Tema = 4)
)

# Resultados por Modalidades PEAMA

Saber20_211 <- Saber20_21 %>% filter(!is.na(PEAMA)) %>% filter(PEAMA != "PEAMA - Sede Manizales - Caldas")

Plot.Boxplot(
  datos    = Saber20_211,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = PEAMA,
  outliers = FALSE,
  ylim     = c(0, 300),
  colores  = RColorBrewer::brewer.pal(4, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro, por Modalidades del Programa PEAMA",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Mopdalidad PEAMA:",
                  hc.Tema = 4)
)

# Treemap resultados generales programas

Plot.Treemap(
  datos       = Saber20_21,
  variables   = SNIES_PROGRA,
  atributo    = PUNTAJE_GLOBAL,
  # textFreq    = "Tamaño de la Muestra",
  estadistico = "Median",
  colores     = viridis(50),
  titulo      = "Mediana Puntaje Global Prueba Saber Pro por Programas Académicos, UNAL general",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20)
)


# Resultados por Sedes 2020-2021 Facultades Bogotá

Saber20_21_Bog <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Bogotá", YEAR %in% c(2020, 2021)) %>% 
  mutate(SNIES_PROGRA = as.character(SNIES_PROGRA))

Plot.Boxplot(
  datos    = Saber20_21_Bog,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = FACULTAD,
  outliers = FALSE,
  ylim     = c(0, 300),
  #colores  = RColorBrewer::brewer.pal(11, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro - Sede Bogotá, por Facultades",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Facultad:",
                  hc.Tema = 4)
)

# Treemap de programas académicos

Plot.Treemap(
  datos       = Saber20_21_Bog,
  variables   = SNIES_PROGRA,
  atributo    = PUNTAJE_GLOBAL,
 # textFreq    = "Tamaño de la Muestra",
  estadistico = "Median",
  colores     = viridis(50),
  titulo      = "Mediana Puntaje Global Prueba Saber Pro por Programas Académicos, Sede Bogotá",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20)
)

find("viridis")

# Resultados por Sedes 2020-2021 Facultades Medellín

Saber20_21_Med <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", YEAR %in% c(2020, 2021))%>% 
  mutate(SNIES_PROGRA = as.character(SNIES_PROGRA))

Plot.Boxplot(
  datos    = Saber20_21_Med,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = FACULTAD,
  outliers = FALSE,
  ylim     = c(0, 300),
  #colores  = RColorBrewer::brewer.pal(11, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro - Sede Medellín, por Facultades",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Facultad:",
                  hc.Tema = 4)
)


# Treemap de programas académicos

Plot.Treemap(
  datos       = Saber20_21_Med,
  variables   = SNIES_PROGRA,
  atributo    = PUNTAJE_GLOBAL,
  # textFreq    = "Tamaño de la Muestra",
  estadistico = "Median",
  colores     = viridis(50),
  titulo      = "Mediana Puntaje Global Prueba Saber Pro por Programas Académicos, Sede Medellín",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20)
)

# Resultados por Sedes 2020-2021 Facultades Manizales

Saber20_21_Man <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Manizales", YEAR %in% c(2020, 2021)) %>% 
  mutate(SNIES_PROGRA = as.character(SNIES_PROGRA))

Plot.Boxplot(
  datos    = Saber20_21_Man,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = FACULTAD,
  outliers = FALSE,
  ylim     = c(0, 300),
  #colores  = RColorBrewer::brewer.pal(11, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro - Sede Manizales, por Facultades",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Facultad:",
                  hc.Tema = 4)
)

# Treemap de programas académicos

Plot.Treemap(
  datos       = Saber20_21_Man,
  variables   = SNIES_PROGRA,
  atributo    = PUNTAJE_GLOBAL,
  # textFreq    = "Tamaño de la Muestra",
  estadistico = "Median",
  colores     = viridis(50),
  titulo      = "Mediana Puntaje Global Prueba Saber Pro por Programas Académicos, Sede Manizales",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20)
)


# Resultados por Sedes 2020-2021 Facultades Palmira

Saber20_21_Pal <- UnalData::SaberPro %>% 
  filter(SEDE_NOMBRE_MAT == "Palmira", YEAR %in% c(2020, 2021)) %>% 
  mutate(SNIES_PROGRA = as.character(SNIES_PROGRA))

Plot.Boxplot(
  datos    = Saber20_21_Pal,
  variable = PUNTAJE_GLOBAL,
  grupo1   = YEAR,
  grupo2   = FACULTAD,
  outliers = FALSE,
  ylim     = c(0, 300),
  #colores  = RColorBrewer::brewer.pal(11, "Set1"),
  titulo   = "Distribución Puntaje Global Saber Pro - Sede Palmira, por Facultades",
  labelY   = "Puntaje",
  labelX = "Año",
  libreria = "highcharter",
  estilo   = list(LegendTitle = "Facultad:",
                  hc.Tema = 4)
)

# Treemap de programas académicos

Plot.Treemap(
  datos       = Saber20_21_Pal,
  variables   = SNIES_PROGRA,
  atributo    = PUNTAJE_GLOBAL,
  # textFreq    = "Tamaño de la Muestra",
  estadistico = "Median",
  colores     = viridis(50),
  titulo      = "Mediana Puntaje Global Prueba Saber Pro por Programas Académicos, Sede Palmira",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20)
)



##%######################################################%##
#                                                          #
####              53 Solicitud 22-03-2023               ####
#                                                          #
##%######################################################%##
# 
# Buenas Tardes, 
# 
# Por el presente amablemente me permito solicitar su colaboración informando las cifras históricas de Egresados de los Programas de Pregrado y Posgrado del  Área Curricular Industrial, Organizaciones y Logística: 
#   
#   4024 Ingenieria Industrial 
# 4349 Especialización en Logística y Cadenas de Abastecimiento
# 4330 Especialización en Dirección de Producción y Operaciones
# 4427 Maestria en Ingenieria - Ingenieria Industrial
# 4501 Doctorado en Ingenieria - Industria y Organizaciones. 
# 
# Quedo atento a sus comentarios, 
# 
# Saludos cordiales;
# 
# 
# FREDY BECERRA RODRIGUEZ
# Director Área Curricular Industrial, Organizaciones y Logística
# 

names(UnalData::Graduados)

Gra_Ing_Man <- UnalData::Graduados %>% 
              filter(SNIES_PROGRA %in% c(4124, 108412, 51673, 55151, 55184)) %>% 
              mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
              summarise(Total = n(), .by = c(Periodo, NIVEL, SNIES_PROGRA)) %>% 
              pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0)

# Exportar resultados

write_xlsx(Gra_Ing_Man, "Datos/Entrega53/Gra_Ing_Man.xlsx")


##%######################################################%##
#                                                          #
####              54 Solicitud 31-03-2023               ####
#                                                          #
##%######################################################%##

# Treepmap programas Pre y Post Bogotá por Facultad

# Librerías requeridas

library(tidyverse)
library(viridis)
library(UnalData)
library(UnalR)

# Programas de Pregrado - Facultad de Ciencias Humanas

Pro_Pre_Bog_CH <- UnalData::Matriculados %>% 
                  filter(SNIES_SEDE_MAT == 1101, TIPO_NIVEL == "Pregrado", 
                  YEAR == 2022, SEMESTRE == 2, 
                  FACULTAD == "Ciencias humanas") 

colores <- length(unique(Pro_Pre_Bog_CH$PROGRAMA))

  Plot.Treemap(
  datos       = Pro_Pre_Bog_CH,
  variables   = PROGRAMA,
  colores     = turbo(colores),
  titulo      = "Total de matriculados por programas académicos de pregrado",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Bogotá, Facultad de Ciencias.")
)

# Programas de Postgrado - Facultad de Medicina - Especialidades Médicas
  
Pro_Pos_Bog_Med_EM <- UnalData::Matriculados %>% 
    filter(SNIES_SEDE_MAT == 1101, TIPO_NIVEL == "Postgrado", FACULTAD == "Medicina", 
           NIVEL == "Especialidades médicas",
           YEAR == 2022, SEMESTRE == 2) 

colores <- length(unique(Pro_Pos_Bog_Med_EM$PROGRAMA))

Plot.Treemap(
  datos       = Pro_Pos_Bog_Med_EM,
  variables   = PROGRAMA,
  colores     = turbo(colores),
  titulo      = "Total de matriculados por programas académicos de postgrado",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, 
                     hc.Credits = "Sede Bogotá, Facultad de Medicina, Especialidades Médicas.")
)

##%######################################################%##
#                                                          #
####              55 Solicitud 11-04-2023               ####
#                                                          #
##%######################################################%##

# Tipo: Derecho de Petición

# Asunto: Solicitud de información con fines académicos – investigativos
# Demandante: Yizeth Andrea Quintero Cardona,

# Mail: https://mail.google.com/mail/u/0/#inbox/FMfcgzGsltShrpgbQZwTfbvSGdlwZRZC

# Base de Datos de Aspirantes y Admitidos

Aspirantes18_23 <- UnalData::Aspirantes %>% 
                    filter(YEAR >= 2018) %>%
                    filter(!(TIPO_NIVEL == "Postgrado" & MOD_INS != "Regular")) %>% 
                    filter(!(TIPO_NIVEL == "Pregrado" & is.na(TIPO_INS))) %>% 
                    select(AÑO = YEAR, SEMESTRE, TIPO_NIVEL, NIVEL, 
                           DEP_NAC:LAT_CIU_RES, SEXO, ESTRATO = ESTRATO_ORIG,
                           EDAD = EDAD_MOD, MOD_INS, TIPO_INS, PAES, PEAMA, 
                           SNIES_SEDE, INS_SEDE_NOMBRE, PTOTAL, ADMITIDO, 
                           ADM_SEDE_NOMBRE, ADM_ANDINA_PEAMA, FACULTAD, 
                           SNIES_PROGRA, PROGRAMA) 
  
# Exportar resultados

write_xlsx(Aspirantes18_23, "Datos/Entrega55/Aspirantes18_23.xlsx")

# Base de Datos de Aspirantes y Admitidos

Matriculados18_22 <- UnalData::Matriculados %>% 
  filter(YEAR >= 2018) %>% 
  select(AÑO = YEAR, SEMESTRE, TIPO_NIVEL, NIVEL, 
         DEP_NAC:LAT_CIU_PROC, SEXO, ESTRATO = ESTRATO_ORIG,
         EDAD = EDAD_MOD, TIPO_COL, SNIES_SEDE_ADM:MOV_PEAMA, 
         FACULTAD, SNIES_PROGRA, PROGRAMA) 

# Exportar resultados

write_xlsx(Matriculados18_22, "Datos/Entrega55/Matriculados18_22.xlsx")


##%######################################################%##
#                                                          #
####              56 Solicitud 12-04-2023               ####
#                                                          #
##%######################################################%##

# Mi nombre es Manuela Betancur Morales, estudiante de Ciencia Política de la Facultad de Ciencias Humanas y Económicas de la Sede Medellín. 

# 1. Estadísticas Afros Sede Medellín por Sexo

Afros_Med_Sexo <- UnalData::Matriculados %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", YEAR == 2022, SEMESTRE == 2, 
         PAES == "Población afrocolombiana") %>% summarise(Total = n(),
                                          .by = c(SEXO)) %>% 
                  pivot_wider(names_from = SEXO, values_from = Total) %>% 
                  mutate(Total = Mujeres + Hombres)

Afros_Med_Sexo

write_xlsx(Afros_Med_Sexo, "Datos/Entrega56/Afros_Med_Sexo.xlsx")

# 2. Estadísticas Afros Sede Medellín por Sexo -  Facultad de Ciencias Humanas y Económicas

Afros_Med_Fche_Sexo <- UnalData::Matriculados %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", YEAR == 2022, SEMESTRE == 2, 
         PAES == "Población afrocolombiana",
         FACULTAD == "Ciencias humanas y económicas") %>% summarise(Total = n(),
                                                           .by = c(SEXO)) %>% 
  pivot_wider(names_from = SEXO, values_from = Total) %>% 
  mutate(Total = Mujeres + Hombres)

Afros_Med_Fche_Sexo

write_xlsx(Afros_Med_Fche_Sexo, "Datos/Entrega56/Afros_Med_Fche_Sexo.xlsx")

# 4. Promedio del PBM de estudiantes de pregrado en la Sede Medellín, segmentado por género. 

Med_Sexo_Pbm <- UnalData::Matriculados %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", TIPO_NIVEL == "Pregrado", YEAR == 2022, SEMESTRE == 2) %>% 
  summarise(Media_PBM = mean(PBM_ORIG, na.rm = TRUE),
            Mediana_PBM = median(PBM_ORIG, na.rm = TRUE),
            Total = n(),
            .by = c(SEXO)) 

Med_Sexo_Pbm

write_xlsx(Med_Sexo_Pbm, "Datos/Entrega56/Med_Sexo_Pbm.xlsx")

# 3. Promedio del PBM de estudiantes de pregrado afrodescendientes en la Sede Medellín, segmentado por género. 

Afros_Med_Sexo_Pbm <- UnalData::Matriculados %>% 
  filter(SEDE_NOMBRE_MAT == "Medellín", YEAR == 2022, SEMESTRE == 2, 
         PAES == "Población afrocolombiana") %>% 
  summarise(Media_PBM = mean(PBM_ORIG, na.rm = TRUE),
            Mediana_PBM = median(PBM_ORIG, na.rm = TRUE),
            Total = n(),
            .by = c(SEXO)) 

Afros_Med_Sexo_Pbm

write_xlsx(Afros_Med_Sexo_Pbm, "Datos/Entrega56/Afros_Med_Sexo_Pbm.xlsx")


##%######################################################%##
#                                                          #
####              57 Solicitud 17-04-2023               ####
#                                                          #
##%######################################################%##

# Profesor José Ignacio Maya
# Aspirantes a pregrado y postgrado por programas académicos

Aspirantes_Programas <- read_excel("Datos/Fuentes/Aspirantes Programas.xlsx")

Consolidado_Prog <- Aspirantes_Programas %>% 
                    summarise(Total = n(), .by = c(PERIODO, PROGRAMA_SEDE)) %>% 
                    pivot_wider(names_from = PERIODO,
                                values_from = Total) %>% 
                    arrange(desc(`20191`))

# Programas de postgrado
Cons_Prog_Pos <- UnalData::Aspirantes %>% 
                  filter(YEAR >= 2019 & SEMESTRE == 1 & 
                         TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular") %>% 
                  mutate(PERIODO = paste(YEAR, SEMESTRE, sep = "-")) %>% 
                  summarise(Total = n(), .by = c(PERIODO, NIVEL, INS_SEDE_NOMBRE, FACULTAD, SNIES_PROGRA, PROGRAMA)) %>% 
                  pivot_wider(names_from = PERIODO, values_from = Total) %>% 
                  arrange(NIVEL, INS_SEDE_NOMBRE, FACULTAD, SNIES_PROGRA)

# Base de datos de programas activos - actuales

Programas_Activos <- UnalData::Hprogramas %>% filter(ESTADO == "Activo") %>% 
                     select(COD_PADRE, Programa = PROGRAMA)

Programas <- left_join(UnalData::Hprogramas, Programas_Activos, by = "COD_PADRE") %>% 
             select(SNIES_PROGRA, COD_PADRE, Programa)

# Cruzar base de datos

Cons_Prog_Pos <- left_join(Cons_Prog_Pos, Programas, by = "SNIES_PROGRA")


# Exportar resultados

write_xlsx(Consolidado_Prog, "Datos/Entrega57/Aspirantes_Prog.xlsx")

##%######################################################%##
#                                                          #
####              58 Solicitud 17-04-2023               ####
#                                                          #
##%######################################################%##

# Radar Rendición de Cuentas 2022
# Maria Claudia Galindo


Saber_UNAL_2021 <- UnalData::SaberPro %>% 
                   filter(YEAR == 2021) %>% 
                   summarise(across(.cols= starts_with("PUNT"),
                                    .fns = ~mean(., na.rm = TRUE)))


##%######################################################%##
#                                                          #
####              59 Solicitud 09-05-2023               ####
#                                                          #
##%######################################################%##

# Profesor José Ignacio Maya
# Resultados Nacional SaberPro

SaberPro_Nac_2022 <- read_excel("Datos/Fuentes/2022_SaberPro_Nacional.xlsx", 
                                      sheet = "2022 País_Saber Pro") %>% 
                     select(INST_COD_INSTITUCION,
                            INST_NOMBRE_INSTITUCION,
                            ESTU_PRGM_ACADEMICO,
                            ESTU_SNIES_PRGMACADEMICO,
                            GRUPOREFERENCIA,
                            ESTU_NIVEL_PRGM_ACADEMICO,
                            ESTU_NUCLEO_PREGRADO,
                            MOD_RAZONA_CUANTITAT_PUNT,
                            MOD_LECTURA_CRITICA_PUNT,
                            MOD_COMPETEN_CIUDADA_PUNT,
                            MOD_INGLES_PUNT,
                            MOD_COMUNI_ESCRITA_PUNT,
                            PUNT_GLOBAL,
                            SNIES_SUE:Sigla_G10) %>% 
                    mutate(G13 =  case_when(.$INST_COD_INSTITUCION == 1101  ~ "UNAL-BOGOTÁ",
                                     .$INST_COD_INSTITUCION == 1102  ~ "UNAL-MEDELLÍN",
                                     .$INST_COD_INSTITUCION == 1103  ~ "UNAL-MANIZALES",
                                     .$INST_COD_INSTITUCION == 1104  ~ "UNAL-PALMIRA",
                                     TRUE ~ U_SUE))


# Población UNAL Saber 2022

SaberPro_UNAL_22 <- SaberPro_Nac_2022 %>% 
                    filter(INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)) %>% 
                    rename(SNIES_PROGRA = ESTU_SNIES_PRGMACADEMICO)

# Crear base de programas y Facultades

Prog_Pregrado <- UnalData::Programas %>% 
  filter(TIPO_NIVEL == "Pregrado") %>% 
  select(SNIES_PROGRA, PROGRAMA, SEDE_PROG, FACULTAD, AREAC_SNIES)

# Obtener Facultades de la UNAL

SaberPro_UNAL_22 <- left_join(SaberPro_UNAL_22, Prog_Pregrado, 
                              by = c('SNIES_PROGRA')) %>% 
                    mutate(Facultad = paste(FACULTAD, SEDE_PROG, sep = "-"),
                           Programa = paste(PROGRAMA, SEDE_PROG, sep = "-"),
                           UNAL = "Unal") %>% 
                    rename(`Razonamiento Cuantitativo` = MOD_RAZONA_CUANTITAT_PUNT,
                           `Lectura Crítica` = MOD_LECTURA_CRITICA_PUNT,
                           `Competencias Ciudadanas` = MOD_COMPETEN_CIUDADA_PUNT,
                           `Inglés` = MOD_INGLES_PUNT,
                           `Comunicación Escrita` = MOD_COMUNI_ESCRITA_PUNT,
                           `Puntaje Global` = PUNT_GLOBAL) 
                

# INICIO DE TABLAS

# Tabla Universidades General

Saber_Universidades <- SaberPro_Nac_2022 %>% 
  summarise(Media_Global = round(mean(`PUNT_GLOBAL`),2),
            SD = round(sd(`PUNT_GLOBAL`),2),
            Total = n(),
            .by = c(INST_COD_INSTITUCION, INST_NOMBRE_INSTITUCION)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados

write_xlsx(Saber_Universidades, "Datos/Entrega59/Saber_Universidades.xlsx")


# Tabla SUE

SaberPro_Nac_2022_SUE <- SaberPro_Nac_2022 %>% filter(!is.na(SNIES_SUE))
  
Saber_SUE <- SaberPro_Nac_2022_SUE %>% 
  summarise(Media_Global = round(mean(`PUNT_GLOBAL`),2),
            SD = round(sd(`PUNT_GLOBAL`),2),
            Total = n(),
            .by = c(SNIES_SUE, U_SUE)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados

write_xlsx(Saber_SUE, "Datos/Entrega59/Saber_SUE.xlsx")


# Tabla SUE + UNAL

SaberPro_Nac_2022_SUE_UNAL <- SaberPro_Nac_2022 %>% filter(!is.na(G13))

Saber_SUE_UNAL <- SaberPro_Nac_2022_SUE_UNAL %>% 
  summarise(Media_Global = round(mean(`PUNT_GLOBAL`),2),
            SD = round(sd(`PUNT_GLOBAL`),2),
            Total = n(),
            .by = c(SNIES_SUE, G13)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados

write_xlsx(Saber_SUE_UNAL, "Datos/Entrega59/Saber_SUE_UNAL.xlsx")


# Tabla G10

SaberPro_Nac_2022_G10 <- SaberPro_Nac_2022 %>% filter(!is.na(SNIES_G10))

Saber_G10 <- SaberPro_Nac_2022_G10 %>% 
  summarise(Media_Global = round(mean(`PUNT_GLOBAL`),2),
            SD = round(sd(`PUNT_GLOBAL`),2),
            Total = n(),
            .by = c(SNIES_G10, U_G10)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados

write_xlsx(Saber_G10, "Datos/Entrega59/Saber_G10.xlsx")


# Tabla programas UNAL
Saber_Programas <- SaberPro_UNAL_22 %>% 
                   summarise(Media_Global = round(mean(`Puntaje Global`),2),
                             SD = round(sd(`Puntaje Global`),2),
                             Total = n(),
                             .by = c(SEDE_PROG, FACULTAD, PROGRAMA)) %>% 
                   arrange(desc(Media_Global))


# Exportar resultados

write_xlsx(Saber_Programas, "Datos/Entrega59/Saber_Programas.xlsx")


# Tabla programas UNAL - Todas las competencias

Saber_Programas_all <- SaberPro_UNAL_22 %>% 
  summarise(Media_Global = round(mean(`Puntaje Global`),2),
            SD = round(sd(`Puntaje Global`),2),
            Media_Rcuantitativo = round(mean(`Razonamiento Cuantitativo`),2),
            Media_Lcritica = round(mean(`Lectura Crítica`),2),
            Media_Cciudadanas = round(mean(`Competencias Ciudadanas`),2),
            Media_Ingles = round(mean(`Inglés`),2),
            Media_Cescrita = round(mean(`Comunicación Escrita`),2),
            Total = n(),
            .by = c(SEDE_PROG, FACULTAD, PROGRAMA)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados
write_xlsx(Saber_Programas_all, "Datos/Entrega59/Saber_Programas_all.xlsx")

# Tabla Sedes UNAL - Todas las competencias

Saber_Sedes_all <- SaberPro_UNAL_22 %>% 
  summarise(Media_Global = round(mean(`Puntaje Global`),1),
            SD = round(sd(`Puntaje Global`),1),
            Media_Rcuantitativo = round(mean(`Razonamiento Cuantitativo`),1),
            Media_Lcritica = round(mean(`Lectura Crítica`),1),
            Media_Cciudadanas = round(mean(`Competencias Ciudadanas`),1),
            Media_Ingles = round(mean(`Inglés`),1),
            Media_Cescrita = round(mean(`Comunicación Escrita`),1),
            Total = n(),
            .by = c(SEDE_PROG)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados
write_xlsx(Saber_Sedes_all, "Datos/Entrega59/Saber_Sedes_all.xlsx")


# Tabla UNAL por nucleos del conocimiento del Saber Pro

Saber_Greferencia <- SaberPro_UNAL_22 %>% 
  summarise(Media_Global = round(mean(`Puntaje Global`),1),
            SD = round(sd(`Puntaje Global`),1),
            Media_Rcuantitativo = round(mean(`Razonamiento Cuantitativo`),1),
            Media_Lcritica = round(mean(`Lectura Crítica`),1),
            Media_Cciudadanas = round(mean(`Competencias Ciudadanas`),1),
            Media_Ingles = round(mean(`Inglés`),1),
            Media_Cescrita = round(mean(`Comunicación Escrita`),1),
            Total = n(),
            .by = c(GRUPOREFERENCIA)) %>% 
  arrange(desc(Media_Global))

# Exportar resultados
write_xlsx(Saber_Greferencia, "Datos/Entrega59/Saber_Greferencia.xlsx")



# INICIO FASE DE GRÁFICOS

# Resultados Saber Pro - Por áreas del conocimiento SNIES

colores <- length(unique(SaberPro_UNAL_22$AREAC_SNIES))

Plot.Treemap(
  datos       = SaberPro_UNAL_22,
  variables   = AREAC_SNIES,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro UNAL por Áreas del Conocimiento SNIES, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Resultados Globales UNAL")
)

# Resultados Saber Pro - Todas las Facultades

colores <- length(unique(SaberPro_UNAL_22$Facultad))

Plot.Treemap(
  datos       = SaberPro_UNAL_22,
  variables   = Facultad,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades UNAL, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Resultados Globales UNAL")
)

colores <- length(unique(SaberPro_UNAL_22$Facultad))

Plot.Treemap(
  datos       = SaberPro_UNAL_22,
  variables   = Facultad,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades UNAL, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Resultados Globales UNAL")
)

# Resultados Saber Pro - Todos los programas

colores <- length(unique(SaberPro_UNAL_22$Programa))

Plot.Treemap(
  datos       = SaberPro_UNAL_22 %>% filter(SNIES_PROGRA != 16911),
  variables   = Programa,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Resultados Globales UNAL")
)

# Resultados Globales Todas las pruebas - gráfico de Radar

# Nacional

Plot.Radar(
  datos     = SaberPro_UNAL_22,
  categoria = UNAL,
  variables = vars(`Razonamiento Cuantitativo`,
                   `Lectura Crítica`,
                   `Competencias Ciudadanas`,
                   `Inglés`,
                   `Comunicación Escrita`,
                   `Puntaje Global`),
  colores   = c("#d7191c"),
  rango     = c(0, 300),
  #libreria  = "echarts",
  estilo      = list(ply.Relleno = "none",
                     ply.LegendTitle = "Sede:",
                     ply.Opacidad = 1,
                     ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h"))
)


# Por Sedes

Plot.Radar(
  datos     = SaberPro_UNAL_22,
  categoria = SEDE_PROG,
  variables = vars(`Razonamiento Cuantitativo`,
                   `Lectura Crítica`,
                   `Competencias Ciudadanas`,
                   `Inglés`,
                   `Comunicación Escrita`,
                   `Puntaje Global`),
  colores   = c("#008837", "#2c7bb6", "#fdb863", "#d7191c"),
  rango     = c(0, 200),
  #libreria  = "echarts",
  estilo      = list(ply.Relleno = "none",
                     ply.LegendTitle = "Sede:",
                     ply.Opacidad = 1,
                     ply.LegendPosition = list(x = 0, y = 0, orientation = "h"))
  )

# RESULTADOS POR FACULTADES Y PROGRAMAS DENTRO DE SEDES

SaberPro_Bog_22 <- SaberPro_UNAL_22 %>% filter(SEDE_PROG == "Bogotá")
SaberPro_Med_22 <- SaberPro_UNAL_22 %>% filter(SEDE_PROG == "Medellín")
SaberPro_Man_22 <- SaberPro_UNAL_22 %>% filter(SEDE_PROG == "Manizales") %>% filter(SNIES_PROGRA != 16911)
SaberPro_Pal_22 <- SaberPro_UNAL_22 %>% filter(SEDE_PROG == "Palmira")

# Sede Bogotá - Facultades

colores <- length(unique(SaberPro_Bog_22$FACULTAD))

Plot.Treemap(
  datos       = SaberPro_Bog_22,
  variables   = FACULTAD,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades - Sede Bogotá-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Bogotá")
)

# Sede Bogotá - Programas

colores <- length(unique(SaberPro_Bog_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Bog_22,
  variables   = PROGRAMA,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos - Sede Bogotá-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Bogotá")
)

# Sede Medellín - Facultades

colores <- length(unique(SaberPro_Med_22$FACULTAD))

Plot.Treemap(
  datos       = SaberPro_Med_22,
  variables   = FACULTAD,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades - Sede Medellín-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Medellín")
)

# Sede Medellín - Programas

colores <- length(unique(SaberPro_Med_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Med_22,
  variables   = PROGRAMA,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos - Sede Medellín-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Medellín")
)

# Sede Manizales - Facultades

colores <- length(unique(SaberPro_Man_22$FACULTAD))

Plot.Treemap(
  datos       = SaberPro_Man_22,
  variables   = FACULTAD,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades - Sede Manizales-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Manizales")
)

# Sede Manizales - Programas

colores <- length(unique(SaberPro_Man_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Man_22,
  variables   = PROGRAMA,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos - Sede Manizales-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Manizales")
)

# Sede Palmira - Facultades

colores <- length(unique(SaberPro_Pal_22$FACULTAD))

Plot.Treemap(
  datos       = SaberPro_Pal_22,
  variables   = FACULTAD,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Facultades - Sede Palmira-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Palmira")
)

# Sede Palmira - Programas

colores <- length(unique(SaberPro_Pal_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Pal_22,
  variables   = PROGRAMA,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos - Sede Palmira-, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Palmira")
)

# RESULTADOS PROGRAMAS POR FACULTADES

# Sede Bogotá

SaberPro_Bog_22 <- SaberPro_UNAL_22 %>% filter(SEDE_PROG == "Bogotá")

# Facultad de Artes

SaberPro_Fac_22 <- SaberPro_Bog_22 %>% filter(FACULTAD == "Artes")

colores <- length(unique(SaberPro_UNAL_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Fac_22,
  variables   = Programa,
  atributo    = `Puntaje Global`,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Bogotá - Facultad de Artes")
)

# Facultad de Ciencias

SaberPro_Fac_22 <- SaberPro_Bog_22 %>% filter(FACULTAD == "Ciencias")

colores <- length(unique(SaberPro_UNAL_22$Programa))

Plot.Treemap(
  datos       = SaberPro_Fac_22,
  variables   = Programa,
  atributo    = PUNT_GLOBAL,
  colores     = turbo(colores, alpha = 0, begin = 0.3, end = 1, direction = -1),
  titulo      = "Promedio Puntaje Global Prueba Saber Pro por Programas Académicos, Periodo 2022-2",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 25, hc.Credits = "Sede Bogotá - Facultad de Ciencias")
)


# EXPORTAR FACULTADES ----

SaberPro_Facultades <- read_excel("Datos/Fuentes/2022_SaberPro_Nacional.xlsx", 
                                sheet = "2022 País_Saber Pro") %>% 
  select(SNIES_IES = INST_COD_INSTITUCION,
         IES = INST_NOMBRE_INSTITUCION,
         SNIES_Programa =ESTU_SNIES_PRGMACADEMICO,
         Programa = ESTU_PRGM_ACADEMICO,
         Grupo_Referencia = GRUPOREFERENCIA,
         Nivel = ESTU_NIVEL_PRGM_ACADEMICO,
         Nucleo = ESTU_NUCLEO_PREGRADO,
         Estrato = FAMI_ESTRATOVIVIENDA,
         Tipo_IES = Institución_origen_trans,
         Sexo = ESTU_GENERO, 
         `Razonamiento Cuantitativo` = MOD_RAZONA_CUANTITAT_PUNT,
         `Lectura Crítica` = MOD_LECTURA_CRITICA_PUNT,
         `Competencias Ciudadanas` = MOD_COMPETEN_CIUDADA_PUNT,
         `Inglés` = MOD_INGLES_PUNT,
         `Comunicación Escrita` = MOD_COMUNI_ESCRITA_PUNT,
         `Puntaje Global` = PUNT_GLOBAL) 
                            
# Crear base de programas y Facultades

Prog_Pregrado <- UnalData::Programas %>% 
  filter(TIPO_NIVEL == "Pregrado") %>% 
  select(SNIES_Programa = SNIES_PROGRA, PROGRAMA, SEDE_PROG, FACULTAD, AREAC_SNIES)

# Obtener Facultades de la UNAL

SaberPro_Facultades <- left_join(SaberPro_Facultades, Prog_Pregrado, 
                              by = c('SNIES_Programa')) 
  

# Exportar resultados

write_xlsx(SaberPro_Facultades, "Datos/Entrega59/SaberPro_Facultades.xlsx")

##%######################################################%##
#                                                          #
####              60 Solicitud 09-05-2023               ####
#                                                          #
##%######################################################%##

# Solicitante: Prof. Ruby Esther Leon Diaz

# Quisiera solicitarles la siguiente información:
# 1) Evolución histórica del total de graduados de pregrado de Trabajo Social (sede Bogotá, Facultad de Ciencias Humanas)
# 2) Evolución histórica del total de estudiantes matriculados en el pregrado de Trabajo Social (ibid.)

# Evolución matriculados Trabajo Social

Mat_TS <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA == 15) %>% 
  summarise(`Total Matriculados` = n(), .by = c(YEAR, SEMESTRE)) %>% 
  arrange(YEAR, SEMESTRE) 

# Evolución Graduados Trabajo Social

Gra_TS <- UnalData::Graduados %>% 
  filter(SNIES_PROGRA == 15) %>% 
  summarise(`Total Graduados` = n(), .by = c(YEAR, SEMESTRE)) %>% 
  arrange(YEAR, SEMESTRE)

# Exportar Resultados


write_xlsx(Mat_TS, "Datos/Entrega60/Mat_TS.xlsx")
write_xlsx(Gra_TS, "Datos/Entrega60/Gra_TS.xlsx")


##%######################################################%##
#                                                          #
####              61 Solicitud 01-06-2023               ####
#                                                          #
##%######################################################%##

# Caracterización Programas PAES y PEAMA UNAL
# Solicitante: Rectoría UNAL

Mat_Especial <- UnalData::Matriculados %>% 
  filter(MOD_ADM %in% c("Especial")) %>% 
  filter(TIPO_ADM != "PEAA", YEAR >= 2013) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-"))

Mat_PAES <- Mat_Especial %>% filter(TIPO_ADM == "PAES") %>% filter(PAES != "De La Paz")
Mat_PEAMA <- Mat_Especial %>% filter(TIPO_ADM == "PEAMA")

# ESTADÍSTICAS PAES

# Por Sexo

View(Mat_PAES_Sexo <- Mat_PAES %>%
                 summarise(Total = n(),
                           .by = c(Periodo, PAES, SEXO)) %>% 
                 pivot_wider(names_from = c("PAES", "SEXO"), values_from  = "Total")) 

write_xlsx(Mat_PAES_Sexo, "Datos/Entrega61/Mat_PAES_Sexo.xlsx")

# Por Estrato

View(Mat_PAES_Estrato <- Mat_PAES %>%
       summarise(Total = n(),
                 .by = c(Periodo, PAES, ESTRATO)) %>% 
       pivot_wider(names_from = c("PAES", "ESTRATO"), values_from  = "Total")) 

write_xlsx(Mat_PAES_Estrato, "Datos/Entrega61/Mat_PAES_Estrato.xlsx")

# Por Departamentos de Procedencia

View(Mat_PAES_Dptos <- Mat_PAES %>%
       summarise(Total = n(),
                 .by = c(Periodo, PAES, COD_DEP_PROC, DEP_PROC)) %>% 
       pivot_wider(names_from = c("PAES"), values_from  = "Total", values_fill = 0)) 

write_xlsx(Mat_PAES_Dptos, "Datos/Entrega61/Mat_PAES_Dptos.xlsx")


# Por Municipios de procedencia

View(Mat_PAES_Mpos <- Mat_PAES %>%
       summarise(Total = n(),
                 .by = c(Periodo, PAES, COD_DEP_PROC, DEP_PROC, COD_CIU_PROC, CIU_PROC)) %>% 
       pivot_wider(names_from = c("PAES"), values_from  = "Total", values_fill = 0)) 

write_xlsx(Mat_PAES_Mpos, "Datos/Entrega61/Mat_PAES_Mpos.xlsx")

# ESTADÍSTICAS PEAMA

# Por Sexo

View(Mat_PEAMA_Sexo <- Mat_PEAMA %>%
       summarise(Total = n(),
                 .by = c(Periodo, PEAMA, SEXO)) %>% 
       pivot_wider(names_from = c("PEAMA", "SEXO"), values_from  = "Total")) 

write_xlsx(Mat_PEAMA_Sexo, "Datos/Entrega61/Mat_PEAMA_Sexo.xlsx")


# Por Estrato

View(Mat_PEAMA_Estrato <- Mat_PEAMA %>%
       summarise(Total = n(),
                 .by = c(Periodo, PEAMA, ESTRATO)) %>% 
       pivot_wider(names_from = c("PEAMA", "ESTRATO"), values_from  = "Total")) 

write_xlsx(Mat_PEAMA_Estrato, "Datos/Entrega61/Mat_PEAMA_Estrato.xlsx")


# Por Departamentos de Procedencia

View(Mat_PEAMA_Dptos <- Mat_PEAMA %>%
       summarise(Total = n(),
                 .by = c(Periodo, PEAMA, COD_DEP_PROC, DEP_PROC)) %>% 
       pivot_wider(names_from = c("PEAMA"), values_from  = "Total", values_fill = 0)) 

write_xlsx(Mat_PEAMA_Dptos, "Datos/Entrega61/Mat_PEAMA_Dptos.xlsx")

# Por Municipios de procedencia

View(Mat_PEAMA_Mpos <- Mat_PEAMA %>%
       summarise(Total = n(),
                 .by = c(Periodo, PEAMA, COD_DEP_PROC, DEP_PROC, COD_CIU_PROC, CIU_PROC)) %>% 
       pivot_wider(names_from = c("PEAMA"), values_from  = "Total", values_fill = 0)) 

write_xlsx(Mat_PEAMA_Mpos, "Datos/Entrega61/Mat_PEAMA_Mpos.xlsx")


##%######################################################%##
#                                                          #
####              62 Solicitud 30-06-2023               ####
#                                                          #
##%######################################################%##

# Caracterización Estudiantes Matriculados Química - Bogotá

# David Gustavo Salazar Chávez
# Estudiante Ingeniería Química y Química
# Universidad Nacional de Colombia
# Sede Bogotá

Quimica <- UnalData::Matriculados %>% 
                    filter(SNIES_SEDE_MAT == 1101, SNIES_PROGRA == 36) %>% 
                    summarise(Total = n(), .by = c(YEAR, SEMESTRE))
                    
# La solicitud se repondió por la aplicación.

##%######################################################%##
#                                                          #
####              63 Solicitud 30-06-2023               ####
#                                                          #
##%######################################################%##

# Estadísticas de Ingeniería Eléctrica

# Aspirantes

Adm_Ele <- UnalData::Aspirantes %>% 
       filter(!is.na(SNIES_PROGRA), SNIES_PROGRA == 27) %>% 
       summarise(Total = n(), .by = c(YEAR, SEMESTRE, ADMITIDO))

# Matriculados

Mat_Ele <- UnalData::Matriculados %>% 
  filter(!is.na(SNIES_PROGRA), SNIES_PROGRA == 27) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE))

# Graduados

Grad_Ele <- UnalData::Graduados %>% 
  filter(!is.na(SNIES_PROGRA), SNIES_PROGRA == 27) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE))

# Exportar bases de datos

write_xlsx(Adm_Ele, "Datos/Entrega63/Adm_Ele.xlsx")
write_xlsx(Mat_Ele, "Datos/Entrega63/Mat_Ele.xlsx")
write_xlsx(Grad_Ele, "Datos/Entrega63/Grad_Ele.xlsx")


##%######################################################%##
#                                                          #
####              64 Solicitud 30-08-2023               ####
#                                                          #
##%######################################################%##

# MEN- Municipio de Ipiales

# ¿Cuál de las Sedes de Presencia Nacional (Amazonas o Tumaco) presenta una mayor facilidad de acceso, en términos de distancia y condiciones, para el desplazamiento de los/as jóvenes? 
# ¿Podrían compartirnos datos sociodemográficos de Jardines de Sucumbios tales como: población total, estratificación, número de jóvenes que se gradúan anualmente? 
# ¿Tienen una caracterización de la trayectoria de quienes se gradúan? Es decir, cuentan con información sobre la ocupación de los/as jóvenes una vez son egresados/as de la institución educativa. 

# Código Divipola Municipio de Ipiales
# COD_CIU_RES == 52356

# Aspirantes 

# Por Sedes de Inscripción

Asp_Ipiales_Sedes <- UnalData::Aspirantes %>% 
               filter(TIPO_NIVEL == "Pregrado", COD_CIU_RES == 52356, YEAR == 2023) %>% 
               summarise(Total = n(), .by = c(YEAR, SEMESTRE, INS_SEDE_NOMBRE)) %>% 
               pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Total, values_fill = 0) %>% 
               rename(Año = YEAR, Periodo = SEMESTRE)
View(Asp_Ipiales_Sedes)

write_xlsx(Asp_Ipiales_Sedes, "Datos/Entrega64/Asp_Ipiales_Sedes.xlsx")

# Por Modalidad de Formación

Asp_Ipiales_Programa <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_INS)) %>% 
  pivot_wider(names_from = TIPO_INS, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Asp_Ipiales_Programa)

write_xlsx(Asp_Ipiales_Programa, "Datos/Entrega64/Asp_Ipiales_Programa.xlsx")

# Programa PAES

Asp_Ipiales_PAES <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, PAES)) %>% 
  pivot_wider(names_from = PAES, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Asp_Ipiales_PAES)

write_xlsx(Asp_Ipiales_PAES, "Datos/Entrega64/Asp_Ipiales_PAES.xlsx")

# Por Estrato

Asp_Ipiales_Estrato <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, ESTRATO)) %>% 
  pivot_wider(names_from = ESTRATO, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Asp_Ipiales_Estrato)

write_xlsx(Asp_Ipiales_Estrato, "Datos/Entrega64/Asp_Ipiales_Estrato.xlsx")


# Admitidos 

# Por Sedes de Inscripción

Adm_Ipiales_Sedes <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, INS_SEDE_NOMBRE)) %>% 
  pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Adm_Ipiales_Sedes)

write_xlsx(Adm_Ipiales_Sedes, "Datos/Entrega64/Adm_Ipiales_Sedes.xlsx")

# Por Modalidad de Formación

Adm_Ipiales_Programa <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_INS)) %>% 
  pivot_wider(names_from = TIPO_INS, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Adm_Ipiales_Programa)

write_xlsx(Adm_Ipiales_Programa, "Datos/Entrega64/Adm_Ipiales_Programa.xlsx")


# Programa PAES

Adm_Ipiales_PAES <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, PAES)) %>% 
  pivot_wider(names_from = PAES, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Adm_Ipiales_PAES)

write_xlsx(Adm_Ipiales_PAES, "Datos/Entrega64/Adm_Ipiales_PAES.xlsx")

# Por Estrato

Adm_Ipiales_Estrato <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí", COD_CIU_RES == 52356, YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, ESTRATO)) %>% 
  pivot_wider(names_from = ESTRATO, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Adm_Ipiales_Estrato)

write_xlsx(Adm_Ipiales_Estrato, "Datos/Entrega64/Adm_Ipiales_Estrato.xlsx")

# Matriculados

# Por Sedes de Inscripción

Mat_Ipiales_Sedes <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_PROC == 52356, YEAR == 2022, SEMESTRE ==2) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, SEDE_NOMBRE_MAT)) %>% 
  pivot_wider(names_from = SEDE_NOMBRE_MAT, values_from = Total) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Mat_Ipiales_Sedes)

write_xlsx(Mat_Ipiales_Sedes, "Datos/Entrega64/Mat_Ipiales_Sedes.xlsx")

# Por Modalidad de Formación

Mat_Ipiales_Programa <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_PROC == 52356, YEAR == 2022, SEMESTRE ==2) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_ADM)) %>% 
  pivot_wider(names_from = TIPO_ADM, values_from = Total) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Mat_Ipiales_Programa)

write_xlsx(Mat_Ipiales_Programa, "Datos/Entrega64/Mat_Ipiales_Programa.xlsx")

# Programa PAES

Mat_Ipiales_PAES <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_PROC == 52356, YEAR == 2022, SEMESTRE == 2) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, PAES)) %>% 
  pivot_wider(names_from = PAES, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Mat_Ipiales_PAES)


write_xlsx(Mat_Ipiales_PAES, "Datos/Entrega64/Mat_Ipiales_PAES.xlsx")

# Por Estrato

Mat_Ipiales_Estrato <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_PROC == 52356, YEAR == 2022, SEMESTRE ==2) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, ESTRATO)) %>% 
  pivot_wider(names_from = ESTRATO, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Mat_Ipiales_Estrato)

write_xlsx(Mat_Ipiales_Estrato, "Datos/Entrega64/Mat_Ipiales_Estrato.xlsx")

# Graduados

# Por Sedes de Inscripción

Gra_Ipiales_Sedes <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_NAC == 52356, YEAR == 2022) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, SEDE_NOMBRE_MAT)) %>% 
  pivot_wider(names_from = SEDE_NOMBRE_MAT, values_from = Total) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Gra_Ipiales_Sedes)

write_xlsx(Gra_Ipiales_Sedes, "Datos/Entrega64/Gra_Ipiales_Sedes.xlsx")

# Por Modalidad de Formación

Gra_Ipiales_Programa <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_NAC == 52356, YEAR == 2022) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_ADM)) %>% 
  pivot_wider(names_from = TIPO_ADM, values_from = Total) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Gra_Ipiales_Programa)

write_xlsx(Gra_Ipiales_Programa, "Datos/Entrega64/Gra_Ipiales_Programa.xlsx")

# Programa PAES

Gra_Ipiales_PAES <- UnalData::Graduados %>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_NAC == 52356, YEAR == 2022) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, PAES)) %>% 
  pivot_wider(names_from = PAES, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Gra_Ipiales_PAES)

write_xlsx(Gra_Ipiales_PAES, "Datos/Entrega64/Gra_Ipiales_PAES.xlsx")

# Por Estrato

Gra_Ipiales_Estrato <- UnalData::Graduados%>% 
  filter(TIPO_NIVEL == "Pregrado", COD_CIU_NAC == 52356, YEAR == 2022) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, ESTRATO)) %>% 
  pivot_wider(names_from = ESTRATO, values_from = Total, values_fill = 0) %>% 
  rename(Año = YEAR, Periodo = SEMESTRE)
View(Gra_Ipiales_Estrato)

write_xlsx(Gra_Ipiales_Estrato, "Datos/Entrega64/Gra_Ipiales_Estrato.xlsx")

##%######################################################%##
#                                                          #
####              65 Solicitud 19-09-2023               ####
#                                                          #
##%######################################################%##

# Reporte Calificadora de Riesgo UNAL

# a) Estadísticas estudiantiles para los últimos 5 años: 
# Matriculados, Aspirantes, Admitidos y Matriculados Primera Vez. 
# Si es posible, entregar la información en un archivo de Excel 
# donde se pueda detallar por sede, nivel de estudios (pregrado, posgrado) y programa académico.
# 
# b) Número de estudiantes según su estrato socioeconómico durante los últimos 5 años.

# Requerimiento a

# Aspirantes

# Aspirantes por sede

Asp_Sede <- UnalData::Aspirantes %>% 
  filter(YEAR > 2017, 
        (TIPO_INS == "Regular" & TIPO_NIVEL == "Postgrado")|
        (TIPO_NIVEL == "Pregrado"  & is.na(MOD_INS) != TRUE & is.na(TIPO_INS) != TRUE)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, INS_SEDE_NOMBRE) %>% 
  summarise(Admitidos = n()) %>%
  mutate(INS_SEDE_NOMBRE = ifelse(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE)) %>% 
  pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Admitidos, values_fill = 0) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Aspirantes por Nivel de Formación

Asp_Nivel <- UnalData::Aspirantes %>%
  filter(YEAR > 2017, 
         (TIPO_INS == "Regular" & TIPO_NIVEL == "Postgrado")|
         (TIPO_NIVEL == "Pregrado"  & is.na(MOD_INS) != TRUE & is.na(TIPO_INS) != TRUE)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, TIPO_NIVEL) %>% 
  summarise(Admitidos = n()) %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = Admitidos) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Exportar resultados

Hojas <- list('Sede' = Asp_Sede, 'Nivel' = Asp_Nivel)
openxlsx::write.xlsx(Hojas, file = 'Datos/Entrega65/Aspirantes.xlsx')


# Admitidos

# Admitidos por sede

Adm_Sede <- UnalData::Aspirantes %>% 
  filter(YEAR > 2017, 
         (TIPO_INS == "Regular" & TIPO_NIVEL == "Postgrado")|
         (TIPO_NIVEL == "Pregrado"),
         ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, INS_SEDE_NOMBRE) %>% 
  summarise(Admitidos = n()) %>% 
  pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Admitidos) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Admitidos por Nivel de Formación

Adm_Nivel <- UnalData::Aspirantes %>% 
  filter(YEAR > 2017, 
         (TIPO_INS == "Regular" & TIPO_NIVEL == "Postgrado")|
         (TIPO_NIVEL == "Pregrado"),
         ADMITIDO == "Sí") %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, TIPO_NIVEL) %>% 
  summarise(Admitidos = n()) %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = Admitidos) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Admitidos por programas académicos

Programas <- UnalData::Hprogramas %>% 
  select(SNIES_PROGRA:PROGRAMA, SEDE_PROG) %>% 
  rename(PROGRAMA_FIN = PROGRAMA)

Adm_Programas <- UnalData::Aspirantes %>% 
  left_join(Programas, by = "SNIES_PROGRA") %>% 
  filter(YEAR > 2017, 
         (TIPO_INS == "Regular" & TIPO_NIVEL == "Postgrado")|
         (TIPO_NIVEL == "Pregrado"),
         ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, SEDE_PROG, SNIES_PROGRA, TIPO_NIVEL, NIVEL, COD_PADRE, PROGRAMA_FIN) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Matriculados, values_fill = 0) %>% 
  rename(Sede = SNIES_PROGRA, Nivel = TIPO_NIVEL,  
         Snies = COD_PADRE, Programa = PROGRAMA_FIN) %>% 
  as.data.frame()

# Exportar resultados

Hojas <- list('Sede' = Adm_Sede, 'Nivel' = Adm_Nivel, 'Programas' = Adm_Programas)
openxlsx::write.xlsx(Hojas, file = 'Datos/Entrega65/Admitidos.xlsx')

# Matriculados Primera Vez

# Matriculados primera vez por sede

Mat_Pvez_Sede <- UnalData::Matriculados %>% 
  filter(YEAR > 2017, MAT_PVEZ == "Sí") %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, SEDE_NOMBRE_MAT) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = SEDE_NOMBRE_MAT, values_from = Matriculados) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Matriculados primera vez por Nivel de Formación

Mat_Pvez_Nivel <- UnalData::Matriculados %>% 
  filter(YEAR > 2017, MAT_PVEZ == "Sí") %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, TIPO_NIVEL) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = Matriculados) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Matriculados primera vez por programas académicos

Programas <- UnalData::Hprogramas %>% 
  select(SNIES_PROGRA:PROGRAMA, SEDE_PROG) %>% 
  rename(PROGRAMA_FIN = PROGRAMA)

Mat_Pvez_Programas <- UnalData::Matriculados %>% 
  left_join(Programas, by = "SNIES_PROGRA") %>% 
  filter(YEAR > 2017, MAT_PVEZ == "Sí") %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, SEDE_PROG, TIPO_NIVEL, NIVEL, COD_PADRE, PROGRAMA_FIN) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Matriculados, values_fill = 0) %>% 
  rename(Sede = SEDE_PROG, Nivel = TIPO_NIVEL,  
         Snies = COD_PADRE, Programa = PROGRAMA_FIN) %>% 
  as.data.frame()

# Exportar resultados

Hojas <- list('Sede' = Mat_Pvez_Sede, 'Nivel' = Mat_Pvez_Nivel, 'Programas' = Mat_Pvez_Programas)
openxlsx::write.xlsx(Hojas, file = 'Datos/Entrega65/MatriculadosPvez.xlsx')


# Matriculados

# Matriculados por sede

Mat_Sede <- UnalData::Matriculados %>% 
  filter(YEAR > 2017) %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, SEDE_NOMBRE_MAT) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = SEDE_NOMBRE_MAT, values_from = Matriculados) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Matriculados por Nivel de Formación

Mat_Nivel <- UnalData::Matriculados %>% 
  filter(YEAR > 2017) %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, TIPO_NIVEL) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = TIPO_NIVEL, values_from = Matriculados) %>% 
  arrange(desc(Periodo)) %>% 
  as.data.frame()

# Matriculados por programas académicos

Programas <- UnalData::Hprogramas %>% 
             select(SNIES_PROGRA:PROGRAMA, SEDE_PROG) %>% 
             rename(PROGRAMA_FIN = PROGRAMA)

Mat_Programas <- UnalData::Matriculados %>% 
  left_join(Programas, by = "SNIES_PROGRA") %>% 
  filter(YEAR > 2017) %>%
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
  group_by(Periodo, SEDE_PROG, TIPO_NIVEL, NIVEL, COD_PADRE, PROGRAMA_FIN) %>% 
  summarise(Matriculados = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Matriculados) %>% 
  rename(Sede = SEDE_PROG, Nivel = TIPO_NIVEL,  
         Snies = COD_PADRE, Programa = PROGRAMA_FIN) %>% 
  as.data.frame()

# Exportar resultados

Hojas <- list('Sede' = Mat_Sede, 'Nivel' = Mat_Nivel, 'Programas' = Mat_Programas)
openxlsx::write.xlsx(Hojas, file = 'Datos/Entrega65/Matriculados.xlsx')


# Requerimiento b

Mat_Estrato <- UnalData::Matriculados %>% 
               filter(TIPO_NIVEL == "Pregrado", YEAR > 2017) %>%
               mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%
                      group_by(Periodo, ESTRATO_ORIG) %>% 
               summarise(`Matriculados en Pregrado` = n()) %>% 
               pivot_wider(names_from = ESTRATO_ORIG, values_from = `Matriculados en Pregrado`) %>% 
               arrange(desc(Periodo)) %>% 
               as.data.frame()
               
xlsx::write.xlsx(x = Mat_Estrato, file = "Datos/Entrega65/Mat_Pre_Estrato.xlsx", sheetName="Estrato")

##%######################################################%##
#                                                          #
####              66 Solicitud 26-09-2023               ####
#                                                          #
##%######################################################%##

# Solicitante: Claudia Joya - Sede Orinoquía

# Cordial saludo
# 
# Considerando que el próximo 5 de octubre debe ser presentado
# por parte de la Dirección de Sede en la sesión del Consejo 
# Académico de la Universidad, el proyecto académico de 
# fortalecimiento de la Sede Orinoquia, derivado de los 
# recursos asignados por el Gobierno Nacional para las Sedes 
# de Frontera en los próximos 4 años, y por lo tanto debemos 
# contar con información acerca de la demanda de educación 
# superior en la región, de manera atenta se solicita su 
# colaboración en el sentido de apoyarnos con la 
# actualización de la siguiente información acerca de 
# los bachilleres graduados de los departamentos que 
# componen el área de influencia de la Sede Orinoquia.

# Descargar base de datos Saber 11 Histórica
# Fuente: https://www.datos.gov.co/Educaci-n/Resultados-nicos-Saber-11/kgxf-xxbe 

# Importar Datos

Saber11 <- read.csv("Datos/Fuentes/Resultados_Saber_11.csv")

Saber11_Orinoquia <- Saber11 %>% 
  filter(PERIODO %in% c(20221, 20224),
         ESTU_COD_RESIDE_DEPTO %in% c(81, 85, 99, 94, 95)) 

Saber11_Orinoquia_Tabla <- Saber11_Orinoquia %>% 
                          summarise(Total = n(), .by = c(ESTU_COD_RESIDE_DEPTO, ESTU_DEPTO_RESIDE))

# CONSULTA LENTA - SOLO SIRVE PARA BASES PEQUEÑAS
# 
# Saber11_201 <- read.socrata(
#   "https://www.datos.gov.co/resource/kgxf-xxbe.json",
#   app_token = "WwgZOPb05lIypfR62SfgbeZbK",
#   email     = "albrodriguezr@unal.edu.co",
#   password  = "Socrata2021"
# )


##%######################################################%##
#                                                          #
####              67 Solicitud 02-10-2023               ####
#                                                          #
##%######################################################%##

# Respuesta Derecho de Petición
# Senadora Paloma Valencia

# Punto 1. Estudiantes Nuevos de Pregrado en la UNAL durante 
# los últimos 20 años

# Admitidos

Admi_Pre20 <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo))

xlsx::write.xlsx(x = Admi_Pre20, file = "Datos/Entrega67/Punto1_Adm.xlsx", sheetName="Admitidos")

# Matriculados Primera Vez

Mpvez_Pre20 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo))

xlsx::write.xlsx(x = Mpvez_Pre20, file = "Datos/Entrega67/Punto1_Mpvez.xlsx", sheetName="Mpvez")

# Punto 2. Estudiantes Nuevos de Pregrado en la UNAL durante 
# los últimos 20 años por tipo de colegio y programa

# Serie Admitidos por Colegio

###### No existe información confiable de aspirantes según el tipo de colegio

# Admitidos por Programas Académicos

# Programas de Pregrado Activos UNAL

Prog_Pregrado <- UnalData::Hprogramas %>% 
  filter(TIPO_NIVEL == "Pregrado") %>% 
  select(COD_PADRE, SNIES_PROGRA, Programa = PROGRAMA, Sede = SEDE_PROG, Estado = ESTADO) %>% 
  mutate(Programa = ifelse(SNIES_PROGRA == 22, "Estudios Literarios", ifelse(SNIES_PROGRA == 26, "Ingeniería de Sistemas y Computación", Programa)))

# Admitidos por Programas Académicos

Adm_Prog_Pre <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  left_join(Prog_Pregrado, by = join_by(SNIES_PROGRA)) %>% 
  rename(SNIES = COD_PADRE) %>% 
  summarise(Total = n(), .by = c(Sede, SNIES, Periodo, Programa)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total) %>% 
  filter(!is.na(Sede)) %>% 
  arrange(Sede, SNIES) 
  
xlsx::write.xlsx(x = Adm_Prog_Pre, file = "Datos/Entrega67/Punto2_Adm_Prog.xlsx", sheetName="Programas_pre")


# Matriculados Primera Vez por Tipo de Colegio

Mpvez_Pre_Col <-UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", YEAR >= 2010) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, TIPO_COL)) %>% 
  mutate(TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL)) %>% 
  pivot_wider(names_from = TIPO_COL, values_from = Total, values_fill = 0)
  
xlsx::write.xlsx(x = Mpvez_Pre_Col, file = "Datos/Entrega67/Punto2_MpvezCol.xlsx", sheetName="Mpvez_Pre_Col")

# Matriculados Primera Vez por Programas Académicos 

Mpvez_Pre_Prog <-UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí") %>%  
  left_join(Prog_Pregrado, by = join_by(SNIES_PROGRA)) %>% 
  rename(SNIES = COD_PADRE) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Sede, SNIES, Programa, Periodo)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total) %>% 
  filter(!is.na(Sede)) %>% 
  arrange(Sede, SNIES) 

xlsx::write.xlsx(x = Mpvez_Pre_Prog, file = "Datos/Entrega67/Punto2_Mpvez_Pro.xlsx", sheetName="Mpvez_Pre_Prog")

# Matriculados Primera Vez por Programas Académicos y Colegios

MpvezPre_Prog_Col <-UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí", YEAR >= 2010) %>%  
  left_join(Prog_Pregrado, by = join_by(SNIES_PROGRA)) %>% 
  rename(SNIES = COD_PADRE) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Sede, SNIES, Programa, Periodo, TIPO_COL)) %>% 
  mutate(TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL)) %>% 
  pivot_wider(names_from = TIPO_COL, values_from = Total, values_fill = 0) %>% 
  filter(!is.na(Sede)) %>% 
  arrange(Sede, SNIES) %>% 
  relocate(Oficial, Privado, Otros, .after = Periodo)

xlsx::write.xlsx(x = MpvezPre_Prog_Col, file = "Datos/Entrega67/Punto2_Mpvez_Pre_Col.xlsx", sheetName="MpvezPre_Prog_Col")

# Punto 3. Cantidad de aspirantes por tipo de colegio

# La universidad no cuenta con esta información de manera precisa

# Punto 4. Aspirantes por año en pregrado en la UNAL
           # A cuáles carrera se inscribieron

# Evolución de aspirantes 

Asp_Pre20 <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", !is.na(MOD_INS)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo))

xlsx::write.xlsx(x = Asp_Pre20, file = "Datos/Entrega67/Punto4_Asp.xlsx", sheetName="AspirantesPre")


# No se cuenta con información para aspirantes por programas en pregrado
# Evolución de aspirantes por sedes

Asp_Pre20_Sedes <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", !is.na(MOD_INS), YEAR >= 2015) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, INS_SEDE_NOMBRE)) %>% 
  mutate(INS_SEDE_NOMBRE = ifelse(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE)) %>% 
  pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Total) %>% 
  relocate(Bogotá, Medellín, Manizales, Palmira, `De La Paz`, Orinoquía, Amazonía, Tumaco, Caribe, Universidad,.after = Periodo)

xlsx::write.xlsx(x = Asp_Pre20_Sedes, file = "Datos/Entrega67/Punto4_AspPre_Sedes.xlsx", sheetName="Asp_Pre_Sedes")



# Punto 5. Tasa de admisión por carreras.

# No se cuenta con tasa de admisión por carrera
# Pero sí con tasa de admisión por sedes - desde el 2015.

Adm_Pre20_Sedes <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado", !is.na(MOD_INS), ADMITIDO == "Sí", YEAR >= 2015) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, INS_SEDE_NOMBRE)) %>% 
  mutate(INS_SEDE_NOMBRE = ifelse(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE)) %>% 
  pivot_wider(names_from = INS_SEDE_NOMBRE, values_from = Total) %>% 
  relocate(Bogotá, Medellín, Manizales, Palmira, `De La Paz`, Orinoquía, Amazonía, Tumaco, Caribe, Universidad,.after = Periodo)

xlsx::write.xlsx(x = Adm_Pre20_Sedes, file = "Datos/Entrega67/Punto5_Adm_Sedes.xlsx", sheetName="Asp_Pre_Sedes")

##%######################################################%##
#                                                          #
####              68 Solicitud 04-10-2023               ####
#                                                          #
##%######################################################%##

# Solicitante: Profesor Juan Camilo Restrepo
# Vicerrector Sede Medellín
# Cifras de matriculados y docentes TCE programas áreas de la 
# Salud - Sede Bogotá

# # Matriculados Global
# 
# Unal_Salud_Gen <- UnalData::Matriculados %>% 
#                   filter(YEAR %in% c(2021, 2022), 
#                          SEDE_NOMBRE_MAT == "Bogotá",
#                          FACULTAD %in%  c("Medicina", "Enfermería")|
#                          SNIES_PROGRA == 37) %>% 
#                    mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
#                    summarise(Total = n(), .by = c(Periodo, FACULTAD)) %>% 
#                    pivot_wider(names_from = Periodo, values_from = Total)
#   

# Matriculados Pregrado

Unal_Salud_Pre <- UnalData::Matriculados %>% 
  filter(YEAR %in% c(2021, 2022), 
         SEDE_NOMBRE_MAT == "Bogotá",
         TIPO_NIVEL == "Pregrado",
         FACULTAD %in%  c("Medicina", "Enfermería")|
         SNIES_PROGRA == 37) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-"),
         PROGRAMA = ifelse(PROGRAMA == "Nutrición y dietética", "Nutrición y Dietética",
                           ifelse(PROGRAMA == "Terapia ocupacional", "Terapia Ocupacional", PROGRAMA))) %>% 
  summarise(Total = n(), .by = c(Periodo, FACULTAD, PROGRAMA)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total) %>% 
  rename(Facultad = FACULTAD, Programa = PROGRAMA) %>% 
  arrange(desc(Facultad))
       
xlsx::write.xlsx(x = Unal_Salud_Pre, file = "Datos/Entrega68/Mat_Pre_Salud.xlsx", sheetName="Matriculados")

# # Matriculados Postgrado
# 
# Unal_Salud_Pos <- UnalData::Matriculados %>% 
#   filter(YEAR %in% c(2021, 2022), 
#          SEDE_NOMBRE_MAT == "Bogotá",
#          TIPO_NIVEL == "Postgrado",
#          FACULTAD %in%  c("Medicina", "Enfermería")|
#            SNIES_PROGRA == 37) %>% 
#   mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
#   summarise(Total = n(), .by = c(Periodo, FACULTAD, NIVEL, SNIES_PROGRA)) %>% 
#   pivot_wider(names_from = Periodo, values_from = Total)
# 

##%######################################################%##
#                                                          #
####              70 Solicitud 18-10-2023               ####
#                                                          #
##%######################################################%##

# Solicitud Profesor Fernan Director DNINFOA

SaberUNAL <- UnalData::SaberPro

write_xlsx(SaberUNAL, "Datos/Entrega70/Resultados_SaberPro_UNAL.xlsx")

##%######################################################%##
#                                                          #
####              71 Solicitud 19-10-2023               ####
#                                                          #
##%######################################################%##

# Solicitud de información No. 18331
# Demandante: Sistema Quejas, Reclamos y Sugerencias Nivel Nacional - UNAL

# Solicitud: Solicito estadísticas de estudiantes de la carrera de Lingüística 
# en los años 2022 y 2023, incluyendo especialmente la
# tasa de deserción separados por admisión regular, PAES, PEAMA y especificando
# de que sede PEAMA son. 


# Tendencia General

Total_Mat_Linguistica <- UnalData::Matriculados %>% 
  filter(SNIES_PROGRA == 16938, YEAR >= 2022) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE))
  
write_xlsx(Total_Mat_Linguistica, "Datos/Entrega71/Total_Mat_Linguistica.xlsx")

# Principales Estadísticas
Mat_Linguistica <- UnalData::Matriculados %>% 
                   filter(SNIES_PROGRA == 16938, YEAR >= 2022) %>% 
                   Agregar(formula = SEXO + CAT_EDAD + NACIONALIDAD + ESTRATO_ORIG +
                                     TIPO_COL + PBM + TIPO_ADM  + PAES + PEAMA~ YEAR + SEMESTRE,
                           frecuencia = list(2022:2023, 1:2)) %>% 
                   filter(Clase != "Sin Información") %>% 
                   mutate(Variable = ifelse(Variable == "CAT_EDAD", "EDAD", Variable),
                          Variable = ifelse(Variable == "ESTRATO_ORIG", "ESTRATO", Variable),
                          Variable = ifelse(Variable == "TIPO_COL", "COLEGIO", Variable),
                          Variable = ifelse(Variable == "TIPO_ADM","TIPO DE ADMISIÓN", Variable)) %>% 
                   rename(Año = YEAR, Periodo = SEMESTRE, Modalidad = Clase)


write_xlsx(Mat_Linguistica, "Datos/Entrega71/Mat_Linguistica.xlsx")

##%######################################################%##
#                                                          #
####              72 Solicitud 01-11-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Dirección Nacional de Programas de Pregrado
# Requerimiento: Total de docentes por sedes, facultades y unidades académicas básicas

DocentesUAB <- UnalData::Docentes %>% filter(YEAR == 2023, SEMESTRE == 1) %>% 
       summarise(Total = n(), .by = c(SEDE, FACULTAD, UNIDAD))

write_xlsx(DocentesUAB, "Datos/Entrega72/DocentesUAB.xlsx")

##%######################################################%##
#                                                          #
####              73 Solicitud 17-11-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Profesor Andrés Felipe - Centro de Pensamiento Educación Superior UNAL
# Solicitud: Total de matriculados en pregrado por tipo de colegio

Mat_Colegio <- UnalData::Matriculados %>% 
       filter(TIPO_NIVEL == "Pregrado", YEAR >= 2010) %>%  
       summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_COL)) %>%
       mutate(TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL)) %>% 
       pivot_wider(names_from = TIPO_COL , values_from = Total)

write_xlsx(Mat_Colegio, "Datos/Entrega73/Mat_Colegio.xlsx")


##%######################################################%##
#                                                          #
####              74 Solicitud 07-12-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Ministerio de Educación Nacional
# Solicitud: Respuesta Encuesta Acceso y Permanencia Diferencial

# Importar base de municipios PDET

MunicipiosPDET <- read_excel("Datos/Entrega74/MunicipiosPDET.xlsx")

# Matriculados municipios PDET
Mat_Pre <- UnalData::Matriculados %>% 
          filter(TIPO_NIVEL == "Pregrado") %>% 
          left_join(MunicipiosPDET, by = c("COD_CIU_PROC")) %>% 
          filter(!is.na(SUBREGION)) %>% 
          summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
          arrange(desc(YEAR), desc(SEMESTRE))

# Matriculados primera vez municipios PDET

Mat_pvez_Pre <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado", MAT_PVEZ == "Sí") %>% 
  left_join(MunicipiosPDET, by = c("COD_CIU_PROC")) %>% 
  filter(!is.na(SUBREGION)) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
  arrange(desc(YEAR), desc(SEMESTRE))


# Matriculados primera vez por departamentos PAES

Mat_pvez_Pre_PAES <- UnalData::Matriculados %>% 
  filter(YEAR >= 2020,
         TIPO_NIVEL == "Pregrado", 
         MAT_PVEZ == "Sí", 
         TIPO_ADM == "PAES",
         !PAES %in% c("De La Paz")) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, DEP_PROC))

# Matriculados primera vez por departamentos PEAMA

Mat_pvez_Pre_PEAMA <- UnalData::Matriculados %>% 
  filter(YEAR >= 2020,
         TIPO_NIVEL == "Pregrado", 
         MAT_PVEZ == "Sí", 
         TIPO_ADM == "PEAMA",
        !PEAMA %in% c("PEAMA - Sede Bogotá - Sumapaz")) %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, DEP_PROC))


# Exportar Tablas
write_xlsx(Mat_Pre, "Datos/Entrega74/Mat_PDET.xlsx")
write_xlsx(Mat_pvez_Pre, "Datos/Entrega74/Mat_pvez_PDET.xlsx")
write_xlsx(Mat_pvez_Pre_PAES, "Datos/Entrega74/Mat_pvez_Pre_PAES.xlsx")
write_xlsx(Mat_pvez_Pre_PEAMA, "Datos/Entrega74/Mat_pvez_Pre_PEAMA.xlsx")


##%######################################################%##
#                                                          #
####              VIGENCIA 2024                        ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####              75 Solicitud 10-01-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Profe José Ignacio Maya
# Solicitud: Estudio de Contratistas Nivel Nacional 2023 PGD

# Importar base de datos

Contratos <- read_excel("Datos/Fuentes/Contratos.xlsx")
Proyectos <- read_excel("Datos/Fuentes/Proyectos.xlsx")

# Cruzar bases de datos

Contratos <- Contratos %>% 
            left_join(Proyectos, by = "ID") %>% 
            mutate(Unos = 1,
                   ValorC = case_when(Valor < 10000000 ~ "<10M",
                                      between(Valor, 10000000, 20000000)~ "Entre 10M y 20M",
                                      between(Valor, 20000001, 50000000)~ "Entre 20.1M y 50M",
                                      between(Valor, 50000001, 120000000)~ "Entre 50.1M y 120M",
                                      .default = ">120M"),
                   DiasC = case_when(Dias <=30 ~ "< 30 días",
                                     between(Dias, 31, 90)~ "Entre 31 y 90 días",
                                     between(Dias, 91, 180)~ "Entre 91 y 180 días",
                                     .default = "Más de 180 días"
                                     ),
                   Ejecucion = case_when(P_Ejecutado == 0 ~ "0% de ejecución",
                                         between(P_Ejecutado, 0.001, 50)~ "Entre 1% y 50% de ejecución",
                                         between(P_Ejecutado, 50.001, 99.9999)~ "Entre 51% y 99% de ejecución",
                                         .default = "100% de ejecución"),
                   Proyecto = ifelse(is.na(Proyecto), "SIN INFORMACIÓN", Proyecto))


# Estadísticas Descriptivas Básicas

Estadisticas <- Contratos %>% summarise(`Total Contratos` = n(),
                        `Total Contratistas` = n_distinct(Contratista),
                        `Suma Valor Contratos` = sum(Valor),
                        `Valor Mínimo` = min(Valor),
                        `Mediana Valor Contratos` = median(Valor),
                        `Valor Máximo` = max(Valor),
                        `Duración Mínima en Días` = min(Dias),
                        `Mediana Duración Contratos en Días` = median(Dias),
                        `Duración Máxima en Días` = max(Dias),
                        `Porcentaje Mínimo Ejecutado` = min(P_Ejecutado),
                        `Mediana Porcentaje Ejecutado` = median(P_Ejecutado),
                        `Porcentaje Máximo Ejecutado` = max(P_Ejecutado)) %>% 
            pivot_longer(`Total Contratos`:`Porcentaje Máximo Ejecutado`, 
                         names_to = "Aspecto", 
                         values_to = "Valor") %>% 
            mutate(Valor = as.numeric(format(Valor, scientific = FALSE)),
                   Valor = round(Valor, digits = 0))
 
# Exportar a Excel

write_xlsx(Estadisticas, "Datos/Entrega75/Estadisticas.xlsx")

# Tabla(datos = Estadisticas,
#       estatico = TRUE,
#       colorHead  = "#99d8c9",
#       estilo = list(
#       Tema = 11, Padding = c(0.8, 3))) %>% 
#       cols_align(align = "left", columns = c(`Aspecto`)) %>% 
#       fmt_number(decimals = 10) # No funciona

# Variable Empresa

Plot.Barras(
  datos     = Contratos,
  categoria = Nom_Empresa,
  valores = Unos,
  estatico = TRUE,
  #vertical = TRUE,
  #freqRelativa = FALSE,
  ordinal   = FALSE,
  ylim = c(0, 350),
  titulo     = "Total de Contratos por Empresa QUIPU",
  labelX = "\nEmpresa",
  labelY = "\nTotal de contratos",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))


# Mes de Inicio

Plot.Barras(
  datos     = Contratos,
  categoria = Mes_Inicio,
  valores = Unos,
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 23),
  titulo     = "Mes de Inicio de los Contratos",
  labelX = "\nMes",
  labelY = "\n% de contratos",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))

# Tabla

Contratos %>% 
  summarise(Total = n(), .by = c(Mes_Inicio)) %>% 
  mutate(Porcentaje = Total /sum(Total),
  Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Mes de Inicio` = Mes_Inicio,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
   cols_align(align = "left", columns = c(`Mes de Inicio`))

# Mes de Finalización de los contratos

Plot.Barras(
  datos     = Contratos,
  categoria = Mes_Fin_Year,
  valores = Unos,
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 23),
  titulo     = "Mes de finalización de los contratos",
  labelX = "\nMes",
  labelY = "\n% de contratos",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))

# Tabla

Contratos %>% 
  summarise(Total = n(), .by = c(Mes_Fin_Year)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Mes de Finalización` = Mes_Fin_Year,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Mes de Finalización`))


# Valor de los contratos

Plot.Barras(
  datos     = Contratos,
  categoria = ValorC,
  valores = Unos,
  estatico = TRUE,
  vertical = TRUE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 50),
  titulo     = "Distribución Valor de los Contratos",
  labelX = "\nValor del Contrato",
  labelY = "% de contratos\n",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))

# Tabla

Contratos %>% 
  summarise(Total = n(), .by = c(ValorC)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Valor del Contrato` = ValorC,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Valor del Contrato`))


# Duración de los contratos

Plot.Barras(
  datos     = Contratos,
  categoria = DiasC,
  valores = Unos,
  estatico = TRUE,
  vertical = TRUE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 50),
  titulo     = "Duración de los Contratos (en Días)",
  labelX = "\nDuración del Contrato (en días)",
  labelY = "% de contratos\n",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))


# Tabla

Contratos %>% 
  summarise(Total = n(), .by = c(DiasC)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Duración del Contrato` = DiasC,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Duración del Contrato`))


# Porcentaje de ejecución

Plot.Barras(
  datos     = Contratos,
  categoria = Ejecucion,
  valores = Unos,
  estatico = TRUE,
  vertical = TRUE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 50),
  titulo     = "Porcentaje de Ejecución de los Contratos",
  labelX = "\nPorcentaje de ejecución",
  labelY = "% de contratos\n",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))


Contratos %>% 
  summarise(Total = n(), .by = c(Ejecucion)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`% de ejecución` = Ejecucion,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`% de ejecución`))


# Contratos por proyectos

Contratos %>% 
       filter(Proyecto != "SIN INFORMACIÓN") %>% 
  summarise(Total = n(), .by = c(Proyecto, QUIPU_Proyecto
,Nom_Empresa)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  arrange(desc(Total)) %>% 
  rename(`Código QUIPU`= QUIPU_Proyecto,
         `Empresa QUIPU` = Nom_Empresa) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Proyecto`))

# Ejes del PGD

Plot.Barras(
  datos     = Contratos,
  categoria = Eje,
  valores = Unos,
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 50),
  titulo     = "Total Contratos por Ejes PGD 2022-2024",
  labelX = "\nEje",
  labelY = "% de contratos\n",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))

# Tabla
Contratos %>% 
  summarise(Total = n(), .by = c(Eje)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Eje PGD 2022-2024` = Eje,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Eje PGD 2022-2024`))


# Programas del PGD

Plot.Barras(
  datos     = Contratos,
  categoria = Programa,
  valores = Unos,
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  ylim = c(0, 50),
  titulo     = "Total Contratos por Programas PGD 2022-2024",
  labelX = "\nEje",
  labelY = "% de contratos\n",
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.6, color = "#000000")))

# Tabla
Contratos %>% 
  summarise(Total = n(), .by = c(Programa)) %>% 
  mutate(Porcentaje = Total /sum(Total),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(`Programa PGD 2022-2024` = Programa,
         `Total Contratos` = Total) %>% 
  arrange(desc(`Total Contratos`)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  cols_align(align = "left", columns = c(`Programa PGD 2022-2024`))

##%######################################################%##
#                                                          #
####              76 Solicitud 17-01-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Asesor Rectoría
# Solicitud: Solicitud Estadísticas Admitidos 2024-1

# Importar base de datos

# Pregrado
Adm_Pre_241 <- read_excel("Datos/Fuentes/2024-1 PRE AspAdm Temporal.xlsx") %>% 
               mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
               filter(ADMITIDO == "Sí")

# Postgrado
Adm_Pos_241 <- read_excel("Datos/Fuentes/2024-1 POS AspAdm Temporal.xlsx") %>% 
  filter(TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  filter(ADMITIDO == "Sí")


Adm_241 <- bind_rows(Adm_Pre_241, Adm_Pos_241)

# Extracción de información estadística requerida

# Modalidad de formación

Adm_Mod_Forma <- Adm_241 %>% summarise(Total = n(), .by = c(Periodo, TIPO_NIVEL))
write_xlsx(Adm_Mod_Forma, "Datos/Entrega76/Adm_Mod_Forma.xlsx")

# Sedes

Adm_Sede <- Adm_241 %>% summarise(Total = n(), .by = c(Periodo, ADM_SEDE_NOMBRE))
write_xlsx(Adm_Sede, "Datos/Entrega76/Adm_Sede.xlsx")

# Género

Adm_Genero <- Adm_241 %>% summarise(Total = n(), .by = c(Periodo, SEXO))
write_xlsx(Adm_Genero, "Datos/Entrega76/Adm_Genero.xlsx")

# Modalidad de Acceso

Adm_Mod_Acceso <- Adm_241 %>% summarise(Total = n(), .by = c(Periodo, TIPO_INS))
write_xlsx(Adm_Mod_Acceso, "Datos/Entrega76/Adm_Mod_Acceso.xlsx")

# PEAMA

Adm_PEAMA <- Adm_Pre_241 %>% filter(TIPO_INS == "PEAMA") %>% 
  summarise(Total = n(), .by = c(Periodo, PEAMA))
write_xlsx(Adm_PEAMA, "Datos/Entrega76/Adm_PEAMA.xlsx")


# PAES

Adm_PAES <- Adm_Pre_241 %>% filter(TIPO_INS == "PAES") %>% 
            summarise(Total = n(), .by = c(Periodo, PAES))
write_xlsx(Adm_PAES, "Datos/Entrega76/Adm_PAES.xlsx")


##%######################################################%##
#                                                          #
####              77 Solicitud 19-01-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Jorge Córdoba - Candidado Rectoría UNAL
# Solicitud: Se requiere información estadística oficial
#             general de la Universidad.


# 1. ⁠número inscritos 

Aspirantes <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) |> 
  group_by(Periodo) %>% 
  summarise(Total = n())

write_xlsx(Aspirantes, "Datos/Entrega77/Aspirantes.xlsx")

# 2. número de admitidos

Admitidos <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular"),
         ADMITIDO == "Sí") %>% 
    mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%  
  group_by(Periodo) %>% 
  summarise(Total = n())

write_xlsx(Admitidos, "Datos/Entrega77/Admitidos.xlsx")

# 3. número de matriculados 

Matriculados <- UnalData::Matriculados %>% 
                mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%  
                group_by(Periodo) %>% 
                summarise(Total = n())

write_xlsx(Matriculados, "Datos/Entrega77/Matriculados.xlsx")

# 7. numéro de graduados 

Graduados <- UnalData::Graduados %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%  
  group_by(Periodo) %>% 
  summarise(Total = n())

write_xlsx(Graduados, "Datos/Entrega77/Graduados.xlsx")

# 8. ⁠número de docentes 

Docentes <- UnalData::Docentes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%  
  group_by(Periodo) %>% 
  summarise(Total = n())

write_xlsx(Docentes, "Datos/Entrega77/Docentes.xlsx")


# 9. ⁠número de administrativos 

Administrativos <- UnalData::Administrativos %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>%  
  group_by(Periodo) %>% 
  summarise(Total = n())

write_xlsx(Administrativos, "Datos/Entrega77/Administrativos.xlsx")

# 10. ⁠tasa de deserción 
# 11. ⁠presupuesto 


##%######################################################%##
#                                                          #
####              78 Solicitud 05-02-2023               ####
#                                                          #
##%######################################################%##

# Demandante: Aminta Mendoza Barón - Delegada profesor Germán Albeiro Castaño (inscrito como aspirante en el proceso de Designación de Rector)
# Solicitud: 12.  Población estudiantil (por género), y profesoral (por género) por facultades de la universidad, en el trienio 2021-2023
# Derecho de Petición https://mail.google.com/mail/u/0/#inbox/FMfcgzGxRdsZPDjdxWsJcddQnDWTpbKp

# Sexo Docentes de Carrera por Facultad

Doc_Sexo <- UnalData::Docentes %>% 
            filter(YEAR >= 2020) %>% 
            summarise(Total = n(), .by = c(YEAR, SEMESTRE, SEDE, FACULTAD_O, SEXO)) %>% 
            pivot_wider(names_from = SEXO, values_from = Total, values_fill = 0) %>% 
            mutate(Total = Hombres + Mujeres,
                   Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
            rename(Facultad = FACULTAD_O, Sede = SEDE) %>% 
            select(Periodo, Sede:Total)

write_xlsx(Doc_Sexo, "Datos/Entrega78/Doc_Sexo.xlsx")

# Sexo Estudiantes por Facultad

Est_Sexo <- UnalData::Matriculados %>% 
  mutate(FACULTAD = ifelse(SEDE_NOMBRE_MAT %in% c("Amazonía", "Caribe", "Orinoquía", "Tumaco"), "Dirección de Sede", FACULTAD),
         FACULTAD = ifelse(SEDE_NOMBRE_MAT == "De La Paz", "Escuela de pregrado", FACULTAD)) %>% 
  filter(YEAR >= 2020) %>%
  group_by(YEAR, SEMESTRE, SEDE_NOMBRE_MAT, FACULTAD, SEXO) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = SEXO, values_from = Total, values_fill = 0) %>% 
  mutate(Total = Hombres + Mujeres,
         Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  rename(Sede = SEDE_NOMBRE_MAT, Facultad = FACULTAD) %>% 
  ungroup() %>% 
  select(Periodo, Sede:Total)

write_xlsx(Est_Sexo, "Datos/Entrega78/Est_Sexo.xlsx")

##%######################################################%##
#                                                          #
####              79 Solicitud 08-02-2024              ####
#                                                          #
##%######################################################%##

# Jose Ignacio Maya - Información para dar repuesta a Derecho de 
# Petición de la Senadora Cabal.

Mat14 <- read_excel("Datos/Fuentes/Mat14.xlsx") %>% 
          mutate(ID = as.character(ID)) %>% 
          left_join(UnalData::Matriculados %>% filter(YEAR == 2023), by = "ID")


write_xlsx(Mat14, "Datos/Entrega79/Mat14.xlsx")


##%######################################################%##
#                                                          #
####              80 Solicitud 12-02-2024              ####
#                                                          #
##%######################################################%##

# Respuesta Derecho de Petición Sofia Ariza CC 1032503327

# 3.	Informar por escrito el número de estudiantes admitidos para el 
# presente calendario académico y discriminar por carreras 

# Programas Académicos

Sprogra <- UnalData::Hprogramas %>% select(SNIES_PROGRA, SEDE_PROG, FACULTAD_PROGRA)

# Admitidos Pregrado y cruzar programas académicos de origen

Admitidos_Pre <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS),
         YEAR == 2023,
         SEMESTRE == 2,
         ADMITIDO == "Sí") %>% 
  left_join(Sprogra, by = "SNIES_PROGRA") %>% 
  group_by(SEDE_PROG, FACULTAD_PROGRA, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(`Total Admitidos` = n()) %>% 
  rename(`Sede del Programa` = SEDE_PROG,
         `Facultad del Programa`=FACULTAD_PROGRA,
         `Código SNIES del Programa` = SNIES_PROGRA,
         `Nombre del Programa` = PROGRAMA) %>% 
  ungroup()

write_xlsx(Admitidos_Pre, "Datos/Entrega80/Admitidos_Pre.xlsx")


##%######################################################%##
#                                                          #
####              81  Solicitud 05-03-2024              ####
#                                                          #
##%######################################################%##

# Maria Claudia Galindo
# Consolidación datos telaraña SaberPro - Rendición de Cuentas

RC2023 <- UnalData::SaberPro %>% 
           filter(YEAR == 2022) %>% 
           select(TIPO_ADM, starts_with("PUNT")) %>% 
           pivot_longer(PUNTAJE_GLOBAL:PUNT_RAZO_CUANT, 
                        names_to = "Prueba", 
                        values_to = "Puntaje") %>% 
           summarise(Promedio = round(mean(Puntaje, na.rm = TRUE),1), 
                     .by = c(TIPO_ADM, Prueba)) %>% 
           pivot_wider(names_from = TIPO_ADM, values_from = Promedio)
             
# Exportar Consolidado

write_xlsx(RC2023, "Datos/Entrega81/RC2023.xlsx")


##%######################################################%##
#                                                          #
####              82 Solicitud 12-02-2024              ####
#                                                          #
##%######################################################%##

# Respuesta Derecho de Petición Respuesta Vicerrectoría Sede 
# Bogotá

# Memo 251-24
# https://mail.google.com/mail/u/0/#search/derecho+de+petici%C3%B3n/FMfcgzGxSbmJsqkqZVgFbMTgvhVFfdvc

# 1. Proporcionar el número total de estudiantes admitidos en cada una de las carreras
# ofrecidas por la Universidad Nacional de Colombia en la Sede Bogotá, en los últimos 5
# años, desglosados por género y orientación sexual.

# 2. Indicar el número total de estudiantes con discapacidad admitidos por la UNAL
# en cada una de las carreras en la Sede Bogotá, durante los últimos 5 años. 
# Especificar el tipo de discapacidad que presentan (por ejemplo, física, sensorial,
# intelectual, etc.).


# Programas Académicos

Programas <- UnalData::Hprogramas %>% 
  group_by(COD_PADRE) %>% 
  filter(SNIES_PROGRA == max(SNIES_PROGRA)) %>% 
  rename(`Nombre Programa` = PROGRAMA) %>% 
  select(COD_PADRE, `Nombre Programa`) %>% 
  ungroup()

Sprogra <- UnalData::Hprogramas %>% 
           left_join(Programas, by = "COD_PADRE") %>% 
           select(SNIES_PROGRA, `Nombre Programa`, SEDE_PROG, FACULTAD_PROGRA)

# Admitidos por programas y Sexo

Admitidos_19a23_Sexo <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) %>% 
  filter(ADMITIDO == "Sí", YEAR >= 2019, ADM_SEDE_NOMBRE == "Bogotá") %>% 
 left_join(Sprogra, by = "SNIES_PROGRA") %>% 
  group_by(SEDE_PROG, TIPO_NIVEL, NIVEL, FACULTAD_PROGRA, SNIES_PROGRA, `Nombre Programa`, YEAR, SEMESTRE, SEXO) %>% 
  summarise(`Total Admitidos` = n()) %>% 
  rename(`Sede del Programa` = SEDE_PROG,
         `Nivel de Formación` = TIPO_NIVEL, 
         `Modalidad de Formación` = NIVEL,
         `Facultad del Programa`=FACULTAD_PROGRA,
         `Código SNIES del Programa` = SNIES_PROGRA,
         `Año` = YEAR,
         `Semestre` = SEMESTRE,
         `Sexo` = SEXO) %>% 
  arrange(`Sede del Programa`, `Nivel de Formación`, `Modalidad de Formación`, 
          `Facultad del Programa`, `Código SNIES del Programa`, 
          `Nombre Programa`, desc(Año), desc(Semestre)) %>% 
  pivot_wider(names_from = Sexo, values_from = `Total Admitidos`, values_fill = 0) %>% 
  mutate(`Total Admitidos` = Hombres + Mujeres + Transgénero,
         Transgénero = ifelse(Año <= 2022, NA, Transgénero)) %>% 
  ungroup()

write_xlsx(Admitidos_19a23_Sexo, "Datos/Entrega82/Admitidos_19a23_Sexo.xlsx")

# Admitidos por programas, discapacidad y tipo de discapacidad

Admitidos_19a23_Discapacidad <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) %>% 
  filter(ADMITIDO == "Sí", YEAR >= 2019, ADM_SEDE_NOMBRE == "Bogotá") %>% 
  left_join(Sprogra, by = "SNIES_PROGRA") %>% 
  group_by(SEDE_PROG, TIPO_NIVEL, NIVEL, FACULTAD_PROGRA, SNIES_PROGRA, `Nombre Programa`, YEAR, SEMESTRE, DISCAPACIDAD, TIPO_DISC) %>% 
  summarise(`Total Admitidos` = n()) %>% 
  rename(`Sede del Programa` = SEDE_PROG,
         `Nivel de Formación` = TIPO_NIVEL, 
         `Modalidad de Formación` = NIVEL,
         `Facultad del Programa`=FACULTAD_PROGRA,
         `Código SNIES del Programa` = SNIES_PROGRA,
         `Año` = YEAR,
         `Semestre` = SEMESTRE,
         `Discapacidad` = DISCAPACIDAD,
         `Tipo Discapacidad` = TIPO_DISC) %>% 
  arrange(`Sede del Programa`, `Nivel de Formación`, `Modalidad de Formación`, 
          `Facultad del Programa`, `Código SNIES del Programa`, 
          `Nombre Programa`, desc(Año), desc(Semestre)) %>% 
  pivot_wider(names_from = c(`Discapacidad`, `Tipo Discapacidad`), 
              values_from = `Total Admitidos`, 
              values_fill = 0) %>% 
  rename(Visual = Sí_Visual,
         Auditiva = Sí_Auditiva,
         Psicosocial = Sí_Psicosocial,
         Motriz = Sí_Motriz,
         Otras = Sí_Otras,
         Cognitiva = Sí_Cognitiva,
         No = No_NA) %>% 
  mutate(Sí = Visual+Auditiva+Psicosocial+Motriz+Otras+Cognitiva) %>% 
  relocate(Sí, .before = Visual) %>% 
  relocate(Otras, .after = Cognitiva) %>%  
  ungroup()

write_xlsx(Admitidos_19a23_Discapacidad, "Datos/Entrega82/Admitidos_19a23_Discapacidad.xlsx")

# Matriculados por programas y Sexo

Matriculados_19a23_Sexo <- UnalData::Matriculados %>% 
                           filter(YEAR >= 2019, SEDE_NOMBRE_MAT == "Bogotá") %>% 
  left_join(Sprogra, by = "SNIES_PROGRA") %>% 
  group_by(SEDE_PROG, TIPO_NIVEL, NIVEL, FACULTAD_PROGRA, SNIES_PROGRA, `Nombre Programa`, YEAR, SEMESTRE, SEXO) %>% 
  summarise(`Total Matriculados` = n()) %>% 
  rename(`Sede del Programa` = SEDE_PROG,
         `Nivel de Formación` = TIPO_NIVEL, 
         `Modalidad de Formación` = NIVEL,
         `Facultad del Programa`=FACULTAD_PROGRA,
         `Código SNIES del Programa` = SNIES_PROGRA,
         `Año` = YEAR,
         `Semestre` = SEMESTRE,
         `Sexo` = SEXO) %>% 
  arrange(`Sede del Programa`, `Nivel de Formación`, `Modalidad de Formación`, 
          `Facultad del Programa`, `Código SNIES del Programa`, 
          `Nombre Programa`, desc(Año), desc(Semestre)) %>% 
  pivot_wider(names_from = Sexo, values_from = `Total Matriculados`, values_fill = 0) %>% 
  mutate(`Total Matriculados` = Hombres + Mujeres) %>% 
  ungroup()

write_xlsx(Matriculados_19a23_Sexo, "Datos/Entrega82/Matriculados_19a23_Sexo.xlsx")

##%######################################################%##
#                                                          #
####              83 Solicitud 16-04-2024              ####
#                                                          #
##%######################################################%##

# Tendencia UNAL Informe de gestión Periodo Rectoral Dolly
# Solicita: Catalina Vasquez DNPE

# Aspirantes y Admitidos

Aspirantes <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) %>% 
  mutate(GLOBAL = "Total")

# Aspirantes general

Asp_Global <- Agregar(datos = Aspirantes,
  formula    = GLOBAL ~ YEAR + SEMESTRE,
  frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
  intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
    datos = Asp_Global,
    categoria    = "GLOBAL",
    freqRelativa = FALSE,
    invertir     = FALSE,
    ylim         = c(0, 100000),
    colores      = "#525252",
    titulo       = "Evolución Total de Aspirantes en la UNAL",
    labelY       = "Número de Aspirantes \n",
    labelX = "\nPeriodo",
    estatico     = TRUE,
    estilo       = list(
      gg.Tema = list(Tema = "theme_gray"),
      gg.Legend = list(legend.position = "none"),
      gg.Linea  = list(linetype = 5, size = 0.5),
      gg.Repel = list(size = 3.18, color = "red",
        direction = "both", seed = 42, nudge_y = 0.25,
        arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
        box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
        min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
      ),
      gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Aspirantes Pregrado

Asp_Pre <- Agregar(datos = Aspirantes %>% filter(TIPO_NIVEL == "Pregrado"),
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))) 

Plot.Series(
  datos = Asp_Pre,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 100000),
  colores      = "#525252",
  titulo       = "Evolución Total de Aspirantes a Pregrado en la UNAL",
  labelY       = "Número de Aspirantes \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Aspirantes a Postgrado

Asp_Pos <- Agregar(datos = Aspirantes %>% filter(TIPO_NIVEL == "Postgrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))) 

Plot.Series(
  datos = Asp_Pos,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 10000),
  colores      = "#525252",
  titulo       = "Evolución Total de Aspirantes a Postgrado en la UNAL",
  labelY       = "Número de Aspirantes \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


# Admitidos general

Admitidos <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular"),
         ADMITIDO == "Sí") %>% 
  mutate(GLOBAL = "Total")


Adm_Global <- Agregar(datos = Admitidos,
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Adm_Global,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 10000),
  colores      = "#525252",
  titulo       = "Evolución Total de Admitidos en la UNAL",
  labelY       = "Número de Admitidos \n",
  labelX = "\nPeriodo",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Admitidos a Pregrado

Adm_Pre <- Agregar(datos = Admitidos %>% filter(TIPO_NIVEL == "Pregrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))) 

Plot.Series(
  datos = Adm_Pre,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 10000),
  colores      = "#525252",
  titulo       = "Evolución Total de Admitidos a Pregrado en la UNAL",
  labelY       = "Número de Admitidos \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Admitidos a Postgrado

Adm_Pos <- Agregar(datos = Admitidos %>% filter(TIPO_NIVEL == "Postgrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))) 

Plot.Series(
  datos = Adm_Pos,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 3000),
  colores      = "#525252",
  titulo       = "Evolución Total de Admitidos a Postgrado en la UNAL",
  labelY       = "Número de Admitidos \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


# Matriculados General

Matriculados <- UnalData::Matriculados %>% 
  mutate(GLOBAL = "Total")


Mat_Global <- Agregar(datos = Matriculados,
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Mat_Global,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 70000),
  colores      = "#525252",
  titulo       = "Evolución Total de Matriculados en la UNAL",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Matriculados en Pregrado

Mat_Pre <- Agregar(datos = Matriculados %>% filter(TIPO_NIVEL == "Pregrado"),
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Mat_Pre,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 60000),
  colores      = "#525252",
  titulo       = "Evolución Total de Matriculados en Pregrado en la UNAL",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9))

# Matriculados en Pregrado por Programa de Admisión

Especial <- Matriculados %>% filter(TIPO_NIVEL == "Pregrado") %>% 
                             mutate(MOD_ADM = ifelse(SEDE_NOMBRE_ADM == "De La Paz" & YEAR <= 2022, "Regular", MOD_ADM),
                                    TIPO_ADM = ifelse(SEDE_NOMBRE_ADM == "De La Paz" & YEAR <= 2022, "Regular", TIPO_ADM))


Mat_Pre_Adm <- Agregar(datos = Especial,
                   formula    = TIPO_ADM ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))) %>% 
                     filter(Clase != "PEAA")


Plot.Series(
  datos = Mat_Pre_Adm,
  categoria    = "TIPO_ADM",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 45000),
  colores      = c("#e66101", "#0571b0", "#d7191c"),
  titulo       = "Evolución Total de Matriculados en Pregrado Según Modalidad de Admisión",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    LegendTitle = "Tipo de Admisión",
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "black",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9))


# Matriculados en Pregrado Admisión Especial

Mat_Pre_Esp <- Agregar(datos = Especial %>% filter(MOD_ADM == "Especial"),
                       formula    = TIPO_ADM ~ YEAR + SEMESTRE,
                       frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                       intervalo  = list(c(2021, 1), c(2023, 2))) %>% 
  filter(Clase != "PEAA")


Plot.Series(
  datos = Mat_Pre_Esp,
  categoria    = "TIPO_ADM",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 5000),
  colores      = c("#0571b0", "#d7191c"),
  titulo       = "Evolución Total de Matriculados en Pregrado - Programas Admisión Especial",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    LegendTitle = "Programa",
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "black",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9))


# Matriculados en Postgrado

Mat_Pos <- Agregar(datos = Matriculados %>% filter(TIPO_NIVEL == "Postgrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Mat_Pos,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 10000),
  colores      = "#525252",
  titulo       = "Evolución Total de Matriculados en Postgrado en la UNAL",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.5 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


# Matriculados por Areas del Conocimiento CINE

Mat_Area_Cine <- Agregar(datos = UnalData::Matriculados %>% 
                                 mutate(AREA_CINE = ifelse(AREA_CINE == "Ciencias sociales", "Ciencias sociales, periodismo e información", AREA_CINE),
                                        AREA_CINE = ifelse(AREA_CINE == "Tecnologías de la información y la comunicación", "Tecnologías de la información y la comunicación (TIC)", AREA_CINE),
                                        AREA_CINE = ifelse(AREA_CINE == "Ingeniería, industria y construcción", "Ingeniería, Industria y Construcción", AREA_CINE)),
                       formula    = AREA_CINE ~ YEAR + SEMESTRE,
                       frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                       intervalo  = list(c(2021, 1), c(2023, 2))) %>% 
                       filter(Clase != "Sin Información")

# Serie

Plot.Series(
  datos = Mat_Area_Cine,
  categoria    = "AREA_CINE",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 20000),
  colores      = RColorBrewer::brewer.pal(9, "Spectral"),
  titulo       = "Evolución Total de Matriculados por CINE",
  labelY       = "Número de Matriculados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    LegendTitle = "Programa",
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "black",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9))


# Estado actual

Mat_Area_Cine %>% 
  Plot.Barras(categoria = "AREA_CINE",
              ano = 2023,
              periodo = 2,
              estatico = TRUE, 
              freqRelativa = FALSE,
              vertical = FALSE,
              ylim = c(0, 20000),
              addPeriodo = FALSE,
              labelY = "\nTotal matriculados (%)",
              labelX = "\nÁrea CINE",
              titulo = "Total de Matriculados por Área de Conocimiento CINE",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2023-2")))



# Graduados General

Graduados <- UnalData::Graduados %>% 
  mutate(GLOBAL = "Total")


Gra_Global <- Agregar(datos = Graduados,
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Gra_Global,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 6000),
  colores      = "#525252",
  titulo       = "Evolución Total de Graduados en la UNAL",
  labelY       = "Número de Graduados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Graduados en Pregrado

Gra_Pre <- Agregar(datos = Graduados %>% filter(TIPO_NIVEL == "Pregrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Gra_Pre,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 4000),
  colores      = "#525252",
  titulo       = "Evolución Total de Graduados en Pregrado en la UNAL",
  labelY       = "Número de Graduados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.3 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))
  

# Graduados en Postgrado

Gra_Pos <- Agregar(datos = Graduados %>% filter(TIPO_NIVEL == "Postgrado"),
                   formula    = GLOBAL ~ YEAR + SEMESTRE,
                   frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                   intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Gra_Pos,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 3000),
  colores      = "#525252",
  titulo       = "Evolución Total de Graduados en Postgrado en la UNAL",
  labelY       = "Número de Graduados \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.5 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


# Docentes de Carrera

Docentes <- UnalData::Docentes %>% 
  filter(YEAR >= 2019) %>% 
  mutate(GLOBAL = "Total")


Doc_Global <- Agregar(datos = Docentes,
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Doc_Global,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 4000),
  colores      = "#525252",
  titulo       = "Evolución Total Docentes de Carrera en la UNAL",
  labelY       = "Número de Docentes \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.6 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


# Docentes de carrera por máximo nivel formación

Doc_Formación <- Agregar(datos = Docentes,
                      formula    = FORMACION ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 


Plot.Series(
  datos = Doc_Formación,
  categoria    = "FORMACION",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 2200),
  colores      = c("#d7191c","#7b3294","#feb24c", "#2b83ba", "#008837"),
  titulo       = "Evolución Total Docentes de Carrera en la UNAL por Máximo Nivel de Formación",
  labelY       = "Número de Docentes \n",
  estatico     = TRUE,
  estilo       = list(
    LegendTitle = "Nivel de Formación",
    gg.Tema = list(Tema = "theme_gray"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "black",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.6 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Docentes de Carrera por País Máximo Nivel de Formación

Doc_Pais <- UnalData::Docentes %>% 
  filter(YEAR == 2023, SEMESTRE == 2) %>% 
  summarise(Total = n(), .by = c(CODS_PAISU))

Plot.Mundo(
  datos    = Doc_Pais,
  paises   = CODS_PAISU,
  variable = Total,
  titulo   = "Total Docentes de Carrera por Países Máximo Nivel de Formación",
  tipo     = "Pais",
  #titulo   = "País Formación",
  opacidad = 1,
  cortes   = c(0, 10, 100, 1000, Inf),
  colores  = c("#984ea3", "#377eb8", "#4daf4a", "#e41a1c"),
  colBorde = "white",
  estatico = TRUE,
  estilo   = list(
             Theme = 5,
             anchoBorde = 0.2,
             Style = "Intervalo",
             showISO    = list(color = "#00468A", size = 10, fontface = "bold.italic"),
             Legend = list(legend.position = "bottom", legend.direction = "horizontal")
             ))

# Tabla País Máximo Nivel de Formación

Doc_Pais_Tbl <- UnalData::Docentes %>% 
  filter(YEAR == 2023, SEMESTRE == 2) %>% 
  mutate(CODS_PAISU = case_when(CODS_PAISU == "COL" ~ "Colombia",
                                CODS_PAISU == "ESP" ~ "España",
                                CODS_PAISU == "USA" ~ "Estados Unidos",
                                CODS_PAISU == "BRA" ~ "Brasil",
                                CODS_PAISU == "FRA" ~ "Francia",
                                CODS_PAISU == "DEU" ~ "Alemania",
                                CODS_PAISU == "MEX" ~ "México",
                                CODS_PAISU == "GBR" ~ "Reino Unido",
                                CODS_PAISU == "NLD" ~ "Países Bajos",
                                CODS_PAISU == "ARG" ~ "Argentina",
                                .default = "Otros Países")) %>% 
  summarise(Total = n(), .by = c(CODS_PAISU)) %>% 
  arrange(desc(Total)) %>% 
  arrange(CODS_PAISU=="Otros Países") %>% 
  mutate(Porcentaje = round(Total /sum(Total),3),
         Porcentaje = scales::percent(Porcentaje)) %>% 
  rename(País = CODS_PAISU,
         `# de docentes` = Total) %>% 
  Tabla(encabezado  = "Instituciones Acreditadas",
        estatico = TRUE,
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3))) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "center") %>% 
  tab_header(title = "Países Obtención Máximo Nivel de Formación Docentes de Carrera de la UNAL",
             subtitle = md("**Periodo 2023-2**")
  )
Doc_Pais_Tbl

# Funcionarios Administrativos

Administrativos <- UnalData::Administrativos %>% 
  filter(YEAR >= 2019) %>% 
  mutate(GLOBAL = "Total")


Adm_Global <- Agregar(datos = Administrativos,
                      formula    = GLOBAL ~ YEAR + SEMESTRE,
                      frecuencia = list("Year" = 2019:2023, "Period" = 1:2),
                      intervalo  = list(c(2021, 1), c(2023, 2))
) 

Plot.Series(
  datos = Adm_Global,
  categoria    = "GLOBAL",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 4000),
  colores      = "#525252",
  titulo       = "Evolución Total Administrativos de Carrera en la UNAL",
  labelY       = "Número de Administrativos \n",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema = list(Tema = "theme_gray"),
    gg.Legend = list(legend.position = "none"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "red",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.6 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2021-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))

# Resultados SaberPro

Plot.Boxplot(
  datos      = UnalData::SaberPro,
  variable   = PUNTAJE_GLOBAL,
  grupo1     = YEAR,
  jitter     = FALSE,
  colores    = pals::turbo(7),
  colOutlier = "#CBB8FF",
  titulo     = "Distribución Puntaje Global Prueba Saber Pro, estudiantes UNAL.\n",
  labelY     = "Puntaje Global Prueba Saber Pro\n",
  labelX     = "\nAño",
  textBox    = "Score",
  estatico   = TRUE,
  estilo     = list(
    gg.Tema = 5, gg.Legend = list(legend.position = "none"),
    gg.Texto = list(
      caption  = "Información Disponible desde 2016"),
    gg.VarWidth = TRUE, gg.JitWidth = 0.08, gg.JitSize = 0.05
  )
)

# Tabla Saber Pro

Saber_mean <- UnalData::SaberPro %>% select(Año = YEAR, `Competencias Ciudadanas` = PUNT_COMP_CIUD,
                                                        `Comunicación Escrita` = PUNT_COMU_ESCR,
                                                        `Lectura Crítica` = PUNT_LECT_CRIT,
                                                        `Inglés` = PUNT_INGLES,
                                                        `Razonamiento Cuantitativo` = PUNT_RAZO_CUANT,
                                                        `Puntaje Global` = PUNTAJE_GLOBAL) %>% 
              pivot_longer(`Competencias Ciudadanas`:`Puntaje Global`, names_to = "Prueba", values_to = "Puntaje") %>% 
              summarise(Promedio = round(mean(Puntaje, na.rm = TRUE), 2), .by = c(Año, Prueba)) %>% 
              pivot_wider(names_from = Prueba, values_from = Promedio) %>% 
              arrange(desc(Año))


Tabla(datos = Saber_mean, 
  encabezado  = "Encabezado",
      estatico = TRUE,
      estilo      = list(
        Tema = 11, Padding = c(0.3, 3))) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "center") %>% 
  tab_header(title = "Resultados Promedio Prueba Saber Pro Globales y por Competencias, estudiantes UNAL",
             subtitle = md("**Periodo 2016-2022**")
  )

##%######################################################%##
#                                                          #
####              84 Solicitud 24-04-2024              ####
#                                                          #
##%######################################################%##

# Estadísticas de admisión de los últimos años, etc Programa PAES. 
# Solicita: Nicolás Villamil Ibáñez - Periodista

PAES <- Matriculados %>% filter(TIPO_NIVEL == "Pregrado") %>% 
  mutate(MOD_ADM = ifelse(SEDE_NOMBRE_ADM == "De La Paz" & YEAR <= 2022, "Regular", MOD_ADM),
         TIPO_ADM = ifelse(SEDE_NOMBRE_ADM == "De La Paz" & YEAR <= 2022, "Regular", TIPO_ADM)) %>% 
  filter(TIPO_ADM == "PAES")


Mat_PAES <- Agregar(datos = PAES,
                         formula    = PAES ~ YEAR + SEMESTRE,
                         frecuencia = list("Year" = 2010:2023, "Period" = 1:2),
                         intervalo  = list(c(2010, 1), c(2023, 2))) %>% 
            filter(Clase != "De La Paz")


Plot.Series(
  datos = Mat_PAES,
  categoria    = "PAES",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(0, 1500),
  colores      = c("#d7191c","#7b3294","#feb24c", "#2b83ba", "#008837"),
  titulo       = "Evolución Total Matriculados PAES",
  labelY       = "Número de estudiantes \n",
  estatico     = TRUE,
  estilo       = list(
    LegendTitle = "Modalidad PAES",
    gg.Tema = list(Tema = "theme_gray"),
    #gg.Legend = list(legend.position = "bottom"),
    gg.Linea  = list(linetype = 5, size = 0.5),
    gg.Repel = list(size = 3.18, color = "black",
                    direction = "both", seed = 42, nudge_y = 0.25,
                    arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
                    box.padding   = 0.6 ,     # Espacio vacío que se debe respetar alrededor de la caja delimitadora
                    min.segment.length = 0.5 # Entre más bajo más flechas, entre más distancia menos flechas
    ),
    gg.Texto  = list(subtitle = "Periodo 2010-2023")))+
  theme(axis.text.x = element_text(face="bold", size=9),
        axis.line = element_line(colour = "darkblue", size = 0.5, linetype = "solid"))


##%######################################################%##
#                                                          #
####              85 Solicitud 25-04-2024              ####
#                                                          #
##%######################################################%##

# Gráficos Presentación Lanzamiento Libro Filbo 2024

# Mapa Aspirantes

Aspirantes <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")) 

nrow(Aspirantes %>% summarise(Total = n(), .by = c(COD_CIU_NAC)))

Plot.Mapa(
  datos    = Aspirantes,
  depto    = COD_DEP_NAC,
  mpio     = COD_CIU_NAC,
  tipo     = "SiNoMpios",
  titulo   = "Aspirantes UNAL 2008-2023",
  colores  = c("red", "#1AD61A"),
  colSedes = rep("green", 9),
  #opacidad = 0.6,
  #colBorde = "#FF4D00",
  compacto = FALSE,
  textSize = 5,
  limpio   = FALSE
)

# Mapa Admitidos

Admitidos <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular"),
         ADMITIDO == "Sí") 

nrow(Admitidos %>% summarise(Total = n(), .by = c(COD_CIU_NAC)))


Plot.Mapa(
  datos    = Admitidos,
  depto    = COD_DEP_NAC,
  mpio     = COD_CIU_NAC,
  tipo     = "SiNoMpios",
  titulo   = "Admitidos UNAL 2008-2023",
  colores  = c("red", "#1AD61A"),
  colSedes = rep("green", 9),
  #opacidad = 0.6,
  #colBorde = "#FF4D00",
  compacto = FALSE,
  textSize = 5,
  limpio   = FALSE
)


# Mapa Graduados

Graduados <- UnalData::Graduados

nrow(Graduados %>% summarise(Total = n(), .by = c(COD_CIU_NAC)))


Plot.Mapa(
  datos    = Graduados,
  depto    = COD_DEP_NAC,
  mpio     = COD_CIU_NAC,
  tipo     = "SiNoMpios",
  titulo   = "Graduados UNAL 2009-2023",
  colores  = c("red", "#1AD61A"),
  colSedes = rep("green", 9),
  #opacidad = 0.6,
  #colBorde = "#FF4D00",
  compacto = FALSE,
  textSize = 5,
  limpio   = FALSE
)

##%######################################################%##
#                                                          #
####              86 Solicitud 15-05-2024              ####
#                                                          #
##%######################################################%##

# Tendencias Matriculados

# Pregrado

Agregar(datos = UnalData::Matriculados %>% 
          filter(TIPO_NIVEL == "Pregrado") %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2009:2023, "Period" = 1:2),
        intervalo  = list(c(2009, 1), c(2023, 2))) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de estudiantes matriculados en pregrado",
    labelX    = "Periodo",
    labelY    = "Número de matriculados</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 1)
  )

# Postgrado

Agregar(datos = UnalData::Matriculados %>% 
          filter(TIPO_NIVEL == "Postgrado") %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2009:2023, "Period" = 1:2),
        intervalo  = list(c(2009, 1), c(2023, 2))) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de estudiantes matriculados en postgrado",
    labelX    = "Periodo",
    labelY    = "Número de matriculados</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 1)
  )



# Tendencias Matricula Primera Vez

# Pregrado

Agregar(datos = UnalData::Matriculados %>% filter(TIPO_NIVEL == "Pregrado"),
  formula    = MAT_PVEZ ~ YEAR + SEMESTRE,
  frecuencia = list("Year" = 2009:2023, "Period" = 1:2),
  intervalo  = list(c(2009, 1), c(2023, 2))) %>% 
  Plot.Series(
  categoria = "MAT_PVEZ",
  ylim      = c(0, NaN),
  colores   = c("#3182bd", "#8cc63f"),
  titulo    = "Evolución del número de estudiantes de pregrado matriculados por primera vez",
  labelX    = "Periodo",
  labelY    = "Número de matriculados</br>",
  libreria  = "highcharter",
  estilo    = list(hc.Tema = 1)
)

# Postgrado

Agregar(datos = UnalData::Matriculados %>% filter(TIPO_NIVEL == "Postgrado"),
        formula    = MAT_PVEZ ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2009:2023, "Period" = 1:2),
        intervalo  = list(c(2009, 1), c(2023, 2))) %>% 
  Plot.Series(
    categoria = "MAT_PVEZ",
    ylim      = c(0, NaN),
    colores   = c("#3182bd", "#8cc63f"),
    titulo    = "Evolución del número de estudiantes de postgrado matriculados por primera vez",
    labelX    = "Periodo",
    labelY    = "Número de matriculados</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 1))


##%######################################################%##
#                                                          #
####              87 Solicitud 30-05-2024              ####
#                                                          #
##%######################################################%##

# Tendencias Memoria Financiera
# Solicitante: Gerencia Financiera y Equipo de Estadísticas

####
# Aspirantes 
####

# Tendencia 

Agregar(datos = UnalData::Aspirantes %>% 
        mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>% 
        mutate(Clase  = ifelse(Clase == "TOTAL", "Total Aspirantes", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de aspirantes en la UNAL",
    labelX    = "Periodo",
    labelY    = "Número de aspirantes</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Aspirantes,
        formula    = INS_SEDE_NOMBRE ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "INS_SEDE_NOMBRE",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de aspirantes en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de aspirantes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )

# Nivel de Formación

Agregar(datos = UnalData::Aspirantes,
        formula    = NIVEL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "NIVEL",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de aspirantes en la UNAL por nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de aspirantes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de formación:", hc.Tema = 5)
  )

# Sexo

Agregar(datos = UnalData::Aspirantes,
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, 100),
    titulo    = "Evolución del porcentaje de aspirantes en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de aspirantes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Estrato

Agregar(datos = UnalData::Aspirantes,
        formula    = ESTRATO_ORIG ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "ESTRATO_ORIG",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de aspirantes en la UNAL por estrato",
    labelX    = "Periodo",
    labelY    = "% de aspirantes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Estrato:", hc.Tema = 5)
  )

# Inscripción

Agregar(datos = UnalData::Aspirantes %>% filter(NIVEL == "Pregrado"),
        formula    = TIPO_INS ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
        filter(Clase != "PAET") %>% 
  Plot.Series(
    categoria = "TIPO_INS",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de aspirantes a pregrado por programas de inscripción",
    labelX    = "Periodo",
    labelY    = "% de aspirantes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa de inscripción:", hc.Tema = 5)
  )

####
# Admitidos
####

# Tendencia 

Agregar(datos = UnalData::Aspirantes %>% filter(ADMITIDO == "Sí") %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>% 
  mutate(Clase  = ifelse(Clase == "TOTAL", "Total Admitidos", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de admitidos en la UNAL",
    labelX    = "Periodo",
    labelY    = "Número de admitidos</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Aspirantes %>% filter(ADMITIDO == "Sí"),
        formula    = ADM_SEDE_NOMBRE ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "ADM_SEDE_NOMBRE",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de admitidos en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de admitidos</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )

# Nivel de Formación

Agregar(datos = UnalData::Aspirantes %>% filter(ADMITIDO == "Sí"),
        formula    = NIVEL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "NIVEL",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de admitidos en la UNAL por nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de admitidos</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de formación:", hc.Tema = 5)
  )

# Sexo

Agregar(datos = UnalData::Aspirantes %>% filter(ADMITIDO == "Sí"),
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, 100),
    titulo    = "Evolución del porcentaje de admitidos en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de admitidos</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Estrato

Agregar(datos = UnalData::Aspirantes %>% filter(ADMITIDO == "Sí"), 
        formula    = ESTRATO_ORIG ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "ESTRATO_ORIG",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de admitidos en la UNAL por estrato",
    labelX    = "Periodo",
    labelY    = "% de admitidos</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Estrato:", hc.Tema = 5)
  )

# Inscripción

Agregar(datos = UnalData::Aspirantes %>% filter(NIVEL == "Pregrado", ADMITIDO == "Sí"),
        formula    = TIPO_INS ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(Clase != "PAET") %>% 
  Plot.Series(
    categoria = "TIPO_INS",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de admitidos a pregrado por programas de inscripción",
    labelX    = "Periodo",
    labelY    = "% de admitidos</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa de inscripción:", hc.Tema = 5)
  )


####
# Matriculados
####

# Tendencia 

Agregar(datos = UnalData::Matriculados %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>% 
  mutate(Clase  = ifelse(Clase == "TOTAL", "Total Matriculados", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de matriculados en la UNAL",
    labelX    = "Periodo",
    labelY    = "Número de matriculados</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Matriculados,
        formula    = SEDE_NOMBRE_MAT ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "SEDE_NOMBRE_MAT",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )

# Nivel de Formación

Agregar(datos = UnalData::Matriculados,
        formula    = NIVEL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "NIVEL",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en la UNAL por nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de formación:", hc.Tema = 5)
  )

# Sexo

Agregar(datos = UnalData::Matriculados,
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, 100),
    titulo    = "Evolución del porcentaje de matriculados en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Estrato

Agregar(datos = UnalData::Matriculados,
        formula    = ESTRATO_ORIG ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2019:2021, "Period" = 1:2),
        intervalo  = list(c(2019, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "ESTRATO_ORIG",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en la UNAL por estrato",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Estrato:", hc.Tema = 5)
  )

# Inscripción

Agregar(datos = UnalData::Matriculados %>% filter(NIVEL == "Pregrado"),
        formula    = TIPO_ADM ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("PEAA", "PAET")) %>% 
  Plot.Series(
    categoria = "TIPO_ADM",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en pregrado por programas de inscripción",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa de inscripción:", hc.Tema = 5)
  )

# PAES

Agregar(datos = UnalData::Matriculados %>% filter(NIVEL == "Pregrado", TIPO_ADM == "PAES"),
        formula    = PAES ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("PEAA", "PAET")) %>% 
  Plot.Series(
    categoria = "PAES",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en pregrado por modalidades del programa PAES",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Modalidad PAES:", hc.Tema = 5)
  )

# PEAMA

Agregar(datos = UnalData::Matriculados %>% filter(NIVEL == "Pregrado", TIPO_ADM == "PEAMA"),
        formula    = PEAMA ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("PEAA", "PAET")) %>% 
  Plot.Series(
    categoria = "PEAMA",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de matriculados en pregrado por modalidades del programa PEAMA",
    labelX    = "Periodo",
    labelY    = "% de matriculados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa PEAMA:", hc.Tema = 5)
  )

####
# Graduados
####

# Tendencia 

Agregar(datos = UnalData::Graduados %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>% 
  mutate(Clase  = ifelse(Clase == "TOTAL", "Total Graduados", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del porcentaje de graduados en la UNAL",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Graduados,
        formula    = SEDE_NOMBRE_ADM ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "SEDE_NOMBRE_ADM",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )

# Nivel de Formación

Agregar(datos = UnalData::Graduados,
        formula    = NIVEL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "NIVEL",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en la UNAL por nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de formación:", hc.Tema = 5)
  )

# Sexo

Agregar(datos = UnalData::Graduados,
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Estrato

Agregar(datos = UnalData::Graduados,
        formula    = ESTRATO_ORIG ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2019:2021, "Period" = 1:2),
        intervalo  = list(c(2019, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "ESTRATO_ORIG",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en la UNAL por estrato",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Estrato:", hc.Tema = 5)
  )

# Inscripción

Agregar(datos = UnalData::Graduados %>% filter(NIVEL == "Pregrado"),
        formula    = TIPO_ADM ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  filter(!Clase %in% c("PEAA", "PAET")) %>% 
  Plot.Series(
    categoria = "TIPO_ADM",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en pregrado por programas de inscripción",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa de inscripción:", hc.Tema = 5)
  )

# PAES

Agregar(datos = UnalData::Graduados %>% filter(NIVEL == "Pregrado", TIPO_ADM == "PAES"),
        formula    = PAES ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
   Plot.Series(
    categoria = "PAES",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en pregrado por modalidades del programa PAES",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Modalidad PAES:", hc.Tema = 5)
  )

# PEAMA

Agregar(datos = UnalData::Graduados %>% filter(NIVEL == "Pregrado", TIPO_ADM == "PEAMA"),
        formula    = PEAMA ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2016, 1), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "PEAMA",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de graduados en pregrado por modalidades del programa PEAMA",
    labelX    = "Periodo",
    labelY    = "% de graduados</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Programa PEAMA:", hc.Tema = 5)
  )


####
# Resultados Saber Pro
####

# Tendencia 

Plot.Boxplot(
  datos       = UnalData::SaberPro %>% filter(between(YEAR, 2016, 2021)),
  variable    = PUNTAJE_GLOBAL,
  grupo1      = YEAR,
  outliers    = FALSE,
  ylim        = c(0, 300),
  colores     = pals::jet(6),
  titulo      = "Evolución Distribución Puntajes Promedios Globales de la UNAL en la Prueba Saber Pro",
  labelY      = "Puntaje Promedio Global",
  textBox     = "Puntaje Global",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 2)
)

# Sedes

Plot.Boxplot(
  datos       = UnalData::SaberPro %>% filter(between(YEAR, 2016, 2021)),
  variable    = PUNTAJE_GLOBAL,
  grupo1      = YEAR,
  grupo2   = SEDE_NOMBRE_ADM,
  outliers    = FALSE,
  ylim        = c(0, 300),
  colores     = pals::jet(8),
  titulo      = "Evolución Distribución Puntajes Promedios Globales por Sedes de la UNAL en la Prueba Saber Pro",
  labelY      = "Puntaje Promedio Global",
  textBox     = "Puntaje Global",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 2)
)


# Sexo

Plot.Boxplot(
  datos       = UnalData::SaberPro %>% filter(between(YEAR, 2016, 2021)),
  variable    = PUNTAJE_GLOBAL,
  grupo1      = YEAR,
  grupo2   = SEXO,
  outliers    = FALSE,
  ylim        = c(0, 300),
  colores     =  c("#00ABFF", "#F3224B"),
  titulo      = "Evolución Distribución Puntajes Promedios Globales por Sexo de los Estudiantes de la UNAL en la Prueba Saber Pro",
  labelY      = "Puntaje Promedio Global",
  textBox     = "Puntaje Global",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 2)
)

# Estrato

Plot.Boxplot(
  datos       = UnalData::SaberPro %>% filter(between(YEAR, 2016, 2021)),
  variable    = PUNTAJE_GLOBAL,
  grupo1      = YEAR,
  grupo2   = ESTRATO,
  outliers    = FALSE,
  ylim        = c(0, 300),
  colores     = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
  titulo      = "Evolución Distribución Puntajes Promedios Globales por Estrato de los Estudiantes de la UNAL en la Prueba Saber Pro",
  labelY      = "Puntaje Promedio Global",
  textBox     = "Puntaje Global",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 2)
)

# Inscripción

Plot.Boxplot(
  datos       = UnalData::SaberPro %>% filter(between(YEAR, 2016, 2021)),
  variable    = PUNTAJE_GLOBAL,
  grupo1      = YEAR,
  grupo2   = TIPO_ADM,
  outliers    = FALSE,
  ylim        = c(0, 300),
  colores     = c("#00ABFF", "#F3224B", "#29DF2C"),
  titulo      = "Evolución Distribución Puntajes Promedios Globales por Programa de Admisión de los Estudiantes de la UNAL en la Prueba Saber Pro",
  labelY      = "Puntaje Promedio Global",
  textBox     = "Puntaje Global",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 2)
)


####
# Docentes de carrera
####

# Tendencia 

Agregar(datos = UnalData::Docentes %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>% 
  mutate(Clase  = ifelse(Clase == "TOTAL", "Total Docentes", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de docentes de carrera en la UNAL",
    labelX    = "Periodo",
    labelY    = "Número de docentes</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Docentes,
        formula    = SEDE ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "SEDE",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de docentes en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )


# Sexo

Agregar(datos = UnalData::Docentes,
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, 100),
    titulo    = "Evolución del porcentaje de docentes en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Formación

Agregar(datos = UnalData::Docentes,
        formula    = FORMACION ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "FORMACION",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de docentes de la UNAL por máximo nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de formación:", hc.Tema = 5)
  )

# Categoría

Agregar(datos = UnalData::Docentes,
        formula    = CATEGORIA ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "CATEGORIA",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de docentes de la UNAL por categoría",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Categoría del docente:", hc.Tema = 5)
  )

# Dedicación

Agregar(datos = UnalData::Docentes,
        formula    = DEDICACION ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "DEDICACION",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de docentes de la UNAL según dedicación",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Dedicación del docente:", hc.Tema = 5)
  )

# Lugar formación

Agregar(datos = UnalData::Docentes,
        formula    = LUG_FORMACION ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "LUG_FORMACION",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de docentes de la UNAL por lugar de formación",
    labelX    = "Periodo",
    labelY    = "% de docentes</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Lugar de formación:", hc.Tema = 5)
  )


####
# Funcionarios de carrera
####

# Tendencia 

Agregar(datos = UnalData::Administrativos %>% 
          mutate(TOTAL = "TOTAL"),
        formula    = TOTAL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>% 
  mutate(Clase  = ifelse(Clase == "TOTAL", "Total Docentes", Clase)) %>% 
  Plot.Series(
    categoria = "TOTAL",
    ylim      = c(0, NaN),
    colores   = c("#8cc63f"),
    titulo    = "Evolución del número de funcionarios de carrera en la UNAL",
    labelX    = "Periodo",
    labelY    = "Número de funcionarios</br>",
    libreria  = "highcharter",
    estilo    = list(hc.Tema = 5)
  )

# Sedes

Agregar(datos = UnalData::Administrativos,
        formula    = SEDE ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "SEDE",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de funcionarios en la UNAL por sedes",
    labelX    = "Periodo",
    labelY    = "% de funcionarios</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sede:", hc.Tema = 5)
  )


# Sexo

Agregar(datos = UnalData::Administrativos,
        formula    = SEXO ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  filter(!Clase %in% c("Transgénero", "No binario")) %>% 
  Plot.Series(
    categoria = "SEXO",
    ylim      = c(0, 100),
    titulo    = "Evolución del porcentaje de funcionarios en la UNAL por sexo",
    labelX    = "Periodo",
    labelY    = "% de funcionarios</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Sexo:", hc.Tema = 5)
  )

# Nivel

Agregar(datos = UnalData::Administrativos,
        formula    = NIVEL ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "NIVEL",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de funcionarios de la UNAL por nivel de ubicación",
    labelX    = "Periodo",
    labelY    = "% de funcionarios</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Nivel de ubicación:", hc.Tema = 5)
  )


# Formación

Agregar(datos = UnalData::Administrativos,
        formula    = FORMACION ~ YEAR + SEMESTRE,
        frecuencia = list("Year" = 2016:2021, "Period" = 1:2),
        intervalo  = list(c(2017, 2), c(2021, 2))) %>%  
  Plot.Series(
    categoria = "FORMACION",
    ylim      = c(0, NaN),
    titulo    = "Evolución del porcentaje de funcionarios de la UNAL por máximo nivel de formación",
    labelX    = "Periodo",
    labelY    = "% de funcionarios</br>",
    libreria  = "highcharter",
    freqRelativa = TRUE,
    estilo    = list(LegendTitle = "Máximo Nivel de Formación:", hc.Tema = 5)
  )


##%######################################################%##
#                                                          #
####              88 Solicitud 05-06-2024              ####
#                                                          #
##%######################################################%##

# Datos SNIES Universidad 18-21
# Solicitante: Alberto Rodríguez R

# Importar SNIES Universidad

Snies1821 <- read_excel("Datos/Fuentes/Snies1821.xlsx",
                        sheet = "General",
                        guess_max = 600000)

Universidad <- Snies1821 %>% filter(Formacion %in% c("Universitaria", "UNIVERSITARIA"))

# Total Aspirantes

Asp_Total <- Universidad %>% filter(Poblacion == "Inscritos") %>% 
             summarise(Total = sum(Total), .by = c(Ano)) %>% 
             pivot_wider(names_from = Ano, values_from = Total) %>% 
             mutate(Poblacion = "Aspirantes",
                    Variable = "Total",
                    Modalidad = "Total") %>% 
            relocate(Poblacion:Modalidad, .before =  `2018`)

# Aspirantes por Sexo

Asp_Sexo <- Universidad %>% filter(Poblacion == "Inscritos") %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo)) %>% 
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sexo) %>% 
  mutate(Poblacion = "Aspirantes",
         Variable = "Sexo",
         Modalidad = ifelse(Modalidad == "Hombre", "Hombres", "Mujeres")) %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Aspirantes por Sector

Asp_Sector <- Universidad %>% filter(Poblacion == "Inscritos") %>%  
  rename(Sector = `Sector IES`) %>% 
  mutate(Sector = str_to_title(Sector)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sector) %>% 
  mutate(Modalidad = ifelse(Modalidad == "Privada", "Privado", Modalidad),
         Poblacion = "Aspirantes",
         Variable = "Sector") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Aspirantes por Metodología

Asp_Metodologia <- Universidad %>% filter(Poblacion == "Inscritos") %>%  
  mutate(Metodologia = str_to_title(Metodologia)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Metodologia) %>% 
  mutate(Poblacion = "Aspirantes",
         Variable = "Metodologia") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Total Admitidos

Adm_Total <- Universidad %>% filter(Poblacion == "Admitidos") %>% 
  summarise(Total = sum(Total), .by = c(Ano)) %>% 
  pivot_wider(names_from = Ano, values_from = Total) %>% 
  mutate(Poblacion = "Admitidos",
         Variable = "Total",
         Modalidad = "Total") %>% 
  relocate(Poblacion:Modalidad, .before =  `2018`)

# Admitidos por Sexo

Adm_Sexo <- Universidad %>% filter(Poblacion == "Admitidos") %>% 
  mutate(Sexo = str_to_title(Sexo)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo)) %>% 
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sexo) %>% 
  mutate(Poblacion = "Admitidos",
         Variable = "Sexo",
         Modalidad = ifelse(Modalidad == "Hombre", "Hombres", 
                            ifelse(Modalidad == "Mujer", "Mujeres", "Sin información"))) %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Admitidos por Sector

Adm_Sector <- Universidad %>% filter(Poblacion == "Admitidos") %>%  
  rename(Sector = `Sector IES`) %>% 
  mutate(Sector = str_to_title(Sector)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sector) %>% 
  mutate(Modalidad = ifelse(Modalidad == "Privada", "Privado", Modalidad),
         Poblacion = "Admitidos",
         Variable = "Sector") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Admitidos por Metodología

Adm_Metodologia <- Universidad %>% filter(Poblacion == "Admitidos") %>%  
  mutate(Metodologia = str_to_title(Metodologia)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Metodologia) %>% 
  mutate(Poblacion = "Admitidos",
         Variable = "Metodologia") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Total Matricula Pvez

Mpvez_Total <- Universidad %>% filter(Poblacion == "Mpvez") %>% 
  summarise(Total = sum(Total), .by = c(Ano)) %>% 
  pivot_wider(names_from = Ano, values_from = Total) %>% 
  mutate(Poblacion = "Mpvez",
         Variable = "Total",
         Modalidad = "Total") %>% 
  relocate(Poblacion:Modalidad, .before =  `2018`)

# Matricula Pvez por Sexo

Mpvez_Sexo <- Universidad %>% filter(Poblacion == "Mpvez") %>% 
  mutate(Sexo1 = case_when(ID_Sexo == 1 ~ "Hombres",
                           ID_Sexo == 2 ~ "Mujeres",
                           ID_Sexo == 0 ~ "Sin Información")) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo1)) %>% 
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sexo1) %>% 
  mutate(Poblacion = "Mpvez",
         Variable = "Sexo") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Matricula Pvez por Sector

Mpvez_Sector <- Universidad %>% filter(Poblacion == "Mpvez") %>%  
  rename(Sector = `Sector IES`) %>% 
  mutate(Sector = str_to_title(Sector)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Sector) %>% 
  mutate(Modalidad = ifelse(Modalidad == "Privada", "Privado", Modalidad),
         Poblacion = "Mpvez",
         Variable = "Sector") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)

# Matricula Pvez por Metodología

Mpvez_Metodologia <- Universidad %>% filter(Poblacion == "Mpvez") %>%  
  mutate(Metodologia = str_to_title(Metodologia)) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia)) %>%
  pivot_wider(names_from = Ano, values_from = Total) %>%
  rename(Modalidad = Metodologia) %>% 
  mutate(Poblacion = "Mpvez",
         Variable = "Metodologia") %>% 
  relocate(Poblacion:Variable, .before =  Modalidad)




# Base Global

Global <- bind_rows(Asp_Total, Asp_Sexo, Asp_Sector, Asp_Metodologia,
                    Adm_Total, Adm_Sexo, Adm_Sector, Adm_Metodologia,
                    Mpvez_Total, Mpvez_Sexo, Mpvez_Sector, Mpvez_Metodologia)

# Exportar base a Excel

write_xlsx(Global, "Datos/Entrega88/Universidad.xlsx")
  

##%######################################################%##
#                                                          #
####              89 Solicitud 28-06-2024              ####
#                                                          #
##%######################################################%##

# La distribución por  departamento (lugar) de procedencia 
# de los estudiantes del Programa Curricular de Medicina de 2014-2023 
# por periodo académico.

# Solicitante: 

# BEATRIZ MENA BEJARANO
# Directora de Bienestar
# Facultad de Medicina
# Universidad Nacional de Colombia
# Carrera 30 No. 45-03, Edificio 471, oficina 226
# 3165411 / 3165000 Ext 15132

# Consulta

Medicina_Proc <- UnalData::Matriculados %>% 
                 filter(SNIES_PROGRA == 9, 
                        YEAR %in% c(2016:2023), 
                        SEDE_NOMBRE_MAT == "Bogotá") %>% 
                 mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
                 summarise(Total = n(), .by = c(Periodo, DEP_PROC)) %>% 
                 pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0)
                                                                    
write_xlsx(Medicina_Proc, "Datos/Entrega89/Medicina_Procedencia.xlsx")        
                
