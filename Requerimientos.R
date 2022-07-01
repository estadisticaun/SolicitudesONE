######################################-
# Librerías ----
######################################-
library(UnalData)
library(UnalR)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(skimr)
library(forcats)
library(magrittr)

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
        legend.text = element_text(size=13))
  
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
