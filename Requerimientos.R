######################################-
# Librerías ----
######################################-
library(UnalData)
library(UnalR)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

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

