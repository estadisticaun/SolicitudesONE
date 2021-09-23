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
# 6 Solicitud 02-09-2021 -----
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

write_xlsx(Base_Final, "C:/Users/Alberto/Documents/Sistema Estadistico/Rta_ONE/Programas.xlsx")


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



