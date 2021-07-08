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
