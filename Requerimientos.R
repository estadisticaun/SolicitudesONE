######################################-
#Librerías ----
######################################-
library(UnalData)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

######################################-
# 1 Solicit 14-05-2021 -----
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





View(head(UnalData::Matriculados %>% filter(SNIES_PROGRA == 55189) %>% select(PROGRAMA), 1))

