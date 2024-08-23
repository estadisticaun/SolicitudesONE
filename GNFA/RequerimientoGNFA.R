# Librerías Requeridas

library(UnalData)
library(UnalR)
library(tidyverse)
library(writexl)

# Año de solicitud de cifras

Año <- 2023

# Bases de Datos Consolidadas ----

# Aspirantes General

Aspirantes <- UnalData::Aspirantes %>% 
  filter((TIPO_NIVEL == "Pregrado" & !is.na(MOD_INS))|
           (TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular"),
         YEAR == {{Año}}) %>% 
         mutate(Periodo = paste0(YEAR, "-", SEMESTRE))

# Aspirantes Postgrado

Aspirantes_Pos <- UnalData::Aspirantes %>% 
  filter(TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular") %>% 
  filter(YEAR == {{Año}}) %>% 
  mutate(Periodo = paste0(YEAR, "-", SEMESTRE))

# Matriculados

Sprogra <- UnalData::Hprogramas %>% select(SNIES_PROGRA, SEDE_PROG, FACULTAD_PROGRA)

#Matricula provisional 2023-2
Matricula_20232_Prov <- read_excel("Datos/Fuentes/Matricula_20232_Prov.xlsx")

# Adionar a matricula global matriculados proviisonales de 2023-2
MatriculaG <- bind_rows(UnalData::Matriculados, Matricula_20232_Prov)

# Matricula Global
Matriculados <- MatriculaG %>% 
                filter(YEAR == {{Año}}) %>% 
                mutate(Periodo = paste0(YEAR, "-", SEMESTRE),
                       FACULTAD = ifelse(SEDE_NOMBRE_MAT %in% c("Amazonía", "Caribe", "Orinoquía", "Tumaco"), SEDE_NOMBRE_MAT, FACULTAD)) %>% 
                left_join(Sprogra, by = "SNIES_PROGRA")
              
# Datos Requeridos ----

####
# Aspirantes ----
####

# Por Semestre

Asp_Sem <- Aspirantes %>% summarise(Total = n(), .by = c(Periodo))

View(Asp_Sem)
write_xlsx(Asp_Sem, "GNFA/Entrega/Inscritos/Asp_Sem.xlsx")

# Aspirantes por Sede

Asp_Sede <- Aspirantes %>% 
  summarise(Total = n(), .by = c(Periodo, INS_SEDE_NOMBRE)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total) %>% 
  mutate(Total = `2023-1` + `2023-2`)

View(Asp_Sede)
write_xlsx(Asp_Sede, "GNFA/Entrega/Inscritos/Asp_Sede.xlsx")

# Aspirantes por Facultad - Postgrado

Asp_Pos_Facultad <- Aspirantes_Pos %>% 
  group_by(Periodo, INS_SEDE_NOMBRE, FACULTAD) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
  mutate(Total = `2023-1` + `2023-2`) %>% 
  rename(Sede = INS_SEDE_NOMBRE,
         Facultad = FACULTAD)

View(Asp_Pos_Facultad)
write_xlsx(Asp_Pos_Facultad, "GNFA/Entrega/Inscritos/Asp_Pos_Facultad.xlsx")

# Aspirantes por Programa Curricular - Postgrado

Asp_Pos_Programa <- Aspirantes_Pos %>% 
  group_by(Periodo, INS_SEDE_NOMBRE, FACULTAD, NIVEL, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
  mutate(Total = `2023-1` + `2023-2`) %>% 
  rename(Sede = INS_SEDE_NOMBRE,
         Facultad = FACULTAD)

View(Asp_Pos_Programa)
write_xlsx(Asp_Pos_Programa, "GNFA/Entrega/Inscritos/Asp_Pos_Programa.xlsx")


####
# Matriculados ----
####

# Por Semestre

Mat_Sem <- Matriculados %>% summarise(Total = n(), .by = c(Periodo))
View(Mat_Sem)
write_xlsx(Mat_Sem, "GNFA/Entrega/Matriculados/Mat_Sem.xlsx")

# Matriculados por Sede

Mat_Sede <- Matriculados %>% 
  summarise(Total = n(), .by = c(Periodo, SEDE_NOMBRE_MAT)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total) 

View(Mat_Sede)
write_xlsx(Mat_Sede, "GNFA/Entrega/Matriculados/Mat_Sede.xlsx")


# Matriculados por Facultad

Mat_Facultad <- Matriculados %>% 
  group_by(Periodo, SEDE_NOMBRE_MAT, TIPO_NIVEL, FACULTAD) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
  rename(Sede = SEDE_NOMBRE_MAT,
         Facultad = FACULTAD,
         Nivel = TIPO_NIVEL)

View(Mat_Facultad)
write_xlsx(Mat_Facultad, "GNFA/Entrega/Matriculados/Mat_Facultad.xlsx")

# Matriculados por Programa Curricular

Mat_Programa <- Matriculados %>% 
  group_by(Periodo, SEDE_PROG, NIVEL, FACULTAD_PROGRA, SNIES_PROGRA, PROGRAMA) %>% 
  summarise(Total = n()) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
  rename(`Sede del Programa` = SEDE_PROG,
         `Nivel` = NIVEL,
         `Facultad del Programa`=FACULTAD_PROGRA,
         `Código SNIES del Programa` = SNIES_PROGRA,
         `Nombre del Programa` = PROGRAMA) %>% 
  ungroup()

View(Mat_Programa)
write_xlsx(Mat_Programa, "GNFA/Entrega/Matriculados/Mat_Programa.xlsx")


# CALIFICADORA DE RIESGO ----
# SOLICITUD 2024
# Demanda Académica
# Gerencia Nacional Financiera y Administrativa

# a)      Estadísticas estudiantiles para los últimos 5 años: 
# Matriculados, Aspirantes, Admitidos y Matriculados Primera Vez. 
# Si es posible, entregar la información en un archivo de Excel donde se pueda detallar por sede, 
# nivel de estudios (pregrado, posgrado) y programa académico.


# Aspirantes ----

# Por Nivel

Asp_Nivel <- UnalData::Aspirantes %>% filter(YEAR%in% c(2019:2023)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Nivel = TIPO_NIVEL) %>% 
  summarise(Total = n(), .by = c(Nivel, Periodo)) %>% 
  arrange(desc(Nivel))

write_xlsx(Asp_Nivel, "GNFA/Calificadora/2024/Aspirantes/Asp_Nivel.xlsx")


# Por Sedes

Asp_Sedes <- UnalData::Aspirantes %>% filter(YEAR%in% c(2019:2023)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Sede = INS_SEDE_NOMBRE) %>% 
  summarise(Total = n(), .by = c(Sede, Periodo)) %>% 
  arrange(desc(Sede))

write_xlsx(Asp_Sedes, "GNFA/Calificadora/2024/Aspirantes/Asp_Sedes.xlsx")


# Matricula ----

# Por Nivel

Mat_Nivel <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Nivel = TIPO_NIVEL) %>% 
  summarise(Total = n(), .by = c(Nivel, Periodo)) %>% 
  arrange(desc(Nivel))

write_xlsx(Mat_Nivel, "GNFA/Calificadora/2024/Matricula/Mat_Nivel.xlsx")


# Por Sedes

Mat_Sedes <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Sede = SEDE_NOMBRE_MAT) %>% 
  summarise(Total = n(), .by = c(Sede, Periodo)) %>% 
  arrange(desc(Sede))

write_xlsx(Mat_Sedes, "GNFA/Calificadora/2024/Matricula/Mat_Sedes.xlsx")

# Por Programas Académicos

Mat_Programas <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023)) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Sede = SEDE_NOMBRE_MAT,
         Nivel = TIPO_NIVEL,
         Modalidad = NIVEL,
         Programa = PROGRAMA_2) %>% 
  summarise(Total = n(), .by = c(Sede, Nivel, Modalidad, Programa, Periodo)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
    arrange(Sede, Nivel, Modalidad)
 
write_xlsx(Mat_Programas, "GNFA/Calificadora/2024/Matricula/Mat_Programas.xlsx")

# Matricula Primera Vez----

# Por Nivel

Mat_Pvez_Nivel <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023), 
                                                    MAT_PVEZ == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Nivel = TIPO_NIVEL) %>% 
  summarise(Total = n(), .by = c(Nivel, Periodo)) %>% 
  arrange(desc(Nivel))

write_xlsx(Mat_Pvez_Nivel, "GNFA/Calificadora/2024/MatriculaPV/Mat_Pvez_Nivel.xlsx")


# Por Sedes

Mat_Pvez_Sedes <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023), 
                                               MAT_PVEZ == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Sede = SEDE_NOMBRE_MAT) %>% 
  summarise(Total = n(), .by = c(Sede, Periodo)) %>% 
  arrange(desc(Sede))

write_xlsx(Mat_Pvez_Sedes, "GNFA/Calificadora/2024/MatriculaPV/Mat_Pvez_Sedes.xlsx")

# Por Programas Académicos

Mat_Pvez_Programas <- UnalData::Matriculados %>% filter(YEAR%in% c(2019:2023), 
                                                        MAT_PVEZ == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "_")) %>%
  rename(Sede = SEDE_NOMBRE_MAT,
         Nivel = TIPO_NIVEL,
         Modalidad = NIVEL,
         Programa = PROGRAMA_2) %>% 
  summarise(Total = n(), .by = c(Sede, Nivel, Modalidad, Programa, Periodo)) %>% 
  pivot_wider(names_from = Periodo, values_from = Total, values_fill = 0) %>% 
  arrange(Sede, Nivel, Modalidad)

write_xlsx(Mat_Pvez_Programas, "GNFA/Calificadora/2024/MatriculaPV/Mat_Pvez_Programas.xlsx")

