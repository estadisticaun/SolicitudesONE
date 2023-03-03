# Librerías requeridas

library(UnalData)
library(UnalR)
library(tidyverse)

# Calcular total de registros con documentos - sin reperticiones

# Aspirantes

Aspirantes <- UnalData::Aspirantes %>%
              filter(YEAR >= 2023) %>% 
              distinct(ID, .keep_all = TRUE) %>% 
              nrow()
              
# Matriculados

Matricula <- UnalData::Matriculados %>% 
             distinct(ID, .keep_all = TRUE) %>% nrow()

# Graduados

Graduados <- UnalData::Graduados %>% 
  distinct(ID, .keep_all = TRUE) %>% nrow()

# Docentes

Docentes <- UnalData::Docentes %>% 
  distinct(ID, .keep_all = TRUE) %>% nrow()

# Funcionarios

Administrativos <- UnalData::Administrativos %>% 
  distinct(ID, .keep_all = TRUE) %>% nrow()

# SaberPro

SaberPro <- UnalData::SaberPro %>% 
  distinct(ID, .keep_all = TRUE) %>% nrow()

# Cálculo total de registros

paste("El total de registros con documentos no repetidos es:", 
      Aspirantes + Matricula + Graduados + 
        Docentes + Administrativos + SaberPro)
