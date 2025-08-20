rm(list=ls())

library(tidyverse)
library(readr)

df <- read_rds("Bases/Colombia_2024.RDS") %>% # NO LA SUBI PORQUE PESA UN GIGA Y MEDIO
  filter(CLASE == 1)

base_homog <- df %>% 
  mutate(
    PAIS = "Colombia",
    ANO = 2024,
    SEXO = case_when(P3271 == 1 ~ "Varon",  # Sino, está P3039 que es género
                     P3271 == 2 ~ "Mujer"),
    EDAD = P6040,
    WEIGHT  = FEX_C18, # reemplacé por el que encontré, que es este
    COND = "Ocupado",
    CATOCUP = case_when(P6430  %in%  c(1,2,3,8)~ "Asalariado",
                        # Es decir, 
                        # 1: Obrero o empleado de empresa particular,
                        # 2: Obrero o empleado del gobierno,
                        # 3: Empleado doméstico,
                        # 8: Otro, ¿cuál?
                        # este último quizá debería ir al Resto, pero así era en el script
                        P6430  %in%  c(4)~ "Cuenta Propia",
                        P6430  %in%  c(5)~ "Patron",
                        TRUE ~ "Resto"),
    SECTOR = case_when(P6430  %in%  c(1,4,5,7)~ "Priv",
                       P6430  %in%  c(2)~ "Pub",
                       P6430  %in%  c(3)~ "SD",
                       TRUE ~ NA),
    PRECASEG =  case_when(P6920 == 1 ~ 0,
                          P6920 == 2 ~ 1), # revisar si queda alguno == 3 ("Ya es pensionado")
    PRECAREG =  case_when(P6440 == 1 & P6450 == 2 ~ 0,    # contrato escrito
                          !(P6440 == 1 & P6450 == 2)~ 1), # sin contrato escrito
    part.time.inv = case_when(P6800 < 35 & P6810 == 1 ~ "Part Involunt",
                              P6800 < 35 & P6810 != 1 ~ "Part Volunt", # se podría revisar P6810 y P6810S1
                              P6800 >= 35 ~ "Full Time"), 
    PRECAPT = case_when(part.time.inv == "Part Involunt"~1,
                        part.time.inv %in%  c("Part Volunt","Full Time")~0),
    PRECATEMP = case_when(
      P6460 == 1 ~ 0,
      P6460 == 2  ~ 1), # Contrato a término fijo
    CALIF =   case_when(
      substr(OFICIO_C8,1,1) == 0| # Reemplacé OFICIO por OFICIO_C8
        substr(OFICIO_C8,2,2) %in%  1:2 ~ "Alta",
      substr(OFICIO_C8,1,1) != 0 & # Esto es redundante me parece
        substr(OFICIO_C8,2,2) %in% 3:5 ~ "Media",
      substr(OFICIO_C8,1,1) != 0 & # Esto también, creo
        substr(OFICIO_C8,2,2) %in% 6:9 ~ "Baja"),
    TAMA = # ESTO CAMBIÓ, antes era P6870
      case_when(
        P3069  %in% 1:4 ~ "Pequeño", # 1 a 10
        P3069  %in% 5:7 ~ "Mediano", # 11 a 50
        P3069  %in% 8:9  ~ "Grande"),#  51 +,
    TAMA = case_when( P6430 == 4  ~ "Pequeño",
                      TRUE ~TAMA),
    EDUC = # ya no más P6210. lo charle con mi vieja, colombiana, para hacer sentido.
      case_when(P3042 %in% 1:3 ~ "Primaria",
                # 1: Ninguno
                # 2: Preescolar 
                # 3: Básica primaria (1o - 5o)
                # PERO pasé a secundaria "Básica secundaria (6o - 9o)"
                P3042 %in% 5:7 ~ "Secundaria",
                P3042 %in% 8:13 ~ "Terciaria"),
    PRECASALUD = NA, # Queda pendiente buscar si ahora sí hay algo
    ING = INGLABO # no sé qué es esto
  ) 

# Puede ser interesante ir a mirar P1879 (50A. ¿Cuál es la razón principal por la que trabaja en forma independiente en lugar de trabajar para un empleador o patrono ?)


variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","COND","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG","TAMA","CALIF","ING") 

base_homog_final <- base_homog %>% 
  select(all_of(variables))

saveRDS(base_homog_final,file = "bases_homog/colombia_2024.RDS")

