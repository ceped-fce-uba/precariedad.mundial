rm(list=ls())

library(PNADcIBGE) # aparentemente oficial del IBGE: https://cran.r-project.org/web/packages/PNADcIBGE/index.html
library(tidyverse)

pnad_2024_trimestres <- list()

for (i in 1:4) {
   print(paste("Descargando", i, "trimestre"))
   pnad_2024_trimestres[[i]] <- get_pnadc(year = 2024, quarter = i)
 }

BRA1 <- pnad_2024_trimestres[[1]]$variables %>% as_tibble()
BRA2 <- pnad_2024_trimestres[[2]]$variables %>% as_tibble()
BRA3 <- pnad_2024_trimestres[[3]]$variables %>% as_tibble()
BRA4 <- pnad_2024_trimestres[[4]]$variables %>% as_tibble()

Base <- bind_rows(BRA1, BRA2, BRA3, BRA4) %>% 
  rename(PERIODO = Trimestre)

remove(BRA1, BRA2, BRA3, BRA4, pnad_2024_trimestres)

variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

Base <- Base %>% # Primero estos filtros para que no se cuelgue la computadora
  filter(V1022 == 'Urbana') %>% # En 2019 las observaciones Urbanas venían codificadas como '1' y las ruras como '2'.
  filter(VD4002 == "Pessoas ocupadas") # Igual que el script de 2019

Base <- Base %>%
  mutate(ANO = 2024,
    PAIS = "Brasil",
    WEIGHT = V1028, # El que usó Facu en 2019
    SEXO = case_when(V2007 == "Homem" ~ "Varon", 
                     V2007 == "Mulher" ~ "Mujer"), 
    EDAD= V2009,
    EDUC = case_when(
      VD3004 %in% c("Sem instrução e menos de 1 ano de estudo", 
                    "Fundamental incompleto ou equivalente", 
                    "Fundamental completo ou equivalente", 
                    "Médio incompleto ou equivalente")~ "Primaria", 
      VD3004 %in% c("Médio completo ou equivalente",
                    "Superior incompleto ou equivalente")~ "Secundaria", 
      VD3004 %in% c("Superior completo")~ "Terciaria"),
    CATOCUP = case_when(
      VD4009 == "Conta-própria" ~ "Cuenta propia", 
      VD4009 == "Trabalhador familiar auxiliar"~ "Resto", 
      VD4009 == "Empregador" ~ "Patron", 
      VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor público com carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor público sem carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Trabalhador doméstico com carteira de trabalho assinada" ~ "Asalariado",
      VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"~ "Asalariado", 
      VD4009 == "Militar e servidor estatutário" ~ "Asalariado"),
    SECTOR = case_when(
      VD4009 == "Empregado no setor público com carteira de trabalho assinada" ~ "Pub",
      VD4009 == "Empregado no setor público sem carteira de trabalho assinada" ~ "Pub",
      VD4009 == "Militar e servidor estatutário" ~ "Pub",
      VD4009 == "Trabalhador doméstico com carteira de trabalho assinada" ~ "SD",
      VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada" ~ "SD",
      VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ "Priv",
      VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ "Priv",
      VD4009 == "Empregador" ~ "Priv",
      VD4009 == "Conta-própria" ~ "Priv",
      VD4009 == "Trabalhador familiar auxiliar" ~ "Priv"),
    PRECAPT = case_when( 
      VD4031 < 35 & VD4031 > 0 & V4063A == "Sim" ~ 1,               
      VD4031 < 35 & VD4031 > 0 & V4063A == "Não" ~ 0, 
      VD4031 > 34 ~ 0),
    PRECATEMP = case_when(
      V4025 == "Sim" ~ 1,
      V4025 == "Não" ~ 0),
    PRECASEG = case_when(
      VD4012 == "Não contribuinte" ~ 1,          
      VD4012 == "Contribuinte" ~ 0),
    PRECAREG = case_when( 
      VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ 1,
      VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ 0), 
    PRECASALUD = NA, 
    TAMA = case_when( 
      V4018 %in% c("1 a 5 pessoas", "6 a 10 pessoas") ~ "Pequeño",
      V4018 == "11 a 50 pessoas" ~ "Mediano",
      V4018 == "51 ou mais pessoas" ~ "Grande"),
    CALIF = case_when( 
      VD4011 == "Ocupações elementares" ~ "Baja", 
      VD4011 %in% c("Operadores de instalações e máquinas e montadores",
                    "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios",
                    "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",                
                    "Trabalhadores dos serviços, vendedores dos comércios e mercados",
                    "Trabalhadores de apoio administrativo") ~ "Media", 
      VD4011 %in% c("Técnicos e profissionais de nível médio",
                    "Profissionais das ciências e intelectuais",
                    "Diretores e gerentes") ~ "Alta"), # OJO QUE LAS OCUPACIONES MAL DEFINIDAS Y LOS DE LAS FFAA (etc.) QUEDARON AFUERAS SIN QUE DROPIEMOS 
    ING = VD4016) %>% 
  select(all_of(variables)) # parece que quedó obsoleto pasarle directo el vector de variables

saveRDS(Base, "bases_homog/brasil_2024.rds")
