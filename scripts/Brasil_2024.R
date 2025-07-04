rm(list=ls())

library(PNADcIBGE) # https://cran.r-project.org/web/packages/PNADcIBGE/index.html
library(tidyverse)

# Option 1: Store each quarter in a list
pnad_2024_trimestres <- list()

for (i in 1:4) {
  print(paste("Descargando", i, "trimestre"))
  pnad_2024_trimestres[[i]] <- get_pnadc(year = 2024, quarter = i)
}

pnad_2024 <- map(pnad_2024_trimestres, ~ .x$variables %>% as_tibble())

pnad_2024 <- bind_rows(pnad_2024)

# las cuatro bases juntas
#saveRDS(pnad_2024, "Bases/brasil_2024.RDS")
# POR AHORA MUY PESADO...