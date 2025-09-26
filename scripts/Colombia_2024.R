######################################################################
# ESTO QUEDA DE REFERENCIA PERO NO SUBO LAS BASES PORQUE PESAN MUCHO #
######################################################################

rm(list=ls())

library(tidyverse)
library(here)

base_dir <- here("datos") # Esto habría que cambiarlo

# Todos los subdirectorios de meses
months <- list.dirs(path = base_dir, full.names = FALSE, recursive = FALSE)

# Nombres de los 8 módulos
module_files <- c(
  "caracteristicas" = "Características generales, seguridad social en salud y educación.CSV",
  "hogar_vivienda" = "Datos del hogar y la vivienda.CSV",
  "fuerza_trabajo" = "Fuerza de trabajo.CSV",
  "migracion" = "Migración.CSV",
  "no_ocupados" = "No ocupados.CSV",
  "ocupados" = "Ocupados.CSV",
  "otras_formas_trabajo" = "Otras formas de trabajo.CSV",
  "otros_ingresos" = "Otros ingresos e impuestos.CSV"
)

module_lists <- map(names(module_files), ~list()) %>%
  set_names(names(module_files))

for (month in months) {

  for (module_name in names(module_files)) {
    file_path <- file.path(base_dir, month, module_files[[module_name]])
    
    if (file.exists(file_path)) {
      module_df <- suppressMessages(read_csv2(file_path, col_types = cols(.default = "c"))) %>%
        mutate(month = month, .before = 1)
      
      module_lists[[module_name]][[month]] <- module_df
    } else {
      warning("File not found: ", file_path)
    }
  }
}

final_module_tibbles <- map(module_lists, ~bind_rows(.x))

iwalk(final_module_tibbles, ~{
  filename <- paste0("geih_", .y, ".rds")
  write_rds(.x, file = here(filename))
})

# Join and clean, adaptado del código del repo que encontré para el tidyverse
join_and_clean <- function(x, y) {
  key_variables <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR", "FEX_C18")
  by_vars <- intersect(intersect(names(x), names(y)), key_variables)
  
  joined <- left_join(x, y, by = by_vars, suffix = c(".x", ".y")) # LEFT JOIN PARA QUEDARNOS CON OCUPADOS

  cols_y <- names(joined)[endsWith(names(joined), ".y")]
  cols_x <- names(joined)[endsWith(names(joined), ".x")]
  
  joined %>%
    select(-all_of(cols_y)) %>%
    rename_with(~ sub("\\.x$", "", .x), .cols = all_of(cols_x))
}

combined_by_month <- list()

for (month in months) {

  month_modules <- map(module_lists, ~.x[[month]]) %>%
    compact()
  
  if (length(month_modules) > 0 && "ocupados" %in% names(month_modules)) {
    base_ocupados <- month_modules[["ocupados"]]
    other_modules <- month_modules[setdiff(names(month_modules), "ocupados")]
    
    if (length(other_modules) > 0) {
      combined_month <- reduce(other_modules, join_and_clean, .init = base_ocupados)
    } else {
      combined_month <- base_ocupados
    }
    combined_by_month[[month]] <- combined_month
  }
}

ocupados_completo <- bind_rows(combined_by_month)

write_rds(ocupados_completo, file = here("Bases/Colombia_2024.RDS"))
