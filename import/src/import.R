# Author: JRR
# Maintainers: JRR
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# fosas-GIZ-CICR/import/src/import.R

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, janitor)

# Archivos  ---------------------------------------------------------------

files <- list(inp_fosas = here("import/input/fosas_prensa.csv"),
              out_fosas = here("import/output/fosas_clean.rds"))

# Limpieza ----------------------------------------------------------------

keep_cols <- c("inegi", "year", "fecha", "estado", "municipio",
               "descripcion_geo", "fosas", "total_cuerpos", "masculino", "femenino")



clean_fosas <- function(files) {
      df <- read_csv(files$inp_fosas)
      
      df$cve_ente <- if (df$cve_ente < 10) {
            str_pad(df$cve_ente, width= 2, side="left", pad="0")
      }
      
      df$cve_muni <- if (df$cve_muni < 99) {
            str_pad(df$cve_muni, width= 3, side="left", pad="0") 
      } 
      
      df <-  df %>% 
            clean_names() %>%
            mutate(numero_de_fosas = na_if(numero_de_fosas, "na"),
                   total_de_cuerpos_y_osamentas = na_if(total_de_cuerpos_y_osamentas, "na"),
                   masculino = na_if(masculino, "na"),
                   femenino = na_if(femenino, "na")) %>% 
            mutate(ano = as.integer(ano),
                   fecha_de_la_primera_nota_que_refiere_al_hallazgo = as.Date(fecha_de_la_primera_nota_que_refiere_al_hallazgo),
                   estado = as.character(estado),
                   cve_ente = as.character(cve_ente),
                   cve_muni = as.character(cve_muni),
                   sitio_de_hallazgo = as.character(sitio_de_hallazgo),
                   municipio = as.character(municipio),
                   numero_de_fosas = as.integer(numero_de_fosas),
                   total_de_cuerpos_y_osamentas = as.integer(total_de_cuerpos_y_osamentas),
                   masculino = as.integer(masculino), 
                   femenino = as.integer(femenino),
                   inegi = paste0(cve_ente, cve_muni)) %>% 
            rename(fecha = fecha_de_la_primera_nota_que_refiere_al_hallazgo,
                   total_cuerpos = total_de_cuerpos_y_osamentas,
                   fosas = numero_de_fosas,
                   year = ano,
                   descripcion_geo = sitio_de_hallazgo) %>%
            select(all_of(keep_cols))
      
      return(df)
      }


main <- function(){
      print("working on fosas prensa")
      fosas_clean <- clean_fosas(files)
      
      print("writing fosas clean")
      saveRDS(fosas_clean, files$out_fosas) 
      }

main()

# fin. 



      
