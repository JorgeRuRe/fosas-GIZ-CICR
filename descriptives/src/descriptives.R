# Author: JRR
# Maintainers: JRR
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# fosas-GIZ-CICR/descriptives/src/descriptives.R

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, sf, here, tm, tidytext)



# Files -------------------------------------------------------------------
files <- list(fosas_clean = here("import", "output", "fosas_clean.rds"),
              shp_munis = here("descriptives", "input", "Municipios_Mx.shp"),
              mapa_munis = here("descriptives", "output", "mapa_munis."),
              top_sitios = here("descriptives", "output", "top_sitios."),
              tf_idf = here("descriptives", "output", "tf_idf."))


devices <- c("png", "svg")

# Cargar datos  -----------------------------------------------------------
fosas_clean <- readRDS(files$fosas_clean) %>% mutate(inegi=as.character(inegi))
shp_munis <- st_read(files$shp_munis)



# mapa fosas --------------------------------------------------------------------

# agrupar por munis 
casos_por_municipio <- fosas_clean %>% 
      group_by(municipio, inegi) %>%
      summarise(total_fosas = sum(fosas, na.rm = T),
                total_cuerpos = sum(total_cuerpos, na.rm = T),
                total_masculino = sum(masculino, na.rm = T),
                total_femenino = sum(femenino, na.rm = T)) %>% 
      ungroup()


#  Datos para mapa
mapa_prensa <- shp_munis %>%
      left_join(casos_por_municipio,
                by = c("CVEGEO" = "inegi"))


# mapear fosas 
mapa_prensa %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = total_fosas), size = 0.2, color = "black") +
      scale_fill_continuous(breaks = c(0, 30, 60, 100),
                            low = "#BACDB0", high = "#475B63",  na.value = "white",
                            name = "Total de fosas") +
      labs(title = "Distribución territorial del total de fosas observadas por prensa en México",
           subtitle = "2007-2019") +
      theme_void(base_family = "Courier New") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10))


walk(devices, ~ ggsave(filename = file.path(paste0(files$mapa_munis, .x)),
                       device = .x, width = 14, height = 10))

# Análisis de texto -------------------------------------------------------

# Preparar stopwords 
custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                      lexicom = "custom"))


# tokenizar 
fosas_tokens <- fosas_clean %>% 
   select(estado, descripcion_geo) %>% 
   unnest_tokens(word, descripcion_geo)  %>% 
   anti_join(custom_stop_words) %>%
   filter(!str_detect(word, "[0-9]")) %>% 
   ungroup()
  

# graficar top ubicaciones 
fosas_tokens %>% 
   count(word, sort = TRUE) %>%
   filter(!word %in% c("na", "kil", "conocido", "nuevo", "metro", "ju", "rez"))  %>%
   top_n(30) %>%
   mutate(word = reorder(word, n)) %>% 
   ggplot(aes(x = word, y = n)) +
   geom_col(fill = "#558B6E") +
   coord_flip() +
   theme_minimal(base_family = "Courier New") +
   theme(plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         panel.grid.major.x = element_line(colour="grey", size=0.5),
         axis.text.y = element_text(face = "bold")) +
   labs(y = "Count",
        x = NULL,
        title = "Ubicaciones geográfica de fosas clandestinas observadas por prensa",
        subtitle = "Con base en la descripción del texto de la nota") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$top_sitios, .x)),
                       device = .x, width = 14, height = 10))


# filtrar estados para graficar 
estados <- fosas_tokens %>% 
   filter(estado == "Jalisco" | estado == "Nuevo León" | estado == "Zacatecas" |
             estado == "Tamaulipas" | estado == "Veracruz de Ignacio de la Llave" |
             estado == "Morelos") %>%
   count(word, estado, sort = TRUE) %>%
   ungroup() 



# sacar TF-IDF 
estados_tf <- estados %>% 
   bind_tf_idf(word, estado, n)  %>%
   filter(!word %in% c("na", "kil", "conocido",
                       "mart", "lugarconocido", "aproximadamente", 
                       "conocida", "denominada", "sota", "mecánico", "aires", "altos", 
                       "terracer", "rquez", "méxico", "tecom", "san", "inmediaciones", "colindante",
                       "comunidad")) %>%
   arrange(-tf_idf) %>%
   ungroup() %>%
   group_by(estado) %>%
   top_n(3)



# Graficar 
estados_tf %>%
   ggplot(aes(reorder(word, tf_idf), tf_idf, fill= estado)) +
   geom_col(fill = "#558B6E") +
   coord_flip() +
   facet_wrap(~ estado, scales = "free", ncol = 3) +
   theme_classic(base_family = "Courier New") +
   theme(plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(face = "bold", hjust = 0.5),
         panel.grid.major.x = element_line(colour="grey", size=0.5),
         axis.text.y = element_text(face = "bold"),
         axis.text.x = element_text(face = "bold")) +
   labs(y = NULL,
        x = NULL,
        title = "Sitios característicos donde se localizan fosas clandestinas observadas por prensa",
        subtitle = "En seis entidades de la república ")
   
walk(devices, ~ ggsave(filename = file.path(paste0(files$tf_idf, .x)),
                       device = .x, width = 14, height = 10))
# FIN. 


