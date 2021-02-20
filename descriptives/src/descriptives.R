# Author: JRR
# Maintainers: JRR
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, sf, here, tm, tidytext)



# Files -------------------------------------------------------------------
files <- list(fosas_clean = here("import", "output", "fosas_clean.rds"),
              mapa_munis = here("descriptives", "output", "mapa_munis."),
              top_sitios = here("descriptives", "output", "top_sitios."),
              tf_idf = here("descriptives", "output", "tf_idf."))


devices <- c("png", "svg")

# Cargar datos  -----------------------------------------------------------
fosas_clean <- readRDS(files$fosas_clean) %>% mutate(inegi=as.character(inegi))




# Análisis de texto -------------------------------------------------------

# Preparar stopwords 
custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                      lexicom = "custom"))


# tokenizar el texto de las columnas 
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
         plot.subtitle = element_text(face = "bold", hjust = 0.5),
         panel.grid.major.x = element_line(colour="grey", size=0.5),
         axis.text.y = element_text(face = "bold", size = 12),
         axis.text.x = element_text(face = "bold", size = 12)) +
   labs(y = "Total de veces mencionadas en sitios",
        x = NULL,
        title = "Ubicaciones geográficas de fosas clandestinas observadas por prensa",
        subtitle = "Con base en la descripción del texto de la nota") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$top_sitios, .x)),
                       device = .x, width = 14, height = 10))


# filtrar estados para sacar TF-IDF
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
   mutate(estado = ifelse(estado == "Veracruz de Ignacio de la Llave", "Veracruz", 
                                   estado)) %>% 
   ggplot(aes(reorder(word, tf_idf), tf_idf, fill= estado)) +
   geom_col(fill = "#558B6E") +
   coord_flip() +
   facet_wrap(~ estado, scales = "free", ncol = 3) +
   theme_classic(base_family = "Courier New") +
   theme(plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(face = "bold", hjust = 0.5),
         panel.grid.major.x = element_line(colour="grey", size=0.5),
         axis.text.y = element_text(face = "bold", size = 12),
         strip.background = element_blank(),
         strip.text = element_text(size = 14, face = "bold")) +
   labs(y = NULL,
        x = NULL,
        title = "Sitios característicos donde se localizan fosas clandestinas observadas por prensa",
        subtitle = "En seis entidades de la república")
   
walk(devices, ~ ggsave(filename = file.path(paste0(files$tf_idf, .x)),
                       device = .x, width = 14, height = 10))



# FIN. 