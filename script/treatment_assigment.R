
# ------------------------------------------------------------------------- #
#           Identificación de unidades de salud en las parroquias           #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(pins)
library(gt)


# PINS --------------------------------------------------------------------


path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

pin_list(carpeta)


# Base del indice ---------------------------------------------------------

ids_index <- pin_read(carpeta,"bdd_index_ids")

# Base de establecimientos ------------------------------------------------

establecimientos <- pin_read(carpeta,"bdd_establecimientos_clase")


# Identificación temporal del tratamiento ---------------------------------


establecimientos <- establecimientos %>% 
  ungroup() %>% 
  mutate(
    
    # Identificacion de parroquias donde no había (t-1) pero 
    # luego si (t)
    
    across(.cols = 3:15,
           .fns =  ~lag(.x) == 0 & .x >0),
    
    anio = as.numeric(anio)
    )  


# incremento --------------------------------------------------------------

establecimientos <- establecimientos %>% 
  rowwise() %>% 
  mutate(crecimiento_dotacion = sum(across(.cols = 3:15)))

establecimientos %>% 
  count(anio,crecimiento_dotacion) %>% 
  ggplot() +
  geom_col(aes(x = crecimiento_dotacion,
               y = n)) +
  facet_wrap(.~anio)

pre_panel <- establecimientos %>% 
  ungroup() %>% 
  group_by(dpa_parroq) %>% 
  mutate(
    pre_indicador = (anio <= 2017 & crecimiento_dotacion == 0),
    post_indicador = (anio > 2017 & crecimiento_dotacion > 0)) %>% 
  summarise(across(c(pre_indicador,
                     post_indicador),
                   sum,
                   na.rm = T))


# Parroquias donde: -------------------------------------------------------


pre_panel <- pre_panel %>% 
  mutate(grupo = case_when(
    
    # No habia en el t-1
    # Si hay en el t
    
    pre_indicador >= 1  & post_indicador >=1 ~ "tratamiento",
    
    # No habia en t-1
    # Siguió sin haber en t
    
    pre_indicador >= 1 & post_indicador == 0 ~ "control",
    
    # Si habia en t-1
    # Sigue habiento en t
    
    pre_indicador == 0 & post_indicador >= 1 ~ "control_2",
    
    # Si habia en t-1
    # No hay en t
    
    pre_indicador == 0 & post_indicador == 0 ~ "control_3",
  ))


# Demanda instatisfecha -------------------------------------------


ids_index <- pre_panel %>% 
  select(dpa_parroq,grupo) %>% 
  right_join(ids_index)


pin_write(board = carpeta,
          x = ids_index,
          name = "bdd_index_ids",
          description = "Asignación de grupos de control y tratamiento",
          versioned = T)


# Pequeño EDA -------------------------------------------------------------



