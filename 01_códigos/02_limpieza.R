#------------------------------------------------------------------------------#
# Objetivo:                 Procesar respuestas de la encuesta PRICOVID
#
# Encargada:                Regina Isabel Medina Rosales
# Contacto:                 regina.medina@alumnos.cide.edu
# Fecha de creación:        08 de junio de 2021 
# Fecha de actualización:   14 de junio de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Limpiar área de trabajo 
rm(list=ls())

# Cargar librerías 
require(pacman)
p_load(readxl, tidyverse, tidyr, stringr)

# Dirección 
inp <- "03_datos_limpios/"
out <- "03_datos_limpios/"

# 1. Cargar datos --------------------------------------------------------------

# Base unificada de todas las respuestas
load(paste0(inp, "df_PRICO_resultados.RData"))

# Codebook para decodificar respuestas numéricas
df_codebook_crudo <- read_excel("02_datos_crudos/codebook_final.xls")


# 2. Decodificar respuestas numéricas ------------------------------------------

# Guardar nombres de las variables del codebook 
v_names         <- names(df_codebook_crudo)

# Guardar el texto de las respuestas
df_codebook     <- df_codebook_crudo    %>% 
    rename(
        q_id   = v_names[2], 
        a_id   = v_names[4], 
        a_text = v_names[5])            %>% 
    select(q_id, a_id, a_text)          %>% 
    filter(
        !is.na(q_id), 
        !is.na(a_id))

# View(df_codebook)

# Guardar el texto de las preguntas
df_preguntas <- df_codebook_crudo       %>% 
    rename(
        q_id   = v_names[2], 
        a_id   = v_names[4], 
        q_text = v_names[5])            %>% 
    filter(is.na(a_id), q_id %in% 1:53) %>% 
    select(q_id, q_text)                %>% 
    # Quitar indicaciones y código html
    mutate(
        q_text = str_replace_all(q_text, "<b>"    , ""), 
        q_text = str_replace_all(q_text, "</b>"   , ""), 
        q_text = str_replace_all(q_text, "<p>"    , ""), 
        q_text = str_replace_all(q_text, "</p>"   , ""), 
        q_text = str_replace_all(q_text, "<br>"   , ""),  
        q_text = str_replace_all(q_text, "<div>"  , ""), 
        q_text = str_replace_all(q_text, "</div>" , ""), 
        q_text = str_replace_all(q_text, "<font color=\"#000000\">", ""), 
        q_text = str_replace_all(q_text, "<font color=\"#008000\">", ""), 
        q_text = str_replace_all(q_text, "<font color=\"#0000FF\">", ""), 
        q_text = str_replace_all(q_text, "</font>", ""), 
        q_text = str_replace_all(q_text, "&nbsp;" , " "), 
        q_text = str_replace_all(q_text, "Por favor, marque todas las opciones que apliquen.", ""), 
        q_text = str_replace_all(q_text, "Por favor, marque todas las que apliquen.", ""), 
        q_text = str_replace_all(q_text, "Por favor indique su respuesta con número.", ""), 
        q_text = str_replace_all(q_text, "Por favor, indique con número.", "")
    )                                                   %>% 
    # Cambiar texto de consentimiento informado 
    mutate(q_text = case_when(
        q_id == 1 ~ "¿Acepta participar en esta encuesta?", 
        T ~ q_text))                            %>% 
    # Preguntas que no aparecen en las respuestas
    # filter(!(q_id %in% c(6, 8, 11, 14, 17, 31, 36)))    %>% 
    filter(!(q_id %in% c(28, 40)))

# Preguntas que faltan en la base:     6, 8, 11, 14, 17, 31, 36
# Preguntas que son sólo indicaciones: 28, 40, 

# View(df_preguntas)

# Preguntas y respuestas con códigos y texto 
df_resultados <- df_respuestas                      %>% 
    full_join(df_codebook,  by = c("q_id", "a_id")) %>% 
    full_join(df_preguntas, by = c("q_id"))         %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq)

# View(df_resultados)

# 3. Sintetizar y decodificar respustas ----------------------------------------

# Guardar nombres de las variables de la base de resultados 
v_names     <- names(df_PRICO_resultados)
v_variables <- as.character(v_names[6:180])


# 3.1. Consentimiento informado ------------------------------------------------

# Consentimiento 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 1])
    
df_r1 <- df_PRICO_resultados            %>% 
    group_by(consentimiento)            %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[1], 
        names_to = "q_code")            %>% 
    mutate(q_id = 1)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    # complete(a_text, nesting(a_id, a_text), fill = list(freq = 0))
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# 3.2. Sociodemográficos -------------------------------------------------------

# Mayoría de edad  
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 2])

df_r2 <- df_PRICO_resultados            %>% 
    group_by(mayor_edad)                %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[2], 
        names_to = "q_code")            %>% 
    mutate(q_id = 2)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Género
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 3])

df_r3 <- df_PRICO_resultados            %>% 
    group_by(genero)                    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[3], 
        names_to = "q_code")            %>% 
    mutate(q_id = 3)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    # Agregar de manera manual los valores de las respuestas porque el codebook está mal
    mutate(a_text = case_when(
        a_id == 0 ~ "Prefiero no responder", 
        a_id == 1 ~ "Masculino", 
        a_id == 2 ~ "Femenino", 
        a_id == 3 ~ "Otro", 
    ), 
        a_text = factor(a_text, 
            levels = v_levels)) %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Entidad 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 4])

df_r4 <- df_PRICO_resultados            %>% 
    group_by(entidad)                   %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[4], 
        names_to = "q_code")            %>% 
    mutate(q_id = 4)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Parentesco con persona ppl 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 5])

df_r5 <- df_PRICO_resultados            %>% 
    group_by(parentesco)                %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[5], 
        names_to = "q_code")            %>% 
    mutate(q_id = 5)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


table(df_PRICO_resultados$Q_5_S)
v_otros <- unique(df_PRICO_resultados$Q_5_S)


# Prisión preventiva
# v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 6])
# 
# df_r6 <- df_PRICO_resultados            %>% 
#     group_by(prision_preventiva)        %>% 
#     summarise(freq = n())               %>% 
#     pivot_longer(
#         cols = v_variables[7], 
#         names_to = "q_code")            %>% 
#     mutate(q_id = 6)                    %>% 
#     rename(a_id = value)                %>% 
#     left_join(df_preguntas, 
#         by = "q_id")                    %>% 
#     left_join(df_codebook, 
#         by = c("q_id", "a_id"))         %>% 
#     mutate(a_text = factor(a_text, 
#         levels = v_levels))             %>% 
    # mutate(total = sum(freq))           %>% 
    # group_by(a_id, a_text)              %>%
    # mutate(porcentaje = round(freq*100/total, 1), 
    #     p_text = paste0(porcentaje, "%")) %>% 
    # select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   




# 3.3. Detención y contingencia ------------------------------------------------

# Año de detención 
df_r7 <- df_PRICO_resultados            %>% 
    group_by(año_detencion)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[7], 
        names_to = "q_code")            %>% 
    mutate(q_id = 7)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    # Agregar de manera manual los valores de las respuestas porque el codebook está mal
    mutate(
        a_text = case_when(
            a_id == 99 ~ "No sé / No quiero responder", 
            T ~ as.character(a_id) 
        ), 
        # a_text = factor(a_text, 
        #     levels = v_levels)
    ) %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Mes de detención 
# v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 8])
# 
# df_r8 <- df_PRICO_resultados            %>% 
#     group_by(mes_detención)             %>% 
#     summarise(freq = n())               %>% 
#     pivot_longer(
#         cols = v_variables[8], 
#         names_to = "q_code")            %>% 
#     mutate(q_id = 8)                    %>% 
#     rename(a_id = value)                %>% 
#     left_join(df_preguntas, 
#         by = "q_id")                    %>% 
#     left_join(df_codebook, 
#         by = c("q_id", "a_id"))         %>% 
#     mutate(a_text = factor(a_text, 
#         levels = v_levels))             %>% 
    # mutate(total = sum(freq))           %>% 
    # group_by(a_id, a_text)              %>%
    # mutate(porcentaje = round(freq*100/total, 1), 
    #     p_text = paste0(porcentaje, "%")) %>% 
    # select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Centro penitenciario 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 9])

df_r9 <- df_PRICO_resultados            %>% 
    group_by(centro)                    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[8], 
        names_to = "q_code")            %>% 
    mutate(q_id = 9)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(a_text = case_when(
        a_id == 99 ~ v_levels[3], 
        T ~ a_text), 
        a_text = factor(a_text,
            levels = v_levels))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Frecuencia de visita previo a la pandemia 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 10])

df_r10 <- df_PRICO_resultados           %>% 
    group_by(frecuencia_visita_prev)    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[9], 
        names_to = "q_code")            %>% 
    mutate(q_id = 10)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(a_id == 99 ~ v_levels[10], T ~ a_text),
        a_text = factor(a_text, levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# table(df_PRICO_resultados$Q_11_S)
# v_otros <- unique(df_PRICO_resultados$Q_11_S)

# Frecuencia de visita posterior a la pandemia 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 12])

df_r12 <- df_PRICO_resultados           %>% 
    group_by(frecuencia_visita_post)    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[10], 
        names_to = "q_code")            %>% 
    mutate(q_id = 12)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(a_id == 99 ~ v_levels[7], T ~ a_text),
        a_text = factor(a_text, levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   




# Razón de no visita 1 (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 13])

df_r13 <- df_PRICO_resultados           %>%
    group_by(razon_no_visita_post_O1)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = v_variables[11],
        names_to = "q_code")            %>%
    mutate(q_id = 13)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[10], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# table(df_PRICO_resultados$Q_13_S)
# v_otros <- unique(df_PRICO_resultados$Q_13_S)


# Motivo de cierre de centro 1:
# df_r22 <- df_PRICO_resultados           %>% 
#     group_by(cierre_centro_O1)          %>% 
#     summarise(freq = n())               %>% 
#     pivot_longer(
#         cols = v_variables[22], 
#         names_to = "q_code"
#     ) %>% 
#     mutate(q_id = NA)                   %>% 
#     select(q_id, q_code, value, freq)



# Acceso a recursos
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 18])

df_r15 <- df_PRICO_resultados           %>% 
    group_by(acceso_recursos)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[31], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 15)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
        levels = c("No aplica", v_levels)))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Sistema de videollamadas
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 16])

df_r16 <- df_PRICO_resultados           %>% 
    group_by(sis_videollamada)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[32], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 16)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Acceso a sistema de videollamada
# df_17 

# Cosas que provee la persona (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 18])

df_r18 <- df_PRICO_resultados           %>%
    group_by(cosas_O1)                  %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = v_variables[33],
        names_to = "q_code"
    ) %>%
    mutate(q_id = 18)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[15], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# View(df_r18)

# Cambio en el costo de visita 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 19])

df_r19 <- df_PRICO_resultados           %>% 
    group_by(costo_cambio)              %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[48], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 19)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  0 ~ v_levels[4],
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  3 ~ v_levels[3],
            a_id ==  99 ~ v_levels[5],
        ), 
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Cambio en las cosas 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 20])

df_r20 <- df_PRICO_resultados           %>% 
    group_by(cosas_cambio)              %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[49], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 20)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  3 ~ v_levels[3],
            a_id ==  99 ~ v_levels[4],
        ), 
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Cambio en las horas de traslado 

# El tiempo del traslado requiere un tratamiento especial 
# Debo sumar el número de horas con el número de minutos 

# v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 21])
# 
# df_r21_a <- df_PRICO_resultados         %>%
#     group_by(horas_traslado)            %>%
#     summarise(freq = n())               %>%
#     pivot_longer(
#         cols = v_variables[50],
#         names_to = "q_code"
#     ) %>%
#     mutate(q_id = 21)                    %>%
#     select(q_id, q_code, value, freq)
# 
# df_r21_b <- df_PRICO_resultados           %>%
#     group_by(minutos_traslado)            %>%
#     summarise(freq = n())               %>%
#     pivot_longer(
#         cols = v_variables[51],
#         names_to = "q_code"
#     ) %>%
#     mutate(q_id = 21)                    %>%
#     mutate(total = sum(freq))           %>%
#     group_by(a_id, a_text)              %>%
#     mutate(porcentaje = round(freq*100/total, 1),
#         p_text = paste0(porcentaje, "%")) %>%
#         ungroup() %>% 
#     select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)


# View(df_r21_b)



# Cambio en el tiempo de traslado 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 22])

df_r22 <- df_PRICO_resultados           %>% 
    group_by(traslado_cambio)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[52], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 22)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  3 ~ v_levels[3],
            a_id ==  99 ~ v_levels[4],
        ), 
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Gasto de una visita 
df_r23 <- df_PRICO_resultados           %>% 
    group_by(gasto_visita)              %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[53], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 23)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
                     T ~ as.character(a_id)), 
        a_text = factor(a_text))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Gasto de visitas al mes 
df_r24 <- df_PRICO_resultados           %>% 
    group_by(gasto_mes)                 %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[54], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 24)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        a_text = case_when(
            a_id == 99 ~ "No sé / No quiero responder", 
            T ~ as.character(a_id)), 
        a_text = factor(a_text))  %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Situaciones en las que ha tenido que pagar 

v_topics <- unique(df_codebook$a_text[df_codebook$q_id == 25])[1:9]
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 25])[10:12]

# Pagar para ingresar o que alguno de los acompañantes ingrese
df_r25_1 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_1)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[55], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[1]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Hacerle llegar artículos 
df_r25_2 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_2)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[56], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[2]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Darle atención médica 

df_r25_3 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_3)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[57], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[3]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Que le toque cama al interno 

df_r25_4 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_4)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[58], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[4]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Que el interno tenga cubrebocas
df_r25_5 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_5)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[59], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[5]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Desinfectar la celda del interno 
df_r25_6 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_6)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[60], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[6]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Tener a su interno en un área menos poblada
df_r25_7 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_7)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[61], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[7]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Que no lo aíslen 
df_r25_8 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_8)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[62], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[8]), 
        a_text = case_when(
            a_id ==  1 ~ v_levels[1],
            a_id ==  2 ~ v_levels[2],
            a_id ==  99 ~ v_levels[3],
        ), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Brindarle protección dentro del centro 
df_r25_9 <- df_PRICO_resultados         %>% 
    group_by(T_situaciones_pago_9)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[63], 
        names_to = "q_code")            %>% 
    mutate(q_id = 25)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text, v_topics[9]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# View(df_r25_1)
# View(df_r25_2)
# View(df_r25_3)
# View(df_r25_4)
# View(df_r25_5)
# View(df_r25_6)
# View(df_r25_7)
# View(df_r25_8)
# View(df_r25_9)



# 3.4. Medidas dentro del centro penitenciario ---------------------------------

v_topics <- unique(df_codebook$a_text[df_codebook$q_id == 26])[1:9]
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 26])[10:12]

# Prohibición de visitas presenciales
df_r26_1 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_1)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[64], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[1]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Medidas de distanciamiento social 
df_r26_2 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_2)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[65], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[2]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Gel antibacterial 
df_r26_3 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_3)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[66], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[3]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Limitación en la cantidad de cosas
df_r26_4 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_4)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[67], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[4]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Procesos de desinfección
df_r26_5 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_5)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[68], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[5]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Uso de cubrebocas 
df_r26_6 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_6)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[69], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[6]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Acceso a agua y jabón
df_r26_7 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_7)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[70], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[7]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Filtros en la entrada
df_r26_8 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_8)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[71], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[8]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Sanitización de productos 
df_r26_9 <- df_PRICO_resultados         %>% 
    group_by(T_medidas_visitas_9)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[72], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 26)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste("En el centro penitenciario donde está su interno(a), ",
            v_topics[9]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   




# View(df_r26_1)
# View(df_r26_2)
# View(df_r26_3)
# View(df_r26_4)
# View(df_r26_5)
# View(df_r26_6)
# View(df_r26_7)
# View(df_r26_8)
# View(df_r26_9)


# Medidas para personas internas (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)

v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 27])

df_r27 <- df_PRICO_resultados           %>%
    group_by(medidas_internos_O1)       %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = v_variables[73],
        names_to = "q_code"
    ) %>%
    mutate(q_id = 27)                   %>%
    rename(a_id = value)                %>%
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == 88 ~ v_levels[11],
            a_id == 99 ~ v_levels[12],
            T ~ a_text),
        a_text = factor(a_text, levels = v_levels))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# 
# View(df_r27)
# table(df_r27$a_id)
# table(df_r27$a_text)
# unique(df_r27$a_text)


# 3.5. Salud de personas internas ----------------------------------------------

# La pregunta 28 son instrucciones 

# Confirmados covid
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 29])

df_r29 <- df_PRICO_resultados           %>% 
    group_by(confirmados_covid19)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[85], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 29)                   %>% 
    rename(a_id = value)                %>% 
        left_join(df_preguntas,
            by = "q_id")                %>%
        left_join(df_codebook,
            by = c("q_id", "a_id"))         %>%
        mutate(
            a_text = case_when(a_id == 99 ~ v_levels[3], T ~ a_text),
            a_text = factor(a_text, levels = v_levels))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Pruebas covid
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 30])

df_r30 <- df_PRICO_resultados           %>% 
    group_by(prueba_covid19)            %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[86], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 30)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, levels = v_levels))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Pregunta 31: Le informaron sobre el resultado de la prueba
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 31])


# Síntomas covid 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 32])

df_r32 <- df_PRICO_resultados           %>% 
    group_by(sintomas_covid19)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[87], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 32)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, levels = v_levels))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# ¿Informó a las autoridades?
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 33])

df_r33 <- df_PRICO_resultados           %>% 
    group_by(informar_autoridades)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[88], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 33)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Razón de no informar 01 (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 34])

df_r34 <- df_PRICO_resultados           %>% 
    group_by(razon_no_informar_O1)      %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[89], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 34)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


table(df_PRICO_resultados$Q_34_S)
v_otros <- unique(df_PRICO_resultados$Q_34_S)



# Medidas positivas covid 01 (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 35])

df_r35 <- df_PRICO_resultados               %>% 
    group_by(medidas_positivo_covid19_O1)   %>% 
    summarise(freq = n())                   %>% 
    pivot_longer(
        cols = v_variables[96], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 35)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Condiciones durante el aislamiento 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 36])

# Afectación por covid19 (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 37])

df_r37 <- df_PRICO_resultados          %>% 
    group_by(afectacion_covid19_O1)     %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[104], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 37)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  0 ~ v_levels[8], 
            a_id ==  1 ~ v_levels[1], 
            a_id ==  2 ~ v_levels[2], 
            a_id ==  3 ~ v_levels[3], 
            a_id ==  4 ~ v_levels[4], 
            a_id ==  5 ~ v_levels[5], 
            a_id ==  6 ~ v_levels[6], 
            a_id ==  7 ~ v_levels[7], 
            # a_id ==  8 ~ v_levels[], # No se codificó ningún 8, sólo un 0
            a_id ==  9 ~ v_levels[9], 
            a_id == 99 ~ v_levels[10], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


table(df_PRICO_resultados$Q_35_S)
v_otros <- unique(df_PRICO_resultados$Q_35_S)


# Servicio médico 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 38])

df_r38 <- df_PRICO_resultados           %>% 
    group_by(servicio_medico)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[115], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 38)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[4], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# 3.6. Situación jurídica de la persona interna --------------------------------

# Acceso a la justicia
v_topics <- unique(df_codebook$a_text[df_codebook$q_id == 39])[1:5]
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 39])[6:8]


# ¿Se restringió el acceso a su abogado?
df_r39_1 <- df_PRICO_resultados         %>% 
    group_by(T_acceso_justicia_1)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[116], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 39)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[1]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# ¿Se han restringido las visitas de derechos humanos?
df_r39_2 <- df_PRICO_resultados         %>% 
    group_by(T_acceso_justicia_2)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[117], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 39)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[2]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# ¿Se ha restringido el proceso judicial de su interno?
df_r39_3 <- df_PRICO_resultados         %>% 
    group_by(T_acceso_justicia_3)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[118], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 39)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[3]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# ¿El trabajo de los jueces de ejecución?
df_r39_4 <- df_PRICO_resultados         %>% 
    group_by(T_acceso_justicia_4)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[119], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 39)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[4]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# ¿Las notificaciones judiciales?
df_r39_5 <- df_PRICO_resultados         %>% 
    group_by(T_acceso_justicia_5)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[120], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 39)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[5]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# View(df_r39_1)
# View(df_r39_2)
# View(df_r39_3)
# View(df_r39_4)
# View(df_r39_5)


# La pregunta 40 es una explicación 

# Problemas que ha enfrentado 
v_topics <- unique(df_codebook$a_text[df_codebook$q_id == 41])[1:8]
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 41])[9:11]

# Con el trabajo
df_r41_1 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_1)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[121], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[1]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Con sus hijos
df_r41_2 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_2)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[122], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[2]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Con sus vecinos
df_r41_3 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_3)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[123], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[3]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Con las escuelas de sus hijos
df_r41_4 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_4)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[124], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[4]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Con su salud
df_r41_5 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_5)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[125], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[5]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Con su pareja
df_r41_6 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_6)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[126], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[6]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Con su familia
df_r41_7 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_7)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[127], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[7]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Con su negocio
df_r41_8 <- df_PRICO_resultados         %>% 
    group_by(T_problemas_8)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[128], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 41)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    mutate(
        q_text = paste(q_text,
            v_topics[8]), 
        a_text = case_when(
            a_id ==  1  ~ v_levels[1],
            a_id ==  2  ~ v_levels[2],
            a_id ==  99 ~ v_levels[3]), 
        a_text = factor(a_text, 
            levels = c(v_levels)))      %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# View(df_r41_1)
# View(df_r41_2)
# View(df_r41_3)
# View(df_r41_4)
# View(df_r41_5)
# View(df_r41_6)
# View(df_r41_7)
# View(df_r41_8)



# Problema  de salud (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 42])

df_r42 <- df_PRICO_resultados           %>% 
    group_by(problemas_salud_O1)        %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[129], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 42)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas,
        by = "q_id")                    %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[9], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


table(df_PRICO_resultados$Q_42_S)
v_otros <- unique(df_PRICO_resultados$Q_42_S)


# Cambio en los problemas  
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 43])

df_r43 <- df_PRICO_resultados          %>% 
    group_by(problemas_cambio)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[139], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 43)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[4], 
            T ~ a_text),
        a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Cambio en obtener información 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 44])

df_r44 <- df_PRICO_resultados          %>% 
    group_by(info_cambio)               %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[140],
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 44)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[4], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Cambio en la corrupción 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 45])

df_r45 <- df_PRICO_resultados           %>% 
    group_by(corrupcion_cambio)         %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[141], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 45)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id ==  0 ~ v_levels[4], 
            a_id ==  1 ~ v_levels[1], 
            a_id ==  2 ~ v_levels[2], 
            a_id ==  3 ~ v_levels[3], 
            a_id == 99 ~ v_levels[5], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Información de salud
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 46])

df_r46 <- df_PRICO_resultados          %>% 
    group_by(info_salud)               %>% 
    summarise(freq = n())              %>% 
    pivot_longer(
        cols = v_variables[142], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 46)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Medios (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 47])

df_r47 <- df_PRICO_resultados           %>% 
    group_by(medios_O1)                 %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[143], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 47)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# 3.7. Ocupación del familiar --------------------------------------------------

# Número de hijos 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 48])

df_r48 <- df_PRICO_resultados           %>% 
    group_by(hijos)                     %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[151], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 48)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[1], 
            T ~ as.character(a_id)),
        a_text = factor(a_text))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Ocupación (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 49])

df_r49 <- df_PRICO_resultados           %>% 
    group_by(ocupacion_O1)              %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[152], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 49)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[17], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



table(df_PRICO_resultados$Q_49_S)
v_otros <- unique(df_PRICO_resultados$Q_49_S)


# Trabajo
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 50])

df_r50 <- df_PRICO_resultados          %>% 
    group_by(trabajo)                   %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[170], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 50)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   




# Razón de no trabajar
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 51])

df_r51 <- df_PRICO_resultados          %>% 
    group_by(razon_sin_trabajo)         %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[171], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 51)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[9], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


table(df_PRICO_resultados$Q_51_S)
v_otros <- unique(df_PRICO_resultados$Q_51_S)


# Ingresos 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 52])

df_r52 <- df_PRICO_resultados          %>% 
    group_by(ingresos)                  %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[173], 
        names_to = "q_code") %>% 
    mutate(q_id = 52)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[4], 
            T ~ a_text),
        a_text = factor(a_text, 
            levels = v_levels))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Comentarios adicionales
table(df_PRICO_resultados$Q_53_S)
v_otros <- unique(df_PRICO_resultados$Q_53_S)

# View(df_r52)
# View(df_preguntas)
# View(df_codebook)
# View(df_codebook_crudo)

# 4. Unificar bases ------------------------------------------------------------

# Preguntas que son sólo indicaciones: 28, 40, 
# Preguntas que faltan en la base:     6, 8, 11, 14, 17, 31, 36


df_unida <- rbind(
    df_r1,   df_r2,   df_r3,   df_r4,   df_r5,   
    # df_r6,   
    df_r7,   
    # df_r8,   
    df_r9, 
    df_r10, 
    # df_r11, 
    df_r12, df_r13, 
    # df_r14, 
    df_r15, df_r16, 
    # df_r17, 
    df_r18, df_r19, 
    df_r20,  
    # df_r21, # Tiempo de traslado: necesita tratamiento especial
    df_r22,  df_r23,  df_r24,  
    df_r25_1,
    df_r25_2,
    df_r25_3,
    df_r25_4,
    df_r25_5,
    df_r25_6,
    df_r25_7,
    df_r25_8,
    df_r25_9,
    df_r26_1,
    df_r26_2,
    df_r26_3,
    df_r26_4,
    df_r26_5,
    df_r26_6,
    df_r26_7,
    df_r26_8,
    df_r26_9,
    df_r27,  
    # df_r28, # No es pregunta, son instrucciones
    df_r29, 
    df_r30,  
    # df_r31,
    df_r32,  df_r33,  df_r34,  df_r35, 
    # df_r36,  
    df_r37,  df_r38,  
    df_r39_1,
    df_r39_2,
    df_r39_3,
    df_r39_4,
    df_r39_5, 
    # df_r40,  # No es pregunta, son instrucciones
    df_r41_1,
    df_r41_2,
    df_r41_3,
    df_r41_4,
    df_r41_5,
    df_r41_6,
    df_r41_7,
    df_r41_8,
    df_r42,  df_r43,  df_r44,  df_r45,  df_r46,  df_r47,  df_r48,  df_r49, 
    df_r50,  df_r51,  df_r52) 

# View(df_unida)

# 5. Guardar datos ------------------------------------------------------------

df_resultados_frecuencias <- df_unida 

save(df_resultados_frecuencias, file = "03_datos_limpios/df_resultados_frecuencias.RData")

df_formato_xlsx  <- df_unida %>% 
    rename(
        `Número de la pregunta` = q_id, 
        `Código de pregunta`    = q_code, 
        `Pregunta`              = q_text, 
        `Código de respuesta`   = a_id, 
        `Respuesta`             = a_text, 
        `Frecuencia`            = freq, 
        `Porcentaje (numérico)` = porcentaje, 
        `Porcentaje`            = p_text
    ) 

openxlsx::write.xlsx(df_formato_xlsx, "03_datos_limpios/df_resultados_frecuencias.xlsx")

# FIN. -------------------------------------------------------------------------
