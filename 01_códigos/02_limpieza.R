#------------------------------------------------------------------------------#
# Objetivo:                 Procesar respuestas de la encuesta PRICOVID
#
# Encargada:                Regina Isabel Medina Rosales
# Contacto:                 regina.medina@alumnos.cide.edu
# Fecha de creación:        08 de junio de 2021 
# Fecha de actualización:   04 de julio de 2021
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
df_respuestas     <- df_codebook_crudo    %>% 
    rename(
        q_id   = v_names[2], 
        a_id   = v_names[4], 
        a_text = v_names[5])            %>% 
    select(q_id, a_id, a_text)          %>% 
    filter(
        !is.na(q_id), 
        !is.na(a_id))

# View(df_codebook)
 q_tiempo_larga <- "Aproximadamente, ¿cuánto tiempo le toma llegar desde su casa hasta el centro penitenciario?las horas y minutos. Si no sabe cuánto tiempo toma el traslado o desea <u>NO</u> responder, por favor ingrese un <u>0</u> en cada campo."
 q_tiempo_corta <- "Aproximadamente, ¿cuánto tiempo le toma llegar desde su casa hasta el centro penitenciario?"

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
        q_text = str_replace_all(q_text, "Por favor, indique con número.", ""), 
    )                                                   %>%
    mutate(q_text = ifelse(q_text == q_tiempo_larga, q_tiempo_corta, q_text)) %>% 
    # Cambiar texto de consentimiento informado 
    mutate(q_text = case_when(
        q_id == 1 ~ "¿Acepta participar en esta encuesta?", 
        T ~ q_text))                                    %>% 
    # Preguntas que no aparecen en las respuestas
    # filter(!(q_id %in% c(6, 8, 11, 14, 17, 31, 36)))    %>% 
    filter(!(q_id %in% c(28, 40)))

# Preguntas que faltan en la base:     6, 8, 11, 14, 17, 31, 36
# Preguntas que son sólo indicaciones: 28, 40, 

df_codebook <- df_respuestas                                %>% 
    full_join(df_preguntas, by = c("q_id"))                 %>% 
    select(q_id, q_text, a_id, a_text)


# Buscar disparidades
names(df_PRICO_resultados)  # Hay 184 nombres de varialbes en la base 
unique(df_codebook$q_id)    # Pero sólo aparecen 53 preguntas 



# 3. Sintetizar y decodificar respustas ----------------------------------------

# Guardar nombres de las variables de la base de resultados 
v_names     <- names(df_PRICO_resultados)
v_variables <- as.character(v_names[6:192])

length(v_names)

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
    left_join(df_codebook, 
        by = c("q_id", "a_id"))  %>% 
    mutate(a_text = factor(a_text, 
        levels = v_levels))             %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    # complete(a_text, nesting(a_id, a_text), fill = list(freq = 0))
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r1)

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
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 6])

df_r6 <- df_PRICO_resultados            %>%
    group_by(prision_preventiva)        %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = v_variables[7],
        names_to = "q_code")            %>%
    mutate(q_id = 6)                    %>%
    rename(a_id = value)                %>%
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    # Agregar de manera manual los valores de las respuestas porque el codebook está mal
    mutate(
        a_text = case_when(
            a_id == 99 ~ "No sé / No quiero responder",
            T ~ as.character(a_text)
        ),
        a_text = factor(a_text,
            levels = v_levels)
    ) %>% 
    mutate(total = sum(freq))           %>%
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1),
        p_text = paste0(porcentaje, "%")) %>%
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)


# View(df_r6)

# 3.3. Detención y contingencia ------------------------------------------------

# Año de detención 
df_r7 <- df_PRICO_resultados            %>% 
    group_by(año_detencion)             %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[8], 
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
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 8])

df_r8 <- df_PRICO_resultados            %>%
    group_by(mes_detención)             %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = v_variables[9],
        names_to = "q_code")            %>%
    mutate(q_id = 8)                    %>%
    rename(a_id = value)                %>%
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



# Centro penitenciario 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 9])

df_r9 <- df_PRICO_resultados            %>% 
    group_by(centro)                    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[10], 
        names_to = "q_code")            %>% 
    mutate(q_id = 9)                    %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[11], 
        names_to = "q_code")            %>% 
    mutate(q_id = 10)                   %>% 
    rename(a_id = value)                %>% 
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



# Razón de no visita previo a la pandemia 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 11])

# Seleccionar todas las variables
df_no_visita <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("razon_no_visita_prev"))

table(df_no_visita$razon_no_visita_prev_O1) # Aquí hay múltiples resultados
table(df_no_visita$razon_no_visita_prev_O2) # Aquí hay tres respuestas (para 2 y 3)
table(df_no_visita$razon_no_visita_prev_O3) # Aquí hay una respuesta para 4
table(df_no_visita$razon_no_visita_prev_O4) # No hay respuestas
table(df_no_visita$razon_no_visita_prev_O5) # No hay respuestas
table(df_no_visita$razon_no_visita_prev_O6) # No hay respuestas


# No entendía por qué había tantas columnas para la misma pregunta de opción múltiple

# Hasta que hice el cruce
table(df_no_visita$razon_no_visita_prev_O1, df_no_visita$razon_no_visita_prev_O2)
table(df_no_visita$razon_no_visita_prev_O1, df_no_visita$razon_no_visita_prev_O3) 
table(df_no_visita$razon_no_visita_prev_O2, df_no_visita$razon_no_visita_prev_O3) 

# Lo que me está mostrando son personas que respondieron a más de una cosa 
# La primera variable me muestra cuál fue su primera selección 
# Las demás me muestran su segunda o tercera selección 


# Debo transformar la base para juntar todas las respuestas 
df_r11_long <- df_no_visita %>%  
    select(-c(razon_no_visita_prev_O4:razon_no_visita_prev_O6)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "razon_no_visita_prev"
    ) %>% 
    filter(razon_no_visita_prev != -1)

# View(df_r11_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r11_long$SbjNum))          # Coincide con las 15 personas que nunca fueron


df_r11 <- df_r11_long           %>%
    group_by(razon_no_visita_prev)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "razon_no_visita_prev",
        names_to = "q_code")            %>%
    mutate(q_id = 11)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r11)

# table(df_PRICO_resultados$Q_11_S)
# v_otros <- unique(df_PRICO_resultados$Q_11_S)

# Frecuencia de visita posterior a la pandemia 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 12])

df_r12 <- df_PRICO_resultados           %>% 
    group_by(frecuencia_visita_post)    %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[19], 
        names_to = "q_code")            %>% 
    mutate(q_id = 12)                   %>% 
    rename(a_id = value)                %>% 
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




# Razón de no visita 1 (Aquí hay más de una base igual que en razon_no_visita_prev) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 13])

# Seleccionar todas las variables
df_no_visita <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("razon_no_visita_post"))

table(df_no_visita$razon_no_visita_post_O1)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O2)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O3)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O4)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O5)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O6)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O7)  # Sí hay respuestas
table(df_no_visita$razon_no_visita_post_O8)  # No hay respuestas
table(df_no_visita$razon_no_visita_post_O9)  # No hay respuestas
table(df_no_visita$razon_no_visita_post_O10) # No hay respuestas

# Debo transformar la base para juntar todas las respuestas 
df_r13_long <- df_no_visita %>%  
    select(-c(razon_no_visita_post_O8:razon_no_visita_post_O10)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "razon_no_visita_post"
    ) %>% 
    filter(razon_no_visita_post != -1)
     
# View(df_r13_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r13_long$SbjNum))          # Coincide con las 125 personas que no pudieron realizar la visita

df_r13 <- df_r13_long           %>%
    group_by(razon_no_visita_post)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "razon_no_visita_post",
        names_to = "q_code")            %>%
    mutate(q_id = 13)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[10], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>% 
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r13)

# table(df_PRICO_resultados$Q_13_S)
# v_otros <- unique(df_PRICO_resultados$Q_13_S)


# Motivo de cierre de centro :
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 14])

# Seleccionar todas las variables
df_cierre <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("cierre_centro"))

table(df_cierre$cierre_centro_O1) # Sí hay respuestas 
table(df_cierre$cierre_centro_O2) # Sí hay respuestas
table(df_cierre$cierre_centro_O3) # Sí hay respuestas
table(df_cierre$cierre_centro_O4) # Sí hay respuestas
table(df_cierre$cierre_centro_O5) # Sí hay respuestas
table(df_cierre$cierre_centro_O6) # Sí hay respuestas
table(df_cierre$cierre_centro_O7) # Sí hay respuestas
table(df_cierre$cierre_centro_O8) # No hay respuestas 
table(df_cierre$cierre_centro_O9) # No hay respuestas


# Debo transformar la base para juntar todas las respuestas 
df_r14_long <- df_cierre %>%  
    select(-c(cierre_centro_O8:cierre_centro_O9)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "cierre_centro"
    ) %>% 
    filter(cierre_centro != -1)

# View(df_r14_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r14_long$SbjNum)) # Todas las personas de la encuesta

df_r14 <- df_r14_long           %>%
    group_by(cierre_centro)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "cierre_centro",
        names_to = "q_code")            %>%
    mutate(q_id = 14)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[9], T ~ a_text),
        a_text = factor(a_text,
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# View(df_r14)

# Acceso a recursos
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 15])

df_r15 <- df_PRICO_resultados           %>% 
    group_by(acceso_recursos)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[40], 
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
        cols = v_variables[41], 
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
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 17])

df_r17 <- df_PRICO_resultados           %>% 
    group_by(sis_videollamada_acceso)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[42], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 17)                   %>% 
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

# Cosas que provee la persona 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 18])


# Seleccionar todas las variables
df_cosas <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("cosas"))


table(df_cosas$cosas_O1)
table(df_cosas$cosas_O2)
table(df_cosas$cosas_O3)
table(df_cosas$cosas_O4)
table(df_cosas$cosas_O5)
table(df_cosas$cosas_O6)
table(df_cosas$cosas_O7)
table(df_cosas$cosas_O8)
table(df_cosas$cosas_O9)
table(df_cosas$cosas_O10)
table(df_cosas$cosas_O11)
table(df_cosas$cosas_O12)
table(df_cosas$cosas_O13)
table(df_cosas$cosas_O14) # No hay cosas
table(df_cosas$cosas_O15) # No hay cosas

# Debo transformar la base para juntar todas las respuestas 
df_r18_long <- df_cosas %>%  
    select(-c(cosas_O14:cosas_O15, cosas_cambio)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "cosas"
    ) %>% 
    filter(cosas != -1)

# View(df_r18_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r18_long$SbjNum)) # Todas las personas de la encuesta

df_r18 <- df_r18_long           %>%
    group_by(cosas)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "cosas",
        names_to = "q_code")            %>%
    mutate(q_id = 18)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[15], T ~ a_text),
        a_text = factor(a_text,
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# View(df_r18)

# Cambio en el costo de visita 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 19])

df_r19 <- df_PRICO_resultados           %>% 
    group_by(costo_cambio)              %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[58], 
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
        cols = v_variables[59], 
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

v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 21])

df_r21_a <- df_PRICO_resultados             %>%
    group_by(horas_traslado)                %>% 
    mutate(
        horas_traslado = as.character(horas_traslado), 
        horas_traslado = case_when(
            horas_traslado == "-1"   ~ "No aplica", 
            horas_traslado == "2.59" ~ "3", 
            T ~ horas_traslado), 
        a_id = horas_traslado)                       %>%
    summarise(freq = n())                   %>%
    pivot_longer(
        cols = v_variables[60],
        names_to = "q_code"
    ) %>%
    mutate(q_id = 21)                       %>%
    select(q_id, q_code, value, freq)

df_r21_b <- df_PRICO_resultados             %>%
    group_by(minutos_traslado)              %>%
    summarise(freq = n())                   %>%
    pivot_longer(
        cols = v_variables[61],
        names_to = "q_code"
    ) %>%
    mutate(q_id = 21)                       %>%
    mutate(total = sum(freq))               %>%
    group_by(a_id, a_text)                  %>%
    mutate(porcentaje = round(freq*100/total, 1),
        p_text = paste0(porcentaje, "%"))   %>%
        ungroup() %>%
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)


# Tomar sólo las horas de traslado
df_r21 <- df_r21_a                      %>% 
    left_join(df_preguntas, 
        by = "q_id")                    %>%
    rename(a_id = value)                %>% 
    mutate(a_text = a_id)               %>% 
    mutate(porcentaje = round(freq*100/sum(freq), 1),
        p_text = paste0(porcentaje, "%")) %>%
    ungroup() %>%
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)

 # View(df_r21)


# Cambio en el tiempo de traslado 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 22])

df_r22 <- df_PRICO_resultados           %>% 
    group_by(traslado_cambio)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[62], 
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
        cols = v_variables[63], 
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
        cols = v_variables[64], 
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
        cols = v_variables[65], 
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
        cols = v_variables[66], 
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
        cols = v_variables[67], 
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
        cols = v_variables[68], 
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
        cols = v_variables[69], 
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
        cols = v_variables[70], 
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
        cols = v_variables[71], 
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
        cols = v_variables[72], 
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
        cols = v_variables[73], 
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
        cols = v_variables[74], 
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
        cols = v_variables[75], 
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
        cols = v_variables[76], 
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
        cols = v_variables[77], 
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
        cols = v_variables[78], 
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
        cols = v_variables[79], 
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
        cols = v_variables[80], 
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
        cols = v_variables[81], 
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
        cols = v_variables[82], 
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


# REGRESAR AQUÍ -----
# Medidas para personas internas (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)

v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 27])

df_minternos <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("medidas_internos"))

table(df_minternos$medidas_internos_O1)
table(df_minternos$medidas_internos_O2)
table(df_minternos$medidas_internos_O3)
table(df_minternos$medidas_internos_O4)
table(df_minternos$medidas_internos_O5)
table(df_minternos$medidas_internos_O6)
table(df_minternos$medidas_internos_O7)
table(df_minternos$medidas_internos_O8)
table(df_minternos$medidas_internos_O9)
table(df_minternos$medidas_internos_O10)
table(df_minternos$medidas_internos_O11) # No hay respuestas 
table(df_minternos$medidas_internos_O12) # No hay respuestas 


# Debo transformar la base para juntar todas las respuestas 
df_r27_long <- df_minternos %>%  
    select(-c(medidas_internos_O11:medidas_internos_O12)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "medidas_internos"
    ) %>% 
    filter(medidas_internos != -1)

# View(df_r27_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r27_long$SbjNum)) # Todas las personas de la encuesta

df_r27 <- df_r27_long           %>%
    group_by(medidas_internos)   %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "medidas_internos",
        names_to = "q_code")            %>%
    mutate(q_id = 27)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 88 ~ v_levels[11], 
            a_id == 99 ~ v_levels[12], 
            T ~ a_text),
        a_text = factor(a_text,
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r27)

# 3.5. Salud de personas internas ----------------------------------------------

# La pregunta 28 son instrucciones 

# Confirmados covid
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 29])

df_r29 <- df_PRICO_resultados           %>% 
    group_by(confirmados_covid19)       %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[95], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 29)                   %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[96], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 30)                   %>% 
    rename(a_id = value)                %>% 
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

df_r31 <- df_PRICO_resultados           %>% 
    group_by(resultado_prueba_covid19)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[97], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 31)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], 
            T ~ a_text),
        a_text = factor(a_text, levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Síntomas covid 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 32])

df_r32 <- df_PRICO_resultados           %>% 
    group_by(sintomas_covid19)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[98], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 32)                    %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[99], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 33)                   %>% 
    rename(a_id = value)                %>% 
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], T ~ a_text),
        a_text = factor(a_text, 
            levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   



# Razón de no informar 01 (PENDIENTE VER POR QUÉ HAY MÁS DE UNA BASE)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 34])

df_informar <- df_PRICO_resultados                          %>% 
    select(SbjNum, starts_with("razon_no_informar"))

table(df_informar$razon_no_informar_O1)
table(df_informar$razon_no_informar_O2)
table(df_informar$razon_no_informar_O3)
table(df_informar$razon_no_informar_O4) # No hay respuestas 
table(df_informar$razon_no_informar_O5) # No hay respuestas
table(df_informar$razon_no_informar_O6) # No hay respuestas

# Debo transformar la base para juntar todas las respuestas 
df_r34_long <- df_informar %>%  
    select(-c(razon_no_informar_O4:razon_no_informar_O6))   %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "razon_no_informar"
    ) %>% 
    filter(razon_no_informar != -1)

# View(df_r34_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r34_long$SbjNum)) # Coincide con las 11 personas que no informaron

df_r34 <- df_r34_long                   %>%
    group_by(razon_no_informar)         %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "razon_no_informar",
        names_to = "q_code")            %>%
    mutate(q_id = 34)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[6], 
            T ~ a_text),
        a_text = factor(a_text,
            levels = c("No aplica", v_levels))
        )             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r34)

table(df_PRICO_resultados$Q_34_S)
v_otros <- unique(df_PRICO_resultados$Q_34_S)



# Medidas positivas covid (Necesaria la transformación de la base larga)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 35])


df_mpositivo <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("medidas_positivo_covid19"))

table(df_mpositivo$medidas_positivo_covid19_O1)
table(df_mpositivo$medidas_positivo_covid19_O2)
table(df_mpositivo$medidas_positivo_covid19_O3)
table(df_mpositivo$medidas_positivo_covid19_O4) # No hay respuestas 
table(df_mpositivo$medidas_positivo_covid19_O5) # No hay respuestas
table(df_mpositivo$medidas_positivo_covid19_O6) # No hay respuestas
table(df_mpositivo$medidas_positivo_covid19_O7) # No hay respuestas

# Debo transformar la base para juntar todas las respuestas 
df_r35_long <- df_mpositivo %>%  
    select(-c(medidas_positivo_covid19_O4:medidas_positivo_covid19_O7)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "medidas_positivo_covid19"
    ) %>% 
    filter(medidas_positivo_covid19 != -1)

# View(df_r35_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r35_long$SbjNum)) 

df_r35 <- df_r35_long                   %>%
    group_by(medidas_positivo_covid19)  %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "medidas_positivo_covid19",
        names_to = "q_code")            %>%
    mutate(q_id = 35)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica",
            a_id == 99 ~ v_levels[7], 
            T ~ a_text),
        a_text = factor(a_text,
            levels = c("No aplica", v_levels))
    )             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# View(df_r35)


# Condiciones durante el aislamiento 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 36])

df_r36 <- df_PRICO_resultados           %>% 
    group_by(condiciones_aislamiento_covid19)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[115], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 36)                    %>% 
    rename(a_id = value)                %>% 
    left_join(df_codebook,
        by = c("q_id", "a_id"))         %>%
    mutate(
        a_text = case_when(
            a_id == -1 ~ "No aplica", 
            a_id == 99 ~ v_levels[3], 
            T ~ a_text),
        a_text = factor(a_text, levels = c("No aplica", v_levels)))             %>%
    mutate(total = sum(freq))           %>% 
    group_by(a_id, a_text)              %>%
    mutate(porcentaje = round(freq*100/total, 1), 
        p_text = paste0(porcentaje, "%")) %>% 
    ungroup() %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   


# Afectación por covid19 (formato de base larga)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 37])

df_afecta <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("afectacion"))

table(df_afecta$afectacion_covid19_O1)
table(df_afecta$afectacion_covid19_O2)
table(df_afecta$afectacion_covid19_O3)
table(df_afecta$afectacion_covid19_O4)
table(df_afecta$afectacion_covid19_O5)
table(df_afecta$afectacion_covid19_O6)
table(df_afecta$afectacion_covid19_O7)
table(df_afecta$afectacion_covid19_O8)
table(df_afecta$afectacion_covid19_O9)  # No hay respuesta
table(df_afecta$afectacion_covid19_O10) # No hay respuesta

# Debo transformar la base para juntar todas las respuestas 
df_r37_long <- df_afecta %>%  
    select(-c(afectacion_covid19_O9:afectacion_covid19_O10)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "afectacion_covid19"
    ) %>% 
    filter(afectacion_covid19 != -1)

# View(df_r37_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r37_long$SbjNum)) 

df_r37 <- df_r37_long                   %>%
    group_by(afectacion_covid19)          %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "afectacion_covid19",
        names_to = "q_code")            %>%
    mutate(q_id = 37)                   %>%
    rename(a_id = value)                %>% 
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
            levels = c("No aplica", v_levels))
    )             %>%
    mutate(total = sum(freq))                               %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r37)

# Servicio médico 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 38])

df_r38 <- df_PRICO_resultados           %>% 
    group_by(servicio_medico)           %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[127], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 38)                   %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[128], 
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
        cols = v_variables[129], 
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
        cols = v_variables[130], 
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
        cols = v_variables[131], 
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
        cols = v_variables[132], 
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
        cols = v_variables[133], 
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
        cols = v_variables[134], 
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
        cols = v_variables[135], 
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
        cols = v_variables[136], 
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
        cols = v_variables[137], 
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
        cols = v_variables[138], 
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
        cols = v_variables[139], 
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
        cols = v_variables[140], 
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



# Problema  de salud (Transformación a formato largo)
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 42])

df_salud <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("problemas_salud"))

table(df_salud$problemas_salud_O1)
table(df_salud$problemas_salud_O2)
table(df_salud$problemas_salud_O3)
table(df_salud$problemas_salud_O4)
table(df_salud$problemas_salud_O5)
table(df_salud$problemas_salud_O6)
table(df_salud$problemas_salud_O7) # No hay respuesta 
table(df_salud$problemas_salud_O8) # No hay respuesta
table(df_salud$problemas_salud_O9) # No hay respuesta

 # Debo transformar la base para juntar todas las respuestas 
df_r42_long <- df_salud %>%  
    select(-c(problemas_salud_O7:problemas_salud_O9)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "problemas_salud"
    ) %>% 
    filter(problemas_salud != -1)

# View(df_r42_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r42_long$SbjNum)) 

df_r42 <- df_r42_long                   %>%
    group_by(problemas_salud)          %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "problemas_salud",
        names_to = "q_code")            %>%
    mutate(q_id = 42)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(total = sum(freq))                               %>% 
    mutate(a_text = ifelse(a_id == 99, "No aplica", a_text), 
        a_text = factor(a_text, levels = c("No aplica", v_levels))) %>% 
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r42)

table(df_PRICO_resultados$Q_42_S)
v_otros <- unique(df_PRICO_resultados$Q_42_S)


# Cambio en los problemas  
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 43])

df_r43 <- df_PRICO_resultados          %>% 
    group_by(problemas_cambio)          %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[151], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 43)                    %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[152],
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 44)                   %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[153], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 45)                   %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[154], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 46)                   %>% 
    rename(a_id = value)                %>% 
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



# Medios (Tranformación de base formato long) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 47])

df_medios <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("medios"))

table(df_medios$medios_O1)
table(df_medios$medios_O2)
table(df_medios$medios_O3)
table(df_medios$medios_O4)
table(df_medios$medios_O5)
table(df_medios$medios_O6) # No hay respuesta
table(df_medios$medios_O7) # No hay respuesta

# Debo transformar la base para juntar todas las respuestas 
df_r47_long <- df_medios %>%  
    select(-c(medios_O6:medios_O7)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "medios"
    ) %>% 
    filter(medios != -1)

# View(df_r47_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r47_long$SbjNum)) # Toda la encuesta

df_r47 <- df_r47_long                   %>%
    group_by(medios)          %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "medios",
        names_to = "q_code")            %>%
    mutate(q_id = 47)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(total = sum(freq))                               %>% 
    mutate(a_text = ifelse(a_id == 99, "No aplica", a_text),
        a_text = factor(a_text, levels = c("No aplica", v_levels))) %>%
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r47)


# 3.7. Ocupación del familiar --------------------------------------------------

# Número de hijos 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 48])

df_r48 <- df_PRICO_resultados           %>% 
    group_by(hijos)                     %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[163], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 48)                   %>% 
    rename(a_id = value)                %>% 
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



# Ocupación (Convertir a formato largo) 
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 49])

df_ocupa <- df_PRICO_resultados %>% 
    select(SbjNum, starts_with("ocupacion"))

table(df_ocupa$ocupacion_O1)
table(df_ocupa$ocupacion_O2)
table(df_ocupa$ocupacion_O3)
table(df_ocupa$ocupacion_O4)
table(df_ocupa$ocupacion_O5) # A partir de aquí no hay respuesta
table(df_ocupa$ocupacion_O6)
table(df_ocupa$ocupacion_O7)
table(df_ocupa$ocupacion_O8)
table(df_ocupa$ocupacion_O9)
table(df_ocupa$ocupacion_O10)
table(df_ocupa$ocupacion_O11)
table(df_ocupa$ocupacion_O12)
table(df_ocupa$ocupacion_O13)
table(df_ocupa$ocupacion_O14)
table(df_ocupa$ocupacion_O15)
table(df_ocupa$ocupacion_O16)
table(df_ocupa$ocupacion_O17)


# Debo transformar la base para juntar todas las respuestas 
df_r49_long <- df_ocupa %>%  
    select(-c(ocupacion_O5:ocupacion_O17)) %>% 
    pivot_longer(
        cols      = -SbjNum, 
        names_to  = "pregunta", 
        values_to = "ocupacion"
    ) %>% 
    filter(ocupacion != -1)

# View(df_r49_long)

# Revisar que siga el mismo número de identificadores únicos
length(unique(df_PRICO_resultados$SbjNum))
v_obs <- length(unique(df_r49_long$SbjNum)) # Toda la encuesta

df_r49 <- df_r49_long                   %>%
    group_by(ocupacion)          %>%
    summarise(freq = n())               %>%
    pivot_longer(
        cols = "ocupacion",
        names_to = "q_code")            %>%
    mutate(q_id = 49)                   %>%
    rename(a_id = value)                %>% 
    left_join(df_codebook, 
        by = c("q_id", "a_id"))         %>% 
    mutate(total = sum(freq))                               %>% 
    mutate(a_text = ifelse(a_id == 99, "No aplica", a_text),
        a_text = factor(a_text, levels = c("No aplica", v_levels))) %>%
    group_by(a_id, a_text)                                  %>%
    mutate(porcentaje = round(freq*100/v_obs, 1), 
        p_text = paste0(porcentaje, "%"))                   %>% 
    ungroup()                                               %>% 
    select(q_id, q_code, q_text, a_id, a_text, freq, porcentaje, p_text)   

# View(df_r49)
table(df_PRICO_resultados$Q_49_S)
v_otros <- unique(df_PRICO_resultados$Q_49_S)


# Trabajo
v_levels <- unique(df_codebook$a_text[df_codebook$q_id == 50])

df_r50 <- df_PRICO_resultados          %>% 
    group_by(trabajo)                   %>% 
    summarise(freq = n())               %>% 
    pivot_longer(
        cols = v_variables[182], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 50)                    %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[183], 
        names_to = "q_code"
    ) %>% 
    mutate(q_id = 51)                    %>% 
    rename(a_id = value)                %>% 
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
        cols = v_variables[185], 
        names_to = "q_code") %>% 
    mutate(q_id = 52)                    %>% 
    rename(a_id = value)                %>% 
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
# Preguntas que faltaban en la base:   6, 8, 11, 14, 17, 31, 36

df_unida <- rbind(
    df_r1,   df_r2,   df_r3,   df_r4,   df_r5,   
    df_r6,
    df_r7,   
    df_r8,
    df_r9, 
    df_r10, 
    df_r11,
    df_r12, df_r13, 
    df_r14,
    df_r15, df_r16, 
    df_r17,
    df_r18, df_r19, 
    df_r20,  
    df_r21, # Tiempo de traslado: necesita tratamiento especial
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
    df_r31,
    df_r32,  df_r33,  df_r34,  df_r35, 
    df_r36,
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
