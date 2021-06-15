#------------------------------------------------------------------------------#
#
# Objetivo:                 Contabilizar respuestas efectivas de en PRICOVID
#
# Encargada:                Regina Isabel Medina Rosales
# Contacto:                 regina.medina@alumnos.cide.edu
# Fecha de creación:        01 de marzo de 2021 
# Fecha de actualización:   21 de abril de 2021
#
#------------------------------------------------------------------------------#


# 0. Configuración inicial -----------------------------------------------------
# Limpiar área de trabajo 
rm(list=ls())

# Cargar librerías 
library(readxl)         # Librería para importar archivos xlsx
library(dplyr)          # Librería para limpiar datos
library(stringr)        # Librería para manejar variables tipo texto 

# Dirección 
inp <- "02_datos_crudos/"
out <- "03_datos_limpios/"



# 1. Cargar datos --------------------------------------------------------------

df_raw_pre <- read_excel(paste0(inp, "PRISIÓN-COVID_2021_05_04_11_55.xlsx")) # Hasta el 4 de abril
df_raw_cru <- read_excel(paste0(inp, "PRISIÓN-COVID_2021_01_06_09_28.xlsx"))



# 2. Filtrar resultados --------------------------------------------------------

df_raw_pre <- df_raw_pre                            %>% 
    mutate(fecha = as.Date(str_sub(Date, 1, 10)))   %>% 
    select(SbjNum, SbjNam, Date, Duration, Upload, consentimiento:fecha)


df_raw_cru <- df_raw_cru                            %>% 
    mutate(fecha = as.Date(str_sub(Date, 1, 10)))   %>% 
    filter(fecha >= "2021-04-04") %>% 
    select(SbjNum, SbjNam, Date, Duration, Upload, consentimiento:fecha)

dim(df_raw_pre)
dim(df_raw_cru)

df_raw <- df_raw_pre                                %>%
    bind_rows(df_raw_cru)                           %>% 
    filter(SbjNam == "campo")


# Distinguir los resultados 
table(df_raw_pre$SbjNam)    # Base previa a la caída de los servidores
table(df_raw_cru$SbjNam)    # Base posterior a la caída de los servidores
table(df_raw$SbjNam)        # Base conjunta

# Guardar respaldo de resultado unificados y filtrados
df_PRICO_resultados <- df_raw
write.csv(df_PRICO_resultados, paste0(out, "df_PRICO_resultados.csv"))
save(df_PRICO_resultados, file = paste0(out, "df_PRICO_resultados.RData"))


# 3. Explorar base -------------------------------------------------------------

# Nombre de las variables
names(df_raw)

# Respuestas abiertas
length(unique(df_raw$Q_5_S))    # Relación con la persona internada
length(unique(df_raw$S_13_9))   # ¿Por qué no visitó a su interno?    
length(unique(df_raw$S_34_5))   # ¿Por qué no informó de los síntomas?
length(unique(df_raw$S_35_6))   # ¿Qué pasó luego de que dio positivo a covid?
length(unique(df_raw$S_37_9))   # ¿Cómo le afectó a usted que tuviera covid?
length(unique(df_raw$S_42_8))   # ¿Qué tipo de problemas de salud ha tenido?
length(unique(df_raw$S_47_6))   # ¿Cómo se entera de las medidas?
length(unique(df_raw$S_49_16))  # ¿A qué se dedica usted generalmente?





