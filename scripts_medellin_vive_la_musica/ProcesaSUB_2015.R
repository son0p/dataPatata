rm(list=ls())

# Se crea una función para abrir cualquier dataframe en una hoja de cálculo. Esto se hace para verificar
open_in_excel <- function(some_df){
    tFile<-tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="/tmp")
    write.csv2(some_df, tFile)
    system(paste('libreoffice ', tFile, '&'))
}

# Se crea una función para normalizar el nombre de las columnas retirando los espacios, las tildes y los signos de puntuación raros
normaliza_texto <- function(texto) {
    texto <- str_replace_all(texto,"[[:punct:]]"," ")
    texto <- stri_trans_general(texto, "Any-Title")
    texto <- str_replace_all(texto,"\\s{2,4}"," ")
    texto <- stri_trans_general(texto, "latin-ascii")
    texto <- str_trim(texto)
    texto
}

# Carga de librerías
install.packages("jsonlite") # Se instala la libería jsonlite, esto solo se hace una vez
require(plyr) # Se carga plyr para la manipulación de múltiples fuentes de información en forma vectorial
library(dplyr) # Se carga dplyr para la manipulación de data.frames a manera de pipes
require(tidyr) # Se carga tidyr para organizar información
require(stringr) # Se carga stringr para la manipulación de strings
require(stringi) # Se carga stringr para la manipulación de strings
require(readxl) # Se carga readxl para la leer archivos de excel
require(lubridate) # Se carga lubridate para la manipulación de fechas
require(jsonlite) # Se carga jsonlite para la manipulación de json
require(rhandsontable) # Se carga rhandsontable para publicar tablas en sitios web

## googlesheets
## require(devtools)
## install.packages("googlesheets")
devtools::install_github("jennybc/googlesheets") # Se instala el paquete googlesheets
require(googlesheets) # Se carga googlesheets para acceder a archivos alojados en esta plataforma
# Give googlesheets permission to access your spreadsheets and google drive
gs_auth() # Se genera el token de autenticación para acceder a googlesheets
user_session_info <- gs_user() # Se obtiene la información de usuario autenticado
gs_ls("Asistencias") # Se lista los archivos que tengan alguna coincidencia con "Asistencias"
## If you plan to consume data from a sheet or edit it, you must first register it.
asistencias <- gs_title("Asistencias Casas de La cultura, IE y Hospital") # Se obtienen los datos de el archivo "Asistencias Casas de La cultura, IE y Hospital"
str(asistencias) # Se ve la estructura de datos de el objeto obtenido
nWorkSheets <- asistencias$n_ws # Se leen el número de hojas que tiene el archivo
nSheetNames <- asistencias$ws$ws_title # se obtienen los nombres de las hojas que tiene el archivo
nSheetNames <- nSheetNames[-c(1,2)] # se retiran del archivo las hojas 1 y 2 porque no son de nuestro interés

## Función para leer las hojas de un mismo archivo
leer_hojas <- function(ws_title) {
    asistencias %>% gs_read(ws_title)
}

## setNames -> http://stackoverflow.com/questions/15553036/rename-id-column-in-ldply para que tengamos el .id con cada pestaña de donde viene
asistencias_todas <- ldply(setNames(nm = nSheetNames) ,leer_hojas) # Se leen todas la hojas y se guardan en una sola lista. Se le asigna el nombre de la hoja como id para la lista colapsada en una sola tabla

## Retira los registros con NA
idx_na <- which(is.na(asistencias_todas$Apellidos)) ## Se obtienen los índices donde el apellido de los participantes es NA
asistencias_todas <- asistencias_todas[-idx_na,] ## Se retiran los registros donde el apellido es NA

## Retira registros con las palabras TOTAL ASISTENTES", "CANTIDAD DE CLASES", "PROMEDIO ASISTENTES", "TOTAL HOMBRES", "TOTAL MUJERES"
retirar_resumen <- c("TOTAL ASISTENTES", "CANTIDAD DE CLASES", "PROMEDIO ASISTENTES", "TOTAL HOMBRES", "TOTAL MUJERES")
idx_retirar <- unlist(lapply(retirar_resumen, function(x) which(asistencias_todas$Apellidos == x))) ## Se ejecuta función para obtener índices de valores a retirar no deseados para la comparación
asistencias_todas <- asistencias_todas[-idx_retirar,] ## Se retiran definitivamente los registros donde aparecen estos valores no deseados

asistencias_para_comparar <- asistencias_todas[,c(1,3,4,5)] # Se seleccionan las columnas que quiero tener en cuenta para la comparación
colnames(asistencias_para_comparar) <- c("equipamiento","apellidos","nombres","documento") # Se les asigna los nombres a estas columnas
glimpse(asistencias_para_comparar) # Se verifica la estructura de datos de la tabla para comparar y que si tenga los nombres de columnas adecuados

sub_2015 <- read.csv2("./sub_actualizado_2015.csv", sep = ",", skip = 6) # Se carga el archivo desde el computador con el consolidado en los registros de inscripción SUB
sub_para_comparar <- sub_2015[,c(3,4,5,6,7,25)] # Se seleccionan las columnas para hacer la comparación de este archivo con el descargado de gdrive
colnames(sub_para_comparar) <- c("documento","primer_nombre","segundo_nombre","primer_apellido","segundo_apellido","equipamiento") # Se nombran las columnas para comparar
glimpse(sub_2015) # Se verifica la estructura de datos

## Concateno nombres para ambas listas
attach(sub_para_comparar) # Cargo el data.frame a un entorno para acceder directamente a las columnas con el nombre sin necesidad de usar el nombre del data.frame
sub_para_comparar$nombre_apellido <- paste0(primer_nombre," ",segundo_nombre," ",primer_apellido," ",segundo_apellido) # Se concatenan nombres y apellidos
detach(sub_para_comparar) # Saco al data.frame del entorno donde puedo usar directamente el nombre de las columnas

attach(asistencias_para_comparar) # Cargo el data.frame a un entorno para acceder directamente a las columnas con el nombre sin necesidad de usar el nombre del data.frame
asistencias_para_comparar$nombre_apellido <- paste0(nombres," ", apellidos) # Se concatenan nombres y apellidos
detach(asistencias_para_comparar) # Saco al data.frame del entorno donde puedo usar directamente el nombre de las columnas

sub_para_comparar$nombre_apellido <- normaliza_texto(sub_para_comparar$nombre_apellido) # Se normaliza el texto con la función descrita arriba
asistencias_para_comparar$nombre_apellido <- normaliza_texto(asistencias_para_comparar$nombre_apellido) # Se normaliza el texto con la función descrita arriba

match_nombre_exacto <- inner_join(asistencias_para_comparar, sub_para_comparar, by = 'nombre_apellido') # Se hace el match de los nombres que coinciden exactamente
no_match_nombre_exacto <- anti_join(asistencias_para_comparar, sub_para_comparar, by = 'nombre_apellido')# Se guardan los nombres que no coinciden exactamente
open_in_excel(match_nombre_exacto) # Se verifican en excel
open_in_excel(no_match_nombre_exacto)# Se verifican en excel
open_in_excel(sub_para_comparar)# Se verifican en excel

# Usando la función agrep se hacen búsqueda no exactas que se alojan en una lista. Cada nombre puede tener varios resultados de match no exacto
busqueda_nombres <- lapply(setNames(asistencias_para_comparar$nombre_apellido, paste0(asistencias_para_comparar$nombre_apellido," - ",asistencias_para_comparar$equipamiento)), function(x) agrep(x,sub_para_comparar$nombre_apellido,max.distance = 0.2))

## http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
resultado_busqueda <- lapply(busqueda_nombres, function(x) sub_para_comparar[x,c("equipamiento","nombre_apellido")]) # Se extrae la información en los índices resultado de la búsqueda no exacta.

cat(toJSON(resultado_busqueda), file = "ResultadosBusqueda.json") #Guarda el archivo convertido a json para visualizar en el sitio http://jsonviewer.stack.hu/
