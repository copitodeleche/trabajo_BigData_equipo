#Paquetes preliminares

install.packages("downloader")
install.packages("readxl")
install.packages("pxR")
library(pxR)
library(tidyverse)
library(data.table)
library(rlist)
library(fs)
library(curl)
library(rio)
library(readxl)
library(downloader)
library(writexl)


##Saúl##

#-------------------Creamos una carpeta para guardar los datos------------------

dir_create("./docs/datos")

#Descargamos los datos y los exportamos sin pulir: 

#---------------------Índice de Precios de Vivienda-----------------------------

url <- "https://www.ine.es/jaxiT3/files/t/es/px/25171.px?nocab=1"
aa <- read.px(url)

#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "data.csv", sep = ",")
IPV <- read.csv("data.csv")
rm(list = ls()[!ls() %in% c("IPV")])

#Los guardamos sin arreglar
write.csv(IPV,  file = "./docs/datos/ipv.csv")



#-------------------------Compra-venta de vivienda------------------------------

ruta_xls <- "https://apps.fomento.gob.es/BoletinOnline2/sedal/34010110.XLS"

download.file(ruta_xls, "datos.xls", mode = "wb")

nombres_hojas <- c("2004,2005,2006,2007,2008", "2009,2010,2011,2012,2013", 
                   "2014,2015,2016,2017,2018", "2019,2020,2021,2022,2023")
# Leer todas las hojas en una lista
for (i in seq_along(nombres_hojas)) {
  hoja <- nombres_hojas[i]
  df <- readxl::read_excel("datos.xls", sheet = i) 
  write.csv(df, file = paste0(hoja, ".csv"), row.names = FALSE)}

#Juntamos las cuatro sheets en una y después arreglaremos el desastre
compraventa_2004 <- read.csv("2004,2005,2006,2007,2008.csv")
compraventa_2009 <- read.csv("2009,2010,2011,2012,2013.csv")
compraventa_2014 <- read.csv("2014,2015,2016,2017,2018.csv")
compraventa_2019 <- read.csv("2019,2020,2021,2022,2023.csv")

cv <- bind_cols(compraventa_2009,compraventa_2014)
cv <- bind_cols(cv,compraventa_2019)
compraventa_2004 <- compraventa_2004[1:nrow(cv), ]
cv <- bind_cols(cv,compraventa_2004)


cv <- cv %>%
  filter(row_number() %in% 6:72)
cv <- cv %>%
  filter(!row_number() %in% 2)

#Los guardamos sin arreglar
write.csv(cv,  file = "./docs/datos/cv.csv")
rm(list = ls())


#--------------------------Renta por edad y sexo--------------------------------

url3 <- "https://www.ine.es/jaxiT3/files/t/es/px/9942.px?nocab=1"
aa <- read.px(url3)

#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "data.csv", sep = ",")
renta_edad <- read.csv("data.csv")

#Los guardamos sin arreglar
write.csv(renta_edad, file = "./docs/datos/renta_edad.csv")

rm(list = ls())


#---------------------------Renta por nacionalidad------------------------------

url4 <- "https://www.ine.es/jaxiT3/files/t/es/px/9945.px?nocab=1"
aa <- read.px(url4)

#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "data.csv", sep = ",")
renta_nacionalidad <- read.csv("data.csv")

#Los guardamos sin arreglar
write.csv(renta_nacionalidad, file = "./docs/datos/renta_nacionalidad.csv")

rm(list = ls())


#---------------------Hogares por regimen de tenencia edad----------------------

url4 <- "https://www.ine.es/jaxiT3/files/t/es/px/9994.px?nocab=1"
aa <- read.px(url4)

#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "data.csv", sep = ",")
vivienda_edad <- read.csv("data.csv")

#Los guardamos sin arreglar
write.csv(vivienda_edad, file = "./docs/datos/vivienda_datos.csv")

rm(list = ls())

#Borramos archivos como las sheets separadas y otros datos
file.remove("2004,2005,2006,2007,2008.csv", "2009,2010,2011,2012,2013.csv",
            "2014,2015,2016,2017,2018.csv", "2019,2020,2021,2022,2023.csv", 
            "compraventa.csv", "data.csv", "datos.xls", "./docs/datos/25171.px",
            "./docs/datos/34010110.XLS")


#----------------------Procedemos a limpiar los datos---------------------------

#-------------Creamos una carpeta para guardar los datos pulidos----------------

dir_create("./datos_pulidos")
library(zoo)

###AHORA LIMPIAMOS LOS DATOS PARA TRABAJAR MEJOR CON ELLOS###

#-------------------Indice de precios de la Vivienda----------------------------

cv <- import(file = "./docs/datos/cv.csv")

#Elimino las columnas duplicadas indicando el nombre de la provincia 
#para dejar solo una columna de provincias

cv <- cv[-1,]
colnames(cv) <- NULL
columnas_duplicadas <- duplicated(t(cv))
print(columnas_duplicadas)

cv <- cv[, !columnas_duplicadas]
colnames(cv) <- paste0("col_", 1:ncol(cv))
#Elimino otra columnaa duplicadaa que el argumento no encuenrtra 

cv <- cv[, -62]

#Doy nombre a la posición [1,2] de provincias para después poder trabajar mejor
#Ya que convertiremos la primera fila en colnames

cv[1,2] <- "Provincia"
colnames(cv)

#Seleccionamos la primera fila del df y lo convertimos en un vector. Después, 
#Hacemos que los NA se sustituyan por el texto de su izquierda (Remplazamos NA`s
#por el año al que corresponde esa columnas)

df <- cv %>%
  slice(1) %>%
  unlist()

df <- na.locf(df)

#Ahora convertimos el vector en la fila 1 que después convrtiremos en colnames
#para trabajar mejor.

cv[1,] <- df 

#Juntamos la fila 1 y 2 a partir de la tercer columna separando el texto con "_" 
#y le rellenamos las dos columnas restantes con los nombres que nos vengan mejor 
#para trabajar
aa <- cv[1,]
aa <- c("","Provincia",paste(cv[1,3:ncol(cv)],cv[2,3:ncol(cv)], sep = "_"))

#Convertimos el vector aa en colnames

colnames(cv) = aa

#Quitamos la primera y la segunda fila, es información que ya indica colnames

cv <- cv[-(1:2), -(1)]
colnames(cv)

#Hacemos que todas las columnas que indican año_trimestre pivoten y dejen los
#valores que contenían las transacciones por un lado y el año_trimestre por otro

Transacciones_por_provincia <- cv %>% pivot_longer(cols = starts_with("Año"), 
                                                   names_to = "Año_Trimestre", 
                                                   names_prefix = "Año", 
                                                   values_to = "Transacciones",
                                                   values_drop_na = TRUE)

#Guardamos en la carpeta(en formato .csv) de datos pulidos y seguimos

write.csv(Transacciones_por_provincia, 
          file = "datos_pulidos/Transacciones_por_povincia.csv")


#----------------Índice de precios de la vivienda-------------------------------


ipv <- import(file = "./docs/datos/ipv.csv")

#Estos datos están bien, quitamos las dos columnas que no aportan y los guardamos
ipv <- ipv[,-(1:2)]

write.csv(ipv, file = "./datos_pulidos/indice_precio_vivienda.csv")

#rm(list = ls())


#----------------Renta por edad-------------------------------------------------

re <- import(file  = "./docs/datos/renta_edad.csv")

#Renombramos las columnas, seleccionamos lo que nos interesa y borramos el restante
#Exportamos
re <- re %>%
  rename(renta = "value") %>%
  select(!c("V1", "X"))

write.csv(re, file = "./datos_pulidos/renta_por_edad.csv")




#----------------Régimen tenencia de vivienda por edad--------------------------

vd <- import(file  = "./docs/datos/vivienda_datos.csv")

#Renombramos las columnas, seleccionamos lo que nos interesa y borramos el restante
#Exportamos
vd<- vd %>%
  rename(Porcentaje = "value") %>%
  select(!c("V1", "X"))

write.csv(vd, file = "./datos_pulidos/tenencia_de_vivienda.csv")
rm(list = ls())




##JOEL##

#--------------------IMPORTAR DATOS CE / EUROSTAT-------------------------------
options(scipen = 999) #- para quitar la notación científica

install.packages("eurostat")
install.packages("DT")
library(eurostat) 
library(DT) 
library(tidyverse)

info <- search_eurostat("GDP", type = "all")
my_table <-"sdg_08_10"

#Comprobamos que es el PIB pc
label_eurostat_tables(my_table)

df_original <- get_eurostat(my_table, time_format = 'raw', keepFlags = TRUE)
df_names <- names(df_original)
df_original <- label_eurostat(df_original, code = df_names, fix_duplicated = TRUE)

#Creamos otro df para trabajor con él 

df <- df_original

#Ver que hay en el df
library(pjpv.curso.R.2022)
df_aa <- pjpv.curso.R.2022::pjp_dicc(df)
df_bb <- pjpv.curso.R.2022::pjp_valores_unicos(df, nn = 400)

#Vamos a hacer un poco de limpieza
obj_buenos <- c("df", "df_original", "df_bb")
rm(list = setdiff(ls(), obj_buenos))

#Vamos a seguir arreglando el df

df <- df |> 
  rename(year = time_code) |> 
  rename(PIB_pc = values) |> 
  rename(country = geo)

#Chain linked volumes (2010), euro per capita

#Solo queremos el PIB_pc
df <- df |> 
  filter(unit == "Chain linked volumes (2010), euro per capita")

#Eliminamos todo lo que nos molesta a la vista 
df <- df |> 
  select(-c(unit_code, values_code, unit, time, na_item_code, na_item, flags))

df <- df |>  mutate(year =  as.numeric(year))

df <- df |>  mutate(iso_2_code =  eurostat::harmonize_country_code(geo_code))

df <- df |> 
  select(-c(geo_code, flags_code))

library(fs)
dir_create("datos_pulidos") 

#A continuación vamos a exportar los datos para luego importarlas más fácilmente #library(rio)

export(df, "./datos_pulidos/PIB_pc.csv", type = "csv")


#---------------DATOS SOBRE EL SALARIO POR NIVEL DE FORMACIÓN-------------------

library(rio)
library(tidyverse)
url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/13931.csv?nocab=1"
df_ine <- import(url)
df_ine_01 <- df_ine |> 
  filter(Decil %in% ("Total decil")) |> 
  select("Periodo", "Tipo de jornada", "NIVELES DE FORMACION", "Total") |> 
  rename(year = Periodo) |> 
  rename(jornada = 'Tipo de jornada') |> 
  rename(formacion = 'NIVELES DE FORMACION') |>
  rename(salario = Total)

#VAMOS A CONVERTIR LA VARIABLE salario A NUMERIC

#R y el INE me han jodiedo la vida para pasar esto a numeric (antes no funcionaba)
df_ine_01 <- df_ine_01 |> 
  mutate(salario = as.numeric(gsub(",", ".", gsub("\\.", "", salario))))

export(df_ine_01,"./datos_pulidos/salario_formacion.csv", type = "csv")


#-----------------------DATOS SOBRE EL SALARIO POR EDADES-----------------------

url_1 <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/13928.csv?nocab=1"
df_ine_x1 <- import(url_1)

df_ine_02 <- df_ine_x1 |> 
  filter(Decil %in% ("Total decil")) |>
  select("Periodo", "Tipo de jornada", "Grupo de edad", "Total") |> 
  rename(year = Periodo) |> 
  rename(jornada = 'Tipo de jornada') |> 
  rename(grupo_edad = 'Grupo de edad') |>
  rename(salario = Total)

#PASAMOS A NUMERIC
df_ine_02 <- df_ine_02 |> 
  mutate(salario = as.numeric(gsub(",", ".", gsub("\\.", "", salario))))

export(df_ine_02,"./datos_pulidos/salario_grupo_edad.csv", type = "csv")


#------------------------------DATOS SOBRE EL IPC-------------------------------

url_2 <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/50908.csv?nocab=1"
df_ipc <- import(url_2)

df_ipc_01 <- df_ipc |> 
  filter(`Grupos ECOICOP` %in% c("Índice general", "04 Vivienda, agua, electricidad, gas y otros combustibles")) |> 
  select("Periodo", "Grupos ECOICOP", "Tipo de dato", "Total") |> 
  rename(year = Periodo) |>
  rename(grupo = 'Grupos ECOICOP') |> 
  rename(tipo_dato = `Tipo de dato` ) 

#PASAMOS A NUMERIC
df_ipc_01 <- df_ipc_01 |> 
  mutate(Total = as.numeric(gsub(",", ".", gsub("\\.", "", Total))))



#PARA SEPARAR EN LA VARIABLE YEAR LOS DATOS POR AÑOS Y MESES
#df_sep <- df_ipc_01 |> 
#  separate(col = year, into = c("year", "month"), sep = "M")

#df_sep <- df_sep |> 
#  mutate(month = paste0("M", month))

export(df_ipc_01, "./datos_pulidos/IPC_mas_vivienda.csv", type = "csv")


#Saúl
#-------------------------Por tipo de jornada-----------------------------------

df_temporal <- import("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/10894.csv?nocab=1")
df_activos <- import("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4049.csv?nocab=1")
df_parados <- import("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4084.csv?nocab=1")
df_contrato <- import("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/4854.csv?nocab=1")


df_activos <- df_activos %>%
  filter(Sexo %in% "Ambos sexos") %>%
  filter(Unidad %in% "Valor absoluto")
  

df_parados <- df_parados %>%
  filter(Sexo %in% "Ambos sexos") %>%
  filter(Unidad %in% "Valor absoluto")


df_contrato <- df_contrato %>%
  filter(Sexo %in% "Ambos sexos")
  

export(df_activos, file = "./datos_pulidos/activos.csv", format="csv")
export(df_parados, file = "./datos_pulidos/parados.csv", format="csv")
write.csv(df_temporal, file= "./datos_pulidos/temporalidad_edad.csv")
write.csv(df_contrato, file = "./datos_pulidos/contrato_edad.csv")


