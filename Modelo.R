#Paquetes

library(reshape2)
library(tidyverse)
library(car) # regression analysis
library(plotly)
library(ggplot2)
library(reshape2)
library(gganimate)
library(xts)
library(dygraphs)
library(gganimate)
library(gifski)
library(ggthemes)

rm(list = ls())
#Df`s
df_w_edad <- read.csv("C:/Users/saulg/OneDrive/Documentos/GitHub/trabajo_BigData_team/datos_pulidos/salario_grupo_edad.csv")


df_pivot <- pivot_wider(df_w_edad, names_from = "jornada", values_from = "salario")

de16a34 <- df_pivot %>%
  filter(grupo_edad %in% c("De 25 a 34 años", "De 16 a 24 años"))

columna <- rep("De 16 a 34 años", times = 17)
head(de16a34)
de16a34 <- de16a34 %>%
  group_by(year) %>%
  summarise(total = sum(Total)/2,
            tiempo_completo = sum(`Jornada a tiempo completo`)/2,
            tiempo_parcial = sum(`Jornada a tiempo parcial`)/2) %>%
  mutate(grupo_edad = columna)
head(de16a34)
mas35 <- df_pivot %>%
  filter(!grupo_edad %in% c("De 25 a 34 años", "De 16 a 24 años"))

columna2 <- rep("Mayores de 35", times = 17)
mas35 <- mas35 %>% group_by(year) %>%
  summarise(total = sum(Total)/4,
            tiempo_completo = sum(`Jornada a tiempo completo`)/4,
            tiempo_parcial = sum(`Jornada a tiempo parcial`)/4) %>%
  mutate(grupo_edad = columna2)
  
salario <- rbind(mas35, de16a34)

rm(list = ls()[!ls() %in% c("salario")])

#-------------------------------------------------------------------------------

df_contrato <- read.csv("C:/Users/saulg/OneDrive/Documentos/GitHub/trabajo_BigData_team/datos_pulidos/contrato_edad.csv")


#-------------------------------------------------------------------------------
head(df_contrato)

df_contrato <- df_contrato %>% 
  mutate(Total = gsub("\\.", "", Total))
df_contrato <- df_contrato %>%
  mutate(Total = as.numeric(gsub(",", ".", Total)))

head(df_contrato)

df_contrato <- df_contrato %>%
  filter(Tipo.de.jornada %in% c("Jornada a tiempo completo", "Jornada a tiempo parcial")) %>%
  filter(Tipo.de.dato %in% "Porcentaje")

df_contrato_pivot <- df_contrato %>%
  pivot_wider(names_from = "Tipo.de.jornada", values_from = "Total")

df_contrato_pivot <- df_contrato_pivot[,-(1:2)]

de16a34 <- df_contrato_pivot %>%
  filter(Edad %in% c("De 25 a 29 años", "De 16 a 19 años", "De 20 a 24 años", "De 30 a 34 años"))

columna <- rep("De 16 a 34 años", times = 17)
de16a34 <- de16a34 %>%
  mutate_all(~replace(., is.na(.), 0))


##

de16a34 <- de16a34 %>% 
  group_by(Periodo) %>%
  summarise(tiempo_completo = sum(`Jornada a tiempo completo`)/4,
            tiempo_parcial = sum(`Jornada a tiempo parcial`)/4) %>%
  mutate(Edad = columna)

mas35 <- df_contrato_pivot %>%
  filter(Edad %in% c("70 y más años", "De 35 a 39 años", "De 40 a 44 años", "De 45 a 49 años", "De 50 a 54 años", "De 55 a 59 años", "De 60 a 64 años", "De 65 a 69 años"))

columna <- rep("Mayores de 35", times = 17)
mas35 <- mas35 %>%
  mutate_all(~replace(., is.na(.), 0))

mas35 <- mas35 %>% 
  group_by(Periodo) %>%
  summarise(tiempo_completo = sum(`Jornada a tiempo completo`)/8,
            tiempo_parcial = sum(`Jornada a tiempo parcial`)/8) %>%
  mutate(Edad = columna)
  
contrato <- rbind(mas35, de16a34)

rm(list = ls()[!ls() %in% c("salario","contrato")])

##------------------------------------------------------------------------------

df_activos <- read.csv("C:/Users/saulg/OneDrive/Documentos/GitHub/trabajo_BigData_team/datos_pulidos/activos.csv")

##------------------------------------------------------------------------------
head(df_activos)

df_activos <- df_activos %>% 
  mutate(Total = gsub("\\.", "", Total))
df_activos <- df_activos %>%
  mutate(Total = as.numeric(gsub(",", ".", Total)))

head(df_activos)

menos35 <- df_activos %>%
  filter(Edad %in% c("De 25 a 29 años", "De 16 a 19 años", "De 20 a 24 años", "De 30 a 34 años"))
head(menos35)  


menos35 <- menos35 %>%
  mutate(Año = substr(Periodo, 1, 4)) %>%
  group_by(Edad, Sexo, Unidad, Año) %>%
  summarise(Total = sum(Total)/4)

menos35 <- menos35[,-(2:3)]
columna <- rep("De 16 a 34 años", times = 22)

menos35 <- menos35 %>%
  group_by(Año) %>%
  summarise(total = sum(Total)) %>%
  mutate(grupo_edad = columna) %>%
  filter(!Año %in% c("2023")) %>%
  rename(num_activos = "total")



mas35 <- df_activos %>%
  filter(Edad %in% c("70 y más años", "De 35 a 39 años", "De 40 a 44 años", "De 45 a 49 años", "De 50 a 54 años", "De 55 a 59 años", "De 60 a 64 años", "De 65 a 69 años"))

mas35 <- mas35 %>%
  mutate(Año = substr(Periodo, 1, 4)) %>%
  group_by(Edad, Sexo, Unidad, Año) %>%
  summarise(Total = sum(Total)/4)

mas35 <- mas35[,-(2:3)]

columna <- rep("Mayores de 35", times = 22)
mas35 <- mas35 %>%
  group_by(Año) %>%
  summarise(total = sum(Total)) %>%
  mutate(grupo_edad = columna) %>%
  filter(!Año %in% c("2023")) %>%
  rename(num_activos = "total")

activos <- rbind(mas35, menos35)

rm(list = ls()[!ls() %in% c("salario","contrato", "activos")])

##------------------------------------------------------------------------------

df_parados <- read.csv("C:/Users/saulg/OneDrive/Documentos/GitHub/trabajo_BigData_team/datos_pulidos/parados.csv")

##------------------------------------------------------------------------------

head(df_parados)

df_parados <- df_parados %>% 
  mutate(Total = gsub("\\.", "", Total))
df_parados <- df_parados %>%
  mutate(Total = as.numeric(gsub(",", ".", Total)))

head(df_parados)

menos35 <- df_parados %>%
  filter(Edad %in% c("De 25 a 29 años", "De 16 a 19 años", "De 20 a 24 años", "De 30 a 34 años"))
head(menos35)  


menos35 <- menos35 %>%
  mutate(Año = substr(Periodo, 1, 4)) %>%
  group_by(Edad, Sexo, Unidad, Año) %>%
  summarise(Total = sum(Total)/4)

menos35 <- menos35[,-(2:3)]
columna <- rep("De 16 a 34 años", times = 22)

menos35 <- menos35 %>%
  group_by(Año) %>%
  summarise(total = sum(Total)) %>%
  mutate(grupo_edad = columna) %>%
  filter(!Año %in% c("2023")) %>%
  rename(num_parados = "total")

mas35 <- df_parados %>%
  filter(Edad %in% c("70 y más años", "De 35 a 39 años", "De 40 a 44 años", "De 45 a 49 años", "De 50 a 54 años", "De 55 a 59 años", "De 60 a 64 años", "De 65 a 69 años"))

mas35 <- mas35 %>%
  mutate(Año = substr(Periodo, 1, 4)) %>%
  group_by(Edad, Sexo, Unidad, Año) %>%
  summarise(Total = sum(Total)/4) %>%
  mutate_all(~replace(., is.na(.), 0))

mas35 <- mas35[,-(2:3)]

columna <- rep("Mayores de 35", times = 22)
mas35 <- mas35 %>%
  group_by(Año) %>%
  summarise(total = sum(Total)) %>%
  mutate(grupo_edad = columna) %>%
  filter(!Año %in% c("2023")) %>%
  rename(num_parados = "total")

parados <- rbind(mas35, menos35)

rm(list = ls()[!ls() %in% c("salario","contrato", "activos", "parados")])

act_parad <- merge(parados, activos, by = c("Año", "grupo_edad"))

act_parad <- act_parad %>%
  mutate(num_ocupados = num_activos - num_parados)

rm(list = ls()[!ls() %in% c("salario","contrato", "act_parad")])

####----------------------------------------------------------------------------

df_viv <- read.csv("C:/Users/saulg/OneDrive/Documentos/GitHub/trabajo_BigData_team/datos_pulidos/tenencia_de_vivienda.csv")

####----------------------------------------------------------------------------

df_viv <- df_viv %>%
  filter(Sexo.de.la.persona.de.referencia %in% "Ambos sexos")


menos30 <- df_viv %>%
  filter(Edad.de.la.persona.de.referencia %in% c("De 16 a 29 años")) 


menos30 <- menos30[,-(1)]
menos30 <- menos30[,-(4)]
mas30 <- df_viv %>%
  filter(Edad.de.la.persona.de.referencia %in% c("65 y más años", "De 30 a 44 años", "De 45 a 64 años"))
 
mas30 <- mas30[,-(1)]  
mas30 <- mas30[,-(4)]   

columna <- rep("Mayores de 30", times = 228)
  

mas30 <- mutate(mas30, Edad.de.la.persona.de.referencia = columna)
mas30 <- mas30 %>%
  group_by(Periodo, Régimen.de.tenencia.de.la.vivienda.principal, Edad.de.la.persona.de.referencia) %>%
  summarise(Porcentaje = sum(Porcentaje)/3)
  
vivienda <- rbind(mas30,menos30)
vivienda <- vivienda %>%
  rename(grupo_edad = Edad.de.la.persona.de.referencia)
vivienda <- vivienda %>%
  pivot_wider(names_from = "Régimen.de.tenencia.de.la.vivienda.principal", values_from = "Porcentaje")
rm(list = ls()[!ls() %in% c("salario","contrato", "act_parad", "vivienda")])

##------------------------------------------------------------------------------
head(act_parad)

porcentaje_parados <- act_parad %>%
  group_by(Año) %>%
  group_by(Año, grupo_edad) %>%
  mutate(Ocupados_edad = (num_ocupados / num_activos) * 100,
         Parados_edad = (num_parados / num_activos) * 100) %>%
  rename(Periodo = Año)

rm(list = ls()[!ls() %in% c("salario","contrato", "porcentaje_parados", "vivienda")])

contrato <- contrato %>%
  rename(jornada_completa = tiempo_completo, 
         jornada_parcial = tiempo_parcial,
         grupo_edad = Edad)
  
salario <- salario %>%
  rename(salario_completa = tiempo_completo, 
         salario_parcial = tiempo_parcial,
         Periodo = year)

##------------------------------------------------------------------------------

ggplot(vivienda, aes(x = Periodo, y = Propiedad, group = grupo_edad, color = grupo_edad)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  geom_point( color="grey", size=2) +
  theme_solarized()

ggplot(porcentaje_parados, aes(x = Periodo, y = Parados_edad, group = grupo_edad, color = grupo_edad)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  geom_point( color="grey", size=2) +
  theme_solarized()

ggplot(contrato, aes(x = Periodo, y = jornada_completa, group = grupo_edad, color = grupo_edad)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  geom_point( color="grey", size=2) +
  theme_solarized()

ggplot(salario, aes(x = Periodo, y = salario_completa, group = grupo_edad, color = grupo_edad)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  geom_point( color="grey", size=2) +
  theme_solarized()

remotes::install_github("perezp44/pjpv.curso.R.2022", force = TRUE)
