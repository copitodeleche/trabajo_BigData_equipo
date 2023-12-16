#Cargamos todos las librerías que vamos a utilizar----
library(tidyverse)
library(rio)
library(ggplot2)
library(ggridges)

#GRAFICOS SALARIO POR GRUPO DE EDAD----

#GRAFICO RIDGELINE POR GRUPO EDAD
df_edad <- import("./datos_pulidos/salario_grupo_edad.csv")

p1<- ggplot(df_edad, aes(x = salario, y = grupo_edad, fill = grupo_edad)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

p1

#GRÁFICO DE BARRAS AGRUPADAS DEL SALARIO POR GRUPO DE EDAD
df_edad_01 <- df_edad |> 
  filter(grupo_edad != "Total")

df_edad_01$year <- factor(df_edad_01$year)

p2 <- ggplot(df_edad_01, aes(fill = grupo_edad, y = salario, x = year))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Salario por grupo de edad")

p2

#GRÁFICO DE BARRAS STACKEADAS DEL SALARIO POR GRUPO DE EDAD
#NO SE APRECIA BIEN
#p3 <- ggplot(df_edad_01, aes(fill=grupo_edad, y=salario, x=year)) + 
#  geom_bar(position="stack", stat="identity")
#p3

#GRÁFICOS SALARIO POR FORMACIÓN----

#GRAFICO RIDGELINE DEL SALARIO POR FORMACIÓN
df_formacion <- import("./datos_pulidos/salario_formacion.csv")

p4<- ggplot(df_formacion, aes(x = salario, y = formacion, fill = formacion)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

p4

#GRÁFICO DE BARRAS AGRUPADAS DEL SALARIO POR FORMACION
df_form_01 <- df_formacion |> 
  filter(formacion != "Total")

df_form_01$year <- factor(df_form_01$year)

p5 <- ggplot(df_form_01, aes(fill = formacion, y = salario, x = year))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(title = "Salario por formación")

p5

#GRÁFICO DE BARRAS STACKEADAS DEL SALARIO POR FORMACION
#NO SE APRECIA BIEN
#p6 <- ggplot(df_form_01, aes(fill=formacion, y=salario, x=year)) + 
#  geom_bar(position="stack", stat="identity")
#p6
#GRÁFICOS INDICE PRECIO VIVIENDA----

#GRÁFICO LOLLIPOP MEDIA IPV VIVIENDA ESPAÑA
df_IPV <- import("./datos_pulidos/indice_precio_vivienda.csv")


df_IPV_01 <- df_IPV |>
  filter(Índices.y.tasas == "Variación anual") |> 
  filter(Comunidades.y.Ciudades.Autónomas == "Nacional") |> 
  filter(General..vivienda.nueva.y.de.segunda.mano == "General")

df_media_anual <- df_IPV_01 |> 
  mutate(year = substr(Periodo, 1, 4)) |> 
  group_by(year) |> 
  summarise(media_salario = mean(value))

ggplot(df_media_anual, aes(x=year, y=media_salario)) +
  geom_segment( aes(x=year, xend= year, y=0, yend=media_salario), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Índice del Precio de la Vivienda")

