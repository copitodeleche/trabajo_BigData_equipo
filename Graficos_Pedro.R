remotes::install_github("perezp44/pjpv.curso.R.2022")

remotes::install_github("perezp44/pjpv.curso.R.2022", force = TRUE)

#Paquetes preliminares
library(mapSpain)
library(pxR)
library(tidyverse)
library(data.table)
library(rlist)
library(fs)
library(curl)
library(rio)
library(sf)

# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
#Creamos una carpeta para guardar los datos

dir_create("datos")

#Descargamos los datos en formato PX desde el Ine
#Índice de Precios de Vivienda por CCAA: general, vivienda nueva y de segunda mano
#Como la URL es: "https://www.ine.es/jaxiT3/files/t/es/px/25171.px?nocab=1"

#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "data.csv", sep = ",")
#Lo convertimos a CSV para simplificar y creamos un dataframe

write.csv(aa, file = "./datos/data.csv")


df <- read.csv("data.csv")


#Ahora nos ponemos a curra un poquito:
library(ggthemes)
library(plotly)
library(ggExtra)



df1 <- df %>% dplyr::rename(ámbito = Comunidades.y.Ciudades.Autónomas, tipo = General..vivienda.nueva.y.de.segunda.mano, Valor = value) %>% filter (tipo == "General", Índices.y.tasas == "Índice", ámbito == "Nacional")



p1 <- ggplot(df1, aes(x = Periodo, y = Valor)) + labs(title = "Índice precios de la vivienda a nivel nacional", subtitle = "Base 100 = 2015", y = "Valor", x = "Periodo",) + geom_point() + theme_economist() + rotateTextX()

p1

plotly::ggplotly(p1)

df1.1  <- df %>% rename(Territorio = Comunidades.y.Ciudades.Autónomas, tipo = General..vivienda.nueva.y.de.segunda.mano, Valor = value) 

df1.2 <- df1.1 %>% mutate(Territorio = case_when(
  Territorio == "01 Andalucía" ~ "Andalucía",
  Territorio == "02 Aragón" ~ "Aragón",
  Territorio == "03 Asturias, Principado de" ~ "Asturias, Principado de",
  Territorio == "04 Balears, Illes" ~ "Balears, Illes",
  Territorio == "05 Canarias" ~ "Canarias",
  Territorio == "06 Cantabria" ~ "Cantabria",
  Territorio == "07 Castilla y León" ~ "Castilla y León",
  Territorio == "08 Castilla - La Mancha" ~ "Castilla-La Mancha",
  Territorio == "09 Cataluña" ~ "Cataluña",
  Territorio == "10 Comunitat Valenciana" ~ "Comunitat Valenciana",
  Territorio == "11 Extremadura" ~ "Extremadura",
  Territorio == "12 Galicia" ~ "Galicia",
  Territorio == "13 Madrid, Comunidad de" ~ "Madrid, Comunidad de",
  Territorio == "14 Murcia, Región de" ~ "Murcia, Región de",
  Territorio == "15 Navarra, Comunidad Foral de" ~ "Navarra, Comunidad Foral de",
  Territorio == "16 País Vasco" ~ "País Vasco",
  Territorio == "17 Rioja, La" ~ "Rioja, La",
  TRUE ~ Territorio))

df3 <- df1.2 %>% filter (tipo == "General", Índices.y.tasas == "Índice", !(Territorio %in% c("Nacional", "18 Ceuta", "19 Melilla")))

p3 <- ggplot(data=df3) +
  geom_line(aes(x = Periodo, y = Valor, color = Territorio, group = Territorio)) +
  theme_minimal() + labs(title = "Evolución nivel de precios CCAA", subtitle = "(100 = 2015)") + theme_economist() + rotateTextX() 
p3

plotly::ggplotly(p3) 


#-----------------------------------------------------GRÁFICO MAPA
#Cargamos librerías
library(tidyverse)
library(rio)
library(mapSpain)
library(pxR)
library(dplyr)
library(ggthemes)
library(ggExtra) 
#-
df <- read.csv("./datos_pulidos/indice_precio_vivienda.csv")

df1 <- df %>% dplyr::rename(ámbito = Comunidades.y.Ciudades.Autónomas, tipo = General..vivienda.nueva.y.de.segunda.mano, Valor = value) %>% filter (tipo == "General", Índices.y.tasas == "Índice", ámbito == "Nacional")

#GRÁFICO INDICE DE PRECIOS DE LA VIVIENDA A NIVEL NACIONAL
#p1 <- ggplot(df1, aes(x = Periodo, y = Valor)) + labs(title = "Índice precios de la vivienda a nivel nacional", subtitle = "Base 100 = 2015", y = "Valor", x = "Periodo",) + geom_point() + theme_economist() + rotateTextX()

#p1

#plotly::ggplotly(p1)

#-

df1.1  <- df %>% 
  dplyr::rename(Territorio = Comunidades.y.Ciudades.Autónomas, tipo = General..vivienda.nueva.y.de.segunda.mano, Valor = value) 

df1.2 <- df1.1 %>% mutate(Territorio = case_when(
  Territorio == "01 Andalucía" ~ "Andalucía",
  Territorio == "02 Aragón" ~ "Aragón",
  Territorio == "03 Asturias, Principado de" ~ "Asturias, Principado de",
  Territorio == "04 Balears, Illes" ~ "Balears, Illes",
  Territorio == "05 Canarias" ~ "Canarias",
  Territorio == "06 Cantabria" ~ "Cantabria",
  Territorio == "07 Castilla y León" ~ "Castilla y León",
  Territorio == "08 Castilla - La Mancha" ~ "Castilla-La Mancha",
  Territorio == "09 Cataluña" ~ "Cataluña",
  Territorio == "10 Comunitat Valenciana" ~ "Comunitat Valenciana",
  Territorio == "11 Extremadura" ~ "Extremadura",
  Territorio == "12 Galicia" ~ "Galicia",
  Territorio == "13 Madrid, Comunidad de" ~ "Madrid, Comunidad de",
  Territorio == "14 Murcia, Región de" ~ "Murcia, Región de",
  Territorio == "15 Navarra, Comunidad Foral de" ~ "Navarra, Comunidad Foral de",
  Territorio == "16 País Vasco" ~ "País Vasco",
  Territorio == "17 Rioja, La" ~ "Rioja, La",
  TRUE ~ Territorio))

df3 <- df1.2 %>% filter (tipo == "General", Índices.y.tasas == "Índice", !(Territorio %in% c("Nacional", "18 Ceuta", "19 Melilla")))

df4 <- df3 %>% filter(Periodo == "2023T3")

prov <- pjpv.curso.R.2022::LAU2_prov_2020_canarias
plot(prov, max.plot = 1)

library(dplyr)
ccaa <- prov %>%
  select(ine_ccaa.n, geometry, X1, Y1)

ccaa <- prov %>% 
  group_by(ine_ccaa, ine_ccaa.n, ine_ccaa.n.pjp) %>% 
  summarise(.groups = "drop") 

ccaa <- prov %>% 
  group_by(ine_ccaa, ine_ccaa.n, ine_ccaa.n.pjp) %>% 
  summarise(across(everything(), ~first(.), .names = "first_{.col}")) %>% 
  ungroup()

ccaa <- prov %>% 
  group_by(ine_ccaa, ine_ccaa.n, ine_ccaa.n.pjp) %>% 
  slice(1)

ccaa <- prov %>% 
  group_by(ine_ccaa.n) |> 
  count(ine_ccaa.n)

ccaa <- ccaa %>% 
  dplyr::rename(Territorio = ine_ccaa.n)

df5 <- full_join(ccaa, df4, by = "Territorio") %>% filter( !(Territorio %in% c("Ceuta", "Melilla")))

p4 <- ggplot(df5,  aes(fill = as.factor(Valor))) + 
  geom_sf(aes(fill = as.factor(Valor))) + geom_sf_label(aes(label = Valor), size = 3, color = "black") +
  scale_fill_viridis_d(guide = "none") +
  labs(title = "Nivel de precios de la vivienda CCAA (100 = 2015)") + theme_void()

p4

#------------------------------------------------------------ GRÁFICO INTERACTIVO FIJO
library(plotly)
renta_edad <- read.csv("./datos_pulidos/renta_por_edad.csv")
dff  <- renta_edad %>% rename(tipo = Renta.anual.neta.media.por.persona.y.por.unidad.de.consumo) %>% filter(tipo == "Renta neta media por persona", Sexo == "Ambos sexos", !(Edad %in% c("De 18 a 64 años", "Menos de 18 años", "Total")))
p6 <- ggplot(data=dff) +
  geom_line(aes(x = Periodo, y = renta, color = Edad, group = Edad)) +labs (title = "Evolución de la renta neta media por edad", subtitle = "(En euros corrientes)", y = "Renta", x = "Año") + theme_ipsum()

plotly::ggplotly(p6)
#------------------------------------------------------------ GRÁFICO EN MOVIMIENTO
#Cargamos datos
library(rio)
library(tidyverse)
library(gganimate)
library(ggthemes)
library(hrbrthemes)

#-
renta_edad <- read.csv("./datos_pulidos/renta_por_edad.csv")

dff  <- renta_edad %>% rename(tipo = Renta.anual.neta.media.por.persona.y.por.unidad.de.consumo) %>% filter(tipo == "Renta neta media por persona", Sexo == "Ambos sexos", !(Edad %in% c("De 18 a 64 años", "Menos de 18 años", "Total")))


p <- ggplot(dff, aes(x = Periodo, y = renta, color = Edad)) +
  geom_line(size = 1) +
  labs(title = "Evolución de la renta neta media por edad", subtitle = "(Euros corrientes)",
       x = "Año", y = "Renta neta") +
  theme_ipsum() + 
  theme(legend.position = "bottom",
        panel.grid.major = element_line(colour = "gray87", linetype = "dashed"),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "serif", size = 9, colour = "gray17")) + scale_color_manual(values = c("green4", "black", "blue", "red", "orange")) +
  geom_text(aes(label = Edad), nudge_x = 0.5, nudge_y = 0.5, hjust = 0)

p_animated <- p + transition_reveal(Periodo) +
  enter_fade() + 
  exit_fade()

animate(p_animated, nframes = 200, fps = 20)





