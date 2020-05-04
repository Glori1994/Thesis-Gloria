####-----Graficas cobertura de Playa Mero histórica (español e inglés)------


#Voy a realizar la gráfica de abundancia en R para que todas las gráficas tengan la misma estética.

#Llamar los paquetes de interés
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(showtext)

#El primer paso es abrir la hoja de Excel de interés

cobertura_english<-read_xlsx("Cobertura 1996 2017.xlsx", sheet= 5)

#La gráfica en inglés

#Debo transformar los datos a como pide ggplot2, formato vertical

hc_english<-gather(cobertura_english, `1995`, `1996`, `2017`, key= "Year", value= "Cover(%)")

hc_english1<-spread(hc_english, Substrate, "Cover(%)" )

hc_english2<-gather(hc_english1, `Live coral`, `Abiotic substrate`, `Octocorals`, 
                    `Sand`, `Other invertebrates`, `Sand`, `Algae`, key= "Substrate", 
                    value = "Cover")

#Revisamos la naturaleza de los datos
str(hc_english2)

#Convertimos en factor con niveles a Tiempo
cols = c(1:2) #necesito hacer factor el tiempo y el substrato. El substrato porque quiero ordenarlo en el orden que yo quiero y no alfabéticamente.
hc_english2[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

#Revisamos la naturaleza de los datos
str(hc_english2)

class(hc_english2$Substrate)
levels(hc_english2$Substrate)

#vector para ordenar los datos en x como yo quiero
level_order<- factor( hc_english2$Substrate, levels = c('Live coral', 
                                                        'Abiotic substrate',
                                                        'Octocorals',
                                                        'Other invertebrates',
                                                        'Sand',
                                                        'Algae'))
# Hago la grafica de barras
hc_graph<- ggplot(hc_english2, aes(fill= Year, y= Cover, x= level_order)) +
           geom_bar( position= "dodge", stat="identity") + #Esta línea indica que son barras agrupadas por año
           theme(text = element_text(size = 14, family = "Roboto"))+
           scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f"))+
           theme(strip.text.x = element_text(size = 10, face= "bold", family = "Roboto"))
hc_graph<- hc_graph+ labs( x= "Substrate", y= "Cover(%)")
hc_graph<- hc_graph +theme(axis.text.x =  element_text(size = 11))

print(hc_graph)

ggsave("Historical cover english.png", units = c("cm"), width = 28, height = 10, dpi= 300)



####---------Español-----------

#El primer paso es abrir la hoja de Excel de interés

cobertura_espanol<-read_xlsx("Cobertura 1996 2017.xlsx", sheet= 6)


#Debo transformar los datos a como pide ggplot2, formato vertical

hc_espanol<-gather(cobertura_espanol, `1995`, `1996`, `2017`, key= "Año", value= "Cobertura(%)")

hc_espanol1<-spread(hc_espanol, Sustrato, "Cobertura(%)" )

hc_espanol2<-gather(hc_espanol1, `Coral vivo`, `Sustrato abiótico`, `Octocorales`, 
                    `Arena`, `Otros invertebrados`, `Algas`, key= "Sustrato", 
                    value = "Cobertura")

#Revisamos la naturaleza de los datos
str(hc_espanol2)

#Convertimos en factor con niveles a Tiempo
cols = c(1:2) #necesito hacer factor el tiempo y el substrato. El substrato porque quiero ordenarlo en el orden que yo quiero y no alfabéticamente.
hc_espanol2[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

#Revisamos la naturaleza de los datos
str(hc_espanol2)

class(hc_espanol2$Sustrato)
levels(hc_espanol2$Sustrato)

#vector para ordenar los datos en x como yo quiero
level_order_esp<- factor( hc_espanol2$Sustrato, levels = c('Coral vivo', 
                                                        'Sustrato abi�tico',
                                                        'Octocorales',
                                                        'Otros invertebrados',
                                                        'Arena',
                                                        'Algas'))
# Hago la grafica de barras
hc_esp_graph<- ggplot(hc_espanol2, aes(fill= A�o, y= Cobertura, x= level_order_esp)) +
  geom_bar( position= "dodge", stat="identity") + #Esta línea indica que son barras agrupadas por año
  theme(text = element_text(size = 14, family = "Roboto"))+
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f"))+
  theme(strip.text.x = element_text(size = 10, face= "bold", family = "Roboto"))
hc_esp_graph<- hc_esp_graph+ labs( x= "Sustrato", y= "Cobertura(%)")
hc_esp_graph<- hc_esp_graph +theme(axis.text.x =  element_text(size = 11))

print(hc_esp_graph)

ggsave("Historical cover espanol.png", units = c("cm"), width = 28, height = 10, dpi= 300)



