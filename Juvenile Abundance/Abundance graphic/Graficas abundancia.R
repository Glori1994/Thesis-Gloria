#Voy a realizar la gráfica de abundancia en R para que todas las gráficas tengan la misma estética.

#Llamar los paquetes de interés
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
#install.packages("magrittr")
library(magrittr)
library(showtext)

########-------ENGLISH


#El primer paso es abrir la hoja de Excel de interés

abundancia<-read_xlsx("Matriz Madre.xlsx", sheet= 3)


#Quiero sumar por tiempo por especie las abundancias
#primero debo cambiar la estructura de la matriz
#Uso la funcion gather
ab_estr<- gather(abundancia, `AGAsp`,`SIDsp`,`PSEsp`,`COLsp`,`PORsp`,`SCOLsp`,`ORBsp`, 
                 key = "Genera", value = "Abundance" )


#Revisamos la naturaleza de los datos
str(ab_estr)


#Convertimos en factor con niveles a Tiempo
cols = c(1:3)
ab_estr[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

str(ab_estr)


#ponemos la columna "Tiempo" en ingles
ab_english<-rename(ab_estr, Time= Tiempo)

#Fuentes

font_add_google("Roboto Slab", "Roboto")
font_add_google("Literata", "Literata")
font_add_google("Lato", "lato")

font_paths()
font_families()

# Hago la grafica estilo "Faceting"
g_b<-ggplot(ab_english, aes(y=Abundance, x=Time, fill= Time)) + 
     geom_bar( stat="identity") + 
     facet_grid(. ~ Genera) + #este comando indica que los paneles este al lado unos de otros
     scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f", "#ddbc96"))+
     theme(text = element_text(size = 35, family = "Roboto", face = "bold"), 
        axis.text= element_text(size= 30, face = "plain"),
        strip.text.x = element_text(size = 30, face= "bold", family = "Roboto"),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.title.align = 0.5)

plot(g_b)   
  
  
g_b<-g_b+labs(x="Time", y="Abundance of juvenile corals", fill= "Time")     

showtext_auto()

print(g_b)

ggsave("Abundance English.png", units = c("cm"), width = 20, height = 10, dpi= 300)




#####-------------ESPAÑOL BUENO
abundancia<-read_xlsx("Matriz Madre.xlsx", sheet= 3)


#Quiero sumar por tiempo por especie las abundancias
#primero debo cambiar la estructura de la matriz
#Uso la funcion gather
ab_esp<- gather(abundancia, `AGAsp`,`SIDsp`,`PSEsp`,`COLsp`,`PORsp`,`SCOLsp`,`ORBsp`, 
                 key = "Genera", value = "Abundance" )


#Revisamos la naturaleza de los datos
str(ab_esp)


#Convertimos en factor con niveles a Tiempo
cols = c(1:3)
ab_esp[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

str(ab_esp)


#ponemos la columna "Tiempo" en ingles
ab_espanol<-rename(ab_esp, Time= Tiempo)

#Fuentes

font_add_google("Roboto Slab", "Roboto")
font_add_google("Literata", "Literata")
font_add_google("Lato", "lato")

font_paths()
font_families()

# Hago la grafica estilo "Faceting"
g_e<-ggplot(ab_espanol, aes(y=Abundance, x=Time, fill= Time)) + 
  geom_bar( stat="identity") + 
  facet_grid(. ~ Genera) + #este comando indica que los paneles este al lado unos de otros
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f", "#ddbc96"))+
  theme(text = element_text(size = 35, family = "Roboto", face = "bold"), 
        axis.text= element_text(size= 30, face = "plain"),
        strip.text.x = element_text(size = 30, face= "bold", family = "Roboto"),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.title.align = 0.5)


g_e<-g_e+labs(x="Tiempo", y="Abundancia de corales juveniles", fill= "Tiempo")

showtext_auto()

print(g_e)

ggsave("Abundancia Espanol.png", units = c("cm"), width = 20, height = 10, dpi= 300)









####-----Pruebas de tamaño

###pruebas tamaño


g<- ggplot(ab_english, aes(y=Abundance, x=Time, fill= Time)) + 
  geom_bar( stat="identity") + 
  theme(text = element_text(size = 15, family = "Roboto", face = "bold"), axis.text= element_text(size= 10, face = "plain"))



b<-g + facet_grid(. ~ Genera) + #este comando indica que los paneles este al lado unos de otros
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f", "#ddbc96"))+
  theme(strip.text.x = element_text(size = 10, face= "bold", family = "Roboto"))


showtext_auto()

print(b)

ggsave("ejemplo abundance_7.png", units = c("cm"), width = 10, height = 5, dpi= 300)



RStudio.Version()

cite(effects())