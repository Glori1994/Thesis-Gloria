####-----Graficas cobertura de corales histÃ³rica (espaÃ±ol e inglÃ©s)------


#Llamar los paquetes de interÃ©s
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(showtext)

#El primer paso es abrir la hoja de Excel de interÃ©s

coral_cover<-read_xlsx("Cobertura 1996 2017.xlsx", sheet= 7)

#La grÃ¡fica en inglÃ©s

#Debo transformar los datos a como pide ggplot2, formato vertical

cc_english<-gather(coral_cover, `1995`, `1996`, `2017`, key= "Year", value= "Relative Cover(%)")

cc_english1<-spread (cc_english, `Coral genera`, `Relative Cover(%)`)

unique(coral_cover$`Coral genera`) #para buscar los nombres de los gÃ©neros

cc_english2<-gather(cc_english1, `Orbicella sp.`, `Colpophyllia natans`, `Agaricia sp.`, 
                    `Siderastrea sp.`, `Madracis sp.`, `Others`, `Porites sp.`, key= "Coral genera", 
                    value = "Relative Cover(%)")


#Revisamos la naturaleza de los datos
str(cc_english2)

#Convertimos en factor con niveles a Tiempo
cols_cceng = c(1:2) #necesito hacer factor el tiempo y el substrato. El substrato porque quiero ordenarlo en el orden que yo quiero y no alfabÃ©ticamente.
cc_english2[,cols_cceng] %<>% lapply(function(x) as.factor(as.character(x)))

#Revisamos la naturaleza de los datos
str(cc_english2)

class(cc_english2$`Coral genera`)

levels(cc_english2$`Coral genera`)

#vector para ordenar los datos en x como yo quiero/ me gustó más como queda alfabéticamente ordenado, por eso volví texto 
#esta línea, pero de todas maneras la dejo escrita porsia cambio de opinión

#level_order_cceng<- factor( cc_english2$`Coral genera`, levels = c('Orbicella sp.',      
                                                                   #'Agaricia sp.',
                                                                   #'Porites sp.',
                                                                   #'Siderastrea sp.',
                                                                   #'Colpophyllia natans', 
                                                                   #'Madracis sp.', 
                                                                    #'Others'
                        
                                                                   #))
levels<-(cc_english2$`Coral genera`)
print(levels)

# Hago la grafica de barras
ccen_graph<- ggplot(cc_english2, aes(fill= Year, y= `Relative Cover(%)`, x= level_order_cceng)) +
  geom_bar( position= "dodge", stat="identity") + #Esta linea indica que son barras agrupadas por aÃ±o
  theme(text = element_text(size = 14, family = "Roboto"))+
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f"))+
  theme(strip.text.x = element_text(size = 10, face= "bold", family = "Roboto"))
ccen_graph<- ccen_graph+ labs( x= "Coral genera", y= "Relative Cover(%)")
ccen_graph<- ccen_graph +theme(axis.text.x =  element_text(size = 11))

print(ccen_graph)

ggsave("Historical coral cover english.png", units = c("cm"), width = 28, height = 10, dpi= 300)



####---------EspaÃ±ol-----------

#El primer paso es abrir la hoja de Excel de interÃ©s

coral_cobertura<-read_xlsx("Cobertura 1996 2017.xlsx", sheet= 8)

#La grafica en español

#Debo transformar los datos a como pide ggplot2, formato vertical

cc_esp<-gather(coral_cobertura, `1995`, `1996`, `2017`, key= "Año", value= "Cobertura relativa (%)")

cc_esp1<-spread (cc_esp, `Géneros de coral`, `Cobertura relativa (%)`)

unique(coral_cobertura$`Géneros de coral`) #para buscar los nombres de los gÃ©neros

cc_esp2<-gather(cc_esp1, `Orbicella sp.`, `Colpophyllia natans`, `Agaricia sp.`, 
                    `Siderastrea sp.`, `Madracis sp.`, `Otros`, `Porites sp.`, key= "Géneros de coral", 
                    value = "Cobertura relativa (%)")


#Revisamos la naturaleza de los datos
str(cc_esp2)

#Convertimos en factor con niveles a Tiempo
cols_ccesp = c(1:2) #necesito hacer factor el tiempo y el sustrato. El substrato porque quiero ordenarlo en el orden que yo quiero y no alfabÃ©ticamente.
cc_esp2[,cols_ccesp] %<>% lapply(function(x) as.factor(as.character(x)))

#Revisamos la naturaleza de los datos
str(cc_esp2)

class(cc_esp2$`Géneros de coral`)

levels(cc_esp2$`Géneros de coral`)

#vector para ordenar los datos en x como yo quiero/ me gustó más como queda alfabéticamente ordenado, por eso volví texto 
#esta línea, pero de todas maneras la dejo escrita porsia cambio de opinión

#level_order_cceng<- factor( cc_english2$`Coral genera`, levels = c('Orbicella sp.',      
#'Agaricia sp.',
#'Porites sp.',
#'Siderastrea sp.',
#'Colpophyllia natans', 
#'Madracis sp.', 
#'Others'

#))
levels<-(cc_esp2$`Géneros de coral`)
print(levels)

# Hago la grafica de barras
ccesp_graph<- ggplot(cc_esp2, aes(fill= Año, y= `Cobertura relativa (%)`, x= `Géneros de coral`)) +
  geom_bar( position= "dodge", stat="identity") + #Esta linea indica que son barras agrupadas por aÃ±o
  theme(text = element_text(size = 14, family = "Roboto"))+
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f"))+
  theme(strip.text.x = element_text(size = 10, face= "bold", family = "Roboto"))
ccesp_graph<- ccesp_graph+ labs( x= "Géneros de coral", y= "Cobertura relativa (%)")
ccesp_graph<- ccesp_graph +theme(axis.text.x =  element_text(size = 11))

print(ccesp_graph)

ggsave("Historical coral cover espanol.png", units = c("cm"), width = 28, height = 10, dpi= 300)
