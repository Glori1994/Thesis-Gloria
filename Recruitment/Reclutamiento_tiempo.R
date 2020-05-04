#RECLUTAMIENTO
install.packages("gridExtra")


library(ggplot2)
library(xlsx)
library(showtext)
library(gridExtra)

#-----------Reclutamiento por Tiempo

getwd()

reclutamiento1<-read.xlsx("Reclutamiento.xlsx", 3)

#Fuentes

font_add_google("Roboto Slab", "Roboto")
font_add_google("Literata", "Literata")
font_add_google("Lato", "lato")

font_paths()
font_families()


#---------- Reclutamiento en tiempo
#Stacked barplot with multiple groups

#---------ESPAÑOL

j<-ggplot(data = reclutamiento1, aes(x= Intervalos.de.tiempo, y= Reclutas, fill= Especie))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f", "#ddbc96"))+
  theme(text = element_text(size = 35, family = "Roboto", face = "bold"), 
        axis.text= element_text(size= 30, face = "plain"),
        axis.title = element_text(size = 35, family = "Roboto", face = "bold"),
        legend.title.align = 0.5)

j<-j+labs(x= "Intervalos de tiempo", y= "Números de reclutas", fill= "Géneros")

showtext_auto()

plot(j)  

ggsave("Abundancia reclutas.png", units = c("cm"), width = 15, height = 10, dpi= 300)

#---------INGLÉS


i<-ggplot(data = reclutamiento1, aes(x= Intervalos.de.tiempo, y= Reclutas, fill= Especie))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#9dbc9d", "#e75441", "#f78f4f", "#ddbc96"))+
  theme(text = element_text(size = 35, family = "Roboto", face = "bold"), 
        axis.text= element_text(size= 30, face = "plain"),
        axis.title = element_text(size = 35, family = "Roboto", face = "bold"),
        legend.title.align = 0.5)

i<-i+labs(x= "Time intervals", y= "Number of recruits", fill= "Genera")

showtext_auto()

plot(i)  

ggsave("Abundance recruits.png", units = c("cm"), width = 15, height = 10, dpi= 300)



#-----------Total de reclutas

reclutamiento2<-read.xlsx("Reclutamiento.xlsx", 4)

ggplot(data = reclutamiento2, aes(x= especie, y= Reclutas))+
  geom_bar(stat="identity")


#----------Stacked

reclutamiento3<-read.xlsx("Reclutamiento.xlsx", 5)

survivorship_graph<- ggplot(reclutamiento3, aes(fill=Survived, y=Value, x=Genera)) + 
                     geom_bar( stat="identity", width = 0.4)+
                     scale_fill_manual(values = c("#e75441","#9dbc9d"))+
                     scale_y_continuous(name="Number of recruits")

plot(survivorship_graph, ylim=c(0,18), yaxs= "i")
#Tamaño y bold de las letras

survivorship_graph<- survivorship_graph +theme(axis.text.x = element_text(size = 30, family = "Roboto"), #Tama?o de la letra del texto del eje x
                           axis.text.y = element_text(size = 30, family = "Roboto"),
                           axis.title= element_text(size = 32, face = 'bold', margin= margin(t = 0, r = 20, b = 0, l = -5), family = "Roboto"), #etc...
                           legend.title = element_text(size = 32, face = 'bold', family = "Roboto"),
                           legend.title.align = 0.5,
                           legend.text=  element_text(size = 32, family = "Roboto"))#Modifica el texto de la leyenda

showtext_auto()


ggsave("Survivorship english.png", units = c("cm"), width = 10, height = 10, dpi= 300)




#####-------ESPAÑOL
#----------Stacked

reclutamientoesp<-read.xlsx("Reclutamiento.xlsx", 5)

survivorship_esp<- ggplot(reclutamientoesp, aes(fill=Survived, y=Value, x=Genera)) + 
  geom_bar( stat="identity", width = 0.4)+
  scale_y_continuous(name="Número de reclutas")

plot(survivorship_esp)
#Tamaño y bold de las letras

survivorship_esp<- survivorship_esp +theme(axis.text.x = element_text(size = 30, family = "Roboto"), #Tama?o de la letra del texto del eje x
                                               axis.text.y = element_text(size = 30, family = "Roboto"),
                                               axis.title= element_text(size = 32, face = 'bold', margin= margin(t = 0, r = 20, b = 0, l = -5), family = "Roboto"), #etc...
                                               legend.title = element_text(size = 32, face = 'bold', family = "Roboto"),
                                               legend.title.align = 0.5,
                                               legend.text=  element_text(size = 32, family = "Roboto"))#Modifica el texto de la leyenda

survivorship_esp<-survivorship_esp + labs(x= "Géneros de reclutas")+ 
  scale_fill_discrete(name = "Supervivencia", labels = c("No", "Sí"), 
                      values = c("#e75441","#9dbc9d"))

plot(survivorship_esp)


showtext_auto( )


ggsave("Survivorship espanol.png", units = c("cm"), width = 10, height = 10, dpi= 300)


######----------Quiero unir las gráficas


prueba<-grid.arrange(survivorship_graph, i)

ggsave("prueba.png", units = c("cm"), width = 20, height = 30, dpi= 300)
