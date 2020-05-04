#Numero de individuos unicos en las cuadratas


#Llamar los paquetes de interés
library(readxl)
library(dplyr)


#El primer paso es abrir la hoja de Excel de interés
abundancia_total<-read_xlsx("Matriz Madre.xlsx", sheet= 1)

#Vemos el nombre de todas las cuadratas
unique(abundancia_total$Cuadrata)


#Buscamos el numero de individuos unicos para cada cuadrata
unicos_C4<-filter(abundancia_total, Cuadrata== "C04")
C04<-unique(unicos_C4$Individuo) #Extraigo los valores unicos de los nombres de los individuos observados en la cuadrata 4
C04
a<-length(C04) #con el largo puedo ver cuantos individuos unicos habia en la cuadrata en todos los tiempos
a

unicos_C06<-filter(abundancia_total, Cuadrata == "C06")
C06<-c(as.character(unique(unicos_C06$Individuo)))
b<-length(C06)
b

unicos_C12<-filter(abundancia_total, Cuadrata == "C12")
C12<-c(as.character(unique(unicos_C12$Individuo)))
c<-length(C12)
c

unicos_C13<-filter(abundancia_total, Cuadrata == "C13")
C13<-c(as.character(unique(unicos_C13$Individuo)))
d<-length(C13)
d

unicos_C15<-filter(abundancia_total, Cuadrata == "C15")
C15<-c(as.character(unique(unicos_C15$Individuo)))
e<-length(C15)
e

unicos_C16<-filter(abundancia_total, Cuadrata == "C16")
C16<-c(as.character(unique(unicos_C16$Individuo)))
f<-length(C16)
f

unicos_C27<-filter(abundancia_total, Cuadrata == "C27")
C27<-c(as.character(unique(unicos_C27$Individuo)))
g<-length(C27)
g

unicos_C28<-filter(abundancia_total, Cuadrata == "C28")
C28<-c(as.character(unique(unicos_C28$Individuo)))
h<-length(C28)
h
class(h)

sum(a,b,c,d,e,f,g,h)

unique_individuals<-sum(a,b,c,d,e,f,g,h)
unique_individuals
