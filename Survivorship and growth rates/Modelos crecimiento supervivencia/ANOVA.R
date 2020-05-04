#ANOVA
library(readxl)
getwd()
#install.packages("oslrr")

matriz <- read_xlsx("tc intervalos 4-1.xlsx", sheet = 3)
data.frame(matriz, stringsAsFactors= default.stringsAsFactors(TRUE))

#Veo las clases de los datos
str(matriz)

#Convierto en factores los que necesito
matriz$Genera<-as.factor(matriz$Genera)
matriz$Tiempo<-as.factor(matriz$Tiempo)
matriz$Cuadrata<-as.factor(matriz$Cuadrata)
matriz$Survived<-as.factor(matriz$Survived)

#Vuelvo a revisar las clases de los datos
str(matriz)

#Veo cuales son las frecuencias de los individuos que sobrevivieron (1) o no sobrevivieron (0) por genero
table(matriz[,4:5])

#Saco la Colpophylia de la matriz, porque tuvo menos de tres individuos
sinc<- subset(matriz, Genera!= "Colpophylia")
sinc


#ANOVA
aov(formula = Mean ~ Genera, data = matriz)
summary.aov(aov(formula = Mean ~ Genera, data = matriz))




