#Script para hacer las gráficas de cambio de cobertura del bentos en las cuadratas

#Llamar los paquetes de interés
library(ggplot2)
library(readxl)
library(tidyverse)
library(showtext)
library(sysfonts)
library(showtextdb)



.libPaths()


####---Puntos en el tiempo

require(tidyverse)
require(RColorBrewer)
Data <- read.csv("Cobertura_Final_3.csv", stringsAsFactors = F)

#Fuentes

font_add_google("Roboto Slab", "Roboto")
font_add_google("Literata", "Literata")
font_add_google("Lato", "lato")

font_paths()
font_families()

#Hacemos el vector con los nombres de las gr�ficas
organisms<- c("Actinaria", "Calcareous macroalgae.", "CCA", "Cyanobacteria", "Dictyota", 
              "Fleshy macroalgae", "Octocoral", "Rubble", "Sand", "Scleractinian coral", 
              "Sponges", "Turf", "Unknown")
names(organisms)<-c("Actinaria", "Calcareous.macroalgae", "Crustose.coraline", "Cyanobacteria", "Dictyota.sp.", 
                    "Fleshy.macroalgae", "Octocoral", "Rubble", "Sand", "Scleractinian.coral", 
                    "Sponges", "Turf", "Unknown")

#Grafica
grafica<- Data %>%
  pivot_longer(cols = 4:ncol(Data), names_to = "Organism", values_to = "Cover") %>%
  ggplot(aes(x = Tiempo, y = Cover, color = Cuadrata)) +
  geom_point(size = 2) +
  geom_line(size = 1) + xlab("Time") +
  facet_wrap(~ Organism, ncol = 4, labeller = labeller(Organism= organisms)) + # change for the following line if you want free Y scales
  theme(text = element_text(size = 35, family = "Roboto", face = "bold"),
        strip.text.x = element_text(size = 25, face= "plain", family = "Roboto"),
        axis.text.x =element_text(size = 25, face= "plain", family = "Roboto" ),#numeros de la leyenda x
        axis.text.y= element_text(size = 25, face= "plain", family = "Roboto"),
        legend.text = element_text(size = 25, face= "plain", family = "Roboto"),
        ) +
   #facet_wrap(~ Organism, ncol = 3, scales = "free_y") +
  scale_color_manual("Quadrat", values = brewer.pal(8, "Dark2")) +
  #theme_bw()+
 
showtext_auto()

print(grafica)

ggsave("benthic cover.png")#, units = c("cm"), width = 6.52, height = 5.68, dpi= 300)

warnings()




grafica<- grafica + facet_grid(~ Organism, ncol = 3, labeller= labeller(Organism= organisms))

grafica

# New facet label names for dose variable
dose.labs <- c("D0.5", "D1", "D2")
names(dose.labs) <- c("0.5", "1", "2")

# New facet label names for supp variable
supp.labs <- c("Orange Juice", "Vitamin C")
names(supp.labs) <- c("OJ", "VC")

# Create the plot
p + facet_grid(
  dose ~ supp, 
  labeller = labeller(dose = dose.labs, supp = supp.labs)
)



















#####-------BARPLOT


#El primer paso es abrir la hoja de Excel de interés

cobertura<-read_xlsx("Cobertura_Final_3.xlsx")
cobertura
str(cobertura)


#Convertimos en factor con niveles a Tiempo
cols = c(2)
cobertura[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

str(cobertura)

#Convertimos en factor con niveles a cuadrata
cols = c(3)
cobertura[,cols] %<>% lapply(function(x) as.factor(as.character(x)))

str(cobertura)

#Intento con la cobertura de macroalgas carnosas

#Calculo la desviación estandar de la cobertura con respecto al factor tiempo
dicty_sd<-aggregate(`Dictyota sp.`~ Tiempo,cobertura, sd)
#chequeamos el nombre de las columnas
colnames(dicty_sd)
#Cambiamos los nombres de las columnas
dicty_sd<- rename(dicty_sd, Time= Tiempo, sd= `Dictyota sp.`)
  
#Calculo la madia de la cobertura con respecto al factor tiempo
dicty_mean<-aggregate(`Dictyota sp.`~ Tiempo,cobertura, mean)
#chequeamos el nombre de las columnas
colnames(dicty_mean)
#Cambiamos los nombres de las columnas
dicty_mean<- rename(dicty_mean, Time= Tiempo, mean= `Dictyota sp.`)

dicty<-bind_cols(dicty_mean, list(dicty_sd$sd))
dicty<-rename(dicty, sd= V1)
#gráfica

# Grafica de bar plot con sd
ggplot(dicty) +
  geom_bar( aes(x=Time, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar(aes(x=Time, ymin=mean-sd, ymax=mean+sd), colour="orange", alpha=0.9, size=1.5) +
  ggtitle("dicty")

####Cosas extra que no us�....

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






#####-------PARALEL CHART

#install.packages("GGally")
install.packages("hrbrthemes")

# Libraries
library(hrbrthemes)
library(GGally)
library(viridis)

# Data set is provided by R natively
data <- iris

# Plot
ggparcoord(data,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )




#Arreglar la matriz para Alfredo
abundace_long<-pivot_longer(cobertura, names_to= "benthic element", values_to= "Abundance")


pivot_longer(data, cols, names_to = "name", names_prefix = NULL,
             names_sep = NULL, names_pattern = NULL, names_ptypes = list(),
             names_repair = "check_unique", values_to = "value",
             values_drop_na = FALSE, values_ptypes = list())








