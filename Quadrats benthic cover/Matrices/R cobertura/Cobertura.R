 
#Este es el script para generar la matriz de cobertura a partir 
#de la matriz de censo que produce PhotoQuad

#El objetivo es:
#1) sustratos con sus porcentajes totales por cuadrata
#2) tener por columna los tiempos anidados por unidades experimentales
#3)las variables como columna

#Llamo el csv de las especies y sustratos que arroja PhotoQuad
getwd()

list.files(path = "C:/Users/Gloria/Desktop/Tesis/Cobertura/Matrices/R cobertura", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
read.csv("censo_bueno.sppregions.csv")
censo<- read.csv("censo_bueno.sppregions.csv")
censo

#Llamo paquetes necesarios
#install.packages("tidyverse")
library(tidyverse)


#1) SUSTRATOS CON PORCENTAJES TOTALES

#Que columnas me interesan
colnames(censo)

#Me interesan: Image, sppName, SPECIES.Cov.., TOTAL.Cov..

#Limpiar la matriz de censo a solo las columnas con las que voy a trabajar
library(dplyr)
censo_limpio<- select(censo, Image, sppName, SPECIES.Cov.., TOTAL.Cov.. )
censo_limpio

#Elegir solo los valores unicos
censo_limpio_unicos<- unique(censo_limpio, incomparables= FALSE)
censo_limpio_unicos

#pero necesito quitar la columna Image

censo_limpio_bueno<- censo_limpio_unicos[-1]
censo_limpio_bueno

#2)TIEMPOS ANIDADOS CON UNIDADES EXPERIMENTALES

#Quiero generar una columna de cuadratas y otra de tiempos

#Creo vector
cuadratas_tiempos <-pull(censo_limpio_unicos, Image)
cuadratas_tiempos

#Chequeamos el tipo de data, necesitamos characters
typeof(cuadratas_tiempos)

#Convertimos la data a character
cuadratas_tiempos_C <- as.character(cuadratas_tiempos)
typeof(cuadratas_tiempos_C)

#Finalmente separamos las expresiones de "_"
vectores_cuadratas<- strsplit(cuadratas_tiempos_C, "_")
vectores_cuadratas

C_T_O<- do.call(rbind, vectores_cuadratas)
C_T_O

#Quedaron tres columnas: numero de la cuadrata, tiempo y "ortomo.jpg"
#necesito nada mas el tiempo y la cuadrata
C_T<- C_T_O[,-3]
C_T

#Creamos un data.frame de las cuadratas y los tiempos
C_T_DF<- data.frame("Cuadrata"= C_T[,1], "Tiempo"= C_T[,2] )
C_T_DF

#junto los dataframes de columna y tiempo con especies etc
censo_completo <- bind_cols(C_T_DF, censo_limpio_bueno)
censo_completo


#3) PONER LAS VARIABLES COMO COLUMNAS
#Debo generar un nuevo dataframe que tenga como columnas La cuadrata, los tiempos
#Las especies

#Columna de Turf

turf <- censo_limpio_unicos %>% 
  select(TOTAL.Cov..) %>% 
  distinct() %>% 
  mutate(TOTAL.Cov.. = 100-TOTAL.Cov..) %>% #
  rename(Turf = TOTAL.Cov..)

#La 

Final_casi <- censo_completo %>%
  select(-TOTAL.Cov..) %>% #le quito la columna de total de cobertura
  mutate(Tiempo = as.numeric(Tiempo)) %>% #cambio la columna de tiempo a numerico
  group_by(Cuadrata, Tiempo, sppName) %>% #agrupar por cuadrata, tiempo y SppName (las que coincidan en esos tres aspectos). 
  #este paso es importante porque hay veces que se repiten especies porque tenian distintos ID.
  #la funcion que sigue no puede tener numeros duplicados
  summarise(sum(SPECIES.Cov..)) %>% #sumamos las especies iguales con distintas formas de crecimiento
  spread(value = "sum(SPECIES.Cov..)", key = sppName, fill = 0) %>% #esta funcion coloca las variables como columna y rellena con 0 los valores que esten vacios
  bind_cols(turf)#se agrrego la columna de turf


#Guardar CSV 
write.csv(Final_casi, file= "Cobertura_final_casi.csv")  

Final<- Final_casi %>%
  mutate(Agaricia = sum(c(`Agaricia agaricites (agaricites)`, 
                          `Agaricia agaricites (purpurea)`,
                          `Agaricia tenuifolia`))) %>% 
  select(-c(`Agaricia agaricites (agaricites)`, 
            `Agaricia agaricites (purpurea)`,
            `Agaricia tenuifolia`)) %>% 
  mutate(Orbicella = sum(c(`Orbicella faveolata`, 
                           `Orbicella annularis`,
                           `Orbicella sp.`))) %>% 
  select(-c(`Orbicella faveolata`, 
            `Orbicella annularis`,
            `Orbicella sp.`)) %>% 
  mutate(Porites = sum(c(`Porites astreoides`, 
                         `Porites branneri`))) %>% 
  select(-c(`Porites astreoides`, 
            `Porites branneri`)) %>%
  rename(Colpophyllia = `Colpophyllia natans`)


#Guardo el CSV
write.csv(Final, file= "Cobertura_Final.csv") #Guardar csv


#Final_2 (grandes grupos taxon?micos)
Final_3<- Final %>%
  mutate(`Scleractinian coral` = sum(c(`Colpophyllia`,
                                       `Favia fragum`,
                                       `Other (Scleractinian encrusting)`,
                                       `Siderastrea siderea`,
                                       `Agaricia`,
                                       `Orbicella`,
                                       `Porites`,
                                       `Pseudodiploria strigosa`))) %>%
  select(-c(`Colpophyllia`,
            `Favia fragum`,
            `Other (Scleractinian encrusting)`,
            `Siderastrea siderea`,
            `Agaricia`,
            `Orbicella`,
            `Porites`,
            `Pseudodiploria strigosa`)) %>%
  mutate(`Fleshy macroalgae`= sum(c(
    `Other (Fleshy algae)`,
    `Ventricaria sp.`))) %>%
  select(-c(
    `Other (Fleshy algae)`,
    `Ventricaria sp.`)) %>%
  mutate(`Calcareous macroalgae` = sum(c(`Galaxaura sp.`,
                                         `Other (Calcareous algae)`,
                                         `Padina sp.`))) %>%
  select(-c(`Galaxaura sp.`,
            `Other (Calcareous algae)`,
            `Padina sp.`)) %>%
  mutate(`Sponges` = sum(c(`Other (Sponge encrusting)`,
                           `Other (Sponge erect)`,
                           `Other (Sponge massive)`))) %>%
  select(-c(`Other (Sponge encrusting)`,
            `Other (Sponge erect)`,
            `Other (Sponge massive)`)) %>%
  mutate(`Unknown` = sum(c(`Biotic (unknown)`,
                           `Artifact`))) %>%
  select(-c(`Biotic (unknown)`,
            `Artifact`)) %>%
  rename(Octocoral = `Eunicea sp.`)
#Guardo el CSV
write.csv(Final_3, file= "Cobertura_Final_3.csv") #Guardar csv









#INTENTOS FALLIDOS


#Lo primero que tengo que hacer es extraer los valores unicos de la columna de especies (sppName)
#Esos valores serán los nombres de las variables que luego irán en la columna


#Para ello extraigo la columna de sppName del dataframe "censo"
sustratos <- censo_completo[,3]
sustratos

#Quiero generar un data frame de 32 columnas * 33 filas
#32 columnas= 30 niveles (sustratos)+ tiempo + cuadrata
#33 filas= encabezados + 32 (combinacion cuadratas*tiempo)



#Extraer valores unicos de "sustratos"
sustratos_unicos<- unique(sustratos, incomparables = FALSE)
sustratos_unicos

#Extraigo valores unicos de las cuadratas
cuadratas_unicas <- unique(censo_completo$Cuadrata)
cuadratas_unicas

#armando la estructura de la matriz
#vector de cuadratas* cuatro tiempos
cuadrata1<-rep(cuadratas_unicas, each=4) 
cuadrata1

cuadrata<-as.character(cuadrata1)
typeof(cuadrata)


#vector de tiempos * cuadratas
tiempo<- rep(1:4, times= 8)
tiempo

#Hago una matriz vacia con los nombres de las variables
#matriz con dimension que necesito
df <- data.frame(matrix(ncol = 30, nrow = 32))
df

d<-as.character(sustratos_unicos)
d

names(df)<- c(d)
df

#matriz final
censo_final<- cbind(cuadrata, tiempo, df)
censo_final






