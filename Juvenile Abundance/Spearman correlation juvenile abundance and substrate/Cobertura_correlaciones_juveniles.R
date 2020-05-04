#Vamos a estudiar las correlaciones entre las variables del sustrato y 
#la abundancia de corales juveniles

#Para ello vamos a utilizar el tutorial de http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
#Cargamos los paquetes necesarios

#install.packages("ggpubr")
library("ggpubr")
help(package="ggpubr")
citation(package = "ggpubr")

#En este caso vamos a utilizar la prueba de correlación de Spearman, porque los datos probablemente no estén normalmente distribuidos
#La prueba de Spearman produce coeficientes de correlación basados en rangos para datos no-paramétricos
#Llamar los paquetes de interÃ©s
library(readxl)

#El primer paso es abrir la hoja de Excel de interÃ©s
abundancia_correlaciones<-read_xlsx("CoberturaFinal_juveniles_rugosidad.xlsx")

#Abundancia_rugosidad
ab_ru<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$Rugosidad,  method = "spearman")

ab_ru

#Abundancia_fleshy macroalgae
ab_ma<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$`Fleshy macroalgae`,  method = "spearman")

ab_ma

#Abundancia_calcareous macroalgae
ab_cm<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$`Calcareous macroalgae`,  method = "spearman")

ab_cm


#Abundancia_sand
ab_sand<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$`Sand`,  method = "spearman")

ab_sand

#Abundancia_scleractinian coral
ab_coral<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                  abundancia_correlaciones$`Scleractinian coral`,  method = "spearman")

ab_coral

#Abundancia cianobact
ab_ciano<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                   abundancia_correlaciones$Cyanobacteria,  method = "spearman")

ab_ciano

#Abundancia cca
ab_cca<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                   abundancia_correlaciones$`Crustose coraline`,  method = "spearman")

ab_cca

#Abundancia actinaria
ab_ac<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                 abundancia_correlaciones$Actinaria,  method = "spearman")

ab_ac

#Abundancia dictyota
ab_di<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$`Dictyota sp.`,  method = "spearman")
ab_di

#Abundancia octo
ab_octo<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$Octocoral,  method = "spearman")
ab_octo

#Abundancia esponjas
ab_spon<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                abundancia_correlaciones$Sponges,  method = "spearman")
ab_spon

#Abundancia rubble
ab_rubb<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                  abundancia_correlaciones$Rubble,  method = "spearman")
ab_rubb

#Abundancia_turf
ab_turf<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                  abundancia_correlaciones$Turf,  method = "spearman")

ab_turf

#Unknown
ab_unkn<-cor.test(abundancia_correlaciones$`Juvenile abundance`, 
                  abundancia_correlaciones$Unknown,  method = "spearman")

ab_unkn