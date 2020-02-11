setwd("~/Desktop/Master/materias/Metodos cl‡sicos/1era ACP y AF/Tecnicas factoriales en R")
library(readxl)
library(dplyr)
library(corrplot)
library(psych)
TMMS_Tecnicas_factoriales <- read_excel("TMMS Tecnicas factoriales.xlsx")
View(TMMS_Tecnicas_factoriales)

PCA <- TMMS_Tecnicas_factoriales

#Para quitar columnas
#Quita primer columna entera 
# %>% quiere decir que donde haya parentesis mete lo que hay adelante de la pipe
PCA <- PCA %>% subset(select = -1) %>% subset(select = - Sexo) %>% subset(select = - Nota_Media)

#Ver las correlaciones entre las variables.
PCACORR<- PCA %>% cor() %>% corrplot()

#Ponemos en n el numero de sujeros que haya
#Como el barltlett necesita una matriz cuadrada usamos la correlaci—n
#Que logramos gracias a corrplot
cortest.bartlett(PCACORR, n=136)
#Vemos que la matriz es esferica porque se rechaza h0 las variazas son distintas
KMO(PCA)
#KMO alto, bueno para el PCA
scree(PCA) #Vamos a hacer PCa con 4 factores
FA1 <- PCA %>% prcomp()
FA1
summary(FA1) #Para ver cual es la varianza explicada 

#AF
FA2 <- PCA %>% factanal(factor = 3)
summary(FA2)
FA2$
FA2$loadings #Loading son las saturaciones
colors <- c(rep("orange", 8), rep("red", 8), rep ("yellow", 8))
#Que nos coga la 1er columna, que es el primer vector
# [x,n] x son las filas y la n son las colum en este caso tomamos toda la col 1 
barplot(FA2$loadings[,1], col = colors)

barplot(FA2$loadings[,2], col = colors)

barplot(FA2$loadings[,3], col = colors)

FA2max <- factanal(PCA, factors = 3, scores = "Bartlett", rotation = "varimax")
FA2max
barplot(FA2max$loadings[,1], col = colors)

barplot(FA2max$loadings[,2], col = colors)

barplot(FA2max$loadings[,3], col = colors)
plot(FA2max$scores[,2:3])
