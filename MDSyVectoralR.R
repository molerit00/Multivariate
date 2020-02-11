library(haven)
setwd("~/Desktop/Master/materias/1-Metodos cla??sicos/7ma Modelo Vectorial/MDS CON R-20191118")
SINT_ALERG <- read_sav("Desktop/Master/materias/Metodos cla??sicos/7ma Modelo Vectorial/DATOS PRA??CTICA MODELO VECTORIAL/DATOS VECTORIAL ALERGIAS/SINT_ALERG.sav")
View(SINT_ALERG)
library(vegan)
#Coga de la col1 a la 20 xq en la 21 dice de que lugar son los hospitales
SINT_ALERG <- SINT_ALERG[,1:20]
sin <- SINT_ALERG
#Vamos a eliminar los casos perdidos
sin <- na.omit(sin)
sin
#Los alergenos vah de la col 2 a la nueva, 
#Creamos nuevo dataframe de alerjernos
sin.ale <- sin[,2:9]
head(sin.ale)
#nuevo dataframe solo de sintomas
sin.sint <- sin[,10:20]
head(sin.sint)
#Transformamos ese dataframe en matris
sin.sint <- as.matrix(sin.sint)
View(sin.sint)
#Estas que son las cuasas las transformamos en numercos
sin.ale <- as.numeric(sin.ale)

#Hacemos el MDS a los sintomas porque es lo que queremos explicar

mdssin.sin <- metaMDS(sin.sint)
plot(mdssin.sin, type="t")

#A los alergenos los hacemos vectores porque son los que explican.
fit <-  envfit(mdssin.sin, sin.ale, perm=999)

#Correlaci—n entre vectores y las dimensiones
fit$vectors

plot(fit)

#Para ver en el graf las variables que son significativas
plot(fit, p.max = 0.05, col = "grey")

#Scores son las coordenadas de la cabeza del vector
scores(fit, "vectors")



