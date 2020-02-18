library(haven)
PaisesProteinas <- read_sav("~/Desktop/Master/materias/2-Metodos clásicos/3era cluster/Cluster  practica-20191111/PaisesProteinas.sav")
View(PaisesProteinas)

data <- PaisesProteinas
data.scale <- scale(data[,-1])

install.packages(SDraw, dependencies = TRUE)
library(MultBiplotR)
library(GGEBiplotGUI)
library(SDraw)


data.scale <- princomp(data.scale)

#:: para aclarar el paquete al que pertenece el comando
#choice para que eliga unos det ejes, scala que no lo escale
biplotFIT<- stats::biplot(data.scale, choice = 1:2, scale=1 )
