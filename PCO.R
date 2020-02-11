#Analisis de correspondencias principales

library(scales)
 library(geometry)
 library(rgl)
 library(deldir)
library(GPArotation)
library(MASS)
library("splom")

install.packages("http://biplot.usal.es/multbiplot/multbiplot-in-r/multbiplotr_191119tar.gz", repos = NULL, type="source")
library(MultBiplotR)

data("RAPD")
head(RAPD)
View(RAPD)
RAPD<- as.matrix(RAPD)
dist <- BinaryProximities(RAPD, coef= 4)
dist$Coefficient
pco <- dist %>% PrincipalCoordinates(dimension = 3)
pco
plot(pco)
grupos <- c("B", "B",  "B", "B", "B",  "B", "B", "B",  "B", "B", "B",  "B", "B", "B",
            "SP", "C", "C", "Co",  "Co",  "CP", "CP", "CP", "CP", "CL", "MEX", "POJ", "POJ", "R
 agnar",
            "PR", "PR", "PR", "PR", "PR",  "V",  "V", "V", "V", "V",  "V",  "V", "V", "V",
            "V", "V", "V", "V", "V", "V", "V", "V")
grupos <- as.factor(grupos)

pco <- pco %>% AddCluster2Biplot(ClusterType = "us", Groups = grupos)
pco
plot(pco, PlotClus= T)
MDS
