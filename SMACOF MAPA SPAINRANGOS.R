install.packages("MASS")
library(MASS)
install.packages("plotrix")
library(plotrix)
install.packages("smacof")
library(smacof)
#importamos datos de excel: spainrangos.xlsx
spainrangos

D=spainrangos[,-1]
D=as.matrix(D)
D
#Hacemos el mds ordinal
help("mds")

mdsD=mds(D, ndim = 2, type = c("ordinal"), 
         weightmat = NULL, init = "torgerson", ties = "primary", verbose = FALSE, 
         relax = FALSE, modulus = 1, itmax = 1000, eps = 1e-06, 
         spline.degree = 2, spline.intKnots = 2)
summary(mdsD)
str(mdsD)
mdsD$conf
mdsD$stress

plot(mdsD)
plot(mdsD, type = "p", label.conf = list(label = TRUE, col = "darkgray"), pch = 25, col = "red")
plot(mdsD,"Shepard")
plot(mdsD,"resplot")
plot(mdsD,"bubbleplot")
plot(mdsD,"stressplot")


#AHORA VAMOS A HACERLO CON LA FUNCIÓN SAMMON DE LA LIBRERRIA MASS y un archivo
#que viene por defecto: swiss
install.packages("MASS")
library(MASS)
data(swiss)
swiss
swiss.x=swiss[,-1]
swiss.x=as.matrix(swiss.x)

#no son distancias así que calculamos las distancias con la función dist
#por defecto utiliza la distancia euclidea
help(dist)
swiss.dist=dist(swiss.x)
swiss.NMDS=sammon(dist(swiss.x))
summary(swiss.NMDS)
str(swiss.NMDS)
swiss.NMDS$stress

plot(swiss.NMDS$points, type="n", main="MAPA DE SUIZA")
text(swiss.NMDS$points, labels = row.names(swiss),cex=0.6)

swiss.shepard <- Shepard(swiss.dist, swiss.NMDS$points)
swiss.shepard
plot(swiss.shepard,pch=".")
lines(swiss.shepard$x, swiss.shepard$yf, type = "S")


#AHORA VAMOS A HACERLO CON LA FUNCIÓN isoMDS (método de Kruskal)

swiss
swiss.x=swiss[,-1]
swiss.x=as.matrix(swiss.x)
swiss.dist=dist(swiss.x)
swiss.dist
swiss.Krusk=isoMDS(swiss.dist)
swiss.Krusk
plot(swiss.Krusk$points,type="n")
text(swiss.Krusk$points, labels = row.names(swiss),cex=0.6)
swiss.sh <- Shepard(swiss.dist, swiss.Krusk$points)
swiss.sh
plot(swiss.sh,pch=".")
lines(swiss.sh$x, swiss.sh$yf, type = "S")
