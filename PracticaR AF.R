setwd("~/Desktop/Master/materias/Metodos cla??sicos/1era ACP y AF/Tecnicas factoriales en R")
#Primero poner el directorio donde quiero
tmms <- read.table("TMMS Tecnicas factoriales1.txt", header = TRUE ) 
#header coge los nombres de las variables
#Dataframe puede tener numeros y caracteres
tmms
plot(tmms)
head(tmms)
summary(tmms)
nrow(tmms)
ncol(tmms)
#Ver las dimensiones
dim(tmms)
boxplot(tmms)
mm <- tmms[,1:4]
as.numeric(mm)
mmm <- mm %>% ("IE1"> 2)
#Ver la dimensi—n a ver si es correcta
class(tmms)
head(tmms)
#Analizar 1ero la matriz de correlaciones
#Ver si existe correlaci—n
ctmms <- cor(tmms)
ctmms
#Ver en gr‡fico para eso instalamos un paquete
install.packages("corrplot")
corrplot(ctmms)
#Grafica con escala de color
#Tiene sentido un analisis factorial 
det(ctmms)
#Determinante de la matiz de datos si es 0 es que existe relaci—n entre variables
#Cercano a 0 tiene relaci—n entre variables
#Test de barlett: esfericidad: h0 existe esfericidad, nos interesa rechazar
#Ver que hay relaci—n
#Test de barlett
library("psych")
library(stats)
#Barlett sobre la matriz de correlaciones, necita el tama–o muestral
#Lo hacemos de matriz de correlaci—n.
cortest.bartlett(ctmms, n=136 )
#Nos interesa el pvaloer que es menos a 0,05 nos da significativo, seguimos con la corr
#Medida KMO es una medida es una medida no es un contraste mas cerca a 1 mas aptos para 
#un analisis factorial
#Se hace desde el dara.frame matriz directa
KMO(tmms)
#Vemos el valor que es 0.85


#Comenzar con AF
#Supuestos
#Tantos ejes de explicar la varianza como el 75% de la variaxi—n 
#Valores propios mayores a 1
#Ver el gr‡fico
#Funci—n scree
scree(tmms)
#Quedarse con lo que son mayores a 1 tenemos 4
#Quedarse con 4 si vemos la regla de codo pero quedemos buscar 3 nosotros
#Nos dice que hay 4 porque en el gr‡fico de colores vemos que necesita un 4to para explicar
#La poca correlaci—n con algunas variables

#AF sin rotaci—n

#Queremos ver 3 factores lo hacemos con barlet
fa1 <- factanal(tmms, factors = 3, scores = "Bartlett", rotation = "none")
summary(fa1)
print(fa1)
#Ver matriz de loadings, ver las cargas que es lo mismo que las saturaciones
#La que esta por abajo de 0 las hace 0 para que sean mas interpretables
#Matriz de pesos ver que una componente tiene mas pesos de una y menos de otra
#fallamos porque esta todo lo mismo tiene que estar diferenciado entre cada uno
#Crear vector de colores para verlo mejor
colors <- c(rep("orange", 8), rep("yellow",8), rep("green", 8))
#Ver las saturaciones corchetes para ver las dimensiones de la matriz
#Ponemos [.1] para ver el factor 1
barplot(fa1$loadings[,1], col = colors)
#Combinaci—n de todos los items, no sirve

barplot(fa1$loadings[,2], col = colors)
#Ver la 3era
barplot(fa1$loadings[,3], col = colors)
#Esta representa si hay relaci—n inversa 
#Saturaci—n que importa

#No podemos crear variables en ellos de las dimensiones que queriamos
fa2 <- factanal(tmms, factors = 3, scores = "Bartlett", rotation = "varimax")
print(fa2)
barplot(fa2$loadings[,1], col = colors)
barplot(fa1$loadings[,2], col = colors)
barplot(fa1$loadings[,3], col = colors)
#Rotaci—n varimax hace que los factores se extremicen, los que cargas altas + altas 
#Los bajos + bajos
#Rota los ejes X y los Y
#Ver los 3 graficos en pantalla de graficos
#el 1 son las filas y el 3 son las columnoas
par(mfrow=c(1,1))
barplot(fa2$loadings[,1], col = colors)
barplot(fa1$loadings[,2], col = colors)
barplot(fa1$loadings[,3], col = colors)
fa1
fa2
#Se reparten la = cantidad de info pero la reparten de otra manera
#Ya estudiamos las cargas ahora hay que ver como se comportan los 
#individuos en las variables latentes


#Scores vamos a ver que tiene adentro
fa1$scores
#Imporime estudiantes y factores latentes puntuanciones de los individuos en cada factor
#Pintar las 2 primeras columnas por eso ponemos el ,1:2
#Pch para que los piuntos esten reyenos
#Queremos agregar una linea abline para eso
plot(fa2$scores[,1:2], xlab="Atenci—n", ylab = "Claridad", pch=19)
#Dos lineas que crucen el grafico para interpretarlo mejor.
abline(h=0, v=0)
#Puntuaciones factorealies
#Comunalidad: si es alta aparece mucha en el factor latente, 
#lo que explica el item del factor latente
#la info que no se explica mediante el factor latente
#Unicidad lo que no explica el factor del item
#Analisis factorial, vemos


#ACP
#Como no empezamos de 0 solo vamos a ahcer una parte del ACP
pca1 <- prcomp(tmms)
pca1
#Rotaci—n le dice a las cargas, no hace rotaci—n solo que se llamas as’ es la matriz de cargas
print(pca1)
summary(pca1)
#Muestra cuan explicado esta por pocas variables muestra cada una y cual acumulada.

#Matriz de cargas
#Mismo gr‡fico que antes.
barplot(pca1$rotation[,1], col = colors)
#Todos los items cargan en la 1er componente, vuelve a no dar info

barplot(pca1$rotation[,2], col = colors)
#Mismo que en el AF porque son resultados parecidos, en conjutno dan igual.
barplot(pca1$rotation[,3], col = colors)
#ES NECESARIO APLICAR TECNICA DE ROTACIîN
#Vamos a rotar las cargas para mejorarlas
pcarot <- varimax(pca1$rotation)
#En los resultados de varimaz lo carga como rotation
pcarot$loadings
barplot(pca1$rotation[,1], col = colors)
barplot(pca1$rotation[,1], col = colors)
barplot(pca1$rotation[,1], col = colors)
#Funci—n principal porque la otra no funciona bien
pca.rot <- principal(tmms, nfactors = 3, rotate = "varimax")
pca.rot
#tenemos items y componentes en columnas donde dice Rc1, Rc2 y Rc3
#Mas claro verlo con el gr‡fico.
par(mfrow=c(1,3))
barplot(pca.rot$loadings[,1], col=colours())
barplot(pca.rot$loadings[,2], col=colours())
barplot(pca.rot$loadings[,3], col=colours())
#Vemos los 3 graficos juntos para analizar si esta bien que las 3 latentes explican cosas distintas



















