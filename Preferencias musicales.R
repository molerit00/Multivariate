setwd("~/Desktop/Master/materias/Metodos cla??sicos/9na analisis unfolding")
library(readxl)
Preferencias_Musicales <- read_excel("Preferencias Musicales.xlsx")
View(Preferencias_Musicales)
head(Preferencias_Musicales)
Preferencias_Musicales
#Nueva variable con generos musicales
generos = c("Rock",	"Pop",	"Reggaeton",	"Disco",	"Electronica",	"Clasica", "Flamenco", "Latina",	"Jazz",	"Folk")

Preferencias_Musicales<- read_excel("~/Downloads/Preferencias Musicales (respuestas)-2.xlsx")
#Nos interesan las columnmas 6 a la 15
Pref=as.data.frame(Preferencias_Musicales[,6:15])

Nombres=as.matrix(Preferencias_Musicales[])
#Que nos de la dimesi—n de la matriz, la 1era nada mas.
n=dim(Pref)[1]
#Matriz de indices, 10 columnas, la llenamos solo con 0
#A esa matriz la llamamos indicies
Preferencias=matrix(0, n, 10)

#Recorremos todos los individuos de la matrizz, de 1 hasta n
for (i in 1:n)
  #Recorremos todos los generos 
  for (j in 1:10)
    #Matriz de preferencias del individuo i le decimos que de todos los generos
    #Rellenamos la matriz de preferencias
    #J vale 10 la preferencia del individuo en el 10mo lugar
    #Pone unicamente donde esta la preferencia 10
  
    Preferencias[i,which(Pref[i,j]==generos)]=j

colnames(Preferencias)=generos
rownames(Preferencias)=as.character(Nombres)

write.table(Preferencias, file="Preferencias.txt")
res <- unfolding(Preferencias)
res
plot(res)
unfolding(Preferencias,  )
unfolding(Preferencias, model = "Ratio")



