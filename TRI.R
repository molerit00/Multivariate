
library(mirt) #tri
library(foreign) #Para leer los datos desde spss que las columnas queden bien.
library(ltm) #Modelos de rasgo latente
library(psych) #Psicometria
library(lordif) #Funcionamiento diferencial de los items
datos

#cambiar el directorio de trabajo al que contiene el archivo
# Todo funcionará mejor si sus columnas están todas definidas como numéricas
#Pone true para que lo transforme en dataframe
#Hay que leerlo de esa forma
head(Datos)
dim(Datos) #La dimensión de los datos

dim1

# ---------------------------------------------------------------------------------------------
# Seleccionamos los datos que contienen los items de la escala A



# Analizamos la escala A
# Calculamos, por ejemplo, la matriz de correlaciones tetracóricas (paquete psych)
#Son binarias

#Curva de información de los items, cual tiene mayor información, esta relacionado con la cantidad de
#Discriminación de cada item, cual es mas dificil, cual diferencia mas
plot(TetrA)
TetrA
#Matriz de correl como siempre, todas son positivas, en cuestionarios siempre pasa esto
#Puntos de corte estimados estan en una escala estandarizadas entre -4 y 4
#Lo hacemos con irt.fa
irtA=irt.fa(dim1)
irtA$plot
# Mostramos la parte correspondiente al análisis factorial
irtA$fa # Saturaciones y comunalidades
#MR1Las saturaciones en el modelo factorial, factor esta rel con todas las variables
#h2 la comunalidad y u2 la unicidad
# Mostramos información general
summary(irtA)
print(irtA)

# Scree Plot diagrama de sedimentación
#Valoe propio dominante y los demas por abajo de 1.
plot(irtA$fa$values, main = "Scree Plot")
# Puntos de corte de la correlación tetracórica
irtA$tau
# Parámetros en términos de discriminación y dificultad 
#vemos que items discriminan mas y cual tiene
#Mas dificultad. La dificultad es depende del conocimiento/capacidad del individuo
#De la posibilidad de acertar el item (SON ESCALAS NORMALES ESTANDAR)
#La probabilidad es alta de acertar para el conocimiento 0 significa que discrimina mucho.
irtA$irt
# Dibujos
dev.new()
op <- par(mfrow=c(3,1)) # Divide el gráfico en 3 para las representaciones siguientes
plot(irtA,type="ICC") # Curvas Caracteríaticas de los items
plot(irtA,type="IIC") # Curvas de información de los items
plot(irtA,type="test") # Curva de informacion del test cantidad
#De información del test
par(op) # Devuelve el gráfico a su estado normal

# Comparamos los resultados con los de otros paquetes : ltm
# El modelo más sencillo es el modelo de Rasch que supone las discriminaciones iguales a 1
# la restriccion se añade con constraint = cbind(ncol(A) + 1, 1) (ver la ayuda help(rasch))
raschA=rasch(A, constraint = cbind(ncol(A) + 1, 1))
raschA
raschA$coefficients
dev.new()
op <- par(mfrow=c(2,1)) # Divide el gráfico en 2 para las representaciones siguientes
plot(raschA,type="ICC") # Curvas Caracteríaticas de los items
plot(raschA,type="IIC") # Curvas de información de los items
par(op) # Devuelve el gráfico a su estado normal
