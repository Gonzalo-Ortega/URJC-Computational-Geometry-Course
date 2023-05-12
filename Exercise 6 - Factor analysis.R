################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 15/04/22                                                              #
#                                                                              #
# Practica 6: Analisis Factorial                                               #
#                                                                              #
################################################################################

# Instalamos las librerias
library(psych)
library(GPArotation)

# Cargamos el fichero de datos
file_location <- "D:/Escritorio/datosAsignaturas.txt"
M <-read.table(file_location, header = T, sep = "")

# Hacemos el test de esfericidad de Bartlett
bartlett.test(M)

## Modelo 1 ####################################################################

# Realizamos el an�lisis de los componentes principales sin rotar
model1 <- princomp(M, cor = TRUE)

# Calculamos la varianza de cada factor
summary(model1)

# Puntuaciones en los factores
loadings(model1)

# Dibujamos el gr�fico de sedimentaci�n
plot(model1, main = "", type = "lines")


## Modelo 2 ####################################################################

# Establecemos el numero de factores, los rotamos y los pintamos
model2 <- principal(M, nfactors = 2, rotate = "varimax")
model2

# Varianza de cada factor
summary(model2)

# Puntuaciones en los factores
loadings(model2)

# Pintamos la componente de puntuaciones de los casos del modelo
model2 $scores

# Dibujamos el gr�fico de componentes
biplot(model2, main = "")

## Modelo 3 ####################################################################

# Generamos el analisis factorial basao en maxima verosimilitud y lo pintamos
model3 <- factanal(M, 3, rotation = "varimax")
print(model3, digits = 2, cutoff = .3, sort = TRUE)

# Pintamos la grafica con los nombres de las variables
load <- model3 $loadings[,1:2]
plot(load, type = "n", main = "", xlab = "", ylab = "") 
text(load, labels = names(M), cex = .7)
