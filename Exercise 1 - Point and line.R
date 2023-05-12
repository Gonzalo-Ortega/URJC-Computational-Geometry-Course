################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 24/02/22                                                              #
#                                                                              #
# Ejercicio 2: Determinar si un punto esta por encima o por debajo de un plano #
#              en R3.                                                          #
################################################################################

# Se define el plano como una funci�n lineal de x e y en z.
plane <- function(x, y){
  2*x
}

# Se define el punto como un vector con sus tres coordenadas.
point <- c(1, 1, 6)

# Comparando la coordenada z del punto con la coordenada z del plano evaluado 
# en las coordenadas x e y del punto se dermina la posicion del punto respecto
# del plano.
if (point[3] > plane(point[1], point[2])) {
  print("El punto esta por encima del plano")
} else if (point[3] < plane(point[1], point[2])){
  print("El punto esta por debajo del plano")
} else {
  print("El punto esta en el plano")
}
