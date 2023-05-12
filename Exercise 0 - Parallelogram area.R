
################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 17/02/22                                                              #
#                                                                              #
# Ejercicio 1: Calcular el area del paralelogramo formado por dos vectores     #
#              en el plano.                                                    #
################################################################################

# Definimos los vectores v y w en el plano:
v <- c(1, 2)
w <- c(-1, 1)

# Formamos una matriz con los dos vectores por filas:
M = matrix(c(v, w), 2, 2)

# Calculamos el area del paralelogramo formado por los dos vectores calculando 
# el modulo del producto vectorial de v por w, ie., el determinante de la matriz
# formada por dichos vectores:
A = det(M)

# Finalmente, mostramos el resultado:
A


################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 17/02/22                                                              #
#                                                                              #
# Funcion para dibujar el paralelogramo formado por un par de vectores.        #
################################################################################

# Importamos la libreria grafica:
library(plotrix)

# Definimos los vectores v y w en el plano:
v <- c(1, 2)
w <- c(-1, 1)

# Definimos los colores:
BgColor <- "#F4F6F7"
TriangleColor <- "#D7DBDD"
VectorColor <- "#F1948A"
HeightColor <- "#85C1E9"

# Funci??n que dibuja el paralelogramo formado por dos vectores.
draw <- function(v, w) {
  # Ejes:
  plot(c(-3, 3), c(0, 3), xlab = "x", ylab = "y")
  
  # Paralelogramo y triangulo:
  polygon(c(0, v[1], v[1]+w[1], w[1]), c(0, v[2], v[2]+w[2], w[2]),
          col = BgColor)
  polygon(c(0, v[1], w[1]), c(0, v[2], w[2]), col = TriangleColor)
  
  # Vectores:
  arrows(0, 0, v[1], v[2], angle = 10, lwd = 3, col = VectorColor)
  text(v[1]/2, v[2]/2, pos = 4, offset = 1, "v")
  arrows(0, 0, w[1], w[2], angle = 10, lwd = 3, col = VectorColor)
  text(w[1]/2, w[2]/2, pos = 2, offset = 1, "w")
  
  # Calculos auxiliares:
  lgth_v <- sqrt(v%*%v)
  lgth_w <- sqrt(w%*%w)
  unit_v <- c(v[1]/lgth_v, v[2]/lgth_v)
  cos_vw <- (v%*%w)/(lgth_v*lgth_w)
  
  # Altura:
  lines(c(w[1], lgth_w*cos_vw*unit_v[1]), c(w[2], lgth_w*cos_vw*unit_v[2]), lwd 
        = 3, col = HeightColor)
  text((w[1] + lgth_w*cos_vw*unit_v[1])/2, (w[2] + lgth_w*cos_vw*unit_v[2])/2, 
       pos = 4, offset = 1, "h")
}

# Llamamos a la funcion:
draw(v, w)
