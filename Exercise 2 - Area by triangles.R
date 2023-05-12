################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 24/02/22                                                              #
#                                                                              #
# Practica 1: Hallar el area de una figura plana mediante triangulos.          #
#                                                                              #
################################################################################

# Parametro para recorrer la curva
t <- seq(0, 2*pi, by = 0.01)

# Curva parametrizada con forma de mariposa
x = sin(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)
y = cos(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)

# Colores a utilizar
bg_color <- "#F4F6F7"
triangle_color <- "#D7DBDD"
border_color <- "#273746"

# Se pinta la curva
plot(c(-3, 3), c(-2, 3), type = "n", xlab = "x", ylab = "y", axes = FALSE)
axis(1)
axis(2)
polygon(x, y, col = bg_color, border = border_color)

# Se hace una aproximacion rudimentaria de la figura trazando triangulos 
# en su interior de forma manual:

# Se crea una lista para almacenar los triangulos:
tl <- list()

# Ala superior derecha
tl[[1]] <- c(1, 50, 100)
tl[[2]] <- c(50, 75, 100)
tl[[3]] <- c(1, 100, 130)

# Ala superior izquierda
tl[[4]] <- c(1, 630-50, 630-100)
tl[[5]] <- c(630-50, 630-75, 630-100)
tl[[6]] <- c(1, 630-100, 630-130)

# Ala inferior derecha
tl[[7]] <- c(188, 200, 250)
tl[[8]] <- c(200, 230, 250)
tl[[9]] <- c(188, 270, 250)

# Ala inferior izquierda
tl[[10]] <- c(188, 630-200, 630-250)
tl[[11]] <- c(630-200, 630-230, 630-250)
tl[[12]] <- c(188, 630-270, 630-250)

# Cuerpo
tl[[13]] <- c(1, 130, 188)
tl[[14]] <- c(1, 630-130, 188)

# Cabeza
tl[[15]] <- c(1, 630-305, 305)

# Dibujamos los triangulos
for (i in tl) {
  polygon(c(x[i[1]], x[i[2]], x[i[3]]),
          c(y[i[1]], y[i[2]], y[i[3]]),
          lwd = 1, col = triangle_color, border = border_color)
}

# Calculamos el area
area = 0
for (i in tl) {
  v <- c(x[i[2]] - x[i[1]], y[i[2]] - y[i[1]])
  w <- c(x[i[3]] - x[i[1]], y[i[3]] - y[i[1]])
  M <- matrix(c(v, w), 2, 2)
  triangle_area = abs(det(M))/2
  area = area + triangle_area
}
area












