################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 22/03/22                                                              #
#                                                                              #
# Practica 5: El plano Afin                                                    #
#                                                                              #
################################################################################

## OPERACIONES EN EL PLANO #####################################################

# Funcion de traslacion
translation <- function(point_list, movement) {
  i <- 1
  result <- list()
  for (point in point_list) {
    result[[i]] <- point + movement
    i <- i + 1
  }
  return(result)
}

# Funcion de rotacion
rotation <- function(point_list, center, angle) {
  turn_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
  
  i <- 1
  result <- list()
  for (point in point_list) {
    result[[i]] <- (turn_matrix %*% (point - center)) + center
    i <- i + 1
  }
  return(result)
}

# Funcion de simetria respecto a una recta
line_symetry <- function(point_list, line) {
  n <- line[1]
  t <- 2*atan(line[2])
  M1 <- c(-n*sin(t), n + n*cos(t))
  M2 <- matrix(c(cos(t), sin(t), sin(t), -cos(t)), 2, 2)
  
  i <- 1
  result <- list()
  for (point in point_list) {
    result[[i]] <- M1 + M2 %*% point
    i <- i + 1
  }
  return(result)
}

# Funcion de homotecia
homothety <- function(point_list, center, ratio) {
  i <- 1
  result <- list()
  for (point in point_list) {
    result[[i]] <- c(center[1] * (1 - ratio),
                     center[2] * (1 - ratio)) + ratio * point
    i <- i + 1
  }
  return(result)
}

## FUNCIONES AUXILIARES ########################################################

# Dibuja una lista de puntos
paint_points <- function(point_list, col) {
  for (point in point_list){
    points(point[1], point[2], pch = 19, col = col)
  }
}

# Funcion para obtener una lista de puntos dado un poligono
poligon_to_point_list <- function(x, y) {
  result <- list()
  for (i in 1 : length(x)) {
    result[[i]] <- c(x[i], y[i])
  }
  return(result)
}

# Funciones para obtener un poligono dada una lista de puntos
point_list_to_poligon_x <- function(point_list) {
  x <- c()
  i <- 1
  for (point in point_list) {
    x[i] <- point[1]
    i <- i + 1
  }
  return(x)
}
point_list_to_poligon_y <- function(point_list) {
  y <- c()
  i <- 1
  for (point in point_list) {
    y[i] <- point[2]
    i <- i + 1
  }
  return(y)
}

## PRUEBAS #####################################################################

# Definimos los colores y los limites de la grafica:
col <- c("#FBB4AE80", "#B3CDE380", "#CCEBC580", "#DECBE480", "#FED9A680")
grey <- "323232"
min <- -30
max <- 30

# Pintamos nuestro sistema de referencia:
plot(c(min, max), c(min, max), type = "n", xlab = "", ylab = "")

# Definimos nuestra lista de puntos y la pintamos:
pl <- list()
pl[[1]] <- c(4, 4)
pl[[2]] <- c(-2, 1)
pl[[3]] <- c(5, -6)
paint_points(pl, col[1])
polygon(point_list_to_poligon_x(pl), point_list_to_poligon_y(pl),
        col = col[1], border = col[1], lwd = 2.5)
legend("topleft", legend = c("Original"), lwd = 5, col = c(col[1]))

# PRUEBA TRASLACION
# Definimos el vector de la traslacion:
movement <- c(-5, 2)
# Aplicamos la traslacion:
result <- translation(pl, movement)
# Pintamos el resultado:
paint_points(result, col[2])
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[2], border = col[2], lwd = 2.5)
legend("topleft",
       legend = c("Original", "Traslacion respecto (-5, 2)"),
       lwd = 5, col = c(col[1], col[2]))

# PRUEBA ROTACION
# Definimos el angulo y centro de la rotacion:
center <- c(0, 0)
angle <- pi/4
# Aplicamos la rotacion:
result <- rotation(pl, center, angle)
# Pintamos el resultado:
paint_points(result, col[3])
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[3], border = col[3], lwd = 2.5)
points(center[1], center[2], pch = 19, col = grey)
legend("topleft",
       legend = c("Original", "Rotacion de centro (0, 0) y angulo \u03c0/4"),
       lwd = 5, col = c(col[1], col[3]))

# PRUEBA SIMETRIA
# Definimos la recta:
line <- c(2, 3)
# Aplicamos la simetria:
result <- line_symetry(pl, line)
# Pintamos el resultado:
paint_points(result, col[4])
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[4], border = col[4], lwd = 2.5)
abline(line[1], line[2], col = grey, lwd = 2.5)
legend("topleft",
       legend = c("Original", "Simetria respecto a la recta y = 3x + 2 "),
       lwd = 5, col = c(col[1], col[4]))

# PRUEBA HOMOTECIA
# Definimos el centro y la razon:
center <- c(1, 2)
ratio <- 4
# Aplicamos la homotecia:
result <- homothety(pl, center, ratio)
# Pintamos el resultado:
paint_points(result, col[5])
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[5], border = col[5], lwd = 2.5)
points(center[1], center[2], pch = 19, col = grey)
legend("topleft",
       legend = c("Original", "Homotecia de razon 4 y centro (1, 2)"),
       lwd = 5, col = c(col[1], col[4]))

## PRUEBAS MARIPOSA ############################################################

# Curva parametrizada con forma de mariposa
t <- seq(0, 2*pi, by = 0.01)
x = sin(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)
y = cos(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)
polygon(x, y, col = col[1], border = col[1], lwd = 2.5)

# Pasamos el poligono de la mariposa a una lista de puntos
pl <- poligon_to_point_list(x, y)

# Traslacion
movement <- c(-5, -5)
result <- translation(pl, movement)
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[2], border = col[2], lwd = 2.5)

# Giro
center <- c(0, -5)
angle <- 3*pi/5
result <- rotation(pl, center, angle)
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[3], border = col[3], lwd = 2.5)

# Simetria
line <- c(9, -2)
result <- line_symetry(pl, line)
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[4], border = col[4], lwd = 2.5)

# Homotecia
center <- c(-5, 3)
ratio <- 2
result <- homothety(pl, center, ratio)
polygon(point_list_to_poligon_x(result), point_list_to_poligon_y(result),
        col = col[5], border = col[5], lwd = 2.5)

# Leyenda
legend("topleft",
       legend = c("Original", "Traslacion", "Giro", "Simetria", "Homotecia"),
       lwd = 5, col = col)
