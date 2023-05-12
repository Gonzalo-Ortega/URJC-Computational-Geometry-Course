################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 22/03/22                                                              #
#                                                                              #
# Practica 7: Envolvente convexa                                               #
#                                                                              #
################################################################################

## FUNCIONES AUXILIARES ########################################################

# Genera una lista de numeros aleatorios
random_point_list <- function(number_of_points, min, max) {
  pl <- list()
  for (i in 1 : number_of_points) {
    x <- runif(1, min, max)
    y <- runif(1, min, max)
    pl[[i]] <- c(x, y)
  }
  return(pl)
}

# Dibuja una lista de puntos
paint_points <- function(point_list, col) {
  for (point in point_list){
    points(point[1], point[2], pch = 19, col = col)
  }
}

# Funciones de conversion
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

## FUNCIONES DEL ALGORITMO #####################################################

# Quicksort adaptado para ordenar listas de puntos segun su angulo.
angle_quick_sort <- function(l, first, last) {
  p <- get_higher_point(pl)
  i <- first
  j <- last
  pivot <- (get_angle(p, l[[i]]) + get_angle(p, l[[j]]))/2
  while (i < j) {
    while (get_angle(p, l[[i]]) < pivot) {
      i <- i + 1
    }
    while (get_angle(p, l[[j]]) > pivot) {
      j <- j - 1
    }
    if (i <= j) {
      aux <- l[[j]]
      l[[j]] <- l[[i]]
      l[[i]] <- aux
      i <- i + 1
      j <- j - 1
    }
  }
  if (first < j) {
    l <- angle_quick_sort(l, first, j)
  }
  if (last > i) {
    l <- angle_quick_sort(l, i, last)
  }
  return(l)
}

# Devuelve el angulo formado por la recta horizontal que contiene a p1 y la
# recta que une p1 y p2.
get_angle <- function(p1, p2) {
  # Si la coordenada x es diferente calculamos el angulo.
  if (p2[1] != p1[1]) {
    # Hipotenusa del triangulo.
    h <- sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
    # Cateto opuesto al angulo.
    l <- sqrt((p2[2] - p1[2])^2)
    # Si el p2 esta a la derecha de p1 se devuelve el angulo directamente.
    if (p1[1] < p2[1]) {
      return(asin(l/h))
    }
    # En caso contrario se devuelve el complementario del angulo.
    else {
      return(pi - asin(l/h))
    }
  }
  # Si ambas coordenadas son iguales definimos el angulo como 0.
  else if (p1[1] == p2[1]) {
    return(0)
  }
  # Si la coordenada y es menor, entonces el angulo es pi/2.
  else {
    return(pi/2)
  }
}

# Devuelve el punto con coordenada y mas alta de una lista de puntos. O(n)
get_higher_point <- function(point_list) {
  p1 <- pl[[1]]
  for (p2 in point_list) {
    if (p2[2] > p1[2]) {
      p1 <- p2
    }
  }
  return(p1)
}

# Dados tres puntos, determina si forma un giro a la derecha al recorrerlos en
# orden. O(1)
rigth_turn <- function(p1, p2, p3) {
  # Se calcula la recta y = ax + b que pasa por p1 y p2.
  a <- (p2[2] - p1[2]) / (p2[1] - p1[1])
  b <- ( -a * p1[1] + p1[2])
  # Si recorremos la recta de izquierda a derecha comprobamos si p3 esta por
  # encima de la recta.
  if (p1[1] < p2[1]) {
    return(p3[2] < (a * p3[1] + b))
  }
  # Si se recorre de deerecha a izquierda se comprueba si p3 esta debajo.
  else  {
    return(p3[2] > (a * p3[1] + b))
  }
}

# Dada una lista de puntos ordenada por angulo, devuelve los vertices de la 
# envolvene convexa. O(n*log(n))
make_convex_hull <- function(pl) {
  pl <- angle_quick_sort(pl, 1, length(pl))
  i <- 1
  while (i + 2 <= length(pl)) {
    if (rigth_turn(pl[[i]], pl[[i + 1]], pl[[i + 2]])) {
      i <- i + 1
    }
    else {
      pl <- pl[-(i + 1)]
      i <- i - 1
    }
  }
  return(pl)
}

## PRUEBAS #####################################################################

col <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6")
col_t <- c("#FBB4AE50", "#B3CDE350", "#CCEBC550", "#DECBE450", "#FED9A650")
grey <- "323232"
dark_grey <- "#323232"
ligth_grey <- "#E5E5E5"


number_of_points <- 10
min <- -100
max <- 100

# Generamos una lista aleatoria de puntos
pl <- random_point_list(number_of_points, min, max)

# Pintamos el sistema de referencia y los puntos.
plot(c(min, max), c(min, max), type = "n", xlab = "", ylab = "")
paint_points(pl, col[1])

# Calculamos la envolvente convexa.
hull <- make_convex_hull(pl)

# Pintamos el poligono.
polygon(point_list_to_poligon_x(hull), point_list_to_poligon_y(hull),
        col = col_t[1], border = col[1], lwd = 2.5)

## GRAFICA GIRO ##############################################################

pl <- angle_quick_sort(pl, 1, length(pl))

a <- (pl[[2]][2] - pl[[1]][2]) / (pl[[2]][1] - pl[[1]][1])
b <- (-a * pl[[1]][1] + pl[[1]][2])

abline(b, a, col = grey, lwd = 2.5)

text(pl[[1]][1], pl[[1]][2] + 10, col = dark_grey, bquote(paste('P'['i'])))

text(pl[[2]][1], pl[[2]][2] + 10, col = dark_grey, bquote(paste('P'['j'])))

text(pl[[3]][1], pl[[3]][2] + 10, col = dark_grey, bquote(paste('P'['k'])))

## GRAFICA ANGULO ##############################################################

pl <- angle_quick_sort(pl, 1, length(pl))

t <- list()
t[[1]] <- pl[[1]]
t[[2]] <- pl[[2]]
t[[3]] <- c(pl[[2]][1], pl[[1]][2])
polygon(point_list_to_poligon_x(t), point_list_to_poligon_y(t),
        col = ligth_grey, border = grey, lwd = 2.5)

abline(t[[1]][2], 0, col = grey, lwd = 2.5)
text(90, pl[[1]][2] + 10, col = dark_grey, "r")

points(pl[[1]][1], pl[[1]][2], pch = 19, col = col[1])
text(pl[[1]][1], pl[[1]][2] + 10, col = dark_grey, "P")

points(pl[[2]][1], pl[[2]][2], pch = 19, col = col[1])
text(pl[[2]][1] + 10, pl[[2]][2], col = dark_grey, bquote(paste('P'['i'])))

points(pl[[2]][1], pl[[1]][2], pch = 19, col = grey)
text(pl[[2]][1], pl[[1]][2] + 10, col = dark_grey, bquote(paste('P'['h'])))
