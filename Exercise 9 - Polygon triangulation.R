################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 15/04/22                                                              #
#                                                                              #
# Practica 9: Triangulacion de poligonos                                       #
#                                                                              #
################################################################################

## FUNCIONES DEL ALGORITMO #####################################################

# Calcula la distancia entre dos puntos.
distance <- function(p1, p2) {
  d <- sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(d)
}

# Comprueba si el triangulo formado por tres puntos es agudo.
angle <- function(p1, p2, p3) {
  # Lados del triangulo
  a <- distance(p2, p3)
  b <- distance(p1, p3)
  c <- distance(p1, p2)
  # Calculamos el angulo
  B <- acos((a^2 + c^2 - b^2) / (2 * a * c))
  if (rigth_turn(p1, p2, p3)) {
    return(B)
  }
  else {
    return(2*pi - B)
  }
}

# Comprueba si tres puntos hacen un giro a la derecha.
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

# Devuelve el pligono dado recorrido en sentido horario.
clockwise_polygon <- function(pl) {
  n <- length(pl)
  total_angle <- 0
  
  for (i in seq(1, n)) {
    p1 <- pl[[((i - 2) %% n) + 1]]
    p2 <- pl[[((i - 1) %% n) + 1]]
    p3 <- pl[[((i) %% n) + 1]]
    total_angle <- total_angle + angle(p1, p2, p3)
  }
  total_angle <- round(total_angle, 5)
  expected_angle <- round(pi*(n-2), 5)
  #print(total_angle)
  #print(expected_angle)
  if (total_angle == (expected_angle)) {
    #print("SIN cambio de sentido")
    return(pl)
  }
  clockwise_polygon <- list()
  for (i in seq(1, n)) {
    clockwise_polygon[[i]] <- pl[[n - i + 1]]
  }
  #print("cambio de sentido")
  return(clockwise_polygon)
}

# Verifica si dos segmentos se cortan.
edge_cut <- function(e1, e2) {
  # Extremos del primer segmento
  A <- e1[1:2]
  B <- e1[3:4]
  #Extremos del segundo segmento
  C <- e2[1:2]
  D <- e2[3:4]
  
  # Comprobamos si las proyecciones de los segmentos se solapan.
  if(!((A[1] > C[1] && A[1] < D[1]) ||
       (A[1] > D[1] && A[1] < C[1]) ||
       (B[1] > C[1] && B[1] < D[1]) ||
       (B[1] > D[1] && B[1] < C[1]) ||
       (C[1] > A[1] && C[1] < B[1]) ||
       (C[1] > B[1] && C[1] < A[1]) ||
       (D[1] > A[1] && D[1] < B[1]) ||
       (D[1] > B[1] && D[1] < A[1]))) {
    return(FALSE)
  }
  
  # Comprobamos si los puntos de cada segmento estan en lados opuestos del otro.
  M1 = matrix(c(A-D, C-D), 2, 2)
  M2 = matrix(c(B-D, C-D), 2, 2)
  M3 = matrix(c(C-B, A-B), 2, 2)
  M4 = matrix(c(D-B, A-B), 2, 2)
  return(((det(M1)*det(M2) < 0) && (det(M3)*det(M4) < 0)))
}

# Comprueba si un segmento se cruza con algun otro de una lista.
exist_cut <- function(e1, edge_list) {
  for (e2 in edge_list) {
    if (edge_cut(e1, e2)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Devuelve una lista con las aristas del poligono.
polygon_edges <- function(pl) {
  polygon_edges_list <- list()
  l <- length(pl)
  for (i in seq(0, length(pl)-1)) {
    p1 <- pl[[(i %% l) + 1]]
    p2 <- pl[[((i + 1) %% l) + 1]]
    polygon_edges_list[[i+1]] <- c(p1, p2)
  }
  return(polygon_edges_list)
}

# Devuelve una lista con las diagonales interiores del poligono.
valid_edges <- function(pl) {
  valid_edges <- list()
  k <- 1
  polygon_edges <- polygon_edges(pl)
  l <- length(pl)
  
  for (i in seq(1, l - 1)) {
    p1 <- pl[[((i - 2) %% l) + 1]]
    p2 <- pl[[((i - 1) %% l) + 1]]
    p3 <- pl[[((i) %% l) + 1]]
    polygon_angle <- angle(p1, p2, p3)
    
    for (j in seq(i + 1, l - 1)) {
      p3 <- pl[[(j %% l) + 1]]
      edge_angle <- angle(p1, p2, p3)
      edge <- c(p2, p3)
      
      if (((edge_angle < polygon_angle) && !(exist_cut(edge, polygon_edges)))) {
        valid_edges[[k]] <- edge
        k <- k + 1
      }
    }
  }
  return(valid_edges)
}

# Ordena una lista de aristas segun su longitud en orden creciente.
distance_quick_sort <- function(l, first, last) {
  i <- first
  j <- last
  a <- c(l[[i]][1], l[[i]][2])
  b <- c(l[[i]][3], l[[i]][4])
  c <- c(l[[j]][1], l[[j]][2])
  d <- c(l[[j]][3], l[[j]][4])
  d1 <- distance(a, b)
  d2 <- distance(c, d)
  pivot <- (d1 + d2)/2
  while (i < j) {
    while (d1 < pivot) {
      i <- i + 1
      a <- c(l[[i]][1], l[[i]][2])
      b <- c(l[[i]][3], l[[i]][4])
      d1 <- distance(a, b)
    }
    while (d2 > pivot) {
      j <- j - 1
      c <- c(l[[j]][1], l[[j]][2])
      d <- c(l[[j]][3], l[[j]][4])
      d2 <- distance(c, d)
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
    l <- distance_quick_sort(l, first, j)
  }
  if (last > i) {
    l <- distance_quick_sort(l, i, last)
  }
  return(l)
}

# Selecciona diagonales que no se crucen entre las diagonales validas.
triangulate <- function(pl) {
  pl <- clockwise_polygon(pl)
  valid_edges <- valid_edges(pl)
  #paint_edges(valid_edges, grey)
  ordered_edges <- distance_quick_sort(valid_edges, 1, length(valid_edges))
  triangulation_edges <- list()
  k <- 1
  for (edge in ordered_edges) {
    if (!(exist_cut(edge, triangulation_edges))) {
      triangulation_edges[[k]] <- edge
      k <- k + 1
    }
  }
  return(triangulation_edges)
}

## FUNCIONES AUXILIARES ########################################################

# Devuelve un poligono hecho a mano.
hand_made_polygon <- function() {
  pl <- list()
  pl[[1]] <- c(-100, -25)
  pl[[2]] <- c(-75, 50)
  pl[[3]] <- c(-60, 0)
  pl[[4]] <- c(-40, 0)
  pl[[5]] <- c(-25, 50)
  pl[[6]] <- c(-10, 0)
  pl[[7]] <- c(10, 0)
  pl[[8]] <- c(25, 50)
  pl[[9]] <- c(40, 0)
  pl[[10]] <- c(60, 0)
  pl[[11]] <- c(75, 50)
  pl[[12]] <- c(100, -25)
  return(pl)
}

# Devuleve un poligono aleatorio de n vertices.
random_polygon <- function(n, min, max) {
  wait_time <- 0.01
  repeat {
    # Se inicializa una lista de vertices y otra de aristas
    pl <- list()
    edge_list <- list()
    # Se anade el primer vertice
    x <- runif(1, min, max)
    y <- runif(1, min, max)
    pl[[1]] <- c(x, y)

    # Para cada vertice se comprueba que forme una arista valida
    for (i in seq(2, n - 1)) {
      initial_time <- proc.time()[1]
      
      repeat {
        x <- runif(1, min, max)
        y <- runif(1, min, max)
        edge <- c(pl[[i-1]][1], pl[[i-1]][2], x, y)
        time <- proc.time()[1] - initial_time
        #print(time)
        if(!exist_cut(edge, edge_list)) {
          pl[[i]] <- c(x, y)
          edge_list[[i-1]] <- edge
          print(i)
          break
        }
        if (time > wait_time) {
          print("out of time")
          break
        }
      }
      if (time > wait_time) {
        break
      }
    }
    if (time > wait_time) {
      next
    }
    initial_time <- proc.time()[1]
    repeat {
      x <- runif(1, min, max)
      y <- runif(1, min, max)
      edge1 <- c(pl[[n-1]][1], pl[[n-1]][2], x, y)
      edge2 <- c(pl[[1]][1], pl[[1]][2], x, y)
      if(!exist_cut(edge1, edge_list) && !exist_cut(edge2, edge_list)) {
        pl[[n]] <- c(x, y)
        return(pl)
      }
      time <- proc.time()[1] - initial_time
      if (time > wait_time) {
        print("out of time")
        break
      }
    }
  }
}

# Devuelve un poligono regular de n vertices.
regular_polygon <- function(n, r) {
  # Parametro para recorrer la curva
  t <- seq(0, 2*pi-2*pi/n, by = 2*pi/n)
  
  # Curva parametrizada con forma de circunferencia
  x = r * sin(t)
  y = r * cos(t)
  
  pl <- poligon_to_point_list(x, y)
  return(pl)
}

# Devuelve un "poligono" en forma de mariposa.
butterfly_polygon <- function(n, r) {
  # Parametro para recorrer la curva
  t <- seq(0, 2*pi-2*pi/n, by = 2*pi/n)
  
  # Curva parametrizada con forma de mariposa
  x = (sin(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)) * r
  y = (cos(t) * (exp(1)^cos(t) - 2*cos(4*t) - sin(t/12)^5)) * r - max/5
  
  pl <- poligon_to_point_list(x, y)
  return(pl)
}

# Pinta una lista de vertices.
paint_edges <- function(edges_list, col) {
  for (edge in edges_list) {
    segments(edge[1], edge[2], edge[3], edge[4], col = col)
  }
}

# Pasa dos listas de coordenadas a una lista de puntos.
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

col <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6")
col_t <- c("#FBB4AE50", "#B3CDE350", "#CCEBC550", "#DECBE450", "#FED9A650")
grey <- "323232"
dark_grey <- "#323232"
ligth_grey <- "#E5E5E5"

min <- -100
max <- 100
n <- 30

polygon <- hand_made_polygon()
polygon <- butterfly_polygon(100, max/3)
polygon <- regular_polygon(n, max)
polygon <- random_polygon(n, min, max)

plot(c(min, max), c(min, max), type = "n", xlab = "", ylab = "")
polygon(point_list_to_poligon_x(polygon), point_list_to_poligon_y(polygon),
        col = col_t[4], border = col[4], lwd = 2.5)

t <- triangulate(polygon)
paint_edges(t, col[4])
