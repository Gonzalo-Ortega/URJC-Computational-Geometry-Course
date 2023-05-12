################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 15/04/22                                                              #
#                                                                              #
# Practica 8: Localizacion                                                     #
#                                                                              #
################################################################################

## FUNCIONES DEL ALGORITMO #####################################################

# Calcula la distancia entre dos puntos.
distance <- function(p1, p2) {
  d <- sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)
  return(d)
}

# Comprueba si el triangulo formado por tres puntos es agudo.
is_acute_triangule <- function(p1, p2, p3) {
  # Lados del triangulo
  a <- distance(p2, p3)
  b <- distance(p1, p3)
  c <- distance(p1, p2)
  
  # Calculamos el primer angulo
  A <- acos((b^2 + c^2 - a^2) / (2 * b * c))
  # Comprobamos si el angulo es agudo
  if (A >= pi/2) {
    return(FALSE)
  }
  # En caso de serlo, comprobamos si los son los otros dos
  else {
    B <- acos((a^2 + c^2 - b^2) / (2 * a * c))
    return (((B < pi/2) && ((A + B) > pi/2)))
  }
}

# Calcula el circuncentro de tres puntos.
circumcenter <- function(p1, p2, p3) {
  # Lados del triangulo
  a <- distance(p2, p3)
  b <- distance(p1, p3)
  c <- distance(p1, p2)
  
  # Angulos del triangulo
  A <- acos((b^2 + c^2 - a^2) / (2 * b * c))
  B <- acos((a^2 + c^2 - b^2) / (2 * a * c))
  C <- acos((a^2 + b^2 - c^2) / (2 * a * b))
  
  # Circuncentro
  d <- 1 / (sin(2*A) + sin(2*B) + sin(2*C))
  c <- d * c(p1[1]*sin(2*A) + p2[1]*sin(2*B) + p3[1]*sin(2*C),
             p1[2]*sin(2*A) + p2[2]*sin(2*B) + p3[2]*sin(2*C))
  return(c)
}

# Calcula el circulo minimo que contiene los puntos de una lista dada,
# devuelve las dos coordenadas del centro y el radio.
make_smallest_circle <- function(pl) {
  i <- 3
  # PASO 1: Caso base, se elige una pareja de puntos pi, pj.
  p1 <- pl[[1]]
  p2 <- pl[[2]]
  
  while (TRUE) {
    # PASO 2: Constrimos el circulo de diametro C, pi, pj.
    c <- (p1 + p2)/2
    r <- distance(c, p1)
    cir <- c(c[1], c[2], r)
    paint_circle_aux(cir)
    
    # PASO 3: Comprobamos si todos los puntos restantes estan en el circulo.
    while ((i <= length(pl)) && (distance(pl[[i]], c) < r)) {
      i <- i + 1
    }
    if (i > length(pl)) {
      # FIN: En caso de ser asi, se devulve el circulo calculado.
      return(c(c[1], c[2], r))
    }
    # En caso contrario, al menos un punto queda fuera y pasamos al paso 4.
    p3 <- pl[[i]]
    i <- i + 1
    while (TRUE) {
      # PASO 4: Comprobamos si el angulo formado por los tres puntos es agudo.
      polygon(c(p1[1], p2[1], p3[1]), c(p1[2], p2[2], p3[2]), border = grey)
      if (is_acute_triangule(p1, p2, p3)) {
        # PASO 5: En caso de serlo, se calcula el circulo que pasa por los tres.
        c <- circumcenter(p1, p2, p3)
        r <- distance(c, p1)
        cir <- c(c[1], c[2], r)
        paint_circle_aux(cir)
        # Y se comprueba que el resto de puntos queden dentro.
        while ((i <= length(pl)) && (distance(pl[[i]], c) < r)) {
          i <- i + 1
        }
        if (i > length(pl)) {
          # FIN: En caso de ser asi, se devulve el circulo calculado.
          return(c(c[1], c[2], r))
        }
        
        # PASO 6: Si queda alguno fuera, obtenemos Q, el punto mas alejado de
        #         este entre los tres del triangulo.
        p4 <- pl[[i]]
        i <- i + 1
        if ((distance(p1, p4) > distance(p2, p4)) && (distance(p1, p4) > distance(p3, p4))) {
          Q <- p1
          p1 <- p3
        }
        else if ((distance(p2, p4) > distance(p1, p4)) && (distance(p2, p4) > distance(p3, p4))){
          Q <- p2
          p2 <- p3
        }
        else {
          Q <- p3
        }
        
        # PASO 7: Obtenemos R, el punto de los dos sobrantes que esta en el lado
        #         opuesto al punto que se queda fuera respecto de la recta que
        #         pasa por el circuncentro y por Q.
        
        # Se calcula la recta y = ax + b que pasa por Q y el centro.
        a <- (Q[2] - c[2]) / (Q[1] - c[1])
        b <- ( -a * c[1] + c[2])
        
        # Comprobamos si p4 y p1 estan en lados opuestos.
        case1 <- p4[2] < (a * p4[1] + b) && p1[2] > (a * p1[1] + b)
        case2 <- p4[2] > (a * p4[1] + b) && p1[2] < (a * p1[1] + b)
        if (case1 || case2) {
          R <- p1
        }
        else {
          R <- p2
        }
        p1 <- Q
        p2 <- R
        p3 <- p4
        
        # Una vez obtenidos los tres puntos se vuelve al PASO 4.
      }
      # Si el triangulo no era agudo se toman sus dos vertices mas alejados.
      else if (distance(p1, p3) > distance(p2, p3)) {
        p2 <- p3
        break
      }
      else {
        p1 <- p3
        break
      }
      # Y se vuelve al PASO 2
    }
  }
}

## FUNCIONES AUXILIARES ########################################################

# Genera una lista de numeros aleatorios.
random_point_list <- function(number_of_points, min, max) {
  pl <- list()
  for (i in 1 : number_of_points) {
    x <- runif(1, min, max)
    y <- runif(1, min, max)
    pl[[i]] <- c(x, y)
  }
  return(pl)
}

# Pinta los puntos de una lista de puntos.
paint_points <- function(point_list, col) {
  i <- 1
  for (point in point_list) {
    points(point[1], point[2], pch = 19, col = col)
    i <- i + 1
  }
}

# Pinta los puntos de una lista de puntos.
paint_points_text <- function(point_list, col) {
  i <- 1
  for (point in point_list) {
    points(point[1], point[2], pch = 19, col = col)
    text(point[1], point[2],
         pch = 4, labels = i,
         cex = 0.6, pos = 4, col = dark_grey)
    i <- i + 1
  }
}

# Dibuja un circulo dados su sentro y su radio.
paint_circle <- function(circle, color_i) {
  c1 <- circle[1]
  c2 <- circle[2]
  r <- circle[3]
  
  # Parametro para recorrer la curva
  t <- seq(0, 2*pi, by = 0.01)
  
  # Curva parametrizada con forma de mariposa
  x = c1 + r * cos(t)
  y = c2 + r * sin(t)
  
  polygon(x, y, col = col_t[color_i], border = col[color_i], lwd = 2.5)
}

paint_circle_aux <- function(circle) {
  c1 <- circle[1]
  c2 <- circle[2]
  r <- circle[3]
  
  # Parametro para recorrer la curva
  t <- seq(0, 2*pi, by = 0.01)
  
  # Curva parametrizada con forma de mariposa
  x = c1 + r * cos(t)
  y = c2 + r * sin(t)
  
  polygon(x, y, border = grey)
}

## PRUEBAS #####################################################################

col <- c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6")
col_t <- c("#FBB4AE50", "#B3CDE350", "#CCEBC550", "#DECBE450", "#FED9A650")
grey <- "323232"
dark_grey <- "#323232"
ligth_grey <- "#E5E5E5"

number_of_points <- 5
min <- -10
max <- 10

# Generamos una lista aleatoria de puntos
pl <- random_point_list(number_of_points, min/2, max/2)

# Pintamos el sistema de referencia y los puntos.
plot(c(min, max), c(min, max), type = "n", xlab = "", ylab = "")

circle <- make_smallest_circle(pl)

paint_circle(circle, 2)
paint_points(pl, col[2])
points(circle[1], circle[2], pch = 19, col = grey)
text(circle[1], circle[2], pos = 4, col = dark_grey, "O")
