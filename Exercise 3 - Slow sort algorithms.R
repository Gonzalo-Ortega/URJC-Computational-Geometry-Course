################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 22/03/22                                                              #
#                                                                              #
# Practica 3: Algoritmos de ordenacion lentos                                  #
#                                                                              #
################################################################################

## BURBUJA #####################################################################

# Mientras que la lista no esta ordenada y no se haya recorrida entera, se van 
# seleccionando pares de elementos adyacentes, intercambiandolos de posicion, si
# no estan ordenados, hasta llegar al final.
bubble_sort <- function(l) {
  i <- 1
  sorted <- FALSE
  while (i < length(l) && !sorted) {
    sorted <- TRUE
    for (j in seq(1, length(l) - i)) {
      if (l[j] > l[j + 1]) {
        sorted <- FALSE
        aux <- l[j + 1]
        l[j + 1] <- l[j]
        l[j] <- aux
      }
    }
    i <- i + 1
  }
  return(l)
}

## INSERCCION ##################################################################

# Se recorre la lista seleccionando en cada iteracion un elemento, este se va 
# comparando con el resto y se inserta en el lugar correpondiente.
insertion_sort <- function(l) {
  for (i in 1 : length(l)) {
    aux <- l[i]
    j <- i - 1
    while (j > 0 && l[j] > aux) {
      l[j + 1] <- l[j]
      j <- j - 1
    }
    l[j + 1] <- aux
  }
  return(l)
}

## SELECCION ###################################################################

# Se busca el elemento mas pequenno en la lista, se intercambia con la primera
# posicion y asi sucesivamente hasta ordenar toda la lista.
selection_sort <- function(l) {
  for (i in 1 : (length(l) - 1)) {
    min = i
    aux = l[i]
    for(j in i : length(l)) {
      if(l[j] < aux) {
        min = j
        aux = l[j]
      }
    }
    l[min] = l[i]
    l[i] = aux
  }
  return(l)
}

## PRUEBAS #####################################################################

# Definimos el numero de elementos
elements <- 1500

# Generamos los numeros aleatorios dentro del rango y los
# guardamos en una lista.
list <- round(runif(elements, 0, elements))

#Calculamos el tiempo que tarda en ordenarlos
tiempo <- proc.time()
sorted_list <- bubble_sort(list)
tiempo <- proc.time() - tiempo
tiempo

# Realizamos lo mismo con los otros dos metodos.

tiempo <- proc.time()
sorted_list <- insertion_sort(list)
tiempo <- proc.time() - tiempo
tiempo

tiempo <- proc.time()
sorted_list <- selection_sort(list)
tiempo <- proc.time() - tiempo
tiempo




