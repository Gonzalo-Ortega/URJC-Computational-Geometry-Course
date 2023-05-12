################################################################################
# Autores: Gonzalo Ortega - Sergio Hernandez                                   #
#                                                                              #
# Fecha: 22/03/22                                                              #
#                                                                              #
# Practica 4: Algoritmos de ordenacion rapidos                                 #
#                                                                              #
################################################################################

## MEZCLA ######################################################################

# Para este ordenamiento, usamos la funcion merge_sort, la cual utiliza la 
# funcion merge.

# Ordena los elementos de las sublistas formadas por un solo elemento
# devolviendo la lista completa ordenada.
merge <- function(left, right) {
  result <- numeric(length(left) + length(right)) # Lista final.
  i <- 1 # Contador de la parte izqierda.
  j <- 1 # Contador de la parte derecha.
  r <- 1 # Contador del resultado.
  # Vamos comparando los elementos de las sublistas creando sublistas cada vez  
  # mas grandes, y añadiendolos de forma ordenada a la lista final que se
  # devolvera.
  for(r in 1 : length(result)) {
    if((i <= length(left) && left[i] < right[j]) || j > length(right)) {
      result[r] <- left[i]
      i <- i + 1
    } else {
      result[r] <- right[j]
      j <- j + 1
    }
  }
  return(result)
}

# Esta funcion divide una lista en mitades de forma recursiva hasta obtener
# sublistas formadas por un solo elemento.
merge_sort <- function(l) {
  if(length(l) > 1) {
    # Dividimos la lista en dos mitades.
    half <- length(l) %/% 2
    # De forma recursiva, las mitades derecha e izquierda obtenidas en el paso
    # anterior se van dividiendo a su vez en otras dos mitades.
    left <- merge_sort(l[1 : half])
    right <- merge_sort(l[(half + 1) : length(l)])
    # Una vez que tenemos todos los elementos individuales, se llama a la 
    # funcion merge, devolviendo cuando salga la lista ordenada.
    merge(left, right)
  } else {
    return(l)
  }
}

## RAPIDO ######################################################################


# Algoritmo facilmente adaptable a otros tipos de datos
quick_sort <- function(l, first, last) {
  # Elegimos un pivote haciendo la media del primer y el ultimo elemento y 
  # colocamos a su izquierda los elementos menores y a su derecha los elementos
  # mayores.
  i <- first
  j <- last
  pivot <- (l[i] + l[j] + l[(i+j)%/%2])/3
  while (i < j) {
    while (l[i] < pivot) {
      i <- i + 1
    }
    while (l[j] > pivot) {
      j <- j - 1
    }
    if (i <= j) {
      aux <- l [j]
      l[j] <- l[i]
      l[i] <- aux
      i <- i + 1
      j <- j - 1
    }
  }
  # De forma recursiva, repetimos el proceso con la parte derecha e izquierda,
  # obteniendo al final la lista ordenada.
  if (first < j) {
    l <- quick_sort(l, first, j)
  }
  if (last > i) {
    l <- quick_sort(l, i, last)
  }
  return(l)
}

# Algoritmo eficiente completamente
quick_sort <- function(l) {
  if(length(l) > 1){
    rest <- c()
    half <- length(l) %/% 2 
    pivot <- l[half]        
    for(i in 1 : half - 1){
      rest[i] <- l[i]
    }
    next_half <- half + 1
    length <- length(l)
    for(i in next_half : length) {
      rest[i-1] <- l[i]
    }
    left <- quick_sort(rest[rest < pivot])
    right <- quick_sort(rest[rest >= pivot])
    return(c(left, pivot, right))
  } else {
    return(l)
  }
}


## MONTICULOS ##################################################################

# Para este metodo, usamos la funcion heap_sort, la cual utiliza create_heap
# y adjust_heap.

# Crea el arbol binario.
create_heap = function(l, n){
  for(i in (n/2) : 1){
    l = adjust_heap(l, i, n)
  }
  return(l)
}

# Se encarga de que pasado un arbol binario, el padre siempre sea mayor que los
# hijos. Utiliza recursividad.
adjust_heap = function(l, k, n) {
  left = 2*k
  right = 2*k + 1
  max = k
  if(k <= n/2) {
    if(left <= n && l[left] >= l[max]) {
      max = left
    }
    if(right <= n && l[right] >= l[max]) {
      max = right
    }
    if(max != k) {
      aux = l[k]
      l[k] = l[max]
      l[max] = aux
      l = adjust_heap(l, max, n)
    }
  }
  return(l)
}

# En esta funcion se ordena la lista utilizando las funciones anteriores
heap_sort = function(l) {
  n = length(l)
  l = create_heap(l, n)
  for(i in 1 : n) {
    aux = l[n - i + 1]
    l[n - i + 1] = l[1]
    l[1] = aux
    l = adjust_heap(l, 1, n-i)
  }
  return(l)
}

## PRUEBAS #####################################################################

# Definimos el numero de elementos
items <- 100

# Generamos los numeros aleatorios dentro del rango y los
# guardamos en una lista
list <- round(runif(items, 0, items))

# Calculamos el tiempo que tarda en ordenarlos
tiempo <- proc.time()
sort_list <- merge_sort(list)
tiempo <- proc.time() - tiempo
tiempo

# Realizamos lo mismo con los otros dos metodos

list <- round(runif(items, 0, items))
tiempo <- proc.time()
sort_list <- quick_sort(list, 1, length(list))
tiempo <- proc.time() - tiempo
tiempo
sort_list

list <- round(runif(elements, 0, elements))
tiempo <- proc.time()
sort_list <- heap_sort(list)
tiempo <- proc.time() - tiempo
tiempo

















