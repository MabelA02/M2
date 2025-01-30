# Definimos la función para operar matrices cuadradas
operar_matrices <- function(mat1, mat2 = NULL, operacion) {
  # Verificamos que las matrices sean cuadradas y de las mismas dimensiones
  if (operacion != "transposicion") {
    if (!is.matrix(mat1) || !is.matrix(mat2)) {
      stop("Ambos argumentos deben ser matrices.")
    }
    
    if (nrow(mat1) != ncol(mat1) || nrow(mat2) != ncol(mat2)) {
      stop("Ambas matrices deben ser cuadradas.")
    }
    
    if (nrow(mat1) != nrow(mat2)) {
      stop("Las matrices deben tener las mismas dimensiones.")
    }
  } else {
    if (!is.matrix(mat1)) {
      stop("El primer argumento debe ser una matriz.")
    }
    
    if (nrow(mat1) != ncol(mat1)) {
      stop("La matriz debe ser cuadrada.")
    }
  }
  
  # Realizamos la operación solicitada
  switch(operacion,
         suma = return(mat1 + mat2),
         resta = return(mat1 - mat2),
         multiplicacion = return(mat1 %*% mat2),
         transposicion = return(t(mat1)),
         stop("Operación no válida. Usa 'suma', 'resta', 'multiplicacion' o 'transposicion'.")
  )
}

# Función para leer una matriz desde la entrada del usuario
leer_matriz <- function(nombre) {
  cat(paste("Ingresa los elementos de la matriz", nombre, "(separados por espacios):\n"))
  elementos <- scan(what = numeric(), nmax = 100, quiet = TRUE)
  
  cat("Ingresa el número de filas de la matriz:\n")
  filas <- as.integer(readline())
  
  # Calculamos el número de columnas
  columnas <- length(elementos) / filas
  
  if (columnas != round(columnas)) {
    stop("El número de elementos no es compatible con el número de filas.")
  }
  
  matriz <- matrix(elementos, nrow = filas, byrow = TRUE)
  return(matriz)
}

# Leer las matrices del usuario
matriz1 <- leer_matriz("1")
matriz2 <- leer_matriz("2")

# Elegir la operación
cat("Elige la operación (suma, resta, multiplicacion, transposicion):\n")
operacion <- readline()

# Realizar la operación
if (operacion == "transposicion") {
  resultado_transposicion <- operar_matrices(matriz1, operacion = "transposicion")
  print("Resultado de la transposición:")
  print(resultado_transposicion)
} else {
  resultado <- operar_matrices(matriz1, matriz2, operacion)
  print(paste("Resultado de la", operacion, ":"))
  print(resultado)
}
