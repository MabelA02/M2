# Definimos la función para operar matrices cuadradas
operar_matrices <- function(mat1, mat2, operacion) {
  # Verificamos que las matrices sean cuadradas y de las mismas dimensiones
  if (!is.matrix(mat1) || !is.matrix(mat2)) {
    stop("Ambos argumentos deben ser matrices.")
  }
  
  if (nrow(mat1) != ncol(mat1) || nrow(mat2) != ncol(mat2)) {
    stop("Ambas matrices deben ser cuadradas.")
  }
  
  if (nrow(mat1) != nrow(mat2)) {
    stop("Las matrices deben tener las mismas dimensiones.")
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

# Ejemplo de uso
matriz1 <- matrix(1:4, nrow = 2)
matriz2 <- matrix(5:8, nrow = 2)

# Suma
resultado_suma <- operar_matrices(matriz1, matriz2, "suma")
print("Resultado de la suma:")
print(resultado_suma)

# Resta
resultado_resta <- operar_matrices(matriz1, matriz2, "resta")
print("Resultado de la resta:")
print(resultado_resta)

# Multiplicación
resultado_multiplicacion <- operar_matrices(matriz1, matriz2, "multiplicacion")
print("Resultado de la multiplicación:")
print(resultado_multiplicacion)

# Transposición
resultado_transposicion <- operar_matrices(matriz1, NULL, "transposicion")
print("Resultado de la transposición:")
print(resultado_transposicion)
