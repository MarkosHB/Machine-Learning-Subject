############################################
###### Lab 2 - Support Vector Machine ######
####### Autor: Marcos Hidalgo Baños ########
############################################

# Importamos las Librer�as necesarias
library (kernlab)
library(e1071)

# Creamos la funci�n que dir� a que clase pertenece cada punto
print_clasificacion <- function (x, w, b) {
  if ((t (w) %*% x + b) >= 0)
  {
    print (x)
    print("Pertence a la clase: 1")
  }
  else
  {
    print (x)
    print("Pertence a la clase: -1")
  }
}

# Funcion que obtiene el SVM del df proporcionado
calcularDatosPractica <- function(df) {
  
  # Indicamos que la columna y es la importante
  df$y <- as.factor(df$y)
  
  # Creamos el SVM de los datos con un kernel lineal
  svm <- svm(y~., df , kernel="linear")
  
  #Vectores de soporte
  vs <- df[svm$index,1:2]
  
  # Vector de pesos normal al hiperplano (W)
  # Hacemos el CrosProduct entre los vectores soporte y el coe. de Lagrange
  w <- crossprod(as.matrix(vs), svm$coefs)
  
  # Calcular ancho del canal
  width = 2/(sqrt(sum((w)^2)))
  
  # Calcular vector B
  b <- -svm$rho
  
  return(list(svm=svm, vs=vs, w=w, width=width, b=b))
}

############################################
################ APARTADO A ################
############################################

# Creamos el conjunto de datos
dataA <- data.frame(
  x1 = c(0, 4),
  x2 = c(0, 4),
  y = c(1, -1)
)

# Puntos
A1=c(0,0)
A2=c(4,4)

# Hacemos la magia...
solA = calcularDatosPractica(dataA)

# 1) Vectores Soporte
print(solA$vs)
plot(solA$svm, dataA)

# 2) Calculamos los valores del kernel
cat("KAA = ", t (A1) %*% A1)
cat("KAB = ", t (A1) %*% A2)
cat("KBB = ", t (A2) %*% A2)

# 3) Ancho del canal
print(solA$width)

# 4) Vector de pesos normal al Hiperplano
print(solA$w)

# 5) Vector B
print(solA$b)

# 6) Calculamos la ecuacion del hiperplano y de los planos de soporte positivo y negativo
paste(c("[",solA$w,"]' * x + [",solA$b,"] = 0"), collapse=" ")
paste(c("[",solA$w,"]' * x + [",solA$b,"] = 1"), collapse=" ")
paste(c("[",solA$w,"]' * x + [",solA$b,"] = -1"), collapse=" ")

# 7) Determinamos a la clase que pertenece cada uno
print_clasificacion(A1, solA$w, solA$b)
print_clasificacion(A2 ,solA$w, solA$b)


############################################
################ APARTADO B ################
############################################

# Creamos el conjunto de datos
dataB <- data.frame(
  x1 = c(2, 0, 1),
  x2 = c(0, 0, 1),
  y = c(1, -1, -1)
)

# Puntos
B1=c(2,0)
B2=c(4,4)
B3=c(1,1)

# Hacemos la magia...
solB = calcularDatosPractica(dataB)

# 1) Vectores Soporte
print(solB$vs)
plot(solB$svm, dataB)

# 2) Calculamos los valores del kernel
cat("KAA = ", t (B1) %*% B1)
cat("KAB = ", t (B1) %*% B2)
cat("KAC = ", t (B1) %*% B3)
cat("KBB = ", t (B2) %*% B2)
cat("KBC = ", t (B2) %*% B3)
cat("KCC = ", t (B3) %*% B3)

# 3) Ancho del canal
print(solB$width)

# 4) Vector de pesos normal al Hiperplano
print(solB$w)

# 5) Vector B
print(solB$b)

# 6) Calculamos la ecuacion del hiperplano y de los planos de soporte positivo y negativo
paste(c("[",solB$w,"]' * x + [",solB$b,"] = 0"), collapse=" ")
paste(c("[",solB$w,"]' * x + [",solB$b,"] = 1"), collapse=" ")
paste(c("[",solB$w,"]' * x + [",solB$b,"] = -1"), collapse=" ")

# 7) Determinamos a la clase que pertenece cada uno
print_clasificacion(c(5, 6),solB$w, solB$b)
print_clasificacion(c(1, -4),solB$w, solB$b)

############################################
################ APARTADO C ################
############################################

# Creamos el conjunto de datos
dataC <- data.frame(
  x1 = c(2, 2, -2, -2, 1, 1, -1, -1),
  x2 = c(2, -2, -2, 2, 1, -1, 1, -1),
  y = c(1, 1, 1, 1, -1, -1, -1, -1)
)

#Puntos
C1 = c(2,2)
C2 = c(2, -2)
C3 = c(-2, -2)
C4 = c(-2, 2)
C5 = c(1,1)
C6 = c(1,-1)
C7 = c(-1,1)
C8 = c(-1,-1)

listaPuntosC = list(C1,C2,C3,C4,C5,C6,C7,C8)

# Hacemos la magia...
solC = calcularDatosPractica(dataC)

# 1) Vectores Soporte
print(solC$vs)

# 2) Calculamos los valores del kernel
listaKernelC = list()
for (i in 1:length(listaPuntosC)) {
  for (j in i:length(listaPuntosC)) {
    listaKernelC <- append(listaKernelC, t (listaPuntosC[[i]]) %*% listaPuntosC[[j]])
  }
}

# 3) Ancho del canal
print(solC$width) # Inf.

############
# CONCLUSION 
############
# Vemos que el ancho del canal obtenido es
# infinito, por lo que podemos decir que no 
# se puede establecer una clara division
# de manera lineal. 
# Intentaremos transformar los puntos


############################################
################ APARTADO D ################
############################################

funcion_transformacion <- function(a) {
  if(sqrt(a[1]^2 + a[2]^2) > 2) {
    a[1] <- 4 - a[2] + abs(a[1] - a[2])
    a[2] <- 4 - a[1] + abs(a[1] - a[2])
  }
  a
}

# Aplicamos la transformacion
D1 = funcion_transformacion(C1)
D2 = funcion_transformacion(C2)
D3 = funcion_transformacion(C3)
D4 = funcion_transformacion(C4)
D5 = funcion_transformacion(C5)
D6 = funcion_transformacion(C6)
D7 = funcion_transformacion(C7)
D8 = funcion_transformacion(C8)

listaPuntosD = list(D1,D2,D3,D4,D5,D6,D7,D8)

# Montamos el nuevo dataframe 
dataD = data.frame(
  x1 = c(D1[1], D2[1], D3[1], D4[1],
         D5[1], D6[1], D7[1], D8[1]),
  x2 = c(D1[2], D2[2], D3[2], D4[2],
         D5[2], D6[2], D7[2], D8[2]),
  y = c(1,1,1,1,-1,-1,-1,-1)
)

# Hacemos la magia...
solD = calcularDatosPractica(dataD)

# 1) Vectores Soporte
print(solD$vs)
plot(solD$svm, dataD)

# 2) Calculamos los valores del kernel
listaKernelD = list()
for (i in 1:length(listaPuntosD)) {
  for (j in i:length(listaPuntosD)) {
    listaKernelD <- append(listaKernelD, t (listaPuntosD[[i]]) %*% listaPuntosD[[j]])
  }
}

# 3) Ancho del canal
print(solD$width)

# 4) Vector de pesos normal al Hiperplano
print(solD$w)

# 5) Vector B
print(solD$b)

# 6) Calculamos la ecuacion del hiperplano y de los planos de soporte positivo y negativo
paste(c("[",solD$w,"]' * x + [",solD$b,"] = 0"), collapse=" ")
paste(c("[",solD$w,"]' * x + [",solD$b,"] = 1"), collapse=" ")
paste(c("[",solD$w,"]' * x + [",solD$b,"] = -1"), collapse=" ")

# 7) Determinamos a la clase que pertenece cada uno
print_clasificacion(c(5, 6),solD$w, solD$b)
print_clasificacion(c(1, -4),solD$w, solD$b)


############################################
################ APARTADO E ################
############################################

# Creamos el conjunto de datos
dataE <- data.frame(
  x1 = c(3, 3, 6, 6, 1, 0, 0, -1),
  x2 = c(1, -1, 1, -1, 0, 1, -1, 0),
  y = c(1, 1, 1, 1, -1, -1, -1, -1)
)

E1 = c(3,1)
E2 = c(3,-1)
E3 = c(6,1)
E4 = c(6,-1)
E5 = c(1,0)
E6 = c(0,1)
E7 = c(0,-1)
E8 = c(-1,0)

listaPuntosE = list(E1,E2,E3,E4,E5,E6,E7,E8)

# Hacemos la magia...
solE = calcularDatosPractica(dataE)

# 1) Vectores Soporte
print(solE$vs)
plot(solE$svm, dataE)

# 2) Calculamos los valores del kernel
listaKernelE = list()
for (i in 1:length(listaPuntosE)) {
  for (j in i:length(listaPuntosE)) {
    listaKernelE <- append(listaKernelE, t (listaPuntosE[[i]]) %*% listaPuntosE[[j]])
  }
}

# 3) Ancho del canal
print(solE$width)

# 4) Vector de pesos normal al Hiperplano
print(solE$w)

# 5) Vector B
print(solE$b)

# 6) Calculamos la ecuacion del hiperplano y de los planos de soporte positivo y negativo
paste(c("[",solE$w,"]' * x + [",solE$b,"] = 0"), collapse=" ")
paste(c("[",solE$w,"]' * x + [",solE$b,"] = 1"), collapse=" ")
paste(c("[",solE$w,"]' * x + [",solE$b,"] = -1"), collapse=" ")

# 7) Determinamos a la clase que pertenece cada uno
print_clasificacion(c(4,5), solE$w, solE$b)


############################################
################ APARTADO F ################
############################################

data(iris)
colnames(iris)[colnames(iris) == "Species"] <- "y"

# Hacemos la magia...
solIRIS = calcularDatosPractica(iris)

# 1) Vectores Soporte
print(solIRIS$vs)
# plot(solIRIS$svm, iris)

# 3) Ancho del canal
print(solIRIS$width)

# 4) Vector de pesos normal al Hiperplano
print(solIRIS$w)

# 5) Vector B
print(solIRIS$b)

# 6) Calculamos la ecuacion del hiperplano y de los planos de soporte positivo y negativo
paste(c("[",solIRIS$w[1,1],"]' * x1 + [",solIRIS$b,"] = 0"), collapse=" ")
paste(c("[",solIRIS$w,"]' * x + [",solIRIS$b,"] = 1"), collapse=" ")
paste(c("[",solIRIS$w,"]' * x + [",solIRIS$b,"] = -1"), collapse=" ")

# 7) Determinamos a la clase que pertenece cada uno
flor_ejemplo = iris[1, 1:4] # Flor setosa (la primera del df)
predict(solIRIS$svm, flor_ejemplo)


##########################################################################################
##########################################################################################