#############################
####### Laboratorio 1 #######
#############################
# Autor: Marcos Hidalgo Baños 

# Definicion del dataframe
df <- data.frame("x1"=c(
  2.771244718,
  1.728571309,
  3.678319846,
  3.961043357,
  2.999208922,
  7.497545867,
  9.00220326,
  7.444542326,
  10.12493903,
  6.642287351
),
"x2"=c(
  1.784783929,
  1.169761413,
  2.81281357,
  2.61995032,
  2.209014212,
  3.162953546,
  3.339047188,
  0.476683375,
  3.234550982,
  3.319983761
),
"y"=c(
  0,
  0,
  0,
  0,
  0,
  1,
  1,
  1,
  1,
  1
))

#Funcion para obtener la division del arbol
# @param df ; Dataframe con los datos
# @param y ; Nombre de la variable que distingue entre clases
DividirArbol <- function(df, y) {
  
  # Nombre de la variable que va a divir mejor
  nombre_var = NULL
  # Valor que va a ser el umbral para dividir
  valor_umbral = NULL  
  # Inicializamos el contador al mayor valor posible
  varianza_min = Inf
  
  # Iteramos sobre las columnas (variables) del df
  for (variable in colnames(df)) {
    valores = unique(df[[variable]]) # Eliminados los posibles repetidos
    # Iteramos para cada variable sus correspondientes valores
    for (valor in valores) {
      # Realizamos la particion en regiones
      izq = df[df[[variable]] < valor,]
      der = df[df[[variable]] >= valor,]
      
      # Calculamos la media de ambas
      media_izq = mean(izq[[y]])
      media_der = mean(der[[y]])
      
      # Calculamos la suma de la varianza
      varianza = sum((izq[[y]] - media_izq)^2) +
        sum((der[[y]] - media_der)^2)
      
      # Vemos si hemos mejorado con esta particion
      if (varianza < varianza_min) {
        varianza_min = varianza
        nombre_var = variable
        valor_umbral = valor
      }
    }
  }
  return(c(nombre_var, valor_umbral, varianza_min))
}

# Funcion para comprobar si hemos llegado a un nodo hoja
checkLeaf <- function(df, col) {
  return(all(df$col == df$col[1]))
}

# Funcion para comprobar si hay que dividir en regiones o no
divRegiones <- function(df, col) {
  
  # Comprobamos si hemos encontrado un nodo hoja
  if (checkLeaf(df, col)) {
    cat("Etiqueta Nodo -> ", df[1,col])
    
  } else {
    # Continuamos el algortimo por la otra rama 
    div = DividirArbol(df, col)
    cat("Mejor variable para dividir -> ", div[1])
    cat("Mejor valor para la division -> ", div[2])
    cat("Varianza minima -> ", div[3])
    
    df_izq = df[df[div[1]] < as.double(div[2]),]
    df_der = df[df[div[1]] >= as.double(div[2]),]
  
    return(c(df_izq, df_der))
  }

}


######################################################
# Apartado A) Realizar la primera división del árbol
# y encontrar la variable y el valor de dicha división
######################################################

cat("----- Nivel 0 / Nodo Raiz -----")
division = DividirArbol(df, "y")
cat("Mejor variable para dividir -> ", division[1])
cat("Mejor valor para la division -> ", division[2])
cat("Varianza minima -> ", division[3])

df_izq = df[df[division[1]] < as.double(division[2]),]
df_der = df[df[division[1]] >= as.double(division[2]),]


####################################################################
# Apartado B) Determinar dos árboles T1 y T2 de dos niveles tal que 
# T1 explote la región derecha (es decir R2) con el algoritmo 
# descrito anteriormente, mientras que T2 explota la izquierda
####################################################################

cat("-------- Arbol 1: Primero R1 ------------")

cat("----- Nivel 0 / Nodo Raiz -----")
cat("Mejor variable para dividir -> ", division[1])
cat("Mejor valor para la division -> ", division[2])
cat("Varianza minima -> ", division[3])

cat("----- Nivel 1 / Rama Izquierda -----")
divRegiones(df_izq, "y")

cat("----- Nivel 1 / Rama Derecha -----")
divRegiones(df_der, "y")

#################################################

cat("-------- Arbol 2: Primero R2 ------------")

cat("----- Nivel 0 / Nodo Raiz -----")
cat("Mejor variable para dividir -> ", division[1])
cat("Mejor valor para la division -> ", division[2])
cat("Varianza minima -> ", division[3])

cat("----- Nivel 1 / Rama Derecha -----")
divRegiones(df_der, "y")

cat("----- Nivel 1 / Rama Izquierda -----")
divRegiones(df_izq, "y")

#################################################
