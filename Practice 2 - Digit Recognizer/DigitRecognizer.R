##############################################################
########### Practica Evaluable 2. Digit Recognizer ###########
##############################################################

# Autores del script:
#   Francisco Velasco Romero, Joaquín Fernández Suárez
#   Marcos Hidalgo Baños, Rubén Cazorla Rodríguez

library(readr)
library(randomForest)
library(caret)
library(e1071)
library(adabag)

path = "C:/Users/usuario/Downloads/train/train.csv"
path_test = "C:/Users/usuario/Downloads/test/test.csv"

####################################
### Preprocesamiento del dataset ###
####################################

# Importar los datos
train <- read_csv(path)
test <- read_csv(path_test)

# Utilizar solo la mitad del conjunto de datos (debugging)
#train <- train[1:(nrow(train)/4), ]

# Guardar la columna 'label'
train$label <- as.factor(train$label)
labels <- train$label
train_without_labels <- train[, -1]

# Redimensionar las imágenes de 28x28 a 14x14 tomando el promedio de bloques 2x2
preprocesar_imagenes <- function(data) {
  matriz_imagenes <- matrix(0, nrow = nrow(data), ncol = 196)  # 14x14 = 196
  for (k in 1:nrow(data)) {
    img <- matrix(as.numeric(data[k, ]), nrow = 28, ncol = 28, byrow = TRUE)
    resized_img <- matrix(0, nrow = 14, ncol = 14)
    for (i in 1:14) {
      for (j in 1:14) {
        block <- img[(2*i-1):(2*i), (2*j-1):(2*j)]
        resized_img[i, j] <- mean(block)
      }
    }
    matriz_imagenes[k, ] <- as.numeric(resized_img)
  }
  return(matriz_imagenes)
}

# Aplicar preprocesamiento a todo el conjunto de datos
train_preprocesado <- preprocesar_imagenes(train_without_labels)

#aplicamos todo el preprocesamiento al test y lo incluimos en un dataframe
test_reducido <- preprocesar_imagenes(test)
test_preprocesado <- data.frame(test_reducido)

# Crear partición para conjunto de entrenamiento y prueba
set.seed(123)
particion <- createDataPartition(labels, p = 0.8, list = FALSE)
conjunto_entrenamiento <- data.frame(label = labels[particion], train_preprocesado[particion, ])
conjunto_prueba <- data.frame(label = labels[-particion], train_preprocesado[-particion, ])


###############################
### Modelo 1) Random Forest ###
###############################

# Entrenar un modelo de random forest
modelo_rf <- randomForest(label ~ ., data = conjunto_entrenamiento)

# Realizar predicciones en el conjunto de prueba
predicciones_rf <- predict(modelo_rf, conjunto_prueba)

# Evaluar el rendimiento del modelo
confusionMatrix(predicciones_rf, conjunto_prueba$label)

# Calcular y mostrar el accuracy
accuracy_rf <- mean(predicciones_rf == conjunto_prueba$label)
cat("Accuracy RF:", accuracy_rf, "\n") # 0.95


#####################
### Modelo 2) SVM ###
#####################

# Entrenar un modelo de SVM
modelo_svm <- svm(label ~ ., data = conjunto_entrenamiento)
predicciones_svm <- predict(modelo_svm, conjunto_prueba)

# Evaluar el rendimiento del modelo
confusionMatrix(predicciones_svm, conjunto_prueba$label)

# Calcular y mostrar el accuracy
accuracy_svm <- mean(predicciones_svm == conjunto_prueba$label)
cat("Accuracy SVM:", accuracy_svm, "\n") # 0.114


#######################
### Modelo 3) CARET ###
#######################

# Entrenar con CARET
modelo_caret <- train(label ~ ., data = conjunto_entrenamiento, method = "rf")
predicciones_caret <- predict(modelo_caret, conjunto_prueba)

# Evaluar el rendimiento del modelo
confusionMatrix(predicciones_caret, conjunto_prueba$label)

# Calcular y mostrar el accuracy
accuracy_caret <- mean(predicciones_caret == conjunto_prueba$label)
cat("Accuracy CARET:", accuracy_caret, "\n")


##########################
### Boosting: Adaboost ###
##########################

# Creamos el modelo de boosting
modelo_boosting <- boosting(label ~., data = conjunto_entrenamiento, boos = TRUE, mfinal = 10,
                            coeflearn = 'Breiman', control = rpart.control(cp = 0.001, minsplit = 7))
predicciones_boosting = predict (modelo_boosting, newdata = conjunto_prueba)

# Evaluar el rendimiento del modelo
mc_boosting <- predicciones_boosting$confusion

# Calcular y mostrar el accuracy
accuracy_boosting <- sum(diag(mc_boosting))/(sum(mc_boosting))
cat("Accuracy Boosting:", accuracy_boosting, "\n") # 0.917


#########################
### Bagging: Adaboost ###
#########################

# Creamos el modelo de bagging
modelo_bagging <- bagging(formula = label ~., mfinal = 10, data = conjunto_entrenamiento, 
                          control = rpart.control(cp = 0.001, minsplit = 7))
predicciones_bagging <- predict (modelo_bagging, newdata = conjunto_prueba)

# Evaluar el rendimiento del modelo
mc_bagging <- predicciones_bagging$confusion

# Calcular y mostrar el accuracy
accuracy_bagging <- sum(diag(mc_bagging))/(sum(mc_bagging))
cat("Accuracy Bagging:", accuracy_bagging, "\n") # 0.844


################
### Stacking ###
################

#Creamos el dataset para el stacking fusionando las predicciones de los otros modelos y la variable label
dataset_stacking <- data.frame(predicciones_rf, predicciones_svm, predicciones_caret,label=conjunto_prueba$label)

# Creamos el modelo de stacking
modelo_stacking<-train(label~.,data=dataset_stacking,method="rf")

# Realizar predicciones en el conjunto de prueba
predicciones_stacking <- predict(modelo_stacking, conjunto_prueba)

# Evaluar el rendimiento del modelo
confusionMatrix(predicciones_stacking, conjunto_prueba$label)

# Calcular y mostrar el accuracy
accuracy_stacking <- mean(predicciones_stacking == conjunto_prueba$label)
cat("Accuracy stacking:", accuracy_stacking, "\n")


##################
### Predicción ###
##################

#metemos todos los accuracy de los modelos en una lista
lista_accuracy<-list(rf=accuracy_rf,
                   svm=accuracy_svm,
                   caret=accuracy_caret,
                   boosting=accuracy_boosting,
                   bagging=accuracy_bagging,
                   stacking=accuracy_stacking)

#guarda el indice cuyo accuracy sea mayor
ind<-which.max(lista_accuracy)

#guarda el nombre del modelo cuyo indice corresponda en la lista
mejor_modelo <- as.character(names(lista_accuracy)[ind])

cat("el mejor modelo es el ",mejor_modelo, " con un accuracy de ",lista_accuracy[[ind]])

#hacemos la prediccion de todo el set
prediccion_test <- predict(modelo_rf, test_preprocesado)
