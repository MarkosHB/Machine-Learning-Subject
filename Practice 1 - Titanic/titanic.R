###################################
###### Practica 1 - Titanic  ######
### Autor: Marcos Hidalgo Baños ###
###################################

library(nnet) # Perceptron
library(rpart) # Arbol de decision
library(e1071) # svm
library(pROC) # Curva ROC
library(caret) # Validacion cruzada

entrenamiento = "/home/marcos/Documents/RStudio/TITANIC/train.csv"
testeo = "/home/marcos/Documents/RStudio/TITANIC/test.csv"

##################################################
#### Apartado A) Preprocesamiento del dataset ####
##################################################

data_titanic = read.csv(entrenamiento, stringsAsFactors = TRUE)
data_titanic = na.omit(data_titanic) #eliminar datos faltantes

test_titanic = read.csv(testeo, stringsAsFactors = TRUE)
test_titanic = na.omit(test_titanic) #eliminar datos faltantes

#eliminamos las columnas no necesarias
data_titanic$Name = NULL
data_titanic$PassengerId = NULL
data_titanic$Ticket = NULL
data_titanic$Cabin = NULL
data_titanic$Embarked = NULL

test_titanic$Name = NULL
test_titanic$PassengerId = NULL
test_titanic$Ticket = NULL
test_titanic$Cabin = NULL
test_titanic$Embarked = NULL

# Ajustamos la clase a predecir a su tipo
data_titanic$Survived = as.factor(data_titanic$Survived)

# Normalizacion de los datos
exclude_columns <- c("Survived")
data_titanic_normalized <- preProcess(data_titanic[, !(names(data_titanic) %in% exclude_columns)], method = c("center", "scale"))
data_titanic[, !(names(data_titanic) %in% exclude_columns)] <- predict(data_titanic_normalized, newdata = data_titanic[, !(names(data_titanic) %in% exclude_columns)])

#validacion cruzada
set.seed(123)
indexes = createDataPartition(data_titanic$Survived, p = 0.8, list=FALSE)
dtrain = data_titanic[indexes,]
dtest = data_titanic[-indexes,]

#################################################################
#### Apartado B) Pruebas entre los diferentes clasificadores ####
#################################################################

# --------------------------- #
# Clasificador 1 - Perceptron #
# --------------------------- #
Perceptron = list(clasificador = nnet(Survived ~ ., data = dtrain, size = 2),
                  pred = NULL, matrizconfusion = NULL, roc = NULL, accuracy = NULL)

Perceptron$pred = predict(Perceptron$clasificador, dtest)
Perceptron$matrizconfusion = table(Perceptron$pred, dtest$Survived)
Perceptron$roc = auc((dtest$Survived==1)*1, Perceptron$pred[,1])
Perceptron$accuracy = sum(diag(Perceptron$matrizconfusion)) / sum(Perceptron$matrizconfusion)

Perceptron$accuracy #Como es de esperar, es muy bajo

# ---------------------- #
# Clasificador 2 - rpart #
# ---------------------- #
Rpart = list(clasificador = rpart(Survived ~. , data = dtrain),
             pred = NULL, matrizconfusion = NULL, roc = NULL, accuracy = NULL)

Rpart$pred = predict(Rpart$clasificador, dtest)
Rpart$matrizconfusion = table(Rpart$pred[,1], dtest$Survived)
Rpart$roc = auc((dtest$Survived==1)*1, Rpart$pred[,1])
Rpart$accuracy = sum(diag(Rpart$matrizconfusion)) / sum(Rpart$matrizconfusion)

Rpart$accuracy #Como es de esperar, es muy bajo

# -------------------- #
# Clasificador 3 - svm #
# -------------------- #
Svm = list(clasificador = svm(Survived ~. , data = dtrain, kernel="linear"),
           pred = NULL, matrizconfusion = NULL, roc = NULL, accuracy = NULL)

Svm$pred = predict(Svm$clasificador, dtest)
Svm$matrizconfusion = table(Svm$pred, dtest$Survived)
Svm$roc = auc((dtest$Survived==1)*1, as.numeric(as.character(Svm$pred)))
Svm$accuracy = sum(diag(Svm$matrizconfusion)) / sum(Svm$matrizconfusion)

Svm$accuracy #Bastante buen valor para ser la primera prueba

#################################################################
#### Apartado C) Obtener el mejor clasificador de cada clase ####
#################################################################

# -------------------------------------------------- #
# Clasificador 1 - Perceptron (mejor valor del size) #
# -------------------------------------------------- #
obtenerMejorSize = function(mayorTam) {
  mejorSize = 0; mejorAccuracy = 0; mejorPerceptron = NULL;
  
  for (sizeN in 1:mayorTam) {
    perceptron =  nnet (Survived ~. , data=dtrain, size=sizeN, trace=FALSE)
    pred = predict(perceptron, dtest)
    matrizconfusion = table(pred, dtest$Survived)
    accuracy = sum(diag(matrizconfusion)) / sum(matrizconfusion)
    
    if (accuracy > mejorAccuracy) {
      mejorPerceptron = perceptron
      mejorSize = sizeN
      mejorPred = pred
      mejorRoc = auc((dtest$Survived==1)*1, mejorPred[,1])
      mejorMatriz = matrizconfusion
      mejorAccuracy = accuracy
    }
  }
  
  return(list(clasificador = mejorPerceptron, 
              pred = mejorPred,
              matrizconfusion = mejorMatriz, 
              roc = mejorRoc,
              accuracy = mejorAccuracy,
              size = mejorSize))
}

mejorPerceptron = obtenerMejorSize(mayorTam=100)
mejorPerceptron$accuracy #Algo mejor, pero insufuciente
mejorPerceptron$size

# ------------------------------------------ #
# Clasificador 2 - rpart (mejor valor de cp) #
# ------------------------------------------ #
obtenerMejorCP = function(modeloRpart) {
  
  indiceminxerror = which.min(modeloRpart$cptable[, "xerror"])
  mejorCP = modeloRpart$cptable[indiceminxerror, "CP"]
  mejorModelo = prune(modeloRpart, cp = mejorCP)
  predicion = predict(mejorModelo, dtest)
  matrizconfusion = table(predicion[,1], dtest$Survived)
  roc = auc((dtest$Survived==1)*1, predicion[,1])
  accuracy = sum(diag(matrizconfusion)) / sum(matrizconfusion)
  
  return(list(clasificador = mejorModelo,
              pred = predicion,
              matrizconfusion = matrizconfusion,
              roc = roc,
              accuracy = accuracy,
              cp = mejorCP))
}

mejorRpart = obtenerMejorCP(Rpart$clasificador)
mejorRpart$accuracy #Se mejora, pero tampoco es suficiente

# ------------------------------------------- #
# Clasificador 3 - svm (mejor tipo de kernel) #
# ------------------------------------------- #
obtenerMejorKernel = function() {
  
  # Entrenar un modelo SVM con kernel lineal
  modelo_lineal = svm(Survived ~ ., data = dtrain, kernel = "linear")
  matrizConfusionLinealSVM = table(predict(modelo_lineal,dtest), dtest$Survived)
  accuracyLinealSVM = sum(diag(matrizConfusionLinealSVM))/sum(matrizConfusionLinealSVM)
  
  # Entrenar un modelo SVM con kernel radial con tune.svm()
  modelo_radial = tune.svm(Survived ~ ., data = dtrain, kernel = "radial", scale = TRUE ,gamma = 10^(-6:-1), cost = 10^(-1:1))
  mejores_parametros_radial = modelo_radial$best.parameters
  modelo_radial = modelo_radial$best.model
  
  matrizconfusionRadialSVM=table(predict(modelo_radial,dtest), dtest$Survived)
  accuracyRadialSVM= sum(diag(matrizconfusionRadialSVM))/sum(matrizconfusionRadialSVM)
  
  # Entrenar un modelo SVM con kernel polinómico con tune.svm()
  modelo_pol = tune.svm(Survived ~ ., data = dtrain, kernel = "polynomial", degree = 1:3, cost = 10^(-1:1))
  mejores_parametros_pol = modelo_pol$best.parameters
  modelo_pol = modelo_pol$best.model
  
  matrizconfusionPolSVM=table(predict(modelo_pol,dtest), dtest$Survived)
  accuracyPolSVM= sum(diag(matrizconfusionPolSVM))/sum(matrizconfusionPolSVM)
  
  # Entrenar un modelo SVM con kernel sigmoide con tune.svm()
  modelo_sig = tune.svm(Survived ~ ., data = dtrain, kernel = "sigmoid", gamma = 10^(-6:-1), cost = 10^(-1:1))
  mejores_parametros_sig = modelo_sig$best.parameters
  modelo_sig = modelo_sig$best.model
  
  matrizconfusionSigSVM=table(predict(modelo_sig,dtest), dtest$Survived)
  accuracySigSVM= sum(diag(matrizconfusionSigSVM))/sum(matrizconfusionSigSVM)
  
  modelos = list(modelo_lineal, modelo_radial, modelo_pol, modelo_sig)
  accuracy = c(accuracyLinealSVM, accuracyRadialSVM, accuracyPolSVM, accuracySigSVM)
  parametros = list(NULL, mejores_parametros_radial, mejores_parametros_pol, mejores_parametros_sig)
  nombres = c("Lineal", "Radial", "Polinómico", "Sigmoide")
  index = which.max(accuracy)
  
  return(list(clasificador = modelos[index],
              nombre = nombres[index],
              parametros = parametros[index],
              accuracy = accuracy[index]))
}

mejorSvm = obtenerMejorKernel()
mejorSvm$nombre
mejorSvm$accuracy #Con mucha diferencia, el mejor

###################################################################
#### Apartado D) Prediccion con el mejor clasificador de todos ####
###################################################################

# Vemos cual de todos los modelos es el mejor
modelos = list(mejorPerceptron = mejorPerceptron, mejorSvm = mejorSvm, mejorRpart = mejorRpart)
nombre_mejor_modelo = names(modelos)[which.max(sapply(modelos, function(modelo) modelo$accuracy))]
mejor_modelo = modelos[[which.max(sapply(modelos, function(modelo) modelo$accuracy))]]

# Mostramos por pantalla los resultados
print(paste("El mejor modelo es:", nombre_mejor_modelo, "-->",mejor_modelo$nombre, "| Accuracy =", mejor_modelo$accuracy))

# Realizamos la prediccion definitiva con el dataset de testeo
pred_test = predict(mejor_modelo$clasificador, test_titanic, type="class")[[1]]

# Añadimos la nueva columna con la prediccion
test_titanic$Survived = pred_test

