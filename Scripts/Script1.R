# Preparamos el espacio de trabajo ------------------------------------------------------------

# Instalamos paquetes (si no están en el sistema)
# install.packages("data.table")

# Cargamos paquetes
library(data.table)

# Cargamos los datos --------------------------------------------------------------------------

data <- fread(
  # Ubicación del archivo
  input = "data/Datos-proyectosMAG.csv",
)[, -c("INSTRUMENTO")]

lapply(data, unique)

# Asignar NA's a sexos sin información
data[SEXO == "SIN INFORMACION", SEXO := NA][]

# Transformación / Manipulación ---------------------------------------------------------------

data[j = `:=`(
  PROGRAMA = factor(PROGRAMA),
  CODIGO_PROYECTO = factor(CODIGO_PROYECTO),
  # INSTRUMENTO = factor(INSTRUMENTO),
  ANO_FALLO = factor(ANO_FALLO),
  AREA_OCDE = factor(AREA_OCDE),
  SEXO = factor(SEXO)
)][]

data <- na.omit(data)

# Guardamos los datos procesados --------------------------------------------------------------

saveRDS(data, file = "data/data.RDS")

# Calculamos la ganancia de información --------------------------------------------------------------

install.packages("devtools")
install.packages("FSelector")

library(FSelector)

# get information gain results
information.gain(formula(data), data)

# Creamos la partición --------------------------------------------------------------

install.packages('caret')

library(caret) 

# Set seed for reproducibility
set.seed(12345)
# data partition
ind <- createDataPartition(data$SEXO, p = 0.80, list = FALSE)
# 80% training data
train.data <- data[ind, ]
# 20% testing data
test.data <- data[-ind, ]

# árbol de decisiones -----------------------------------------------------

# control parameters
trctrl <- trainControl(method = "cv", classProbs = TRUE)

set.seed(12345)
# fitting decision tree classification model
DTModel <- train(SEXO ~ ., 
                 data = train.data, 
                 method = "rpart",
                 metric = "ROC",
                 parms  = list(split = "information"), 
                 trControl = trctrl) |> suppressWarnings()

# Resumen del modelo
print(DTModel)

# Graficamos el arbol de descisión
library(rpart.plot)
prp(DTModel$finalModel, box.palette = "Reds", tweak = 1.2, varlen = 20)

# Graficamos la importancia de las variables
my_plot <- plot(varImp(DTModel))

# Exploramos el PDF radaptado en tamaño
pdf(file = "output/model-1.pdf", width = 7, height = 70); print(my_plot); dev.off()

# Probando el modelo en el dataset de prueba
PredDTModel <- predict(DTModel, newdata = test.data, type = "prob")

# Comprobamos cuales son las filas del test.data en los cuales las probabildad de ser mujer
# es mayor al 50%
test.data[which(PredDTModel$MUJER > 0.5), table(SEXO)]

# Graficamos las probabilidades del modelo
plot(PredDTModel$MUJER, 
     main = "Scatterplot of Probabilities of being a woman (test data)", 
     xlab = "ID", 
     ylab = "Predicted Probability of being a woman")

# Evaluamos el cut-off del 50% para ser mujer
pred.DT <-  fifelse(PredDTModel$MUJER > 0.50, "MUJER", "HOMBRE")
# Guardamos los valores de la predicción en un vector 
Pred <- as.factor(pred.DT)

# ordering the vectors
Predicted <- ordered(Pred, levels = c("MUJER", "HOMBRE"))
Actual <- ordered(test.data$SEXO, levels = c("MUJER", "HOMBRE"))

# making confusion matrix
cm <-confusionMatrix(table(Predicted,Actual))

print(cm)


# Evaluamos el desempeño del modelo a diferentes puntos de corte ------------------------------

CmFn <- function(cutoff) {
  
 # predicting the test set results
   PredDTModel <- predict(DTModel, test.data,type = "prob")
   C1 <- fifelse(PredDTModel$MUJER > cutoff, "MUJER", "HOMBRE")
   C2 <- test.data$SEXO
   predY <- as.factor(C1)
   actualY <- as.factor(C2)
   Predicted <- ordered(predY, levels = c("MUJER", "HOMBRE"))
   Actual <-  ordered(actualY, levels = c("MUJER", "HOMBRE"))
  # use the confusionMatrix from the caret package
  cm1 <-confusionMatrix(data = Predicted,reference = Actual, positive = "MUJER")
  # extracting accuracy
  Accuracy <- cm1$overall[1]
  # extracting sensitivity
    Sensitivity <- cm1$byClass[1]
  # extracting specificity
    Specificity <- cm1$byClass[2]
  # extracting value of kappa
    Kappa <- cm1$overall[2]
    
  # combined table
    tab <- cbind(Accuracy,Sensitivity,Specificity,Kappa)
    
  return(tab)
}

# sequence of cut-off points        
cutoff1 <- seq( 0, 1, by = .05 )

# loop using "lapply"
tab2    <- lapply(cutoff1, CmFn)

# creating matrix of different metrics
numrows = length(cutoff1)
pm <- matrix(1:numrows*4, nrow = numrows, ncol=4)

#  applying for loop
for (i in 1:numrows){
  pm[i,] = tab2[[i]]}
pm <- as.data.frame(pm)
pm <- cbind(cutoff1, pm)
pm <- dplyr::rename(pm, cutoff =  cutoff1, Accuracy = V1, 
               Senstivity = V2 ,Specificity = V3, kappa = V4)
# printing the table
print(pm)

install.packages("ROCR")
library(ROCR)
    
DTPrediction <- predict(DTModel, test.data,type = "prob")
Prediction <- prediction(DTPrediction[2],test.data$SEXO)
performance <- performance(Prediction, "tpr","fpr")

# plotting ROC curve
plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

DTPrediction <- predict(DTModel, test.data,type = "prob")
Prediction <- prediction(DTPrediction[2],test.data$SEXO)
aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT
