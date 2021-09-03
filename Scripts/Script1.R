# Preparamos el espacio de trabajo ------------------------------------------------------------

# Instalamos paquetes (si no están en el sistema)
# install.packages("data.table")

# Cargamos paquetes
library(data.table)


# Cargamos los datos --------------------------------------------------------------------------

data <- fread(
  # Ubicación del archivo
  input = "data/Datos-proyectosMAG.csv",
)


# Transformación / Manipulación ---------------------------------------------------------------

data[j = `:=`(
  PROGRAMA = factor(PROGRAMA),
  INSTRUMENTO = factor(INSTRUMENTO),
  ANO_FALLO = factor(ANO_FALLO),
  AREA_OCDE = factor(AREA_OCDE),
  SEXO = factor(SEXO)
  
)][]

# Guardamos los datos procesados --------------------------------------------------------------


saveRDS(data, file = "data/data.RDS")


# Calculamos la ganancia de información --------------------------------------------------------------


install.packages("devtools")
install.packages("FSelector")

library(FSelector)

# get information gain results
information.gain(formula(data), data)


# Creamos la partición --------------------------------------------------------------

install.packages('caret', dependencies = TRUE)

library(caret)
# data partition
set.seed(2341)
trainIndex <- createDataPartition(data$SEXO, p = 0.80, list = FALSE)
# 80% training data
train.data <- data[trainIndex, ]
# 20% testing data
test.data <- data[-trainIndex, ]


# árbol de decisiones -----------------------------------------------------

# control parameters
trctrl <- trainControl(method = "cv", classProbs = TRUE)

set.seed(2345)
# fitting decision tree classification model
DTModel <- train(SEXO ~ ., 
                 data = train.data, 
                 method = "rpart",
                 metric = "ROC",
                 parms  = list(split = "information"), 
                 trControl = trctrl)
# model summary
DTModel