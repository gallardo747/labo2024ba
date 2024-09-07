# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("yaml")
require("ranger")
require("randomForest") # solo se usa para imputar nulos

# Carga los hiperparámetros desde el archivo HT3740.txt
parametros_ht <- fread("~/buckets/b1/exp/HT3740/HT3740.txt")

# Loop desde 1 hasta 116, una iteración por cada conjunto de parámetros
for (i in 1:17) {
  
  # Extrae los parámetros de la fila correspondiente del archivo TXT
  PARAM <- list()
  PARAM$experimento <- 3720
  
  # Asigna los valores de los parámetros para el modelo desde el archivo TXT
  PARAM$ranger <- list(
    "num.trees" = parametros_ht$num.trees[i], 
    "mtry" = parametros_ht$mtry[i], 
    "min.node.size" = parametros_ht$min.node.size[i], 
    "max.depth" = parametros_ht$max.depth[i]
  )
  
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  
  setwd("~/buckets/b1/") # Establezco el Working Directory
  
  # creo la carpeta donde va el experimento
  dir.create("./exp/", showWarnings = FALSE)
  carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
  dir.create(paste0("./exp/KA", PARAM$experimento, "/"), showWarnings = FALSE)
  
  setwd(carpeta_experimento)
  
  # Cargo miAmbiente
  miAmbiente <- read_yaml("~/buckets/b1/miAmbiente.yml")
  
  # Cargo los datos
  dataset <- fread(miAmbiente$dataset_pequeno)
  
  # Asigno un valor muy negativo para Data Drifting
  if ("Master_Finiciomora" %in% colnames(dataset)) {
    dataset[is.na(Master_Finiciomora), Master_Finiciomora := -999]
  }
  
  if ("Visa_Finiciomora" %in% colnames(dataset)) {
    dataset[is.na(Visa_Finiciomora), Visa_Finiciomora := -999]
  }
  
  # Defino donde entreno y donde aplico el modelo
  dtrain <- dataset[foto_mes == 202107]
  dapply <- dataset[foto_mes == 202109]
  
  set.seed(miAmbiente$semilla_primigenia) # Establezco la semilla aleatoria
  
  # Ranger necesita la clase de tipo factor
  factorizado <- as.factor(dtrain$clase_ternaria)
  dtrain[, clase_ternaria := factorizado]
  
  # Imputo los nulos, ya que ranger no acepta nulos
  dtrain <- na.roughfix(dtrain)
  
  setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA
  
  # Genero el modelo de Random Forest llamando a ranger()
  modelo <- ranger(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    probability = TRUE, # para que devuelva las probabilidades
    num.trees = PARAM$ranger$num.trees,
    mtry = PARAM$ranger$mtry,
    min.node.size = PARAM$ranger$min.node.size,
    max.depth = PARAM$ranger$max.depth
  )
  
  # Carpintería necesaria sobre dapply
  dapply[, clase_ternaria := NULL]
  dapply <- na.roughfix(dapply)
  
  # Aplico el modelo recién creado a los datos del futuro
  prediccion <- predict(modelo, dapply)
  
  # Genero la entrega para Kaggle
  entrega <- as.data.table(list(
    "numero_de_cliente" = dapply[, numero_de_cliente],
    "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
  )) # genero la salida
  
  # Genero un nombre único para el archivo de Kaggle en cada iteración
  nom_arch_kaggle <- paste0("KA3720_", sprintf("%03d", i), ".csv")
  
  # Genero el archivo para Kaggle
  fwrite(entrega, file = nom_arch_kaggle, sep = ",")
  
  # Preparo todo para el submit
  comentario <- paste0(
    "'",
    "num.trees=", PARAM$ranger$num.trees,
    " mtry=", PARAM$ranger$mtry,
    " min.node.size=", PARAM$ranger$min.node.size,
    " max.depth=", PARAM$ranger$max.depth,
    "'"
  )
  
  comando <- paste0(
    "~/install/proc_kaggle_submit.sh ",
    "TRUE ",
    miAmbiente$modalidad, " ",
    nom_arch_kaggle, " ",
    comentario
  )
  
  ganancia <- system(comando, intern = TRUE)
  
  cat(paste0(ganancia, "\t", nom_arch_kaggle, "\n"), file = "tb_ganancias.txt", append = TRUE)
  
}
