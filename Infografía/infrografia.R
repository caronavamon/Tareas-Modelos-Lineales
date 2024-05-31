library(readr)
library(readxl)
library(caret)
library(pROC)
library(ggplot2)
library(ROCR)
library(precrec)
library(ggrepel)

#-----------------------------------Datos---------------------------------------

datos <- read_excel("Salud mental estudiantes.xlsx")
datos <- datos[, -c(1,6)]

colnames(datos) <- c("Género", "Edad", "Carrera", "Año de carrera", 
                     "Estado civil", "Depresión", "Ansiedad", 
                     "Ataques de pánico", "Buscar especialista")

# Rellenar los valores NA en la columna "Edad" con la media
media_edad <- mean(datos$Edad, na.rm = TRUE)
datos$Edad[is.na(datos$Edad)] <- round(media_edad)

# Convertir a factores las columnas 
datos <- as.data.frame(lapply(datos, as.factor))

#--------------------------------Modelo-----------------------------------------

# Se realiza el modelo logístico
modelo.logit <- glm(Depresión ~ ., data = datos, family = "binomial")
summary(modelo.logit)

#-----------------------------Matriz de confusión-------------------------------

# Predecir las clases
predicciones <- predict(modelo.logit, datos, type = "response")
  

# Encontrar el umbral adecuado de acuerdo al índice de Youden
obj_roc <- roc(datos$Depresión, predicciones, auc=T, ci=T)
umbral <- coords(obj_roc, "best", 
                      ret = c("threshold", "sensitivity", "specificity"), 
                      best.method="youden")

# Convertir las probabilidades predichas en clases binarias de acuerdo al índice 
# encontrado. 
predicciones_clases <- ifelse(predicciones > 0.2943312, "Yes", "No")

# Crear la matriz de confusión
matriz_confusion <- confusionMatrix(as.factor(predicciones_clases), datos$Depresión)
print(matriz_confusion)

#-----------------------------Curva Roc----------------------------------------- 

curva_roc <- evalmod(scores = predicciones, labels = datos$Depresión)
graf_curva_roc <- autoplot(curva_roc)

# Añadir el punto óptimo a la curva ROC
TFP <- 1 - umbral$specificity
recall <- umbral$sensitivity

# Gráfico curva ROC
graf_curva_roc[[1]] + geom_point(aes(x = TFP, y = recall), color = "red", size = 1) +
  geom_text(aes(x = TFP, y = recall, label = sprintf("(%.2f, %.2f)", TFP, recall)), 
                  size = 3, nudge_y = -0.05, nudge_x = 0.20) +
  ggtitle("Curva ROC") +
  xlab("1 - Especificidad") +
  ylab("Sensibilidad")
  
# Dado que el punto Roc está por encima de la recta, significa que es un buen 
# modelo.

