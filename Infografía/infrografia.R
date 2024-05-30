library(readr)
library(readxl)
install.packages("ipred")
install.packages("caret")
library(caret)
library(pROC)
library(ggplot2)
library(ROCR)
library(precrec)

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

modelo.logit <- glm(Depresión ~ ., data = datos, family = "binomial")
summary(modelo.logit)

# Matriz de confusión

# Predecir las clases
predicciones <- predict(modelo.logit, datos, type = "response")
  
# Convertir las probabilidades predichas en clases binarias

obj_roc <- roc(datos$Depresión, predicciones, auc=T, ci=T)
best_thresh <- coords(obj_roc, "best", 
                      ret = c("threshold", "sensitivity", "specificity"), 
                      best.method="youden")
predicciones_clases <- ifelse(predicciones > 0.2943312, "Yes", "No")

# Crear la matriz de confusión
confusion_matrix <- confusionMatrix(as.factor(predicciones_clases), datos$Depresión)

# Ver la matriz de confusión
print(confusion_matrix)

# Curva Roc 

precrec_obj <- evalmod(scores = predicciones, labels = datos$Depresión)
p <- autoplot(precrec_obj)

# Añadir el punto óptimo a la curva ROC
fpr_best <- 1 - best_thresh$specificity
tpr_best <- best_thresh$sensitivity

# Añadir el punto al gráfico
p[[1]] + geom_point(aes(x = fpr_best, y = tpr_best), color = "red", size = 1) +
  ggtitle("Curva ROC") +
  xlab("1 - Especificidad") +
  ylab("Sensibilidad")
  
# Dado que el punto Roc está por encima de la recta, significa que es un buen 
# modelo.