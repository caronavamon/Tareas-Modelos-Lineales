library(readr)
install.packages("ipred")
install.packages("caret")
library(caret)

datos <- read_delim("C:/Users/Caro/Downloads/Student Mental health.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
datos <- datos[, -c(1,6)]
datos <- as.data.frame(lapply(datos, as.factor))

modelo.logit <- glm(Do.you.have.Depression. ~ ., data = datos, family = "binomial")
summary(modelo.logit)

# Análisis odd ratios

# Se puede interpretar que una persona con ansiedad, cuyo parámetro es
#   0.3180 , tiene e^   0.3180  ≈ 1.374376 veces más de probabilidad de tener depresión que de no
# con base al resultado de la regresión.

# Se puede interpretar que una persona que estudia matemática, cuyo parámetro es
# -40.1405, tiene e^ -40.1405 ≈ 3.691496e-18 veces más de probabilidad de tener depresión que de no
# con base al resultado de la regresión.

# Se puede interpretar que una persona que tiene 2 años de estudio en la universidad, cuyo parámetro es
#   1.3035, tiene e^  1.3035 ≈ 3.682162 veces más de probabilidad de tener depresión que de no
# con base al resultado de la regresión.

# Se puede interpretar que una persona que tiene 23 años de edad, cuyo parámetro es
#  40.5080 , tiene e^40.5080 ≈ 3.912018e+17 veces más de probabilidad de tener depresión que de no
# con base al resultado de la regresión.

# Se puede interpretar que un universitario hombre, cuyo parámetro es
#   -0.9400 , tiene e^ -0.9400 ≈ 0.3906278 veces más de probabilidad de tener depresión que de no
# con base al resultado de la regresión


# Matriz de confusión

# Predecir las clases
predicciones <- predict(modelo.logit, datos, type = "response")

# Convertir las probabilidades predichas en clases binarias
predicciones_clases <- ifelse(predicciones > 0.5, "Yes", "No")

# Crear la matriz de confusión
confusion_matrix <- confusionMatrix(as.factor(predicciones_clases), datos$Do.you.have.Depression.)

# Ver la matriz de confusión
print(confusion_matrix)


# Curva Roc 


#Crear el gráfico de la identidad
plot(c(0, 1), c(0, 1), type = "l", xlab = "1-Especificidad", ylab = "Recall", main = "Curva Roc")
abline(a = 0, b = 1, col = "blue")  # Línea de identidad (y = x)

# Agregar el punto aleatorio al gráfico
points(1-0.8857,0.9692, col = "red", pch = 19)

# Dado que el punto Roc está por encima de la recta, significa que es un buen modelo.