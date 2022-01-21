library ( tidyverse )
library ( ggpubr )
library ( ez )

# Lectura del archivo 

dir <- "E:/IME/PEP2_IME_2-2021"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")

# Se piensa utilizar la prueba de ANOVA para más de dos muestras 
# correlacionadas, dado que cada soldado es evaluado por los distintos 
# oficiales evaluadores, y se pide determinar si 
# los niveles de exigencia con que los distintos oficiales evaluadores 
# califican a los snowtroopers son similares o distintos. 

# Hipótesis a contrastar
# H0: La media de la evaluación realizada por los oficiales es la misma para todos.
# HA: La media de la evaluación realizada por los oficiales es distinta para al menos uno. 

# Verificación de condiciones

# 1.- La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
# Esta condición se cumple debido a que la evaluacion de cada oficial se encuentra entre un rango de valores de 500 y 700, 
# siendo así una variable continua, dado que la diferencia que existe entre 700 y 500 es la misma entre 600 y 400

# 2.- Las mediciones son independientes al interior de cada grupo, como se ha forzado a un 
# joven analista de datos a evaluar el resultado del entrenamiento de los nuevos datos se puede 
# asumir que las mediciones son independientes debido al título que tiene el joven y tiene experiencia
# en realizar este tipo de pruebas.

# 3.- Al realizar un gráfico Q-Q para cada grupo se aperecian algunos valores atípicos en los extremos superiores e inferiores,
# por lo que se procede a ser más cautelosos y realizar la prueba con un nivel de exigencia más alto
# alfa = 0.01

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer (c("eval_instructor", "eval_capitan", "eval_comandante","eval_general"),
                                 names_to = "evaluador", values_to = "exigencia")

datos[["evaluador"]] <- factor ( datos [["evaluador"]])

# Comprobción de normalidad.
g <- ggqqplot (datos , x = "exigencia", y = "evaluador", color = "evaluador")
g <- g + facet_wrap (~ evaluador)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# 4.- La matriz de varianzas-covarianzas es esférica,
# para verificar esta condición se utiliza la  prueba de esfericidad de Mauchly que
# es entregada por la función en R ezANOVA, al analizar el p-valor = 0.9051959  mayor a nuestro nivel de significación
# por lo que podemos concluir que sí cumple la condición de esfericidad (hipótesis nula de la prueba de Mauchly).

datos[["id"]] <- factor(datos[["id"]])
# Procedimiento ANOVA con ezANOVA ().
cat ("\n\ Procedimiento ANOVA usando ezANOVA \n\n")

prueba2 <- ezANOVA ( data = datos , dv = exigencia, within = evaluador, wid = id, return_aov = TRUE )

print (prueba2)


# Gráfico del tamaño del efecto .
g2 <- ezPlot (data = datos , dv = exigencia, wid = id, within = evaluador, y_lab = "Nivel de exigencia", x = evaluador)
print ( g2 )

# Luego de realizar la prueba ezANOVA se obtiene un p-valor = 0.09673702 mayor a nuestro nivel de significación,
# se falla en rechazar la hipótesis nula. Por lo tanto con un nivel de confianza del 99% podemos concluir que la media
# de la evaluación realizada por los oficiales es la misma para todos.

# Sin embargo como el Lord Sith ha sido muy 
# claro al solicitar un reporte de aquellos oficiales cuyas 
# evaluaciones presenten diferencias, y observando el gráfico del tamaño del efecto, 
# se procede a realizar un análisis Post Hoc con correciones de Bonferroni. 

# Procedimiento post -hoc de Holm .
holm <- pairwise.t.test ( datos [["exigencia"]] , datos [["evaluador"]] ,p.adj = "holm", paired = TRUE )
cat ("\n Corrección de Holm \n")
print (holm)

# Al realizar el análisis Post Hoc y analizando el grafico del tamaño del efecto, se obtiene que 
# no existen diferencias significativas.
# Por lo que podemos concluir nuevamente con un 99%
# que la media de la evaluación realizada por los oficiales es la misma para todos.


#-----------------------------------------------PREGUNTA DOS ------------------------------------------------------------
library(pROC)
library(caret)
library(dplyr)
library(car)
library(ggpubr)

# Lectura del archivo
dir <- "E:/IME/PEP2_IME_2-2021"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")
#Crear la variable dicotómica para es_clon: 
# 1: Si 
# 0: No 
condicion <- ifelse(datos[["es_clon"]] =="S", 1,  0)
# Se tramsforma es_clon en variable categórica
datos[["es_clon"]] <- factor(condicion)

#se define la semilla con la que trabajaremos, lo cual nos permite
#trabajar siempre con los mismos números
set.seed(4432)

# se define el tamaño de la muestra
tam <- 400

# Se obtiene la muestra de 400 datos
datos <- datos[sample(nrow(datos), tam), ]



n <- nrow(datos)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos[muestra, ]
prueba <- datos[-muestra, ]

columnas <- colnames(datos)

i_es_clon <- which(columnas == "es_clon")
columnas <- columnas[-i_es_clon]

# Se seleccionan las 8 variables predictoras de forma aleatoria
variables <- sample(columnas, 8)
print(variables)

# Se escoge la variable velocidad y peso 
# ya que suponemos que un clon podría 
# ser mejor que un soldado respecto a la velocidad
# y que quizás tenga menor peso, lo cual
# implicaría que fuese más ágil y veloz.

# Ajustar modelo.
modelo <- glm(es_clon ~ peso + velocidad, family = binomial(link ="logit"),data = entrenamiento)
print(summary(modelo))


# ----------- EVALUACIÓN DEL MODELO RLM -----------
# Obtener los residuos y las estadísticas .
output <- data.frame (predicted.probabilities = fitted(modelo))
output [["standardized.residuals"]] <- rstandard(modelo)
output [["studentized.residuals"]] <- rstudent( modelo)
output [["cooks.distance"]] <- cooks.distance(modelo)
output [["dfbeta"]] <- dfbeta(modelo )
output [["dffit"]] <- dffits(modelo)
output [["leverage"]] <- hatvalues(modelo)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deberían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which (abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos1, ]) )

# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(entrenamiento)/nrow(datos)
sospechosos3 <- which(output [["leverage "]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)

cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat (round(leverage.promedio, 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(rownames(entrenamiento[sospechosos3, ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1 ,1 ,any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos4 , ]))

# Detalle de las observaciones posiblemente atí picas .
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort (unique(sospechosos))
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print(entrenamiento[sospechosos, ])
cat("\n\n")
print(output[sospechosos , ])


# -------------- VERIFICACIÓN DE CONDICIONES ----------

# Verificación de multicolinealidad .
cat ("Verificación de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\n VIF :\n")
vifs <- vif (modelo)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )
# Si miramos los factores de inflación de la varianza, 
# en general no parecen ser preocupantes, por lo que se verifica
# la condición de multicolinealidad. 

# Independencia de los residuos.
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(modelo) )



# ----------- EVALUAR EL MODELO -------

# Evaluar el modelo con el conjunto de entrenamiento
cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e , function (p) ifelse( p >= umbral , "1", "0"))
preds_e <- factor ( preds_e , levels = levels ( datos [["es_clon"]]) )

ROC_e <- roc(entrenamiento[["es_clon"]], probs_e)
plot(ROC_e)

matriz_e <- confusionMatrix(preds_e , entrenamiento [["es_clon"]])
print(matriz_e)

# Evaluar el modelo con el conjunto de prueba.

cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(modelo, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( datos [["es_clon"]]) )

ROC_p <- roc(prueba[["es_clon"]] , probs_p)
plot(ROC_p)

matriz_p <-confusionMatrix(preds_p , prueba[["es_clon"]])
print(matriz_p)




