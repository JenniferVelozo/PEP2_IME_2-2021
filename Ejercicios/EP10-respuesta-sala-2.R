################### PREGUNTA 1 ################### 

# Dos artículos reportan el porcentaje de acierto alcanzado por dos algoritmos de clasificación,
# específicamente el Bayes ingenuo (C4) y el Bayes ingenuo oculto (C2), en diferentes conjuntos de prueba
# disponibles en el UCI Machine Learning Repository. ¿Es uno de los algoritmo mejor que el otro?

# H0: No se perciben diferencias entre los porcentajes de acierto entregados por los algoritmos.
# HA: Sí se perciben diferencias entre los porcentajes de acierto entregados por los algoritmos.

# Dado que las muestras a simple vista se ve que no son pareadas, pero sí independientes, se piensa
# para resolver este problema utilizar Prueba de suma de rangos de Wilcoxon para muestras grandes,
# ya que hay más de 5 muestras. Para ejecutar esta prueba es necesario cumplir con 2 condiciones previas:

#1. Se puede decir que ambas muestras son independientes, ya que se tomaron dos algoritmos diferentes
#   para hacer pruebas aleatorias disponibles en el UCI Machine Learning Repository.

#2. La escala de medición empleada es ordinal, ya que se habla en un rango de valores
#   entre 0 y 100, por lo que es posible establecer un orden en los valores,
#   además se sabe que el mejor caso es cuando el porcentaje de acierto es 100 
#   y el peor caso es cuando el porcentaje de acierto es 0. Si comparamos las muestras pueden 
#   haber porcentajes de aciertos “iguales”, “menor que”, “mayor o igual que”.


# Se cargan los datos

texto <-("
 Dataset C2 Dataset C4
 'anneal' 98.00 'cmc' 51.05
 'contact-lenses' 68.33 'credit' 86.23
 'ecoli' 80.04 'grub-damage' 47.79
 'kr-s-kp' 92.46 'monks' 62.24
 'monks1' 100.00 'mushroom' 95.83
 'nursery' 94.28 'page-blocks' 93.51
 'pasture-production' 85.83 'postoperatie' 66.67
 'primary-tumor' 48.08 'segment' 91.30
 'solar-flare-C' 88.24 'soybean' 92.08
 'squash-stored' 58.00 'squash-unstored' 61.67
 'tae' 44.38 'waveform' 79.86
 'white-clover' 79.29 -- --
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

# Hacer filtro de los datos para obtener solo los datos a utilizar en la prueba.
a <- datos[["C2"]]
b <- datos[["C4"]]

# Establecer  nivel de  significación.
alfa  <- 0.05

# Hacer la  prueba suma de rangos de Wilcoxon para muestras grandes.
# El parámetro alternative toma el valor de "greater" ya que se 
# pregunta si un algoritmo es mejor que otro.
prueba  <- wilcox.test(a, b, alternative = "greater", paired = FALSE , conf.level = 1 - alfa)
print(prueba)

# En conclusión, podemos decir que Considerando un nivel de significación alfa= 0,05 < p=0.3474, 
# se falla al rechazar la hipótesis nula en contra de la hipótesis alternativa. 
# En consecuencia, concluimos con 95 % de confianza que no se 
# perciben diferencias entre los porcentajes de acierto entregados por los algoritmos.
# Con lo anterior se puede decir que ninguno de los algoritmos es mejor que el otro.
# No se realiza un análisis Post Hoc ya que el p-value es mayor que alfa, y
# no se rechazó la hipótesis nula.


################### PREGUNTA 2 ################### 
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de los rangos con signo de Wilcoxon, debido a
# problemas con la escala de la variable dependiente en estudio. Indique cuáles serían las variables/niveles
# involucrados en su ejemplo y las hipótesis nula y alternativa a contrastar.

# Referencia: 
# https://www.pauta.cl/economia/ipc-inflacion-ine-crecimiento-economia-chile-incertidumbre#:~:text=Banco%20de%20Chile%3A%20inflaci%C3%B3n%20ser%C3%A1,5%25%20al%20cierre%20de%202021

# Respuesta
# El IPC ha alcanzado su mayor nivel durante 13 años registrando un alza anual aproximada del %6 
# lo que obligará al Banco Central a subir nuevamente la tasa de interés en el mes de Diciembre.
# Debido a esto se decide realizar un estudio, donde se eligen 15 personas al azar que hayan 
# solicitado un crédito hipotecario con tasa de interés variable durante el 2020. Para ello
# se le consulta qué tan satisfecho se encuentra con estas alzas utilizando una escala de Likert (1-5) (donde 
# 1 representa nada satisfecho y 5 muy satisfecho.


# Hipótesis a contrastar:
# H0: Las mismas personas no perciben diferencias en las alzas de la tasa de interés en el período 2020 - 2021.
# HA: Las mismas personas están más satisfechos con las alzas de la tasa de interés en el período de 2020 que la del 2021.

# Variables involucradas:
# se tiene una muestra de datos pareados
# Variables:
# Alza en la tasa de interés: variable independiente (posee dos niveles, ya que es para el año 2020 y 2021)
# Nivel de satisfacción de acuerdo al alza de la tasa de interés (Según escala de Likert): variable dependiente, debido
# a que ésta respuesta depende de cuánto le suba la tasa de interés al cliente que solicitó el crédito hipotecario.



################### PREGUNTA 3 ################### 

# El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos de clasificación en
# diferentes conjuntos de prueba disponibles en el UCI Machine Learning Repository. Los algoritmos
# corresponden a C3: averaged one-dependence estimator (AODE), C6: locally weighted naive-Bayes y C7:
# random forest. ¿Existe un algoritmo mejor o peor que los otros?

# Dado que se tienen 3 muestras correlacionadas y se pide determinar si 
# existe un algoritmo mejor o peor que los otros, se piensa utilizar 
# la prueba de Friedman.

# Formulación de hipótesis:
# H0: Los porcentajes de acierto son similares para todos los algoritmos.
# HA: Al menos uno de los algoritmos alcanza un porcentaje de acierto distinto a
# los demás.

# Cumplimento de condiciones para usar la prueba de Friedman:
# 1.- La variable independiente corresponde al algoritmo, la cual
# es categórica porque puede tomar como valores cualidades o categorías, que
# en este caso son "C3", "C6" y "C7". Con lo anterior, se puede decir además que
# tiene esos 3 niveles mencionados, por lo que se cumple la primera condición. 

# 2.- La escala de la variable dependiente (porcentaje de acierto)
# es ordinal, ya que, al ser porcentajes, sería posible ordenar los valores de
# mayor a menor. 

# 3.- Dado que las muestras provienen de un repositorio confiable, se puede 
# asumir que la muestra es aleatoria e independiente de la población.

# Como se cumplen las condiciones, se procede a usar la prueba de 
# Friedman. 

library (dplyr)
library(tidyverse)
library(ggpubr)
library(ez)

texto <- ("
Dataset C3 C6 C7
'credit' 85.07 85.22 83.33
'eucalyptus' 58.71 59.52 59.40
'glass' 73.83 75.69 73.33
'hepatitis' 83.79 82.50 81.25
'iris' 92.67 92.00 93.33
'optdigits' 96.90 94.20 91.80
'page-blocks' 96.95 94.15 96.97
'pendigits' 97.82 94.81 95.67
'pima-diabetes' 75.01 74.75 72.67
'primary-tumor' 47.49 49.55 38.31
'solar-flare-C' 88.54 87.92 86.05
'solar-flare-m' 87.92 86.99 85.46
'solar-flare-X' 97.84 94.41 95.99
'sonar' 81.26 80.79 78.36
'waveform' 84.92 83.62 79.68
'yeast' 57.74 57.48 56.26
")
datos <- read.table(textConnection(texto), header = TRUE)

# Llevamos los datos a formato largo
datos <- datos %>% pivot_longer(c("C3", "C6", "C7"),
                                names_to = "algoritmo", values_to = "porc_acierto")
datos[["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["Dataset"]] <- factor(datos[["Dataset"]])

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = porc_acierto, wid = Dataset, between = algoritmo, y_lab = "% de acierto obtenido",
             x = algoritmo)
print(g2)

prueba <- friedman.test(porc_acierto ~ algoritmo | Dataset, data = datos)
print(prueba)

# Se fija el nivel de significación
alfa <- 0.05

# Se realiza análisis post hoc en caso de que p-value sea menor que 
# el valor de alfa
if (prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$porc_acierto,
                                   datos$algoritmo,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}


# CONCLUSIONES
# El valor p = 0.00633 es menor nuestro nivel de significación alfa = 0.05,
# por lo que se rechaza la hipótesis nula y se podría decir con un 95% 
# de confianza que al menos uno de los algoritmos alcanza un porcentaje de acierto distinto a
# los demás. 
# Es por lo anterior, que se realiza un análisis post Hoc con correciones de Holm.

# Con el análisis Post Hoc se obtiene que eL p-valor obtenido entre el algoritmo 
# C3 y C7 es mucho menor que nuestro nivel de significación, por lo que se puede 
# decir que existe una diferencia significativa entre dicho algoritmos, y por ende,
# el algoritmo C3 es mejor que el C7, ya que posee un porcentaje de acierto mayor.


################### PREGUNTA 4 ################### 
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de Kruskal-Wallis, debido a problemas con la normalidad
# de los datos. Indiqué cuáles serían las variables/niveles involucrados en su ejemplo y las hipótesis nula y
# alternativa a contrastar.

# El economista Elvis Teck desea evaluar el alza de las tasas de interés de los años 2019, 2020 y 2021, 
# con el fin de determinar si existe alguna diferencia en las alzas . Para ello, ha seleccionado una muestra
# aleatoria independiente de tasas de interés de algunos meses de los años 2019, 2020, 2021.

# Año 2019: 2.76, 3.00, 3.00, 3.00, 3.00, 2.62
# Año 2020: 1.75, 1.75, 1.41, 0.50, 0.49, 0.47, 0.50, 0.35, 0.30, 0.30
# Año 2021: 0.30,	0.30,	0.54,	0.75,	1.50,	2.29,	2.75, 2.75

# Hipótesis a contrastar:
# H0: Las tasas de interés son iguales para los años 2019, 2020 y 2021.
# HA: Al menos en uno de los años se presenta una tasa de interés diferente.

# Variables involucradas:
# Alza en la tasa de interés: variable dependiente, ya que depende del año en que se encuentre el alza.
# Año: variable independiente, la cual posee 3 niveles: 2019, 2020 y 2021.

