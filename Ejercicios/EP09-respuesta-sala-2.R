################### PREGUNTA 1 ###################

################## ENUNCIADO ##################
# El siguiente código R carga los datos que aparecen en una tabla que compara las mejores soluciones
# encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución óptima
# conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
# investigación: ¿hay algoritmos mejores que otros?

# Dado que son muestras correlacionadas, se piensa utilizar ANOVA de una vía para
# muestras correlacionadas.

# Formulación de hipótesis:
# H0: La solución óptima encontrada es la misma para los cuatro algoritmos.
# HA: La solución óptima encontrada es diferente para al menos un algoritmo.

# Verificación de complimiento de condiciones para usar ANOVA de una vía para
# muestras correlacionadas.

# 1.- Dado que los datos no están en una escala de intervalos iguales, 
# se procede a transformarlos, dividiendo cada resultado del
# del algoritmo (es decir, calculando el error de la forma:
# (�ptimo -x )/�ptimo. De esta manera, los datos quedan en una escala de intervalos
# iguales.


# 2.- Se puede asumir que las mediciones son independientes al interior de cada grupo, ya que como
# menciona el enunciado, éstas fueron tomadas de una memoria de título del DIINF.

# 3.- Debemos comprobar que las poblaciones provienen desde una distribución
# normal, para ello se realiza un gráfico Q-Q, donde se pueden observar 
# valores atípicos, por lo tanto se debe ser cauteloso y es por ello que se
# un alfa de 0.01.

# 4.- Al realizar la prueba de ANOVA (ezANOVA) y analizando los resultados
# de la prueba de esfericidad de la prueba de Mauchly, se obtiene un p-valor
# mucho menor que cualquier nivel de significación, por lo que no se cumple
# que la matriz de varianza-covarianza es esférica, es decir, existen diferencias
# significativas entre las varianzas. 



library(tidyverse)
library(ggpubr)
library(ez)


texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 18.6 19.3 20.2 22
'brock400_4' 33 16.8 19.3 20.4 22
'C2000.9' 80 57.2 59.6 62.4 66
'c-fat500-10' 126 125 125 125 126
'hamming10-2' 512 343.2 419.1 422.4 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 34.5 43 45.5 48.2
'MANN_a81' 1100 1082.2 1082.2 1082.2 1095.9
'p-hat1500-1' 12 6.9 8.1 8.9 10
'p-hat1500-3' 94 75.8 77.7 84.6 86
'san1000' 15 7.6 7.6 7.7 10
'san400_0.7_1' 40 19.6 20.5 20.5 21
'san400_0.9_1' 100 44.1 54.4 56.4 92
'frb100-40' 100 66.4 76.7 80.5 82
'frb59-26-1' 59 39.2 45.9 48.3 48
'1et.2048' 316 232.4 268.4 280.9 292.4
'1zc.4096' 379 273.8 293.2 307.4 328.5
'2dc.2048' 24 15.6 18.7 19.9 21
")
datos <- read.table(textConnection(texto), header = TRUE)


datos[["R"]] <- abs(datos[["Optimo"]] - datos[["R"]])/datos[["Optimo"]]
datos[["R2"]] <- abs(datos[["Optimo"]] - datos[["R2"]])/datos[["Optimo"]]
datos[["R3"]] <- abs(datos[["Optimo"]] - datos[["R3"]])/datos[["Optimo"]]
datos[["G"]] <- abs(datos[["Optimo"]] - datos[["G"]])/datos[["Optimo"]]

# Llevar data frame a formato largo

datos <- datos %>% pivot_longer(c("R", "R2", "R3", "G"),
                                names_to = "algoritmo", values_to = "valor")
datos[["algoritmo"]] <- factor(datos[["algoritmo"]])

# Comprobación de normalidad
g <- ggqqplot(datos, x = "valor", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title") 
print(g)

cat("\n Procedimiento ANOVA usando ezANOVA\n")
prueba <- ezANOVA(data = datos, dv = valor, within = algoritmo, 
                  wid = Instancia, return_aov = TRUE)
print(prueba)

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = valor, wid = instancia, between = algoritmo, y_lab = "Valor óptimo obtenido",
             x = algoritmo)
print(g2)

# Dado que no se cumplió la condición de esfericidad, se considera como p-valor, el
# valor p[GG] = 5.078862e-06  que nos entrega la corrección de Greenhouse-Geisser
# porque en este caso el valor ε = 0.4947374 es menor a 0.75.
# El p-valor es menor a cualquier nivel de significación
# por lo que se rechaza la hipótesis nula. 

# Es por lo anterior, que se procede a realizar un  an�lisis  Post Hoc, con el fin de identificar
# si existen diferencias significativas entre los algoritmos y poder decidir si
# un algoritmo es mejor que otro.

alfa <- 0.01

# Procedimiento post-hoc de Bonferroni
cat("Procedimiento post-hoc de Bonferroni\n")
bonferroni <- pairwise.t.test(datos[["valor"]], datos[["algoritmo"]], p.adj = "bonferroni", 
                             paired = TRUE, conf.level = 1-alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm
cat("Procedimiento post-hoc de Holm\n")
holm <- pairwise.t.test(datos[["valor"]], datos[["algoritmo"]], p.adj = "holm", 
                        paired = TRUE, conf.level = 1-alfa)
print(holm)


############## CONCLUSIONES ##############
# De acuerdo a los resultados obtenidos y al gr�fico del tama�o del efecto,
# se puede decir que existe una diferencia significativa entre los algoritmos
# R y R3. Sin embargo, todos los p-valores son menores a nuestro nivel de 
# significaci�n, por lo que podemos concluir con 99% de confianza que 
# que  todos los algoritmos entregan una soluci�n �ptima diferente, y por 
# lo tanto, se podr�a decir que R es mejor que R3.



################### PREGUNTA 2 ###################
# El siguiente es (un resumen de) la descripción de un famoso experimento:
#  Naming the ink color of color words can be difficult. For example, if asked to name the color of
# the word "blue" is difficult because the answer (red) conflicts with the word "blue." This
# interference is called "Stroop Interference" after the researcher who first discovered the
# phenomenon. This case study is a classroom demonstration. Students in an introductory
# statistics class were each given three tasks. In the "words" task, students read the names of 60
# color words written in black ink; in the "color" task, students named the colors of 60 rectangles;
# in the "interference" task, students named the ink color of 60 conflicting color words. The times
# to read the stimuli were recorded.

# El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
# siguiente pregunta de investigación: ¿hay diferencias en los tiempos entre tareas?


# Dado que son muestras correlacionadas, se piensa utilizar ANOVA de una vía para
# muestras correlacionadas.

# Formulación de hipótesis:
# H0: El tiempo medio para leer los estímulos es igual para las tres tareas. 
# HA: El tiempo medio para leer los estímulos es distinto para al menos una tarea. 

# Verificación de complimiento de condiciones para usar ANOVA de una vía para
# muestras correlacionadas.

# 1.- Se verifica la priemra condición, puesto que el tiempo, como
# toda magnitud física, tiene una escala de intervalos iguales (de hecho tiene escala de razón).

# 2.- Se puede asumir que las mediciones son independientes al interior de cada grupo, ya que como
# menciona el enunciado, �stas provienen de un famoso experimento.

# 3.- Debemos comprobar que las poblaciones provienen desde una distribuci�n
# normal, para ello se realiza un gr�fico Q-Q, donde se puede apreciar un valor
# at�pico para el caso de la tarea "colorS", por lo que no se cumple esta condici�n.
# Es por lo anterior, que para llevar a cabo la prueba, se utiliza un alfa = 0.01.

# 4.- Al realizar la prueba de ANOVA (ezANOVA) y analizando los resultados
# de la prueba de esfericidad de la prueba de Mauchly, se obtiene un p-valor
# mucho menor que cualquier nivel de significación, por lo que no se cumple
# que la matriz de varianza-covarianza es esférica, es decir, existen diferencias
# significativas entre las varianzas. 

library(tidyverse)
library(ggpubr)
library(ez)

texto <- ("
words colors interfer
13 22 47
21 19 44
18 19 31
17 23 34
18 19 44
21 20 38
21 17 35
19 15 31
15 26 42
16 15 29
12 5 44
15 19 38
21 19 32
9 25 38
16 16 36
13 24 29
")
datos <- read.table(textConnection(texto), header = TRUE)
instancia <- factor(1:nrow(datos))
datos <- datos%>%add_column(instancia, .before = "words")

datos <- datos %>% pivot_longer(c("words", "colors", "interfer"),
                                names_to = "tarea", values_to = "tiempo")

datos[["tarea"]] <- factor(datos[["tarea"]])


# Comprobaci�n de normalidad
g <- ggqqplot(datos, x = "tiempo", y = "tarea", color = "tarea")
g <- g + facet_wrap(~ tarea)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title") 
print(g)

cat("\n Procedimiento ANOVA usando ezANOVA\n")
prueba <- ezANOVA(data = datos, dv = tiempo, within = tarea, wid = instancia,  return_aov = TRUE)
print(prueba)

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = tiempo, wid = instancia, between = tarea, y_lab = "tiempo",
             x = tarea)
print(g2)

# Dado que no se cumpli� la condici�n de esfericidad, se considera como p-valor, el
# valor p[GG] = 2.018043e-12  que nos entrega la corrección de Huynd-Feldt
# porque en este caso el valor ε = 1.092823 es mayor a 0.75.
# El p-valor es menor a cualquier nivel de significación
# por lo que se rechaza la hipótesis nula. 
# Es por lo anterior, que se procede  a realizar un análisis Post Hoc,
# con el fin de identificar si existen diferencias significativas en los
# tiempos entre las tareas

alfa <- 0.01

# Procedimiento post-hoc de Bonferroni
cat("Procedimiento post-hoc de Bonferroni\n")
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["tarea"]], p.adj = "bonferroni", 
                              paired = TRUE, conf.level = 1-alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm
cat("Procedimiento post-hoc de Holm\n")
holm <- pairwise.t.test(datos[["tiempo"]], datos[["tarea"]], p.adj = "holm", 
                        paired = TRUE, conf.level = 1-alfa)
print(holm)

########## CONCLUSIONES ##########
# De acuerdo a los resultados obtenidos y al gr�fico del tama�o del efecto, se puede apreciar que entre la tarea interfer
# y colors, y entre la tarea interfer y words, hay diferencias significativas, por lo que 
# se puede decir con un 99% de confianza que el tiempo medio para leer los est�mulos 
# para la tarea colors y words es menor que para la tarea interfer. Adem�s, el tiempo
# medio para leer  los est�mulos entre la tarea colors y words es el mismo.