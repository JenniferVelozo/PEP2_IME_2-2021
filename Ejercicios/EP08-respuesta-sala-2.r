################## ENUNCIADO ##################
# El conjunto de datos birthwt del paquete MASS contiene registros de 189 nacimientos en un centro
# médico de Springfield, Massachusetts, en 1986. Estos registros incluyen varias variables interesantes. Un
# equipo médico desea saber si el peso de la madre (en libras) al momento de la última menstruación varía
# de acuerdo a su raza.


# Dado que las muestras son independientes, y se busca comparar simultáneamente 
# 3 medias muestrales, se piensa utilizar ANOVA para muestras independientes. 

# Formulación de hipótesis:
# H0: El peso de las madres al momento de la última menstruación es igual para todas las razas.
# HA: El peso de las madres al momento de la última menstruación es diferente para al menos una de las razas.

# Verifiación de condiciones para usar ANOVA para muestras 
# independientes:

# 1.- La escala con que se mide la variable independiente (peso de la madre en libras),
# tiene las propiedades de una escala de intervalos iguales, ya que si para una instancia i 
# una madre tiene peso 105 libras y otra tiene peso de 100 libras, la diferencia (5 libras) es la misma
# que se presenta para una instancia j en que una madre tiene peso 155 libras y la otra 150 libras.

# 2.- Dado que las muestras provienen de un centro médico y éstas representan
# menos del 10% de la población, por lo que se puede decir que las muestras
# son obtenidas de manera aleatoria e independiente desde la población de origen.

# 3.- Dado que en el gráfico Q-Q se observan algunos valores que podrían ser atípicos,
# es mejor proceder con cautela y usar un nivel de significación alfa = 0,01.

# 4.- Al calcular la homogeneidad de las varianzas, la razón entre la máxima y la mínima
# varianza muestral de cada raza resulta ser superior a 1.5, por lo que, 
# al igual que en el caso anterior, debemos ser cautelosos y usar un alfa = 0.01.

library(MASS)
library (dplyr)
library(ggpubr)
library(ez)

# Se cargan los datos 
datos <- birthwt

datos[["race"]] <- factor(datos[["race"]])
datos[["instancia"]] <- factor(1:nrow(datos))

race_1 <- datos %>% filter(race==1)
race_2 <- datos %>% filter(race==2)
race_3 <- datos %>% filter(race==3)

muestra_race1 <- race_1[["lwt"]]
muestra_race2 <- race_2[["lwt"]]
muestra_race3 <- race_3[["lwt"]]


# Comprobación de normalidad
g <- ggqqplot(datos, x = "lwt", y ="race", color="race")

g <- g + facet_wrap(~ race)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Homogeneidad de las varianzas
var_race1 <- sd(muestra_race1)^2
print(var_race1)
var_race2 <- sd(muestra_race2)^2
print(var_race2)
var_race3 <- sd(muestra_race3)^2
print(var_race3)

varianzas <- c(var_race1, var_race2, var_race3)
homogeneidad <- max(varianzas) / min(varianzas)
cat("Homogeneidad de las varianzas", homogeneidad)


cat("\n Procedimiento ANOVA usando ezANOVA\n")
# Se utiliza type = 3, dado que las muestras no son del mismo tamaño
prueba <- ezANOVA(data = datos, dv = lwt, wid = instancia, between = race, return_aov = TRUE, type = 3)
print(prueba)

# Gráfico del tamaño del efecto
g2 <- ezPlot(data =datos, dv = lwt, wid = instancia, between = race, y_lab = "Peso promedio de la madre [libras]",
             x = race)
print(g2)

# Dado que se obtiene un p-value mucho menor a nuestro alfa = 0.01, se rechaza la hipótesis nula
# en favor de la hipótesis alternativa con un 99% de confianza. Es decir, el peso de las madres 
# al momento de la última menstruación es diferente para al menos una de las razas.
# Es por lo anterior, que se realiza un análisis POST-HOC con correcciones de Bonferroni y Holm.


alfa <- 0.01

# Procedimiento post-hoc de Bonferroni
cat("Procedimiento post-hoc de Bonferroni\n")
bonferroni <- pairwise.t.test(datos[["lwt"]], datos[["race"]], p.adj = "bonferroni", 
                              pool.sd = TRUE, paired = FALSE, conf.level = 1-alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm
cat("Procedimiento post-hoc de Holm\n")
holm <- pairwise.t.test(datos[["lwt"]], datos[["race"]], p.adj = "holm", 
                        pool.sd = TRUE, paired = FALSE, conf.level = 1-alfa)
print(holm)


####### CONCLUSIONES ####### 
# Los valores p obtenidos con el método de Bonferroni y de Holm son diferentes.
# Sin embargo, en ambos casos podemos ver que las razas 2 y 3 presentan una 
# diferencia significativa al comparar el valor p ajustado con nuestro 
# nivel de significación alfa = 0.01. Ahora, si observamos el gráfico del
# tamaño del efecto obtenido para el procedimiento ANOVA, podemos concluir con 
# un 99% de confianza que en la raza 3 el peso de la madre al momento de la 
# última menstruación es menor que para el caso de la raza 2. 

