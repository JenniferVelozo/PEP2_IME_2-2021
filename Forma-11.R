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

