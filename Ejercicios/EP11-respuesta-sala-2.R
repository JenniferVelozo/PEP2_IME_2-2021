# Como habíamos visto a comienzos del semestre, la Encuesta de Caracterización Socioeconómica
# Nacional, Casen, es realizada por el Ministerio de Desarrollo Social de forma periódica para conocer la
# situación de los hogares chilenos con relación a aspectos demográficos, de educación, salud, vivienda,
# trabajo e ingresos. Es la principal fuente de información para estimar la magnitud de la pobreza y la
# distribución del ingreso en el país.
# Se pone a disposición el archivo Datos-Casen-v2.xls, con un subconjunto de los datos obtenidos en la
# Casen 2017. El equipo debe revisar las columnas disponibles en este archivo según la descripción en el
# libro de códigos de la encuesta, que también queda disponible para este ejercicio. Es importante notar que
# en esta encuesta hay datos de carácter colectivo sobre “el hogar” del entrevistado, pero también hay
# datos de carácter individual, que se refieren “al jefe o la jefa de hogar” (no al entrevistado).

# ------------------------ DEFINICIÓN DE FUNCIONES ----------------------------
# Función para calcular la diferencia de medias.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# Valor :
# - diferencia E_1 - E _2.
calcular_diferencia <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# Valor :
# - diferencia E_1 - E _2.
permutar <- function(muestra_1, muestra_2, FUN){
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2, replace = FALSE)
  permutacion_1 <- permutacion[1 : n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))    
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar.
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales.
# Valor :
# - el valorp calculado .

calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if (alternative == "two.sided"){
    numerador <- sum (abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador 
  }
  else if(alternative == "greater"){
    numerador <- sum(abs(distribucion) > valor_observado) + 1
    denominador <- repeticiones
    valor_p <- numerador / denominador
  }
  else{
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.

graficar_distribucion <- function(distribucion, ...){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion", xlab = "Estadístico de interés", ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar.
# - FUN : función del estadístico E para el que se calcula la diferencia.
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater"  o "less" para hipótesis unilaterales.
# - plot : si es TRUE , construye el gráfico de la distribución generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2, repeticiones, FUN, alternative, plot, ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado:", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  
  cat("Valor p: ", valor_p, "\n\n")
}


# Función para hacer emuestreo usando bootstrapping
my_boot <- function(x){
  # Se toma una muestra con reemplazo para cada grupo
  i_de_hecho <- sample(1:n_de_hecho, replace = TRUE) 
  i_legal <- sample(1:n_legal, replace = TRUE)
  i_no_tiene <- sample(1:n_no_tiene, replace = TRUE)
  rbind(de_hecho[i_de_hecho,], legal[i_legal,], no_tiene[i_no_tiene,])
}

# Función para obtener el estadístico F 
my_F <- function(frame){
  anova <- ezANOVA(frame, dv = edad, between = pareja, 
                   wid = instancia, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}

# Función para generar la distribuciones de la diferencia de medias a
# partir de las permutaciones.
distribucion_diferencias <- function(permutaciones, columna_1, columna_2){
  R <- length(permutaciones)
  distribucion <- c()
  for(i in 1:R){
    datos <- as.data.frame(permutaciones[i])
    muestra_1 <- datos %>% filter(pareja == columna_1)
    muestra_2 <- datos %>% filter(pareja == columna_2)
    diferencia <- calcular_diferencia(muestra_1[["edad"]], muestra_2[["edad"]], mean)
    distribucion <- c(distribucion, diferencia)
  }
  return(distribucion)
}
# ------------------ Pregunta 1 ------------------
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.

# Enunciado:
# La investigadora Elsa Pato desea saber si la edad media de las personas 
# de la Región Metropolitana es la misma para aquellas que alcanzaron un Nivel Profesional Incompleto
# y los que alcanzaron Técnico Nivel Superior Completo.

# Para resolver este problema se piensa utilizar prueba de permutaciones para 
# comparar una variable continua de dos muestras independientes con P = 1999
# y un alfa = 0.05.

# Estadístico de interés: la media de la edad de las personas
# Hipótesis a contrastar
# H0: La edad media de las personas de la Región Metropolitana es la misma para las personas  que alcanzaron un Nivel Profesional Incompleto
# y los que alcanzaron Técnico Nivel Superior Completo

# HA: La edad media de las personas de la Región Metropolitana es distinta para las personas  que alcanzaron un Nivel Profesional Incompleto
# y los que alcanzaron Técnico Nivel Superior Completo

# Denotando como μA al promedio de las edades de las personas que alcanzaron un Nivel Profesional Incompleto en la Región
# Metropolitana, y μB al promedio de las edades de las personas que alcanzaron ETécnico Nivel Superior Completo
# en la Región Metropolitana, entonces matemáticamente las hipótesis quedan expreadas como:
# H0: μA - μB = 0 
# HA: μA - μB ≠ 0 

library(readxl)
library(dplyr)
library(ggpubr)

set.seed(523) # semilla inicial

#Fijamos un nivel de significación 
alpha <- 0.05

datos <- read_excel("E:/IME/Datos-Casen-v2.xls")

# Se obtienen las personas que viven en la Región de Tarapacá
rm <- datos %>% filter(region == "Región Metropolitana de Santiago")

# Se obtienen las personas que alcanzaron nivel profesional incompleto
prof_incompleto <- rm %>% filter(e6a == "Profesional Incompleto (Carreras 4  o más años)")

#Se obtienen las personas que alcanzaron nivel Técnico Nivel Superior Completo
tecnico_completo <- rm %>% filter(e6a == "Técnico Nivel Superior Completo (Carreras 1 a 3 años)")

# Se obtiene sólo la columna que indica la edad de la persona
edad_prof_incompleto <- prof_incompleto[["edad"]]
edad_tecnico_completo <- tecnico_completo[["edad"]]

# Se obtienen las muestras
muestra_prof_incompleto <- sample(edad_prof_incompleto, 260)
muestra_tecnico_completo <- sample(edad_tecnico_completo, 300)

R =  1999

contrastar_hipotesis_permutaciones(muestra_prof_incompleto, muestra_tecnico_completo, repeticiones = R, FUN = mean, alternative = "two.sided", plot = TRUE, color = "blue", fill = "blue")


# CONCLUSIONES
# Dado que se obtiene un valor p = 0.0015 < alfa = 0.05, se rechaza la hipótesis
# nula. Por lo tanto, se puede concluir con un 95% de confianza que la edad media de las personas de 
# la Región Metropolitana es distinta para las personas  que alcanzaron un nivel Profesional Incompleto
# y los que alcanzaron nivel Técnico Nivel Superior Completo.

# También se puede apreciar en los gráficos, formados a partir de permutaciones, que los datos tienen 
# una distribución normal.


# ------------------ Pregunta 2 ------------------
# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.

# Enunciado:
# El investigador Armando Casas desea saber si la edad media de las personas de la Región de Tarapacá
# es la misma para aquellos que legalizan su relación, los que tienen pareja de hecho y 
# los que no tienen pareja en su hogar.

# Para este caso se piensa utilizar la técnica de remuestreo de 
# bootstrapping para más de dos muestras independientes, creando una función que haga esto,
# usando alfa = 0.05 y una cantidad de remuestreos P = 1000.

# HIPÓTESIS A CONTRASTAR
# H0: La edad media de las personas de la Región de Tarapacá es la misma para aquellos
#    que legalizan su relación, los que tienen pareja de hecho y 
#    los que no tienen pareja.

# HA: La edad media de las personas de la Región de Tarapacá es diferente al menos en un grupo (aquellos
#    que legalizan su relación, los que tienen pareja de hecho y los que no tienen pareja)


library(readxl)
library(dplyr)
library(ggpubr)
library(boot)
library(ez)

datos <- read_excel("E:/IME/Datos-Casen-v2.xls")

# Se obtienen las personas que viven en la Región de Tarapacá
tarapaca <- datos %>% filter(region == "Región de Tarapacá")

set.seed(523)
# Se obtiene la muestra de tamaño 500
tamano <- 500
muestra <- tarapaca[sample(nrow(tarapaca), tamano),]
edad <- muestra[["edad"]]
pareja <- factor(muestra[["pareja"]])
instancia <- factor(1:tamano)
datos2 <- data.frame(instancia, edad, pareja)

de_hecho <- datos2 %>% filter(pareja == "De hecho")
n_de_hecho <- nrow(de_hecho)

legal <- datos2 %>% filter(pareja == "Legal")
n_legal <- nrow(legal)

no_tiene <- datos2 %>% filter(pareja == "No tiene pareja en el hogar")
n_no_tiene <- nrow(no_tiene)


# Se obtiene el estadístico F original
anova_original <- ezANOVA(datos2, dv = edad, between = pareja, 
                          wid = instancia, return_aov = FALSE)
print(anova_original)

# Establecer cantidad de remuestreos 
R <- 1000
set.seed(456) # segunda semilla


# lapply guarda en lista
distribucion1  <- lapply(1:R, my_boot)

# sapply guarda en un vector
suppressMessages(suppressWarnings(Fs <- sapply(distribucion1, my_F))) # evitar los warnings


p <- calcular_valor_p(Fs, anova_original$ANOVA$F, R, "two.sided")
print(p)

# CONCLUSIONES

# Usando un alpha de 0.05, en consecuencia se obtiene un valor p = 0.01098901 , p < alpha,
# entonces se rechaza la hipótesis nula en favor de la hipótesis alternativa.
# Con un 95% de confianza se puede afirmar que la edad media de las personas de la 
# Región de Tarapacá es diferente al menos en un grupo (aquellos
# que legalizan su relación, los que tienen pareja de hecho y los que no tienen pareja)

# Además como se rechaza la hipótesis nula
# se debe realizar un Análisis Post Hoc

# Análisis Post Hoc


# Se calculan las diferencias observadas entre cada par de muestras
dif_obs_legal_de_hecho <- calcular_diferencia(legal[["edad"]], de_hecho[["edad"]], mean)
dif_obs_legal_no_tiene <- calcular_diferencia(legal[["edad"]], no_tiene[["edad"]], mean)
dif_obs_de_hecho_no_tiene <- calcular_diferencia(de_hecho[["edad"]], no_tiene[["edad"]], mean)

# Generar distribuciones para diferencias entre pares a partir de las
# permutaciones
dif_legal_de_hecho <- distribucion_diferencias(distribucion1, "Legal", "De hecho")
dif_legal_no_tiene <- distribucion_diferencias(distribucion1, "Legal", "No tiene pareja en el hogar")
dif_de_hecho_no_tiene <- distribucion_diferencias(distribucion1, "De hecho", "No tiene pareja en el hogar")

# Obtener valores p
num1 <- sum(abs(dif_legal_de_hecho) > abs(dif_obs_legal_de_hecho) + 1)
den1 <- R + 1
p_legal_de_hecho <- num1/den1

num2 <- sum(abs(dif_legal_no_tiene) > abs(dif_obs_legal_no_tiene) + 1)
den2 <- R + 1
p_legal_no_tiene <- num2/den2

num3 <- sum(abs(dif_de_hecho_no_tiene) > abs(dif_obs_de_hecho_no_tiene) + 1)
den3 <- R + 1
p_de_hecho_no_tiene <- num3/den3

# Gráfico del tamaño del efecto para observar las medias de las edades
g2 <- ezPlot(data =datos2, dv = edad, wid = instancia, between = pareja, y_lab = "Media de las edades",
             x = pareja)
print(g2)


# CONCLUSIONES FINALES
# Al realizar el análisis Post Hoc, podemos notar que todos los grupos presentan diferencias
# significativas, obteniendo los siguientes valores p:
# p valor entre tiene una relación De Hecho con No tiene pareja = 0.2927
# p valor entre tiene una relación Legal con De Hecho = 0.2637
# p valor entre tiene una relación Legal con No tiene pareja = 0.2707

# Por lo tanto, se puede concluir con un 95% de confianza que la edad media 
# es distinta para aquellas personas que legalizan su relación, 
# las que tienen pareja de hecho y las que no tienen pareja en el hogar.

# Además, si se observa el gráfico del tamaño del efecto, es posible notar 
# la gran diferencia que hay entre cada par de grupo, destacando que aquellas personas 
# que tienen alrededor de 50 años tienden a legalizar su relación, siendo esta mayor 
# que la edad media de las personas que no tienen pareja y aquellos que tienen pareja de hecho.