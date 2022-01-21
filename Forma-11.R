# Lectura del archivo 

dir <- "E:/IME/PEP2_IME_2-2021"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos<-read.csv2(arch, fileEncoding = "UTF-8")