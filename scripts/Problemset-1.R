# PROBLEM SET 1
#hola
# limpiar
rm(list=ls())
# cargar librerías
library(pacman)
library(rvest, tidyverse)
# cargar bases y unirlas
# código Laura
# cambiar nombre de variable p6040 a "edad"
edad <- p6040
# cambiar nombre de variable p6500 a "ingreso"
ingreso <- p6050
# cambiar por 0 todas las observaciones < 18, en p6040.
# crear variable "edad2"
edad2 <- (edad^2)
# dat = cbind(edad, edad2, ingreso)
dat = cbind(edad, edad2, ingreso)
# chequear variables con skim(dat) 
skim(dat)
# correr regresión 
lm(ingreso ~ edad edad2, data = dat)
# asignar nombre mod1 a regresión 
mod1 = lm(ingreso ~ edad edad2, data = dat)
# ver coeficiente de la regresión
lm_summary = summary(mod1)$coefficients
lm_summary_print = lm_summary
# R base output # summary(mod1)
# ggplot de la gráfica de la clase de Lucas, cambiar nombres variables y las labs por edad (eje x) e ingreso (eje y)
# crear variable de logaritmo de p6500 y cambiar nombre a "logingreso"
loging <- log(ingreso)
# # cambiar nombre de variable p6020 a "sexo"
sexo <- p6050

