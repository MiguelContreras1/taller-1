# PROBLEM SET 1
#Paquetes a usar
  install.packages("pacman")
  install.packages("rvest")
  library(pacman)
  library(rvest, tidyverse)

#Lectura de URL
  url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html URL chunk 1"
  browseURL(url)

url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
print(url_base)
  #[1] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
  #[2] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
  #[3] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"
  #[4] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"
  #[5] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
  #[6] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html"
  #[7] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"
  #[8] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html"
  #[9] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"
  #[10] "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html"
  

#loop para unir la base en df
  df <- data.frame()
  for (url in url_base) {
    print(url)
    temp <- read_html(url) %>%
      html_table()
    temp <- as.data.frame(temp[[1]])
    df <- rbind(df, temp)
  }
  
  head(df)

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

