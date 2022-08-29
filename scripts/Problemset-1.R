# PROBLEM SET 1 
#Paquetes a usar
  install.packages("pacman")
  install.packages("rvest")
  install.packages("dplyr")
  install.packages("tibble")
  install.packages("skimr")
  library(skimr)
  library(tibble)
  library(dplyr)
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
  
#Restrinjirlo solo para age>=18 y empleado 
  base=subset(df, df$age>17)
  base=subset(base, base$p6240==1) #¿ocu (2mil +) o p6240? con ocu se tienen personas ocupadas no remuneradas

  
#FIltro de variables 
  #Caracteristicas persona
      #college
      #Educlevel 
      #age
      #estrato1
      #sex
      #regSalud
      #cotPension
  #Variables ingreso 
      #p6585S2a2 Subsidio de transporte
      #p6630S1a1 Prima de servicios (12 meses)
      #p7510S6a1 cesantías (12 meses)
      #p6630S2a1 prima navidad (12 meses)
      #p6500 ingreso antes de descuentos cuánto ganó el mes pasado en este empleo 
      #p6585S3 Subsidios 
      #impa ingreso monetario de la primera actividad antes de imputación
      #isa ingreso monetario de la segunda actividad antes de imputación
      #ingtotes ingreso total imputado
      #ingtot ingreso total
      #ingtotob Ingreso total observado
      #y_salary_m 
      #y_ingLab_m
      #y_total_m
      #y_total_m_ha
      #p6500 ingreso antes de descuentos cuánto ganó el mes pasado en este empleo 
      #p6585S3 Subsidios 
      #p6590S1 
      #p6585S2a2 Subsidio de transporte
      #p6240 actividad qué ocupó la mayor parte del tiempo la semana pasada
    
  #Características trabajo/empresa
      #sizeFirm
      #microempresa
      #oficio (qué hace)
      #hoursWorkActualSecondJob
      #hoursWorkUsual
      #informal
      #relab
  
  #Missings  
    #individuo
    sum(is.na(base$estrato1)) #0
    sum(is.na(base$maxEducLevel)) #1
    sum(is.na(base$college)) #0
    sum(is.na (base$regSalud)) #1155
    sum(is.na(base$cotPension )) #0
    
    #trabajo
    sum(is.na(base$sizeFirm)) #0
    sum(is.na(base$microempresa)) #0
    sum(is.na(base$oficio)) #0
    sum(is.na(base$hoursWorkActualSecondJob))#13632
    sum(is.na(base$hoursWorkUsual))#0
    sum(is.na(base$informal)) #0
    sum(is.na(base$relab))#0
   
    #Ingreso 
    sum(is.na(base$age)) #0
    sum(is.na(base$sex)) #0
    sum(is.na(base$p6500)) #4535
    sum(is.na(base$ingtot)) #0
    sum(is.na(base$maxEducLevel)) #1
    sum(is.na(base$y_total_m)) #1265
    
    #Eliminarlos /reemplazarlos 
    base2 = subset(x = df, subset = is.na(age)==FALSE) #eliminarlo 
    base2<- mutate_if(base$maxEducLevel, is.integer, ~replace(., is.na(.), 0))
    is.integer(base$maxEducLevel)
   
    
    
     #Análisis descriptivo  
    
variables=data.frame(base$edad, base$college ,base$Educlevel  ,base$age,base$estrato1,base$sex, base$regSalud,
                     base$cotPension, base$ingtot, base$sizeFirm, 
                     base$microempresa ,base$oficio, base$hoursWorkActualSecondJob, base$hoursWorkUsual,
                     base$informal ,base$relab)

summary (base$edad, base$college ,base$Educlevel  ,base$age,base$estrato1,base$sex, base$regSalud,
         base$cotPension, base$ingtot, base$sizeFirm, 
         base$microempresa ,base$oficio, base$hoursWorkActualSecondJob, base$hoursWorkUsual,
         base$informal ,base$relab)

# crear variable "edad2"
edad2 <- (edad^2)
base = cbind(edad2)
# chequear variables con skim(dat) 
    skim(base)
    base %>%
      dplyr::group_by(sex) %>%
      skim()
    descriptivas<-summary(base)
      view(descriptivas)
    
# correr regresión 
  regresion1<-lm(ingtot ~ age , data = base)

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

#Errores bootstrap
    install.packages("boot")
    boot(data, statistic, R)
    
    eta.fn<-function(data,index)f
    coef(lm(consumption~price+income, data = data, subset = index))
    
    boot(data = gas, statistic = eta.fn, R = 1000)
    