# PROBLEM SET 1 
#PUNTO 1
      #Paquetes a usar
        install.packages("pacman")
        install.packages("rvest")
        install.packages("dplyr")
        install.packages("tibble")
        install.packages("skimr")
        install.packages("ggplot2")
        install.packages("keep")
        install.packages("WRS2")
        install.packages("psych")
        install.packages("rapportools")
        install.packages("apply")
        install.packages("lapply")
        library(WRS2)
        library(rapportools)
        library(ggplot2)
        library(skimr)
        library(tibble)
        library(dplyr)
        library(pacman)
        library(rvest, tidyverse)
        library(keep)
        #
      
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
        base=subset(base, base$ocu==1) #¿ocu (2mil +) o p6240? con ocu se tienen personas ocupadas no remuneradas
        #base2=subset(base, ocu) #¿ocu (2mil +) o p6240? con ocu se tienen personas ocupadas no remuneradas
        #base2=subset(base2, age>17)
        base=base[,-c(1)]
        #lista <- as.list(c(base$college, base$Educlevel, base$age, base$estrato1, base$sex, base$regSalud, base$cotPension, base$ingtot, base$sizeFirm, base$microempresa, base$oficio, base$hoursWorkActualSecondJob, base$hoursWorkUsual, base$informal, base$relab))
        #lista2 <- as.list(c('college', 'Educlevel', 'age', 'estrato1', 'sex', 'regSalud', 'cotPension', 'ingtot', 'sizeFirm','microempresa', 'oficio', 'hoursWorkActualSecondJob', 'hoursWorkUsual', 'informal', 'relab'))
        #lista3 <- c(college, maxEducLevel, age, estrato1, sex, regSalud, cotPension, ingtot, sizeFirm, microempresa, oficio, hoursWorkActualSecondJob, hoursWorkUsual, informal, relab)
       
        base2 <- select(base,college, maxEducLevel, age, estrato1, sex, regSalud, cotPension, ingtot, sizeFirm, microEmpresa, oficio, hoursWorkActualSecondJob, hoursWorkUsual, informal, relab )
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
          sum(is.na(base2$estrato1)) #0
          sum(is.na(base2$maxEducLevel)) #1
          sum(is.na(base2$college)) #0
          sum(is.na (base2$regSalud)) #1420
          sum(is.na(base2$cotPension )) #0
          
          #trabajo
          sum(is.na(base2$sizeFirm)) #0
          sum(is.na(base2$microempresa)) #0
          sum(is.na(base2$oficio)) #0
          sum(is.na(base2$hoursWorkActualSecondJob))#15980
          sum(is.na(base2$hoursWorkUsual))#0
          sum(is.na(base2$informal)) #0
          sum(is.na(base2$relab))#0
         
          #Ingreso 
          sum(is.na(base2$age)) #0
          sum(is.na(base2$sex)) #0
          sum(is.na(base2$p6500)) #4535
          sum(is.na(base2$ingtot)) #0
          sum(is.na(base2$maxEducLevel)) #1
          sum(is.na(base2$y_total_m)) #0
          
          #Eliminarlos /reemplazarlos 
          base2 = subset(x = base2, subset = is.na(maxEducLevel)==FALSE) #eliminarlo
          
          
          base2$regSalud = ifelse(is.na(base2$regSalud)==T,0,base2$regSalud)
          base2$hoursWorkActualSecondJob = ifelse(is.na(base2$hoursWorkActualSecondJob)==T,0,base2$hoursWorkActualSecondJob)
      
          table(base2$regSalud, base2$maxEducLevel)
          table(base2$regSalud, base2$age)
          table(base2$regSalud, base2$sex)
          table(base2$regSalud, base2$cotPension)
          table(base2$regSalud, base2$maxEducLevel)
          
          table(base2$hoursWorkActualSecondJob, base2$sex)
          table(base2$hoursWorkActualSecondJob, base2$age)
          table(base2$hoursWorkActualSecondJob, base2$maxEducLevel)
      
          
          
           #Análisis descriptivo
          
          data(base2)
          describe(base2)
          
          
          
          ingreso <- (as.data.frame(summary(base2))) ; ingreso
          output <- capture.output(ingreso, file=NULL, append =FALSE)
          output_ad <-as.data.frame(output) #convertir summary en tabla
          write.table(x = output_ad, file = "summary.xlsx", sep = " ", 
                      row.names = FALSE, col.names = TRUE)
          
          
          #outliers
          
          rp.outlier(base2[base2$ingtot=="2 Pints", "attractiveness"])
          is.numeric(base2$ingtot)
          
          
          #diferencia de medias
          
          dmedias<- t.test (base2$ingtot ~ base2$sex ) ;dmedias
          Grafico_dmedias <- boxplot(base2$ingtot ~ base2$sex, col= "gray", xlab ="sexo", ylab = "ingreso total")
          
          gp <-  ggplot() + geom_histogram(data = base2,aes(x=ingtot));gp
          g<- plot(base2$age, base2$ingtot)
          
          grafico1 <- ggplot() + geom_histogram(data = unioncabecera, aes(x=P6020)) + 
            ylab("Cantidad") + xlab("Sexo") + ggtitle("Cantidad de personas segun el sexo")+ 
            scale_x_discrete(limit = c("Hombre", "Mujer"))
          
          #varianzas
          lapply(base2[])
          
          base2 %>% var()
         
          
          
          var(base2$ingtot)
          
          ## Media de ingresos
          # por sexo
          a <- base2 %>% group_by(sex) %>% summarize(mean(base$ingtot,na.rm = T));a
          # por edad
          b <- base2 %>% group_by(age) %>% summarize(mean(ingtot,na.rm = T));b 
          # por estrato
          c <- base2 %>% group_by(estrato1) %>% summarize(mean(ingtot,na.rm = T));c
       
          #graficas
          
          #grafico de dispersi?n del ingreso promedio por sexo
          grafico1 <- plot(a, main = "ingreso promedio por sexo", xlab = "sexo", ylab = "Ingreso promedio", pch = 21,  bg = "yellow", col = "red", cex = 1, lwd = 2)
          ggsave(plot= grafico1 , file = "views/Grafico22.jpeg") # puedes agregar los temas predeterminados para mejorar la apariencia dle grafico
          
          #Grafica de ingreso promedio por edad
          grafico2 <- plot(b,type="h",main = "ingreso promedio por edad", xlab = "Edad", ylab = "Ingreso promedio", col = "Darkblue",lwd=2, ylim=c(0,9000000),xlim=c(15,85))
          ggsave(plot= grafico2 , file = "views/Grafico33.jpeg") # puedes agregar los temas predeterminados para mejorar la apariencia dle grafico
          
          
          # crear variable "edad2"
          edad2 <- as.data.frame((base2$age^2))
          base2 <- cbind(base2, edad2)
          
          
          names(base2)[names(base2)=='(base2$age^2)']<- 'age2'
          
          # correr regresión 
          regresion1 <- lm(ingtot ~ age+age2, data= base2); regresion1
          summary(regresion1)
          
                "Residuals:
                  Min       1Q   Median       3Q      Max 
                -2161715 -1080907  -546251    55041 83828662 
                
                Coefficients:
                             Estimate Std. Error     t    value Pr(>|t|)    
                (Intercept) -436662.9   178347.2  -2.448     0.0144 *  
                  age         91143.5     8886.4  10.256    < 2e-16 ***
                  age2         -799.3      102.9  -7.771    8.24e-15 ***
                  ---
                  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                
                Residual standard error: 2653000 on 16539 degrees of freedom
                Multiple R-squared:  0.01716,	Adjusted R-squared:  0.01704 
                F-statistic: 144.4 on 2 and 16539 DF,  p-value: < 2.2e-16"
          
          # ver coeficiente de la regresión
          lm_summary = summary(regresion1)$coefficients
          lm_summary_print = lm_summary
          
          plot(base2$ingtot, base2$age, xlab='Edad', ylab='Igreso total')
          abline(regresion1)
          #Intervalos de confianza 
          predict(regresion1)
          confint(regresion1)
                        "     2.5 %    97.5 %
              (Intercept) -786242.586  -87083.27
              age           73725.129  108561.79
              age2          -1000.862  -597.66"
          
          #Análisis de varianza 
          anova(regresion1)
          "Response: ingtot
                        Df     Sum Sq    Mean Sq F value    Pr(>F)    
          age           1 1.6071e+15 1.6071e+15 228.375 < 2.2e-16 ***
          age2          1 4.2495e+14 4.2495e+14  60.388 8.244e-15 ***
          Residuals 16539 1.1638e+17 7.0370e+12                      
          ---
            Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
          
          # ggplot de la gráfica de la clase de Lucas, cambiar nombres variables y las labs por edad (eje x) e ingreso (eje y)
          
#PUNTO 2 
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
              
    
#3ER PUNT
    #tabla de correlación
    correlacion <- (as.data.frame(cor(base2))) ; correlacion
    output_corr <- capture.output(correlacion, file=NULL, append =FALSE)
    output_corr <-as.data.frame(output_corr) #convertir summary en tabla
    write.table(x = output_corr, file = "summary.xlsx", sep = " ", 
                row.names = FALSE, col.names = TRUE)
    
    #preguntas
    
    #como exportar
    #outliers
    #varianza
    #missing salud
    
    